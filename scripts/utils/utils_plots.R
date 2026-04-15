plot_entropy_map_facet <- function(
    df_entropy,
    sf_bassins,
    id_col = "HYBAS_ID",
    entropy_col = "entropy",
    year_col = "YYYY",
    # Period selection (same format as YYYY, e.g., 19930101)
    period_start = NULL,
    period_end   = NULL,
    # Facet grouping:
    # "YYYY" uses the raw YYYY values; "year" extracts the year (1993, 1994, ...)
    facet_by = c("year", "YYYY"),
    # Optional: aggregate within each facet group if duplicates exist
    # (e.g., multiple runs per basin-year)
    agg = c("none", "mean", "median", "min", "max"),
    # Plot options
    palette = "viridis",
    na_fill = "grey85",
    border_color = NA,
    border_size = 0.1,
    ncol = NULL,
    title = "Entropy maps",
    subtitle = NULL,
    caption = NULL
) {
  # ---- Dependencies ----
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required.", call. = FALSE)
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.", call. = FALSE)
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.", call. = FALSE)
  if (!requireNamespace("rlang", quietly = TRUE)) stop("Package 'rlang' is required.", call. = FALSE)
  if (!requireNamespace("viridis", quietly = TRUE)) stop("Package 'viridis' is required.", call. = FALSE)

  facet_by <- match.arg(facet_by)
  agg <- match.arg(agg)

  # ---- Validation ----
  if (!is.data.frame(df_entropy)) stop("`df_entropy` must be a data.frame/tibble.", call. = FALSE)
  if (!inherits(sf_bassins, "sf")) stop("`sf_bassins` must be an sf object.", call. = FALSE)

  miss_df <- setdiff(c(id_col, entropy_col, year_col), names(df_entropy))
  if (length(miss_df)) stop("Missing columns in df_entropy: ", paste(miss_df, collapse = ", "), call. = FALSE)
  if (!id_col %in% names(sf_bassins)) stop("`id_col` not found in sf_bassins.", call. = FALSE)

  id_sym <- rlang::sym(id_col)
  ent_sym <- rlang::sym(entropy_col)
  yr_sym <- rlang::sym(year_col)

  # ---- Filter by period ----
  x <- df_entropy %>%
    dplyr::mutate(
      .yyy = as.numeric(!!yr_sym),
      .entropy_num = as.numeric(!!ent_sym)
    )

  if (!is.null(period_start)) x <- dplyr::filter(x, .yyy >= as.numeric(period_start))
  if (!is.null(period_end))   x <- dplyr::filter(x, .yyy <= as.numeric(period_end))

  if (nrow(x) == 0) stop("No rows left after period filtering.", call. = FALSE)

  # ---- Build facet variable ----
  if (facet_by == "year") {
    # Extract year from YYYY-like integer (19930101 -> 1993)
    x <- x %>% dplyr::mutate(.facet = floor(.yyy / 10000))
  } else {
    x <- x %>% dplyr::mutate(.facet = .yyy)
  }

  # ---- Optional aggregation per basin within facet ----
  # Ensures 1 entropy value per basin per facet (required for a clean join)
  if (agg != "none") {
    fun <- switch(
      agg,
      mean   = function(x) mean(x, na.rm = TRUE),
      median = function(x) stats::median(x, na.rm = TRUE),
      min    = function(x) min(x, na.rm = TRUE),
      max    = function(x) max(x, na.rm = TRUE)
    )

    x <- x %>%
      dplyr::group_by(!!id_sym, .facet) %>%
      dplyr::summarise(entropy_agg = fun(.entropy_num), .groups = "drop")
  } else {
    # keep as-is but rename consistently
    x <- x %>%
      dplyr::transmute(!!id_sym, .facet, entropy_agg = .entropy_num)
  }

  # ---- Expand sf across facets and join ----
  facets_tbl <- dplyr::distinct(x, .facet)

  sf_long <- sf_bassins %>%
    dplyr::mutate(.tmp_join = 1) %>%
    dplyr::left_join(dplyr::mutate(facets_tbl, .tmp_join = 1), by = ".tmp_join") %>%
    dplyr::select(-.tmp_join)

  sf_long <- sf_long %>%
    dplyr::left_join(x, by = c(setNames(id_col, id_col), ".facet" = ".facet"))

  # ---- Subtitle auto ----
  if (is.null(subtitle)) {
    subtitle <- paste0(
      "Period: ",
      ifelse(is.null(period_start), "-Inf", period_start),
      " to ",
      ifelse(is.null(period_end), "Inf", period_end),
      " | Facet by: ", facet_by,
      if (agg != "none") paste0(" | Aggregation: ", agg) else ""
    )
  }

  # ---- Plot ----
  p <- ggplot2::ggplot(sf_long) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = entropy_agg),
      color = border_color,
      linewidth = border_size
    ) +
    scale_fill_gradient(low = RColorBrewer::brewer.pal(9,name="Reds")[2],
                        high =RColorBrewer::brewer.pal(9,name="Reds")[5],
                        name="Entropy"  )+
    ggplot2::facet_wrap(~.facet, ncol = ncol) +
    # ggplot2::labs(
    #   title = title,
    #   subtitle = subtitle,
    #   caption = caption
    # ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      panel.grid.major = ggplot2::element_line(linewidth = 0.1),
      strip.text = ggplot2::element_text(face = "bold")
    )

  list(sf_map = sf_long, plot = p, data = x)
}



plot_basin_metric_map <- function(
    df,
    sf_bassins,
    id_col = "HYBAS_ID",
    value_col,
    # Optional grouping column for faceting (NOT necessarily time)
    group_col = NULL,
    facet = FALSE,
    ncol = NULL,
    # Optional aggregation if multiple rows per basin (and per group)
    agg = c("none", "mean", "median", "min", "max"),
    # Fill scale
    fill_scale = c("gradient", "viridis"),
    legend_title = NULL,
    na_fill = "grey85",
    border_color = NA,
    border_size = 0.1,
    # gradient options
    gradient_palette = c("Reds", 2, 5),
    # viridis options
    viridis_option = "viridis",
    # labels
    title = NULL,
    subtitle = NULL,
    caption = NULL
) {
  # ---- deps ----
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required.", call. = FALSE)
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.", call. = FALSE)
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.", call. = FALSE)
  if (!requireNamespace("rlang", quietly = TRUE)) stop("Package 'rlang' is required.", call. = FALSE)

  agg <- match.arg(agg)
  fill_scale <- match.arg(fill_scale)

  # ---- validation ----
  if (!is.data.frame(df)) stop("`df` must be a data.frame/tibble.", call. = FALSE)
  if (!inherits(sf_bassins, "sf")) stop("`sf_bassins` must be an sf object.", call. = FALSE)
  if (!id_col %in% names(df)) stop("`id_col` not found in df.", call. = FALSE)
  if (!id_col %in% names(sf_bassins)) stop("`id_col` not found in sf_bassins.", call. = FALSE)
  if (missing(value_col) || !is.character(value_col) || length(value_col) != 1L) {
    stop("`value_col` must be a single column name (character).", call. = FALSE)
  }
  if (!value_col %in% names(df)) stop("`value_col` not found in df.", call. = FALSE)

  if (!is.null(group_col)) {
    if (!is.character(group_col) || length(group_col) != 1L) stop("`group_col` must be NULL or a single column name.", call. = FALSE)
    if (!group_col %in% names(df)) stop("`group_col` not found in df.", call. = FALSE)
  }

  if (isTRUE(facet) && is.null(group_col)) {
    stop("If `facet = TRUE`, you must provide `group_col`.", call. = FALSE)
  }

  if (is.null(legend_title)) legend_title <- value_col

  id_sym <- rlang::sym(id_col)
  val_sym <- rlang::sym(value_col)
  grp_sym <- if (!is.null(group_col)) rlang::sym(group_col) else NULL

  # ---- prepare ----
  x <- df %>%
    dplyr::mutate(
      .id = as.character(!!id_sym),
      .value = as.numeric(!!val_sym),
      .facet = if (is.null(group_col)) "all" else as.character(!!grp_sym)
    )

  # ---- aggregate if needed ----
  if (agg != "none") {
    fun <- switch(
      agg,
      mean   = function(v) mean(v, na.rm = TRUE),
      median = function(v) stats::median(v, na.rm = TRUE),
      min    = function(v) min(v, na.rm = TRUE),
      max    = function(v) max(v, na.rm = TRUE)
    )

    x <- x %>%
      dplyr::group_by(.id, .facet) %>%
      dplyr::summarise(value_agg = fun(.value), .groups = "drop")
  } else {
    x <- x %>%
      dplyr::transmute(.id, .facet, value_agg = .value)
  }

  # ---- expand sf across facets (only if needed) ----
  facets_tbl <- dplyr::distinct(x, .facet)

  sf_long <- sf_bassins %>%
    dplyr::mutate(.tmp_join = 1) %>%
    dplyr::left_join(dplyr::mutate(facets_tbl, .tmp_join = 1), by = ".tmp_join") %>%
    dplyr::select(-.tmp_join)

  # join using basin id
  sf_long <- sf_long %>%
    dplyr::mutate(.id = as.character(.data[[id_col]])) %>%
    dplyr::left_join(x, by = c(".id" = ".id", ".facet" = ".facet"))

  # ---- auto subtitle ----
  if (is.null(subtitle)) {
    subtitle <- paste0(
      if (is.null(group_col)) "No grouping" else paste0("Group: ", group_col),
      if (agg != "none") paste0(" | Aggregation: ", agg) else ""
    )
  }

  # ---- plot ----
  p <- ggplot2::ggplot(sf_long) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = value_agg),
      color = border_color,
      linewidth = border_size
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      panel.grid.major = ggplot2::element_line(linewidth = 0.1),
      strip.text = ggplot2::element_text(face = "bold")
    )

  if (fill_scale == "viridis") {
    if (!requireNamespace("viridis", quietly = TRUE)) stop("Package 'viridis' is required for fill_scale='viridis'.", call. = FALSE)
    p <- p + viridis::scale_fill_viridis(option = viridis_option, na.value = na_fill, name = legend_title)
  } else {
    if (!requireNamespace("RColorBrewer", quietly = TRUE)) stop("Package 'RColorBrewer' is required for fill_scale='gradient'.", call. = FALSE)
    brewer_name <- gradient_palette[[1]]
    low_idx <- gradient_palette[[2]]
    high_idx <- gradient_palette[[3]]
    pal <- RColorBrewer::brewer.pal(9, name = brewer_name)
    p <- p + ggplot2::scale_fill_gradient(
      low = pal[low_idx],
      high = pal[high_idx],
      na.value = na_fill,
      name = legend_title
    )
  }

  if (!is.null(title) || !is.null(subtitle) || !is.null(caption)) {
    p <- p + ggplot2::labs(title = title, subtitle = subtitle, caption = caption)
  }

  if (isTRUE(facet) && !is.null(group_col)) {
    p <- p + ggplot2::facet_wrap(~.facet, ncol = ncol)
  }

  list(sf_map = sf_long, plot = p, data = x)
}


add_obs_class_from_historical_quantiles <- function(
    df,
    id_col = "HYBAS_ID",
    q_col = "Q",
    time_col = "YYYY",
    hist_start_date,
    hist_end_date,
    probs = c(0.25, 0.75),
    class_levels = c("below", "normal", "above"),
    keep_thresholds = TRUE,
    suffix = ""
) {
  # ---- Dependencies ----
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.", call. = FALSE)
  if (!requireNamespace("rlang", quietly = TRUE)) stop("Package 'rlang' is required.", call. = FALSE)

  # ---- Validation ----
  if (!is.data.frame(df)) stop("`df` must be a data.frame/tibble.", call. = FALSE)
  needed <- c(id_col, q_col, time_col)
  miss <- setdiff(needed, names(df))
  if (length(miss)) stop("Missing columns in df: ", paste(miss, collapse = ", "), call. = FALSE)

  if (length(probs) != 2 || any(!is.finite(probs)) || any(probs <= 0) || any(probs >= 1) || probs[1] >= probs[2]) {
    stop("`probs` must be a numeric vector of length 2 in (0,1) with probs[1] < probs[2], e.g. c(0.25, 0.75).",
         call. = FALSE)
  }
  if (!is.finite(hist_start_date) || !is.finite(hist_end_date)) {
    stop("`hist_start_date` and `hist_end_date` must be provided as numeric YYYYMMDD-like values.", call. = FALSE)
  }
  if (as.numeric(hist_start_date) > as.numeric(hist_end_date)) {
    stop("`hist_start_date` must be <= `hist_end_date`.", call. = FALSE)
  }

  id_sym <- rlang::sym(id_col)
  q_sym <- rlang::sym(q_col)
  t_sym <- rlang::sym(time_col)

  # output column names
  q25_name <- paste0("Q25", suffix)
  q75_name <- paste0("Q75", suffix)
  obs_name <- paste0("obs_class", suffix)

  x <- df %>%
    dplyr::mutate(
      .id = as.character(!!id_sym),
      .q = as.numeric(!!q_sym),
      .t = as.numeric(!!t_sym)
    )

  # ---- Compute basin-specific historical thresholds ----
  hist <- x %>%
    dplyr::filter(.t >= as.numeric(hist_start_date), .t <= as.numeric(hist_end_date))

  if (nrow(hist) == 0) {
    stop("No historical rows found in the provided historical period.", call. = FALSE)
  }

  thr <- hist %>%
    dplyr::group_by(.id) %>%
    dplyr::summarise(
      !!q25_name := stats::quantile(.q, probs = probs[1], na.rm = TRUE, names = FALSE),
      !!q75_name := stats::quantile(.q, probs = probs[2], na.rm = TRUE, names = FALSE),
      .groups = "drop"
    )

  # sanity check
  thr_bad <- thr %>%
    dplyr::filter(!is.finite(.data[[q25_name]]) | !is.finite(.data[[q75_name]]) | .data[[q25_name]] >= .data[[q75_name]])
  if (nrow(thr_bad) > 0) {
    stop("Invalid thresholds for at least one basin (non-finite Q25/Q75 or Q25 >= Q75).", call. = FALSE)
  }

  # ---- Join thresholds back and compute obs_class ----
  x2 <- x %>%
    dplyr::left_join(thr, by = c(".id" = ".id")) %>%
    dplyr::mutate(
      !!obs_name := dplyr::case_when(
        is.na(.q) ~ NA_character_,
        .q < .data[[q25_name]] ~ "below",
        .q > .data[[q75_name]] ~ "above",
        TRUE ~ "normal"   # includes Q25 <= Q <= Q75
      ),
      !!obs_name := factor(.data[[obs_name]], levels = class_levels)
    )

  # ---- Return ----
  out <- x2 %>% dplyr::select(-.id, -.q, -.t)

  if (!isTRUE(keep_thresholds)) {
    out <- out %>% dplyr::select(-dplyr::all_of(c(q25_name, q75_name)))
  }

  out
}



# ---- Predicted vs Observed class maps (facet-wrap) ----
# Assumes your sf object looks like: YYYY, HYBAS_ID, pred_class, obs_class, geometry
# Packages
stopifnot(requireNamespace("dplyr", quietly = TRUE))
stopifnot(requireNamespace("tidyr", quietly = TRUE))
stopifnot(requireNamespace("ggplot2", quietly = TRUE))
stopifnot(requireNamespace("sf", quietly = TRUE))

plot_pred_obs_class_maps <- function(
    sf_class,
    id_col = "HYBAS_ID",
    time_col = "YYYY",
    pred_col = "pred_class",
    obs_col  = "obs_class",
    period_start = NULL,
    period_end   = NULL,
    facet_by = c("year", "time"),
    facet_ncol = NULL,
    panel_ncol = 2,           # 2 panels: Predicted / Observed
    drop_na_class = TRUE,
    border_color = NA,
    border_size = 0.1,
    title = "Predicted vs Observed classes",
    subtitle = NULL,
    caption = NULL
) {
  facet_by <- match.arg(facet_by)

  if (!inherits(sf_class, "sf")) stop("`sf_class` must be an sf object.", call. = FALSE)

  needed <- c(id_col, time_col, pred_col, obs_col)
  miss <- setdiff(needed, names(sf_class))
  if (length(miss)) stop("Missing columns in sf_class: ", paste(miss, collapse = ", "), call. = FALSE)

  x <- sf_class %>%
    dplyr::mutate(
      .time = as.numeric(.data[[time_col]])
    )

  if (!is.null(period_start)) x <- dplyr::filter(x, .time >= as.numeric(period_start))
  if (!is.null(period_end))   x <- dplyr::filter(x, .time <= as.numeric(period_end))
  if (nrow(x) == 0) stop("No rows left after period filtering.", call. = FALSE)

  # facet key
  x <- x %>%
    dplyr::mutate(
      .facet = dplyr::if_else(
        facet_by == "year",
        as.character(floor(.time / 10000)),
        as.character(.time)
      )
    )

  # long format: Predicted / Observed
  x_long <- x %>%
    dplyr::select(dplyr::all_of(c(id_col, time_col)), geometry,
                  pred = dplyr::all_of(pred_col),
                  obs  = dplyr::all_of(obs_col),
                  .facet) %>%
    tidyr::pivot_longer(
      cols = c("pred", "obs"),
      names_to = ".type",
      values_to = ".class"
    ) %>%
    dplyr::mutate(
      .type = dplyr::recode(.type, pred = "Predicted", obs = "Observed"),
      .class = as.factor(.class)
    )

  if (isTRUE(drop_na_class)) {
    x_long <- x_long %>% dplyr::filter(!is.na(.class))
  }

  # auto subtitle
  if (is.null(subtitle)) {
    subtitle <- paste0(
      "Period: ",
      ifelse(is.null(period_start), "-Inf", period_start),
      " to ",
      ifelse(is.null(period_end), "Inf", period_end),
      " | Facet by: ", facet_by
    )
  }

  # plot
  p <- ggplot2::ggplot(x_long) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .class),
      color = border_color,
      linewidth = border_size
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.facet),
      cols = ggplot2::vars(.type)
    ) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption, fill = "Class") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      strip.text = ggplot2::element_text(face = "bold"),
      panel.grid.major = ggplot2::element_line(linewidth = 0.1)
    )

  # If you prefer wrap instead of grid:
  # p <- p + ggplot2::facet_wrap(~ .facet + .type, ncol = facet_ncol)

  list(data = x_long, plot = p)
}

# ---- Example ----
