merge_prcp_sst_lists <- function(
    prcp,
    sst,
    id_cols = c("HYBAS_ID", "YYYY","Q"),
    prcp_prefix = "prcp_",
    sst_prefix  = "sst_"
) {
  # ---- validate basic structure ----
  if (!is.list(prcp) || !is.list(sst)) {
    stop("`prcp` and `sst` must be lists.", call. = FALSE)
  }
  
  basins <- union(names(prcp), names(sst))
  
  out <- map(set_names(basins), function(b) {
    
    prcp_b <- prcp[[b]]
    sst_b  <- sst[[b]]
    
    # if one basin is missing entirely in one list, just return the other
    if (is.null(prcp_b) && is.null(sst_b)) return(NULL)
    if (is.null(prcp_b)) return(sst_b)
    if (is.null(sst_b))  return(prcp_b)
    
    models <- union(names(prcp_b), names(sst_b))
    
    map(set_names(models), function(m) {
      p <- prcp_b[[m]]
      s <- sst_b[[m]]
      
      if (is.null(p) && is.null(s)) return(NULL)
      if (is.null(p)) return(s)
      if (is.null(s)) return(p)
      
      # Keep only expected blocks to avoid duplicating id cols
      p_x <- p %>% select(any_of(id_cols), starts_with(prcp_prefix))
      s_x <- s %>% select(any_of(id_cols), starts_with(sst_prefix))
      
      # Join (keeps the PRCP rows; if you prefer strict intersection, use inner_join)
      merged <- p_x %>%
        left_join(s_x, by = id_cols)
      
      # Optional: ensure id_cols exist even if factors/names vary
      merged
    })
  })
  
  # Drop NULL basins (if any) and keep names clean
  compact(out)
}


get_basin_payload <- function(basin_obj, basin_id) {
  # Try the common nested structure first
  x <- basin_obj[[basin_id]]
  if (!is.null(x)) return(x)
  
  # If already "flat" (basin_obj itself contains leaderboards)
  basin_obj
}


extract_leaderboards_long <- function(rds_obj, meta) {
  
  # The outer object may already be a list of basins,
  # or a list with a basins list inside. We assume named list by HYBAS_ID.
  basin_ids <- names(rds_obj)
  if (is.null(basin_ids) || length(basin_ids) == 0) {
    warning("RDS object has no names(); cannot infer basin IDs for file: ", meta$file)
    return(tibble())
  }
  
  imap_dfr(rds_obj, function(basin_obj, basin_id) {
    
    payload <- get_basin_payload(basin_obj, basin_id)
    
    lbs <- payload$leaderboards
    if (is.null(lbs) || length(lbs) == 0) return(tibble())
    
    imap_dfr(lbs, function(lb_df, model_name) {
      if (is.null(lb_df) || !is.data.frame(lb_df) || nrow(lb_df) == 0) return(tibble())
      
      # Expected columns: product, kge, weight
      out <- as_tibble(lb_df)
      
      # Defensive: ensure columns exist / standardize names
      nms <- names(out)
      if (!("product" %in% nms)) out$product <- NA_character_
      if (!("kge" %in% nms))     out$kge     <- NA_real_
      if (!("weight" %in% nms))  out$weight  <- NA_real_
      
      out %>%
        transmute(
          HYBAS_ID = as.character(basin_id),
          model = as.character(model_name),
          product = as.character(product),
          kge = as.numeric(kge),
          weight = as.numeric(weight)
        ) %>%
        mutate(
          predictors = meta$predictors,
          approach = meta$approach,
          final_fuser = meta$final_fuser,
          .before = 1
        )
    })
  })
}


## Fonction for inputs data processing

reduce_data_by_products_eof <- function(
    data_by_products,
    sst_regex = "^sst_",
    num_comp = 15,
    corr_threshold = 0.99,
    corr_method = "pearson",
    remove_linear_comb = FALSE,
    # Naming policy: the reduced EOF columns MUST match the same SST regex
    # Example: "sst_eof_001", ..., "sst_eof_015" -> still matches "^sst_"
    eof_prefix = "sst_eof_",
    eof_digits = 3,
    verbose = TRUE
) {
  # ---- Dependencies ----
  stopifnot(is.list(data_by_products))
  if (!requireNamespace("recipes", quietly = TRUE)) stop("Package 'recipes' is required.", call. = FALSE)
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.", call. = FALSE)
  if (!requireNamespace("purrr", quietly = TRUE)) stop("Package 'purrr' is required.", call. = FALSE)
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' is required.", call. = FALSE)
  
  # ---- Validations ----
  if (!is.numeric(num_comp) || length(num_comp) != 1L || num_comp < 1) {
    stop("`num_comp` must be a single positive integer.", call. = FALSE)
  }
  if (!is.numeric(corr_threshold) || corr_threshold <= 0 || corr_threshold >= 1) {
    stop("`corr_threshold` must be in (0,1), e.g. 0.99.", call. = FALSE)
  }
  if (!is.character(eof_prefix) || !nzchar(eof_prefix)) {
    stop("`eof_prefix` must be a non-empty character string.", call. = FALSE)
  }
  # Ensure the new EOF names still match the SST regex (critical requirement)
  test_name <- paste0(eof_prefix, sprintf(paste0("%0", eof_digits, "d"), 1))
  if (!grepl(sst_regex, test_name)) {
    stop(
      sprintf(
        "The generated EOF column name '%s' does NOT match sst_regex '%s'. Please adjust `eof_prefix` or `sst_regex`.",
        test_name, sst_regex
      ),
      call. = FALSE
    )
  }
  
  basin_ids <- names(data_by_products)
  if (is.null(basin_ids) || !length(basin_ids)) {
    stop("`data_by_products` must be a named list (names = HYBAS_ID).", call. = FALSE)
  }
  
  # ---- Helper: identify SST predictor columns ----
  .get_sst_cols <- function(df) grep(sst_regex, names(df), value = TRUE)
  
  # ---- Collect all climate models across basins ----
  all_models <- sort(unique(unlist(purrr::map(data_by_products, names))))
  if (!length(all_models)) stop("No climate models found inside `data_by_products`.", call. = FALSE)
  
  # ---- Fit one EOF/PCA recipe per climate model using ALL basins (stacked) ----
  eof_recipes <- purrr::map(all_models, function(mdl) {
    dfs <- purrr::map(basin_ids, function(bid) {
      df <- data_by_products[[bid]][[mdl]]
      if (is.null(df) || !is.data.frame(df)) return(NULL)
      
      # Minimal required columns for alignment and to keep your structure consistent
      needed <- c("HYBAS_ID", "YYYY", "Q")
      miss <- setdiff(needed, names(df))
      if (length(miss)) {
        stop(
          sprintf("Model '%s' basin '%s': missing required columns: %s",
                  mdl, bid, paste(miss, collapse = ", ")),
          call. = FALSE
        )
      }
      
      sst_cols <- .get_sst_cols(df)
      if (!length(sst_cols)) return(NULL)
      
      # Keep only modeled columns + minimal ids during training
      df[, c("HYBAS_ID", "YYYY", "Q", sst_cols), drop = FALSE]
    })
    
    dfs <- purrr::compact(dfs)
    if (!length(dfs)) {
      if (isTRUE(verbose)) message("[EOF] Model '", mdl, "': no usable data found. Skipped.")
      return(NULL)
    }
    
    stacked <- dplyr::bind_rows(dfs) %>% 
      dplyr::distinct(YYYY,.keep_all = TRUE)
    sst_cols <- .get_sst_cols(stacked)
    if (!length(sst_cols)) {
      if (isTRUE(verbose)) message("[EOF] Model '", mdl, "': no SST columns matched regex. Skipped.")
      return(NULL)
    }
    
    # Recipe: zv -> nzv -> impute -> (optional lincomb) -> corr -> normalize -> PCA
    rec <- recipes::recipe(
      stats::as.formula(paste("Q ~", paste(sst_cols, collapse = " + "))),
      data = stacked
    ) |>
      recipes::step_zv(recipes::all_predictors(), id = "zv") |>
      recipes::step_nzv(recipes::all_predictors(), id = "nzv") |>
      recipes::step_impute_median(recipes::all_numeric_predictors(), id = "imp_num")
    
    if (isTRUE(remove_linear_comb)) {
      rec <- rec |> recipes::step_lincomb(recipes::all_numeric_predictors(), id = "lincomb")
    }
    
    rec <- rec |>
      recipes::step_corr(
        recipes::all_numeric_predictors(),
        threshold = corr_threshold,
        method = corr_method,
        id = "corr"
      ) |>
      recipes::step_normalize(recipes::all_numeric_predictors(), id = "norm") |>
      recipes::step_pca(recipes::all_numeric_predictors(), num_comp = as.integer(num_comp), id = "pca")
    
    rec_prep <- recipes::prep(rec, training = stacked, verbose = FALSE)
    
    baked_train <- recipes::bake(rec_prep, new_data = stacked)
    pc_cols <- grep("^PC", names(baked_train), value = TRUE)
    
    if (isTRUE(verbose)) {
      message(
        "[EOF] Model '", mdl, "': stacked rows = ", nrow(stacked),
        " | SST cols (raw) = ", length(sst_cols),
        " | PCs created = ", length(pc_cols)
      )
    }
    
    list(model = mdl, recipe = rec_prep, pc_cols = pc_cols)
  })
  
  names(eof_recipes) <- all_models
  eof_recipes <- purrr::compact(eof_recipes)
  if (!length(eof_recipes)) stop("No EOF/PCA recipes could be fitted. Check your data and `sst_regex`.", call. = FALSE)
  
  # ---- Apply EOF transform to every basin/model dataframe ----
  data_reduced <- purrr::imap(data_by_products, function(basin_list, bid) {
    if (!is.list(basin_list)) return(basin_list)
    
    purrr::imap(basin_list, function(df, mdl) {
      if (is.null(df) || !is.data.frame(df)) return(df)
      if (!mdl %in% names(eof_recipes)) return(df)
      
      # Identify SST columns in the original df
      sst_cols <- .get_sst_cols(df)
      if (!length(sst_cols)) return(df) # nothing to reduce
      
      # Bake PCs (recipes may drop non-modeled columns; we will re-attach metadata)
      rec_prep <- eof_recipes[[mdl]]$recipe
      baked <- recipes::bake(rec_prep, new_data = df)
      
      pc_cols <- eof_recipes[[mdl]]$pc_cols
      pc_cols <- intersect(pc_cols, names(baked))
      if (!length(pc_cols)) {
        stop(sprintf("Basin '%s' model '%s': no PC columns produced by bake().", bid, mdl), call. = FALSE)
      }
      
      # Build EOF column names that STILL match the SST regex
      eof_names <- paste0(eof_prefix, sprintf(paste0("%0", eof_digits, "d"), seq_along(pc_cols)))
      
      pcs <- baked[, pc_cols, drop = FALSE]
      names(pcs) <- eof_names
      
      # Preserve all non-SST columns from original df (including HYBAS_ID, YYYY, Q, etc.)
      non_sst_df <- df[, setdiff(names(df), sst_cols), drop = FALSE]
      
      # Final: original non-SST columns + reduced SST (EOF) columns (named as sst_*)
      out <- dplyr::bind_cols(non_sst_df, pcs)
      tibble::as_tibble(out)
    })
  })
  
  # ---- Return reduced data + fitted EOF objects for reproducibility ----
  list(
    data_reduced = data_reduced,
    eof_objects  = eof_recipes,
    meta = list(
      sst_regex = sst_regex,
      num_comp = as.integer(num_comp),
      corr_threshold = corr_threshold,
      corr_method = corr_method,
      remove_linear_comb = isTRUE(remove_linear_comb),
      eof_prefix = eof_prefix,
      eof_digits = eof_digits,
      fitted_models = names(eof_recipes)
    )
  )
}



safe_readRDS <- function(path) {
  if (is.null(path)) return(NULL)
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  readRDS(path)
}

validate_and_load_predictors <- function(
    PRCP_PATH_INPUTS = NULL,
    SST_PATH_INPUTS  = NULL,
    prcp_regex = "prcp",
    sst_regex  = "^sst_",
    # how many basins/models to inspect (fast sanity check)
    n_check_basins = 1,
    n_check_models = 1,
    verbose = TRUE,
    APPLY_PCA= FALSE
) {
  # ---- helper: basic path checks ----
  .check_path <- function(path, label) {
    if (is.null(path)) return(invisible(TRUE))
    if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
      stop(label, " must be NULL or a non-empty character scalar.", call. = FALSE)
    }
    if (!file.exists(path)) {
      stop(label, " file not found: ", path, call. = FALSE)
    }
    invisible(TRUE)
  }
  
  .check_path(PRCP_PATH_INPUTS, "PRCP_PATH_INPUTS")
  .check_path(SST_PATH_INPUTS,  "SST_PATH_INPUTS")
  
  if (is.null(PRCP_PATH_INPUTS) && is.null(SST_PATH_INPUTS)) {
    stop("You must provide at least one of PRCP_PATH_INPUTS or SST_PATH_INPUTS.", call. = FALSE)
  }
  
  # ---- helper: sample a df inside the nested list ----
  .sample_df <- function(x, n_basin = 1, n_model = 1) {
    if (is.null(x)) return(NULL)
    if (!is.list(x) || is.null(names(x)) || !length(x)) return(NULL)
    
    basin_ids <- names(x)[seq_len(min(n_basin, length(x)))]
    for (bid in basin_ids) {
      basin_list <- x[[bid]]
      if (!is.list(basin_list) || !length(basin_list)) next
      
      model_ids <- names(basin_list)[seq_len(min(n_model, length(basin_list)))]
      for (mid in model_ids) {
        df <- basin_list[[mid]]
        if (is.data.frame(df) && nrow(df) > 0) return(df)
      }
    }
    NULL
  }
  
  # ---- helper: detect variable type by column names ----
  .detect_type <- function(df, prcp_regex, sst_regex) {
    if (is.null(df) || !is.data.frame(df)) return(list(is_prcp = FALSE, is_sst = FALSE))
    
    cols <- names(df)
    is_prcp <- any(grepl(prcp_regex, cols, ignore.case = TRUE))
    is_sst  <- any(grepl(sst_regex, cols, ignore.case = FALSE))
    list(is_prcp = is_prcp, is_sst = is_sst)
  }
  
  # ---- read files ----
  prcp_data_by_products <- if (!is.null(PRCP_PATH_INPUTS)) safe_readRDS(PRCP_PATH_INPUTS)
  sst_data_by_products  <- if (!is.null(SST_PATH_INPUTS))  safe_readRDS(SST_PATH_INPUTS)
  
  # ---- validate structure quickly ----
  .validate_list_structure <- function(x, label) {
    if (is.null(x)) return(invisible(TRUE))
    if (!is.list(x) || is.null(names(x)) || !length(x)) {
      stop(label, " must be a named list of basins (HYBAS_ID).", call. = FALSE)
    }
    invisible(TRUE)
  }
  .validate_list_structure(prcp_data_by_products, "PRCP data")
  .validate_list_structure(sst_data_by_products,  "SST data")
  
  # ---- content validation (guard against swapped paths) ----
  if (!is.null(prcp_data_by_products)) {
    df_chk <- .sample_df(prcp_data_by_products, n_check_basins, n_check_models)
    flags  <- .detect_type(df_chk, prcp_regex, sst_regex)
    
    if (!flags$is_prcp && flags$is_sst) {
      stop(
        "PRCP_PATH_INPUTS seems to point to SST data (found SST columns, but no PRCP columns). ",
        "Please provide the correct PRCP file.",
        call. = FALSE
      )
    }
    if (!flags$is_prcp) {
      stop(
        "PRCP_PATH_INPUTS does not look like PRCP data: no columns matched regex '", prcp_regex, "'.",
        call. = FALSE
      )
    }
    if (isTRUE(verbose)) message("[OK] PRCP file validated (PRCP columns detected).")
  }
  
  if (!is.null(sst_data_by_products)) {
    df_chk <- .sample_df(sst_data_by_products, n_check_basins, n_check_models)
    flags  <- .detect_type(df_chk, prcp_regex, sst_regex)
    
    if (!flags$is_sst && flags$is_prcp) {
      stop(
        "SST_PATH_INPUTS seems to point to PRCP data (found PRCP columns, but no SST columns). ",
        "Please provide the correct SST file.",
        call. = FALSE
      )
    }
    if (!flags$is_sst) {
      stop(
        "SST_PATH_INPUTS does not look like SST data: no columns matched regex '", sst_regex, "'.",
        call. = FALSE
      )
    }
    if (isTRUE(verbose)) message("[OK] SST file validated (SST columns detected).")
  }
  
  # ---- default preprocessing flags ----
  auto_pca <- FALSE
  apply_corr <- FALSE
  apply_normalize <- FALSE
  apply_impute <- TRUE
  impute_nominal <- TRUE
  
  # ---- EOF for SST if present ----
  eof_objects <- NULL
  if (!is.null(sst_data_by_products) && APPLY_PCA) {
    res_eof <- reduce_data_by_products_eof(
      sst_data_by_products,
      sst_regex = sst_regex,
      num_comp = 15,
      corr_threshold = 0.99,
      corr_method = "pearson",
      remove_linear_comb = FALSE,
      eof_prefix = "sst_eof_",
      verbose = TRUE
    )
    
    sst_data_by_products <- res_eof$data_reduced
    eof_objects <- res_eof$eof_objects
    
    # If EOF already applied upstream, disable redundant steps downstream
    auto_pca <- FALSE
    apply_corr <- FALSE
    apply_normalize <- FALSE
    apply_impute <- FALSE
    impute_nominal <- FALSE
  }
  
  list(
    prcp_data_by_products = prcp_data_by_products,
    sst_data_by_products  = sst_data_by_products,
    eof_objects = eof_objects,
    flags = list(
      auto_pca = auto_pca,
      apply_corr = apply_corr,
      apply_normalize = apply_normalize,
      apply_impute = apply_impute,
      impute_nominal = impute_nominal
    )
  )
}


##=============================================================================

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



#Fonction 1 : préparation des données

prepare_fused_long <- function(fused_all) {
  # Required fixed columns
  required_cols <- c("HYBAS_ID", "YYYY", "Q", "pred_final")
  
  missing_cols <- setdiff(required_cols, names(fused_all))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Detect intermediate forecast columns automatically
  intermediate_cols <- setdiff(
    names(fused_all),
    c("HYBAS_ID", "YYYY", "Q", "pred_final")
  )
  
  # Build long-format data for plotting
  df_long <- fused_all %>%
    dplyr::mutate(
      HYBAS_ID = as.character(HYBAS_ID),
      YYYY = as.integer(YYYY)
    ) %>%
    tidyr::pivot_longer(
      cols = c("Q", "pred_final", dplyr::all_of(intermediate_cols)),
      names_to = "series",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      series = factor(
        series,
        levels = c("Q", "pred_final", intermediate_cols),
        labels = c("Observed", "Final fusion", intermediate_cols)
      )
    )
  
  return(df_long)
}

#Fonction 2 : courbes facettées par sous-bassin
plot_fused_timeseries_facets <- function(
    fused_all,
    basin_ids = NULL,
    max_basins = 12,
    ncol = 3,
    free_y = TRUE
) {
  # Prepare long data
  df_long <- prepare_fused_long(fused_all)
  
  # Select basins
  all_basins <- unique(df_long$HYBAS_ID)
  
  if (is.null(basin_ids)) {
    basin_ids <- all_basins[seq_len(min(max_basins, length(all_basins)))]
  }
  
  basin_ids <- intersect(as.character(basin_ids), all_basins)
  
  if (length(basin_ids) == 0) {
    stop("No valid basin IDs selected.")
  }
  
  df_plot <- df_long %>%
    dplyr::filter(HYBAS_ID %in% basin_ids)
  
  # ==============================
  # COLOR MANAGEMENT
  # ==============================
  
  series_levels <- levels(df_plot$series)
  
  # Générer une palette automatique
  other_series <- setdiff(series_levels, "Observed")
  
  palette <- setNames(
    RColorBrewer::brewer.pal(max(3, length(other_series)), "Set1")[seq_along(other_series)],
    other_series
  )
  
  # Forcer "Observed" en noir
  palette <- c("Observed" = "black", palette)
  
  # ==============================
  # PLOT
  # ==============================
  
  p <- ggplot2::ggplot(
    df_plot,
    ggplot2::aes(x = YYYY, y = value, color = series, group = series)
  ) +
    ggplot2::geom_line(
      linewidth = 0.7,
      alpha = 0.95
    ) +
    ggplot2::facet_wrap(
      ~ HYBAS_ID,
      ncol = ncol,
      scales = if (free_y) "free_y" else "fixed"
    ) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::labs(
      title = "",
      x = "Year",
      y = "Discharge",
      color = "Series"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
  
  return(p)
}

#Fonction 3 : génération automatique en plusieurs pages

plot_fused_timeseries_pages <- function(
    fused_all,
    basins_per_page = 12,
    ncol = 3,
    free_y = TRUE
) {
  df_long <- prepare_fused_long(fused_all)
  all_basins <- unique(df_long$HYBAS_ID)
  
  basin_groups <- split(
    all_basins,
    ceiling(seq_along(all_basins) / basins_per_page)
  )
  
  plots <- purrr::imap(basin_groups, function(basin_set, i) {
    p <- plot_fused_timeseries_facets(
      fused_all = fused_all,
      basin_ids = basin_set,
      max_basins = basins_per_page,
      ncol = ncol,
      free_y = free_y
    ) +
      ggplot2::labs(
        title = paste0(
          "Observed vs forecasts by basin (page ", i, "/", length(basin_groups), ")"
        )
      )
    
    return(p)
  })
  
  return(plots)
}

#Fonction 4 : boxplots globaux
plot_fused_boxplot <- function(fused_all, exclude_observed = FALSE) {
  df_long <- prepare_fused_long(fused_all)
  
  if (exclude_observed) {
    df_long <- df_long %>%
      dplyr::filter(series != "Observed")
  }
  
  p <- ggplot2::ggplot(
    df_long,
    ggplot2::aes(x = series, y = value, fill = series)
  ) +
    ggplot2::geom_boxplot(outlier.alpha = 0.25) +
    ggplot2::labs(
      title = "Distribution of observed and forecasted values",
      x = NULL,
      y = "Discharge"
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
    )
  
  return(p)
}

#Fonction 5 : boxplots d’erreurs
plot_fused_error_boxplot <- function(fused_all) {
  required_cols <- c("HYBAS_ID", "YYYY", "Q", "pred_final")
  
  missing_cols <- setdiff(required_cols, names(fused_all))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  pred_cols <- setdiff(names(fused_all), c("HYBAS_ID", "YYYY", "Q"))
  
  df_err <- fused_all %>%
    dplyr::mutate(HYBAS_ID = as.character(HYBAS_ID)) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(pred_cols),
      names_to = "model",
      values_to = "prediction"
    ) %>%
    dplyr::mutate(error = prediction - Q)
  
  p <- ggplot2::ggplot(
    df_err,
    ggplot2::aes(x = model, y = error, fill = model)
  ) +
    ggplot2::geom_boxplot(outlier.alpha = 0.25) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::labs(
      title = "Distribution of forecast errors by model",
      x = NULL,
      y = "Prediction error"
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)
    )
  
  return(p)
}


# ---- Example ----

