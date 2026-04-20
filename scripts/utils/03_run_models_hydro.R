#' Compute class probabilities from multi-model seasonal discharge forecasts
#'
#' This function classifies each model forecast into hydrological classes
#' ("below", "normal", "above") using basin-specific historical quantiles,
#' then computes class probabilities as relative frequencies across models.
#'
#' @param seasonal_discharge A data frame containing at least:
#'   `HYBAS_ID`, a year column, and one column per forecast model.
#' @param his_quantile A data frame containing at least:
#'   `HYBAS_ID`, `q25`, and `q75`.
#' @param hybas_col Character string. Name of the basin identifier column.
#'   Default is `"HYBAS_ID"`.
#' @param year_col Character string. Name of the year column.
#'   Default is `"DATE"`.
#' @param q25_col Character string. Name of the lower quantile column.
#'   Default is `"q25"`.
#' @param q75_col Character string. Name of the upper quantile column.
#'   Default is `"q75"`.
#' @param model_cols Optional character vector of model column names.
#'   If `NULL`, all columns except identifier columns are treated as model columns.
#' @param ties Character string indicating how to resolve ties in `class_hat`.
#'   One of `"first"` or `"NA"`.
#'
#' @return A data frame with columns:
#'   `HYBAS_ID`, `YYYY`, `p_below`, `p_normal`, `p_above`, `class_hat`, `entropy`.
#'
#' @export
wass2s_compute_class_probabilities <- function(
    seasonal_discharge,
    his_quantile,
    hybas_col = "HYBAS_ID",
    year_col = "DATE",
    q25_col = "q25",
    q75_col = "q75",
    model_cols = NULL,
    ties = c("first", "NA")
) {
  ties <- match.arg(ties)
  
  # ---------------------------------------------------------------------------
  # Input checks
  # ---------------------------------------------------------------------------
  if (!is.data.frame(seasonal_discharge)) {
    stop("`seasonal_discharge` must be a data.frame.", call. = FALSE)
  }
  
  if (!is.data.frame(his_quantile)) {
    stop("`his_quantile` must be a data.frame.", call. = FALSE)
  }
  
  required_sd_cols <- c(hybas_col, year_col)
  missing_sd_cols <- setdiff(required_sd_cols, names(seasonal_discharge))
  if (length(missing_sd_cols) > 0) {
    stop(
      "Missing required column(s) in `seasonal_discharge`: ",
      paste(missing_sd_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  required_q_cols <- c(hybas_col, q25_col, q75_col)
  missing_q_cols <- setdiff(required_q_cols, names(his_quantile))
  if (length(missing_q_cols) > 0) {
    stop(
      "Missing required column(s) in `his_quantile`: ",
      paste(missing_q_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (is.null(model_cols)) {
    model_cols <- setdiff(
      names(seasonal_discharge),
      c(hybas_col, year_col, "SUBID")
    )
  }
  
  if (length(model_cols) == 0) {
    stop("No model columns detected. Please provide `model_cols`.", call. = FALSE)
  }
  
  missing_model_cols <- setdiff(model_cols, names(seasonal_discharge))
  if (length(missing_model_cols) > 0) {
    stop(
      "The following `model_cols` are missing in `seasonal_discharge`: ",
      paste(missing_model_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # Internal entropy helper
  # ---------------------------------------------------------------------------
  compute_entropy <- function(p_below, p_normal, p_above) {
    p <- cbind(p_below, p_normal, p_above)
    out <- rep(NA_real_, nrow(p))
    
    rs <- rowSums(p)
    valid <- is.finite(rs) & rs > 0
    
    if (any(valid)) {
      p_valid <- p[valid, , drop = FALSE]
      out[valid] <- -rowSums(ifelse(p_valid > 0, p_valid * log(p_valid), 0))
    }
    
    out
  }
  
  # ---------------------------------------------------------------------------
  # Join thresholds
  # ---------------------------------------------------------------------------
  x <- seasonal_discharge |>
    dplyr::left_join(
      his_quantile[, c(hybas_col, q25_col, q75_col)],
      by = hybas_col
    )
  
  if (any(is.na(x[[q25_col]]) | is.na(x[[q75_col]]))) {
    warning("Some rows have missing quantiles after join.")
  }
  
  # ---------------------------------------------------------------------------
  # Compute probabilities row by row
  # ---------------------------------------------------------------------------
  out_list <- vector("list", nrow(x))
  
  for (i in seq_len(nrow(x))) {
    row_i <- x[i, , drop = FALSE]
    
    q25 <- row_i[[q25_col]]
    q75 <- row_i[[q75_col]]
    vals <- as.numeric(row_i[model_cols])
    
    valid <- is.finite(vals)
    
    if (!is.finite(q25) || !is.finite(q75) || sum(valid) == 0) {
      p_below <- NA_real_
      p_normal <- NA_real_
      p_above <- NA_real_
      class_hat <- NA_character_
      entropy <- NA_real_
    } else {
      vals <- vals[valid]
      
      cls <- ifelse(
        vals < q25, "below",
        ifelse(vals > q75, "above", "normal")
      )
      
      n <- length(cls)
      p_below <- sum(cls == "below") / n
      p_normal <- sum(cls == "normal") / n
      p_above <- sum(cls == "above") / n
      
      probs <- c(below = p_below, normal = p_normal, above = p_above)
      max_prob <- max(probs)
      
      winners <- names(probs)[probs == max_prob]
      
      if (length(winners) == 1) {
        class_hat <- winners
      } else {
        class_hat <- if (ties == "first") winners[1] else NA_character_
      }
      
      entropy <- compute_entropy(p_below, p_normal, p_above)
    }
    
    out_list[[i]] <- data.frame(
      HYBAS_ID = row_i[[hybas_col]],
      YYYY = row_i[[year_col]],
      p_below = p_below,
      p_normal = p_normal,
      p_above = p_above,
      class_hat = class_hat,
      entropy = entropy,
      stringsAsFactors = FALSE
    )
  }
  
  out <- do.call(rbind, out_list)
  rownames(out) <- NULL
  out
}

probabilities <- wass2s_compute_class_probabilities(
  seasonal_discharge = final_seasonal_flow,
  his_quantile = his_quantile,
  model_cols=CLIMATE_MODELS,
  hybas_col = "HYBAS_ID",
  year_col = "DATE"
)
