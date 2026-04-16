# ==== Libraries ==============================================================
# ------------------------------------------------------------------------------
# safeload(): Install and load required R packages (CRAN + GitHub)
#
# Purpose:
#   Ensures that a list of required packages is installed and loaded.
#   - Installs missing CRAN packages from the configured repo.
#   - Installs/updates a GitHub package (WASS2SHydroR) via 'remotes'.
#
# Arguments:
#   pkgs          Character vector of package names to ensure are available.
#   update_github Logical. If TRUE, force re-installation of WASS2SHydroR
#                 from GitHub even if it is already installed. Default: FALSE.
#
# Returns:
#   (Invisibly) the result of loading all requested packages.
#
# Notes:
#   - Uses 'remotes::install_github()' which is lighter than 'devtools'.
#   - Respects GITHUB_PAT if set (recommended for rate limits).
#   - Installs dependencies and parallelizes CRAN compilation when possible.
# ------------------------------------------------------------------------------

required_pkgs <- c(
  "WASS2SHydroR","lubridate","ncdf4","stars",
  "sf","tidyr","dplyr","purrr","ggplot2",
  "ggspatial","readr","stringr","tibble"
)

safeload <- function(pkgs, update_github = FALSE,force_reinstallation=FALSE) {
  # Helper: check if a namespace is available without attaching it
  is_installed <- function(x) {
    requireNamespace(x, quietly = TRUE)
  }

  # Report missing packages (before any install attempts)
  missing_pkgs <- pkgs[!vapply(pkgs, is_installed, logical(1))]
  if (length(missing_pkgs)) {
    message("Missing packages detected: ", paste(missing_pkgs, collapse = ", "))
  }

  # Ensure a stable CRAN mirror (optional but recommended for reproducibility)
  if (is.null(getOption("repos")) || isTRUE(getOption("repos")["CRAN"] == "@CRAN@")) {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }

  # Ensure 'remotes' is available for GitHub installations
  if (!is_installed("remotes")) {
    install.packages("remotes", dependencies = TRUE)
  }

  # Install or update the GitHub package if requested or missing
  if (update_github) {

    tryCatch({
      detach("package:WASS2SHydroR", unload = TRUE)
    }, error = function(e){

    })

    remotes::install_github(
      "kiema97/AGRHYMET-WASS2SHydroR",
      build_vignettes = FALSE,
      ref = "staging",
      upgrade = "never",
      auth_token = NULL,
      dependencies = TRUE,
      quiet = TRUE,
      force = force_reinstallation
    )
  }

  # Remove WASS2SHydroR from the missing list (if it was there)
  missing_pkgs <- setdiff(missing_pkgs, "WASS2SHydroR")

  # Install remaining CRAN packages if needed
  if (length(missing_pkgs) > 0) {
    install.packages(
      missing_pkgs,
      dependencies = TRUE,
      Ncpus = max(1L, parallel::detectCores(logical = TRUE) - 1L)
    )
  }

  # Silently attach all requested packages
  invisible(lapply(
    pkgs,
    function(p) suppressPackageStartupMessages(library(p, character.only = TRUE))
  ))
}

# Execute once to ensure your working session has everything loaded
safeload(required_pkgs,update_github = update_github,force_reinstallation = force_reinstallation)

# WASS2SHydroR is expected to be installed; falls back gracefully if missing
has_wass2s <- requireNamespace("WASS2SHydroR", quietly = TRUE)
if (!has_wass2s) {
  message("NOTE: Package 'WASS2SHydroR' not found. You can still run most steps;\n",
          "the function 'wass2s_prepare_data()' will be called only if available.")
}

# Directories
dir.create(PATH_OUTPUT, showWarnings = FALSE)


# Helper: safe parallel plan (base R's parallel via mclapply on Unix; fall back on lapply on Windows)
.parallel_map <- function(X, FUN, ...){
  if (.Platform$OS.type == "windows") {
    # Simple fallback for Windows notebooks to avoid cluster overhead for trainees
    lapply(X, FUN, ...)
  } else {
    parallel::mclapply(X, FUN, mc.cores = N_CORES, ...)
  }
}

catalog_predictors <- function(
    base_dir = PATH_PREDICTORS,
    vars_keep = NULL,
    file_regex = "\\.nc(\\.(gz|zip))?$"  # accepte .nc, .nc.gz, .nc.zip
) {
  # --- Dépendances (pas de library() pour rester robuste en package)
  stopifnot(requireNamespace("tibble",  quietly = TRUE))
  stopifnot(requireNamespace("dplyr",   quietly = TRUE))
  stopifnot(requireNamespace("purrr",   quietly = TRUE))
  stopifnot(requireNamespace("stringr", quietly = TRUE))
  stopifnot(requireNamespace("tidyr",   quietly = TRUE))

  # --- Vérifs de base
  if (is.null(base_dir) || !dir.exists(base_dir)) {
    stop("`base_dir` introuvable : ", base_dir)
  }

  if(!is.null(vars_keep)){
    d <- file.path(base_dir, toupper(vars_keep))

    base_dir <- if (dir.exists(d)) d else base_dir
  }
  # --- Lister les sous-dossiers (1er niveau)
  files <- list.files(base_dir,pattern = file_regex, full.names = TRUE)
  if (length(files) == 0L) {
    return(tibble::tibble(model = character(), var = character(),
                          init_year = integer(), file = character()))
  }

  tibble::tibble(files =files,
                 base_dir = basename(base_dir)) %>%                                 # ne garder que PRCP/SST (paramétrable)
    tidyr::unnest(files, keep_empty = FALSE) %>%                                 # drop lignes vides
    dplyr::mutate(
      file  = files,
      fname = basename(file),

      # Modèle = tout avant le premier point (ex: "CanSIPSIC4" de "CanSIPSIC4.PRCP_f2025.nc")
      model = stringr::str_replace(fname, "^([^.]+)\\..*$", "\\1"),

      # Variable détectée depuis le nom (entre 1er point et suivant/underscore) → upper
      var_detect = stringr::str_to_upper(
        stringr::str_replace(fname, "^[^.]+[._]([A-Za-z]+).*$", "\\1")
      ),

      # Année d'initialisation si motif "_fYYYY" présent
      init_year = dplyr::if_else(
        stringr::str_detect(fname, "_f\\d{4}"),
        readr::parse_integer(stringr::str_extract(fname, "(?<=_f)\\d{4}")),
        NA_integer_
      ),

      # Choix final de la var : détection valide sinon le nom du dossier
      var = var_detect
    ) %>%
    dplyr::select(model, var, init_year, file) %>%
    dplyr::distinct() %>%
    dplyr::arrange(var, model, init_year, file)
}

extract_predictors_nested <- function(hybas_ids,
                                      models,
                                      hist_df = NULL,
                                      predictor = c("PRCP","SST"),
                                      predictors_root = "predictors",
                                      subbasins_sf = NULL,      # sf avec colonne HYBAS_ID (optionnel)
                                      bbox_list = NULL,         # nommée par HYBAS_ID: list("123"=c(xmin,ymin,xmax,ymax))
                                      init_year = NULL,         # ex. 2025 pour viser *_f2025.nc
                                      on_empty = c("warn","stop","quiet"),
                                      use_pca=FALSE,
                                      pca_prefix = "sst_eof_",...) {
  on_empty <- match.arg(on_empty)

  # --- helpers concis ---------------------------------------------------------
  .bbox_for <- function(hid) {
    if (!is.null(bbox_list) && !is.null(bbox_list[[as.character(hid)]])) {
      bb <- bbox_list[[as.character(hid)]]
      stopifnot(is.numeric(bb), length(bb) == 4); return(as.numeric(bb))
    }
    if (!is.null(subbasins_sf)) {
      g <- subbasins_sf[subbasins_sf$HYBAS_ID == hid, , drop = FALSE]
      if (nrow(g)) {
        bb <- sf::st_bbox(sf::st_geometry(g))
        bb_arranged <-as.numeric(c( bb["xmin"], bb["ymin"], bb["xmax"],bb["ymax"])) 
        names(bb_arranged) <- c("xmin","ymin","xmax","ymax")
        return(bb_arranged)
      }
    }
    NULL
  }

  .pattern <- function(model, var) {
    m <- stringr::str_replace_all(model, "\\.", "\\\\.")
    v <- stringr::str_replace_all(var,   "\\.", "\\\\.")
    paste0(sprintf("^%s\\.%s\\.nc$", m, v), "|", sprintf("^%s\\.%s.*_f%d\\.nc$", m, v, init_year))
  }

  .nc_dir <- function(var) {
    d <- file.path(predictors_root, toupper(var))
    if (dir.exists(d)) d else predictors_root
  }

  # .extract_one <- function(hid, model, var) {
  #   dirv <- .nc_dir(var)
  #   pat  <- .pattern(model, toupper(var))
  # 
  #   if(!is.null(hist_df)){
  #     hid_hist_df <- hist_df %>%
  #       dplyr::filter(HYBAS_ID == hid) %>%
  #       dplyr::select(HYBAS_ID,DATE,Q)
  #   }
  # 
  #   fvec <- sort(list.files(dirv, pat, full.names = TRUE))
  #   if (!length(fvec)) {
  #     msg <- sprintf("[HYBAS %s][%s.%s] no files in %s", hid, model, var, dirv)
  #     if (on_empty == "stop") stop(msg) else if (on_empty == "warn") warning(msg)
  #     return(NULL)
  #   }
  #   bb <- if(PREDICTOR_VARS == "PRCP") .bbox_for(hid) else NULL
  #   
  #   df <- purrr::map(fvec, ~{
  #       predictors_data <- tryCatch({
  #         WASS2SHydroR::wass2s_prepare_data(
  #           x = .x, bbox = bb, spatial_reduce = "none",
  #           cell_layout = "wide", cell_prefix = tolower(var), verbose = FALSE
  #         ) |>
  #           dplyr::mutate(
  #             DATE     = lubridate::year(as.Date(.data$DATE)),  # année (entier)
  #             HYBAS_ID = hid
  #           ) %>%
  #           dplyr::select(where(~ !all(is.na(.)))) %>%
  #           dplyr::select(HYBAS_ID, DATE, dplyr::everything())
  #       }, error = function(e) {
  #         message(paste0("Product ", basename(.x), " not avaible for bassin : ", hid, "\n", e))
  #         tibble::tibble(HYBAS_ID = integer(), DATE = integer())
  #       })
  # 
  #       if(is.null(hist_df)){
  #         hid_hist_df <- data.frame(HYBAS_ID =hid,
  #                                   DATE =predictors_data$DATE,
  #                                   Q = NA_integer_)
  #       }
  # 
  # 
  #       predictors_data %>%
  #       dplyr::select(HYBAS_ID,DATE, everything())
  # 
  #   }) |>
  #     dplyr::bind_rows()%>%
  #     distinct(HYBAS_ID,DATE,.keep_all = TRUE) %>%
  #     mutate(HYBAS_ID = as.factor(HYBAS_ID)) %>%
  #     dplyr::filter(DATE>=start_year,DATE<=end_year) %>%
  #     full_join(hid_hist_df %>% mutate(HYBAS_ID = as.factor(HYBAS_ID)), by = c("DATE","HYBAS_ID")) %>%
  #     dplyr::select(HYBAS_ID,DATE,Q, everything()) %>%
  #     rename(YYYY=DATE)
  #   df_s <<- df
  #   df
  # }
  
  
  .extract_one <- function(
    hid,
    model,
    var,
    remove_correlated = FALSE,
    corr_threshold = 0.8,
    corr_method = c("pearson", "spearman"),
    use_pca = TRUE,
    pca_ncomp = NULL,
    pca_var_explained = 0.9,
    pca_center = TRUE,
    pca_scale = TRUE,
    pca_impute = c("median", "mean"),
    pca_prefix = "sst_eof_",
    verbose = TRUE
  ) {
    corr_method <- match.arg(corr_method)
    pca_impute  <- match.arg(pca_impute)
    
    # ---------------------------------------------------------------------------
    # Internal helpers
    # ---------------------------------------------------------------------------
    
    .drop_highly_correlated <- function(data, feature_cols, threshold = 0.95, method = "pearson") {
      if (!length(feature_cols)) {
        return(list(
          data = data,
          kept = character(0),
          dropped = character(0),
          cor_matrix = NULL
        ))
      }
      
      x <- data[, feature_cols, drop = FALSE]
      
      # Keep numeric predictors only
      is_num <- vapply(x, is.numeric, logical(1))
      x <- x[, is_num, drop = FALSE]
      
      if (ncol(x) <= 1) {
        kept <- colnames(x)
        dropped <- setdiff(feature_cols, kept)
        data <- data[, c(setdiff(names(data), feature_cols), kept), drop = FALSE]
        return(list(
          data = data,
          kept = kept,
          dropped = dropped,
          cor_matrix = NULL
        ))
      }
      
      # Remove zero-variance columns before correlation filtering
      sds <- vapply(x, stats::sd, numeric(1), na.rm = TRUE)
      zero_var_cols <- names(sds)[is.na(sds) | sds == 0]
      
      if (length(zero_var_cols)) {
        x <- x[, setdiff(colnames(x), zero_var_cols), drop = FALSE]
      }
      
      if (ncol(x) <= 1) {
        kept <- colnames(x)
        dropped <- unique(c(setdiff(feature_cols, kept), zero_var_cols))
        data <- data[, c(setdiff(names(data), feature_cols), kept), drop = FALSE]
        return(list(
          data = data,
          kept = kept,
          dropped = dropped,
          cor_matrix = NULL
        ))
      }
      
      cor_mat <- suppressWarnings(
        stats::cor(x, use = "pairwise.complete.obs", method = method)
      )
      
      cor_abs <- abs(cor_mat)
      diag(cor_abs) <- 0
      
      to_drop <- character(0)
      
      # Greedy filtering similar in spirit to caret::findCorrelation
      while (TRUE) {
        max_cor <- suppressWarnings(max(cor_abs, na.rm = TRUE))
        
        if (!is.finite(max_cor) || max_cor < threshold) break
        
        idx <- which(cor_abs == max_cor, arr.ind = TRUE)[1, , drop = FALSE]
        c1 <- colnames(cor_abs)[idx[1, 1]]
        c2 <- colnames(cor_abs)[idx[1, 2]]
        
        mean_c1 <- mean(cor_abs[c1, ], na.rm = TRUE)
        mean_c2 <- mean(cor_abs[c2, ], na.rm = TRUE)
        
        drop_col <- if (mean_c1 >= mean_c2) c1 else c2
        to_drop <- c(to_drop, drop_col)
        
        keep_now <- setdiff(colnames(cor_abs), drop_col)
        if (length(keep_now) <= 1) break
        cor_abs <- cor_abs[keep_now, keep_now, drop = FALSE]
      }
      
      kept <- setdiff(colnames(x), unique(to_drop))
      dropped <- unique(c(setdiff(feature_cols, kept), zero_var_cols))
      
      out <- data[, c(setdiff(names(data), feature_cols), kept), drop = FALSE]
      
      list(
        data = out,
        kept = kept,
        dropped = dropped,
        cor_matrix = cor_mat
      )
    }
    
    .impute_for_pca <- function(df, method = "median") {
      out <- df
      
      for (j in seq_along(out)) {
        if (!is.numeric(out[[j]])) next
        
        if (all(is.na(out[[j]]))) {
          out[[j]][] <- 0
        } else {
          fill_value <- if (method == "median") {
            stats::median(out[[j]], na.rm = TRUE)
          } else {
            mean(out[[j]], na.rm = TRUE)
          }
          out[[j]][is.na(out[[j]])] <- fill_value
        }
      }
      
      out
    }
    
    .apply_pca <- function(data,
                           feature_cols,
                           ncomp = NULL,
                           var_explained = NULL,
                           center = TRUE,
                           scale. = TRUE,
                           impute_method = "median",
                           prefix = NULL) {
      if (!length(feature_cols)) {
        return(list(
          data = data,
          pca = NULL,
          pca_cols = character(0),
          ncomp = 0,
          explained = numeric(0)
        ))
      }
      
      x <- data[, feature_cols, drop = FALSE]
      
      # Keep numeric predictors only
      is_num <- vapply(x, is.numeric, logical(1))
      x <- x[, is_num, drop = FALSE]
      
      if (!ncol(x)) {
        return(list(
          data = data[, setdiff(names(data), feature_cols), drop = FALSE],
          pca = NULL,
          pca_cols = character(0),
          ncomp = 0,
          explained = numeric(0)
        ))
      }
      
      # Remove zero-variance columns before PCA
      sds <- vapply(x, stats::sd, numeric(1), na.rm = TRUE)
      keep_sd <- names(sds)[is.finite(sds) & sds > 0]
      x <- x[, keep_sd, drop = FALSE]
      
      if (!ncol(x)) {
        return(list(
          data = data[, setdiff(names(data), feature_cols), drop = FALSE],
          pca = NULL,
          pca_cols = character(0),
          ncomp = 0,
          explained = numeric(0)
        ))
      }
      
      x_imp <- .impute_for_pca(x, method = impute_method)
      
      pca_fit <- stats::prcomp(
        x = x_imp,
        center = center,
        scale. = scale.
      )
      
      explained <- (pca_fit$sdev^2) / sum(pca_fit$sdev^2)
      cum_explained <- cumsum(explained)
      
      if (!is.null(ncomp)) {
        ncomp <- min(max(1, as.integer(ncomp)), ncol(pca_fit$x))
      } else if (!is.null(var_explained)) {
        if (!is.numeric(var_explained) || length(var_explained) != 1 ||
            var_explained <= 0 || var_explained > 1) {
          stop("'pca_var_explained' must be a single numeric value in (0, 1].")
        }
        ncomp <- which(cum_explained >= var_explained)[1]
      } else {
        ncomp <- ncol(pca_fit$x)
      }
      
      pcs <- as.data.frame(pca_fit$x[, seq_len(ncomp), drop = FALSE])
      
      if (!is.null(prefix) && nzchar(prefix)) {
        colnames(pcs) <- paste0(prefix, seq_len(ncomp))
      } else {
        colnames(pcs) <- paste0("PC", seq_len(ncomp))
      }
      
      out <- cbind(
        data[, setdiff(names(data), feature_cols), drop = FALSE],
        pcs
      )
      
      list(
        data = out,
        pca = pca_fit,
        pca_cols = colnames(pcs),
        ncomp = ncomp,
        explained = explained
      )
    }
    
    # ---------------------------------------------------------------------------
    # Main body
    # ---------------------------------------------------------------------------
    
    dirv <- .nc_dir(var)
    pat  <- .pattern(model, toupper(var))
    
    if (!is.null(hist_df)) {
      hid_hist_df <- hist_df %>%
        dplyr::filter(HYBAS_ID == hid) %>%
        dplyr::select(HYBAS_ID, DATE, Q)
    }
    
    fvec <- sort(list.files(dirv, pat, full.names = TRUE))
    
    if (!length(fvec)) {
      msg <- sprintf("[HYBAS %s][%s.%s] no files in %s", hid, model, var, dirv)
      if (on_empty == "stop") {
        stop(msg)
      } else if (on_empty == "warn") {
        warning(msg)
      }
      return(NULL)
    }
    
    bb <- if (PREDICTOR_VARS == "PRCP") .bbox_for(hid) else NULL
    
    df <- purrr::map(fvec, ~{
      predictors_data <- tryCatch({
        WASS2SHydroR::wass2s_prepare_data(
          x = .x,
          bbox = bb,
          spatial_reduce = "none",
          cell_layout = "wide",
          cell_prefix = tolower(var),
          verbose = FALSE
        ) |>
          dplyr::mutate(
            DATE = lubridate::year(as.Date(.data$DATE)),
            HYBAS_ID = hid
          ) %>%
          dplyr::select(where(~ !all(is.na(.)))) %>%
          dplyr::select(HYBAS_ID, DATE, dplyr::everything())
      }, error = function(e) {
        message(
          paste0(
            "Product ", basename(.x),
            " not available for basin: ", hid, "\n", e$message
          )
        )
        tibble::tibble(HYBAS_ID = integer(), DATE = integer())
      })
      
      if (is.null(hist_df)) {
        hid_hist_df <- data.frame(
          HYBAS_ID = hid,
          DATE = predictors_data$DATE,
          Q = NA_real_
        )
      }
      
      predictors_data %>%
        dplyr::select(HYBAS_ID, DATE, dplyr::everything())
    }) |>
      dplyr::bind_rows() %>%
      dplyr::distinct(HYBAS_ID, DATE, .keep_all = TRUE) %>%
      dplyr::mutate(HYBAS_ID = as.factor(HYBAS_ID)) %>%
      dplyr::filter(DATE >= start_year, DATE <= end_year) %>%
      dplyr::full_join(
        hid_hist_df %>% dplyr::mutate(HYBAS_ID = as.factor(HYBAS_ID)),
        by = c("DATE", "HYBAS_ID")
      ) %>%
      dplyr::select(HYBAS_ID, DATE, Q, dplyr::everything()) %>%
      dplyr::rename(YYYY = DATE)
    
    # ---------------------------------------------------------------------------
    # Feature engineering block: correlation filtering and optional PCA
    # ---------------------------------------------------------------------------
    
    id_cols <- c("HYBAS_ID", "YYYY", "Q")
    feature_cols <- setdiff(names(df), id_cols)
    
    # Keep only non-empty feature columns
    feature_cols <- feature_cols[
      vapply(df[feature_cols], function(z) !all(is.na(z)), logical(1))
    ]
    
    dropped_cor <- character(0)
    pca_cols <- character(0)
    pca_fit <- NULL
    pca_explained <- NULL
    
    if (remove_correlated && length(feature_cols) > 1) {
      cor_res <- .drop_highly_correlated(
        data = df,
        feature_cols = feature_cols,
        threshold = corr_threshold,
        method = corr_method
      )
      
      df <- cor_res$data
      feature_cols <- setdiff(names(df), id_cols)
      dropped_cor <- cor_res$dropped
      
      if (verbose && length(dropped_cor)) {
        message(
          sprintf(
            "[HYBAS %s][%s.%s] %d correlated predictors removed (threshold = %.2f).",
            hid, model, var, length(dropped_cor), corr_threshold
          )
        )
      }
    }
    
    if (use_pca && length(feature_cols) > 0) {
      pca_res <- .apply_pca(
        data = df,
        feature_cols = feature_cols,
        ncomp = pca_ncomp,
        var_explained = pca_var_explained,
        center = pca_center,
        scale. = pca_scale,
        impute_method = pca_impute,
        prefix = pca_prefix
      )
      
      df <- pca_res$data
      pca_fit <- pca_res$pca
      pca_cols <- pca_res$pca_cols
      pca_explained <- pca_res$explained
      
      if (verbose) {
        message(
          sprintf(
            "[HYBAS %s][%s.%s] PCA applied: %d component(s) retained.",
            hid, model, var, pca_res$ncomp
          )
        )
      }
    }
    
    df <- df %>%
      dplyr::select(HYBAS_ID, YYYY, Q, dplyr::everything())
    
    # Store metadata as attributes for downstream inspection
    attr(df, "dropped_correlated_predictors") <- dropped_cor
    attr(df, "pca_model") <- pca_fit
    attr(df, "pca_columns") <- pca_cols
    attr(df, "pca_explained_variance") <- pca_explained
    
    df_s <<- df
    df
  }

  # --- cœur : liste imbriquée HYBAS_ID -> MODEL -> DF -------------------------
  out <- vector("list", length(hybas_ids)); names(out) <- as.character(hybas_ids)
  for (hid in hybas_ids  ) {
    message(paste0("Processing bassin : ", hid))
    per_model <- vector("list", length(models)); names(per_model) <- models
    for (m in models) {
      # concatène PRCP/SST par DATE pour ce modèle
      pieces <- lapply(predictor, function(v) .extract_one(hid, m, v,use_pca=use_pca,pca_prefix=pca_prefix,...))
      pieces <- pieces[!vapply(pieces, is.null, logical(1))]
      if (length(pieces)) {
        per_model[[m]] <- Reduce(function(x,y) dplyr::full_join(x,y, by="YYYY"), pieces) |>
          dplyr::arrange(YYYY)
      }
    }
    per_model <- per_model[!vapply(per_model, is.null, logical(1))]
    if (length(per_model)) out[[as.character(hid)]] <- per_model
  }
  # retire HYBAS_ID vides
  out[vapply(out, function(x) length(x) > 0, logical(1))]
}


#' Read yearly historical data and optionally add a Forecast YEAR row per basin
#'
#' @param path Character. Path to the historical file.
#' @param sep Character. Field separator (e.g., "," or "\t").
#' @param id_col Character. Name of the basin ID column in the file (will be renamed to "HYBAS_ID").
#' @param hybas_ids Vector or NULL. Subset of HYBAS_IDs to keep. If NULL, keep all.
#' @param missing_value_code Character/numeric or NULL. Extra missing code (e.g., -999).
#' @param extra_na Character vector. Additional NA strings. Default c("NA","", "NaN").
#' @param check_warn Logical. Warn about missing HYBAS_IDs. Default TRUE.
#' @param fyear Integer or NULL. If set (e.g., 2025), ensure one row per basin for YYYY = fyear
#'             with Q = NA and other columns copied from the latest year of that basin.
#'
#' @return Tibble with columns HYBAS_ID, YYYY (integer), Q (may include NA), and any other columns.
read_historical_df_yearly <- function(path,
                                      sep,
                                      id_col,
                                      hybas_ids = NULL,
                                      missing_value_code = NULL,
                                      extra_na = c("NA", "", "NaN"),
                                      check_warn = TRUE,
                                      fyear = NULL) {
  stopifnot(file.exists(path))
  stopifnot(is.character(sep), length(sep) == 1L)
  stopifnot(is.character(id_col), length(id_col) == 1L)

  na_strings <- unique(c(extra_na,
                         if (!is.null(missing_value_code)) as.character(missing_value_code) else NULL))

  df_raw <- utils::read.table(
    file = path, header = TRUE, sep = sep,
    na.strings = na_strings, check.names = FALSE, stringsAsFactors = FALSE
  )

  if (!(id_col %in% names(df_raw))) {
    stop(sprintf("Column '%s' not found in file: %s", id_col, path))
  }
  if (!("DATE" %in% names(df_raw))) {
    stop("Column 'DATE' not found in file. It must contain years (YYYY).")
  }

  # Standardize columns
  library(dplyr)
  df <- df_raw %>%
    rename(HYBAS_ID = dplyr::all_of(id_col)) %>%
    rename(YYYY = DATE) %>%
    mutate(HYBAS_ID = as.factor(HYBAS_ID),
           YYYY = as.integer(YYYY))

  # Keep only requested basins
  if (!is.null(hybas_ids)) {
    df <- dplyr::filter(df, .data$HYBAS_ID %in% hybas_ids)
    if (check_warn) {
      missing_ids <- setdiff(hybas_ids, unique(df$HYBAS_ID))
      if (length(missing_ids)) {
        warning("HYBAS_IDs not found in file: ", paste(missing_ids, collapse = ", "))
      }
    }
  }

  # Optionally add forecast year row per basin
  if (!is.null(fyear)) {
    fyear <- as.integer(fyear)
    # Ensure Q exists; if absent, create it (all NA) so we can set it below.
    if (!("Q" %in% names(df))) df$Q <- NA_real_

    df <- df %>%
      arrange(.data$HYBAS_ID, .data$YYYY) %>%
      group_by(.data$HYBAS_ID) %>%
      group_modify(~{
        g <- .x
        if (any(g$YYYY == fyear, na.rm = TRUE)) return(g)  # already present

        # Take the latest row of this basin as template for non-year columns
        tmpl <- dplyr::slice_tail(g, n = 1)

        # Set forecast year and Q = NA; keep other columns as in tmpl
        tmpl$YYYY <- fyear
        tmpl$Q <- NA_real_

        dplyr::bind_rows(g, tmpl)
      }) %>%
      ungroup() %>%
      arrange(.data$HYBAS_ID, .data$YYYY)
  } else {
    df <- df %>% arrange(.data$HYBAS_ID, .data$YYYY)
  }

  df
}




