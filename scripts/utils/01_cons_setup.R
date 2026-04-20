# ==============================================================================
# STEP 1: SETUP
# ==============================================================================
# This step prepares the working environment:
# - Loads required packages
# - Checks input files
# - Initializes parallel processing
# - Creates output directories
#
# Expected result:
# Environment is ready to run the forecast workflow
# ==============================================================================

message("▶ STEP 1: Setup environment — START \n")
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
PREDICTOR_VARS <- "Cons"
options(repos = c(CRAN = "https://cloud.r-project.org"))
required_pkgs <- c(
  "WASS2SHydroR","sf","tidyr","dplyr","purrr","ggplot2",
  "ggspatial","readr","stringr","tibble","xgboost","glmnet",
  "kknn","earth","Cubist","nnet","ranger","furrr","future","progressr"
)

safeload <- function(pkgs, update_github = TRUE) {
  # Helper: check if a namespace is available without attaching it
  is_installed <- function(x) {
    requireNamespace(x, quietly = TRUE)
  }
  
  # Report missing packages (before any install attempts)
  missing_pkgs <- pkgs[!vapply(pkgs, is_installed, logical(1))]
  if (length(missing_pkgs)) {
    message("Missing packages detected: ", paste(missing_pkgs, collapse = ", "),"\n")
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
  if (update_github || "WASS2SHydroR" %in% missing_pkgs) {
    tryCatch({
      detach("package:WASS2SHydroR", unload = TRUE)
    }, error = function(e){
      
    })
    remotes::install_github(
      "kiema97/AGRHYMET-WASS2SHydroR",
      build_vignettes = FALSE,
      upgrade = "never",
      auth_token = NULL,
      dependencies = TRUE,
      quiet = FALSE,ref = "staging"
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
safeload(required_pkgs)

check_forecast_inputs <- function() {
  
  message("Checking user inputs...\n")
  
  # ---------------------------------------------------------------------------
  # 1. INPUT FILES
  # ---------------------------------------------------------------------------
  
  if (all(is.null(ML_PRCP_PATH),is.null(ML_SST_PATH),is.null(STAT_PRCP_PATH),is.null(STAT_SST_PATH),is.null(HYDRO_Q_PATH))) {
    stop("SST input file not found:\n   ", )
  }
  

  
  # ---------------------------------------------------------------------------
  # 2. SHAPEFILES
  # ---------------------------------------------------------------------------
  
  if (!is.null(PATH_COUNTRIES) && !file.exists(PATH_COUNTRIES)) {
    stop("Countries shapefile not found:\n   ", PATH_COUNTRIES)
  }
  
  if (!file.exists(PATH_SUBBASINS)) {
    stop("Subbasins shapefile not found:\n   ", PATH_SUBBASINS)
  }
  
  if (!is.null(PATH_RIVERS) && !file.exists(PATH_RIVERS)) {
    stop("Rivers shapefile not found:\n   ", PATH_RIVERS)
  }
  
  # ---------------------------------------------------------------------------
  # 3. MODEL CONFIGURATION
  # ---------------------------------------------------------------------------
  
  if (!is.null(FINAL_FUSER) && is.null(FINAL_FUSER) ) {
    if (!(tolower(FINAL_FUSER) %in% tolower(WASS2SHydroR:::SUPPORTED_FUSERS))) {
      stop(
        "Invalid FINAL_FUSER: ", FINAL_FUSER,
        "\n   Supported values are: ",
        paste(WASS2SHydroR:::SUPPORTED_FUSERS, collapse = ", ")
      )
    }
  }
  
  # MODELS (vector case)
  if (exists("MODELS") && !is.null(MODELS)) {
    
    invalid_models <- MODELS[!tolower(MODELS) %in% tolower(WASS2SHydroR:::SUPPORTED_MODELS)]
    
    if (length(invalid_models) > 0) {
      stop(
        "Invalid MODELS detected: ",
        paste(invalid_models, collapse = ", "),
        "\n   Supported values are: ",
        paste(WASS2SHydroR:::SUPPORTED_MODELS, collapse = ", ")
      )
    }
  }
  
  # ---------------------------------------------------------------------------
  # 4. GENERAL PARAMETERS
  # ---------------------------------------------------------------------------
  
  if (!is.numeric(fyear)) {
    stop("Year must be numeric (e.g., 20260101)")
  }
  
  if (!is.numeric(issue_date)) {
    stop("Issue_date must be numeric (e.g., 20260401)")
  }
  
  if (RUN_IN_PARALLEL && WORKERS < 1) {
    stop("WORKERS must be >= 1")
  }
  
  # ---------------------------------------------------------------------------
  # 5. SUCCESS MESSAGE
  # ---------------------------------------------------------------------------
  
}
check_forecast_inputs()

# 1) Choose number of workers (safe defaults)
# ------------------------------------------------------------------------------
if (!isTRUE(RUN_IN_PARALLEL)) {
  workers <- 1L
} else if (!is.null(WORKERS)) {
  workers <- as.integer(WORKERS)
  if (is.na(workers) || workers < 1L) {
    stop("WORKERS must be a positive integer.", call. = FALSE)
  }
} else {
  workers <- min(length(data_by_products), max(future::availableCores() - 2L, 1L))
}


# ------------------------------------------------------------------------------
# 2) Parallel plan
# - If not parallel, use sequential to avoid any multisession overhead
# ------------------------------------------------------------------------------
if (workers <= 1L) {
  future::plan(future::sequential)
} else {
  future::plan(future::multisession, workers = workers)
}

message("Workers used: ", workers,"\n")
# ------------------------------------------------------------------------------
# 4) Progress handlers
# ------------------------------------------------------------------------------
progressr::handlers(global = TRUE)
progressr::handlers("txtprogressbar")  # or "cli" if you prefer

# ==============================================================================
# 1bis) WORKSPACE SETUP (auto-create operational folders)
# ==============================================================================

WASS2S_ROOT_NAME <- "WASS2S_Operational_Runs"

# If NULL -> create in current working directory
#WASS2S_ROOT_PARENT <- NULL  # e.g. "D:/CCR_AOS/Operational" or NULL

PREDICTOR_SET <- PREDICTOR_VARS
RUN_ID <- sprintf("issue_%s", issue_date)
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
PATH_OUTPUT_ <- if(is.null(WASS2S_ROOT_PARENT)) "WASS2S_Operational_Runs" else WASS2S_ROOT_PARENT
PATH_OUTPUT <- file.path(PATH_OUTPUT_,tolower(APPROACH),PREDICTOR_VARS,RUN_ID)

init_wass2s_workspace <- function(
    root_parent = NULL,
    root_name = "WASS2S_Operational_Runs",
    approach,
    predictor_set,
    run_id,
    subdirs = c("figures",
                "tables",
                "logs",
                "exports","metrics",
                "models", "consolidations"),
    create = TRUE
) {
  # ---- sanitize strings for folder names ----
  approach <- tolower(approach)
  predictor_set <- gsub("[^A-Za-z0-9_]+", "_", toupper(predictor_set))
  run_id <- gsub("[^A-Za-z0-9_]+", "_", run_id)
  
  # ---- root location ----
  root <- if (is.null(root_parent)) {
    file.path(getwd(), root_name)
  } else {
    file.path(root_parent, root_name)
  }
  
  # ---- main run directory ----
  run_dir <- file.path(root, approach, predictor_set,  run_id)
  
  # ---- create directories ----
  if (isTRUE(create)) {
    dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
    for (d in subdirs) dir.create(file.path(run_dir, d), recursive = TRUE, showWarnings = FALSE)
  }
  
  # return paths
  out <- list(
    root = root,
    run_dir = run_dir
  )
  for (d in subdirs) out[[d]] <- file.path(run_dir, d)
  
  out
}

ws <- init_wass2s_workspace(
  root_parent = WASS2S_ROOT_PARENT,
  root_name = WASS2S_ROOT_NAME,
  approach = APPROACH,
  predictor_set = PREDICTOR_SET,
  run_id = RUN_ID
)

message("WASS2S workspace: ", ws$run_dir,"\n")

## Import utils
source("scripts/utils/00_functions.R")
meta <- tibble(
  approach = APPROACH,
  predictors = PREDICTOR_VARS,
  final_fuser = FINAL_FUSER
)

message("✔ STEP 1: Setup environment — COMPLETED SUCCESSFULLY")