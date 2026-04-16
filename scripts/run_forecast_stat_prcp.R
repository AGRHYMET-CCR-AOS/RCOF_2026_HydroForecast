################################################################################
# Seasonal Hydrologic Forecasts with WASS2SHydroR (Statistical Method)
# Clean, documented, and beginner-friendly script
################################################################################

# ==============================================================================
# 1) USER SETTINGS
# Participants should edit only this section
# ==============================================================================
PRCP_PATH_INPUTS <- "data/predictors/PRCP/PRCP_TGO_WAS_PRESASS2026.rds"
SST_PATH_INPUTS <- NULL
COUNTRY_CODE <- "TGO" # "BEN" "GMB" "GHA" "GIN" "CIV" "LBR" "MLI" "MRT" "NER" "NGA" "GNB" "SEN" "SLE" "TGO" "BFA" "TCD" "CPV"
PATH_COUNTRIES   <- "data/statics/was_contries.shp"   # shapefile with GMI_CNTRY field
PATH_SUBBASINS   <- "data/statics/was_presass_subbassins_lev5_processed.shp"
PATH_RIVERS <- "data/statics/was_rivers.shp"
PATH_MASQUE <- NULL
PATH_WAS <- NULL
PATH_OUTLETS <- NULL
APPLY_PCA <- TRUE
PREDICTOR_VARS <-"PRCP"
APPROACH <- "STAT"
WASS2S_ROOT_PARENT <- NULL
RUN_IN_PARALLEL <- TRUE
WORKERS <- 15
FINAL_FUSER <- "rf"
update_github <- TRUE
auto_pca <- TRUE
apply_corr <- TRUE
apply_normalize <-  TRUE
apply_impute = TRUE 
impute_nominal = TRUE
fyears <- c(20210101,20260101)
fyear <- 20260101
issue_date <- 20260401

# ==============================================================================
# 2) DO NOT EDIT BELOW THIS LINE
# Internal workflow
# ==============================================================================
# 1) Setup environment
source("scripts/utils/01_setup.R")

# 2) Prepare input data
source("scripts/utils/02_data_processing.R")

# 3) Run models
source("scripts/utils/03_run_models_stat.R")

# 4) Extract model outputs
source("scripts/utils/04_extract_results.R")

# 5) Compute class probabilities
source("scripts/utils/05_compute_probabilities.R")

# 6) Save numericals outputs
source("scripts/utils/06_save_outputs.R")

# 7) Generate and save maps
source("scripts/utils/07_make_maps.R")





