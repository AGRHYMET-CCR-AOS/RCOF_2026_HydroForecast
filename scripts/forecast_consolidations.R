################################################################################
# Seasonal Hydrologic Forecasts with WASS2SHydroR (Statistical Method)
# Clean, documented, and beginner-friendly script
################################################################################

# ==============================================================================
# 1) USER SETTINGS
# Participants should edit only this section
# ==============================================================================
ML_PRCP_PATH <- "RCOF2026_OUTPUTS/ML-PRCP/WASS2S_Operational_Runs/ml/PRCP/issue_20260401/tables/_ML_PRCP_fused_results_rf_20260416_101500.csv"
ML_SST_PATH <- "RCOF2026_OUTPUTS/ML-SST/WASS2S_Operational_Runs/ml/SST/issue_20260401/tables/_ML_SST_fused_results_rf_20260416_100116.csv"
STAT_PRCP_PATH <- "RCOF2026_OUTPUTS/STAT/WASS2S_Operational_Runs/stat/PRCP/issue_20260401/tables/_STAT_PRCP_fused_results_rf_20260416_120908.csv"
STAT_SST_PATH <- "RCOF2026_OUTPUTS/STAT/WASS2S_Operational_Runs/stat/SST/issue_20260401/tables/_STAT_SST_fused_results_rf_20260416_112312.csv"
HYDRO_Q_PATH <- NULL

COUNTRY_CODE <- NULL # "BEN" "GMB" "GHA" "GIN" "CIV" "LBR" "MLI" "MRT" "NER" "NGA" "GNB" "SEN" "SLE" "TGO" "BFA" "TCD" "CPV"
PATH_COUNTRIES   <- "data/statics/was_contries.shp"   # shapefile with GMI_CNTRY field
PATH_SUBBASINS   <- "data/statics/was_presass_subbassins_lev5_processed.shp"
PATH_RIVERS <- "data/statics/was_rivers.shp"
PATH_MASQUE <- NULL # "static/was_southern_subbasins_lev6.shp"
PATH_WAS <- NULL
PATH_OUTLETS <-NULL
WASS2S_ROOT_PARENT <- NULL
RUN_IN_PARALLEL <- FALSE
APPROACH <- "Cons"
WORKERS <- 1
FINAL_FUSER <- NULL
MODELS <- WASS2SHydroR:::SUPPORTED_MODELS
update_github <- TRUE
fyears <- c(20210101,20260101)
fyear <- 20260101
issue_date <- 20260401

# ==============================================================================
# 2) DO NOT EDIT BELOW THIS LINE
# Internal workflow
# ==============================================================================
# 1) Setup environment
source("scripts/utils/01_cons_setup.R")

# 2) Prepare input data
source("scripts/utils/02_cons_data_processing.R")

# 3) Run models
source("scripts/utils/03_consolidations.R")


# 5) Compute class probabilities
source("scripts/utils/05_cons_compute_probabilities.R")


# 7) Generate and save maps
source("scripts/utils/07_make_cons_maps.R")





