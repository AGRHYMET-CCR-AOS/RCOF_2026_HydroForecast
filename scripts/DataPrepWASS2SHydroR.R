###################################################################################
######              Data Prep for WASS2SHydroR                          ###########
###################################################################################

## Netoyage
# ==== PARAMETERS (participants only edit this block) ==========================
COUNTRY_CODE <- NULL # "BEN" "GMB" "GHA" "GIN" "CIV" "LBR" "MLI" "MRT" "NER" "NGA" "GNB" "SEN" "SLE" "TGO" "BFA" "TCD" "CPV"
PREDICTOR_VARS <-"SST" # "PRCP", "SST"  # choose among available folders under predictors/
# Where things live (relative to project root)
PATH_COUNTRIES   <- "data/statics/was_contries.shp" # shapefile with GMI_CNTRY field
PATH_SUBBASINS   <-"data/statics/was_presass_subbassins_lev5_processed.shp" #"static/subbassins.shp"     # shapefile with HYBAS_ID field
PATH_HISTORICAL  <-"data/predictants/subbassins_seasonnal_discharge_lev5.csv" #"data/was_subbassins_seasonal_data.csv" # columns: DATE, HYBAS_ID, Q, prcp, evap
PATH_PREDICTORS  <- "D:/CCR_AOS/ACTIVITES/AGRHYMET/2026/DCEM/PRESASS/data/-60W-60E_to_-25S-25N/SST"
PATH_OUTPUT <- "data/predictors/SST"
APPLY_PCA <- if(PREDICTOR_VARS=="SST") TRUE else FALSE
FORCE_PCA <- FALSE
update_github <- TRUE
force_reinstallation <- FALSE
FIELD_SEPERATOR <- ","
MISSING_VALUE_CODE <-  "-999"
HISTORICAL_DATA_ID_COL <- "HYBAS_ID"
SUBBASINS_ID_COL <- "HYBAS_ID"
FYEAR <- 2026
start_year <- 1991
end_year <- 2026
# Optional: performance/speed knobs
N_CORES <- 4#max(1, parallel::detectCores() - 1)
#=========== Configuration files ===================================================
source("scripts/utils/helpers_dp.R")
source("scripts/utils/processing.R")
source("scripts/utils/extract_predictors.R")