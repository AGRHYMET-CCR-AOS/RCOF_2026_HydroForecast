# ==============================================================================
# STEP 4: EXTRACT RESULTS
# ==============================================================================
# This step extracts useful outputs from model results:
# - Fused predictions
# - Performance metrics (KGE, RMSE)
# - Leaderboards of models
#
# Expected result:
# Structured tables ready for analysis and export
# ==============================================================================

message("▶ STEP 4: Extract model outputs — START")

tryCatch({
  yprobas <- probabilities %>%
    dplyr::filter(YYYY == lubridate::year(as.Date(as.character(fyear),format = "%Y%m%d"))) %>%
    mutate(HYBAS_ID = as.factor(HYBAS_ID))
  
  
  message("✔ STEP 4: Extract model outputs — COMPLETED SUCCESSFULLY")
  
}, error = function(e) {
  message("❌ STEP 4: Extract model outputs — FAILED")
  stop(e)
})

