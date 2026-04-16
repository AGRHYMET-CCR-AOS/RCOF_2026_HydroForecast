# ==============================================================================
# STEP 6: SAVE OUTPUTS
# ==============================================================================
# This step saves all results to disk:
# - CSV tables
# - RDS objects
# - Performance metrics
#
# Expected result:
# All results stored in the outputs/ directory
# ==============================================================================

message("▶ STEP 6: Save numerical outputs — START")

tryCatch({
  
  file_rds <- file.path(
    file.path(PATH_OUTPUT,"exports"),
    paste0(PREDICTOR_VARS, "_seasonal_forecast_", FINAL_FUSER, "_", timestamp, ".rds")
  )
  
  saveRDS(res_forecast, file_rds)
  
  file_csv <- file.path(
    file.path(PATH_OUTPUT,"tables"),
    paste0(COUNTRY_CODE,"_",APPROACH, "_", PREDICTOR_VARS, "_probabilities_", FINAL_FUSER, "_", timestamp, ".csv")
  )
  
  write.csv(probabilities, file_csv, row.names = FALSE)
  
  
  file_fused <- file.path(
    file.path(PATH_OUTPUT,"tables"),
    paste0(COUNTRY_CODE,"_",APPROACH,"_",PREDICTOR_VARS,"_fused_results_", FINAL_FUSER, "_",timestamp,".csv"))
  
  write.csv(fused_all, file_fused, row.names = FALSE)
  
  file_perf_tbl <- file.path(
    file.path(PATH_OUTPUT,"metrics"),
    paste0(COUNTRY_CODE,"_",APPROACH,"_",PREDICTOR_VARS,"_performances_", FINAL_FUSER, "_",timestamp,".csv"))
  
  write.csv(perf_tbl, file_perf_tbl, row.names = FALSE)
  
  file_leaderboards <- file.path(
    file.path(PATH_OUTPUT,"metrics"),
    paste0(COUNTRY_CODE,"_",PREDICTOR_VARS,"_products_performances_", FINAL_FUSER, "_",timestamp,".csv"))
  
  write.csv(leaderboards, file_leaderboards, row.names = FALSE)
  
  
  message("✔ STEP 6: Save numerical outputs — COMPLETED SUCCESSFULLY")
  
}, error = function(e) {
  message("❌ STEP 6: Save numerical outputs — FAILED")
  stop(e)
})

