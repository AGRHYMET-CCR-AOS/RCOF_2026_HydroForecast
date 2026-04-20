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
  file_csv <- file.path(
    file.path(PATH_OUTPUT,"tables"),
    paste0(COUNTRY_CODE,"_",APPROACH, "_", PREDICTOR_VARS, "_probabilities_", FINAL_FUSER, "_", timestamp, ".csv")
  )
  
  write.csv(probabilities, file_csv, row.names = FALSE)
  
  
  message("✔ STEP 6: Save numerical outputs — COMPLETED SUCCESSFULLY")
  
}, error = function(e) {
  message("❌ STEP 6: Save numerical outputs — FAILED")
  stop(e)
})

