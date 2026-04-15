message("Saving numerical outputs")
# ==============================================================================
# 6) SAVE NUMERIC OUTPUTS
# ==============================================================================
file_rds <- file.path(
  file.path(PATH_OUTPUT,"exports"),
  paste0(PREDICTOR_VARS, "_seasonal_forecast_stat_", FINAL_FUSER, "_", timestamp, ".rds")
)

saveRDS(res_forecast, file_rds)

file_csv <- file.path(
  file.path(PATH_OUTPUT,"tables"),
  paste0(COUNTRY_CODE, "_", PREDICTOR_VARS, "_statistic_probabilities_", FINAL_FUSER, "_", timestamp, ".csv")
)

write.csv(probabilities, file_csv, row.names = FALSE)


file_fused <- file.path(
  file.path(PATH_OUTPUT,"tables"),
  paste0(COUNTRY_CODE,"_",PREDICTOR_VARS,"_fused_stat_results_", FINAL_FUSER, "_",timestamp,".csv"))

write.csv(fused_all, file_fused, row.names = FALSE)

file_perf_tbl <- file.path(
  file.path(PATH_OUTPUT,"metrics"),
  paste0(COUNTRY_CODE,"_",PREDICTOR_VARS,"_performances_", FINAL_FUSER, "_",timestamp,".csv"))

write.csv(perf_tbl, file_perf_tbl, row.names = FALSE)

file_leaderboards <- file.path(
  file.path(PATH_OUTPUT,"metrics"),
  paste0(COUNTRY_CODE,"_",PREDICTOR_VARS,"_products_performances_", FINAL_FUSER, "_",timestamp,".csv"))

write.csv(leaderboards, file_leaderboards, row.names = FALSE)

message("Numeric outputs saved.")