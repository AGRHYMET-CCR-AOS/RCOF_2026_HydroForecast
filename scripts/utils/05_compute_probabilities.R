# ==============================================================================
# STEP 5: COMPUTE PROBABILITIES
# ==============================================================================
# This step converts predictions into probabilistic classes:
# - Below normal
# - Normal
# - Above normal
#
# Based on historical distribution of streamflow
#
# Expected result:
# Probabilities for each class per basin
# ==============================================================================
message("▶ STEP 5: Compute class probabilities — START")

tryCatch({
  
  probabilities <- map(hybas_ids,~{
    preds <- predictions %>%
      dplyr::filter(HYBAS_ID == .x)
    rr <- c(preds$Q-preds$pred)^2
    
    error_sd <- sd(c(preds$Q-preds$pred),na.rm = TRUE)
    error_rmse <- sqrt(mean(rr,na.rm = TRUE))
    
    proba <- WASS2SHydroR::wass2s_class_from_forecast(df = preds,
                                                      q_hist = preds$Q,
                                                      sigma =error_sd )
    return(proba)
  }) %>% bind_rows()
  
  yprobas <- probabilities %>%
    dplyr::filter(YYYY == fyear) %>%
    mutate(HYBAS_ID = as.factor(HYBAS_ID))
  
  
  message("✔ STEP 5: Compute class probabilities — COMPLETED SUCCESSFULLY")
  
}, error = function(e) {
  message("❌ STEP 5: Compute class probabilities — FAILED")
  stop(e)
})


