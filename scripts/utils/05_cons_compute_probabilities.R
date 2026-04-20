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
hybas_ids <- unique(merged$HYBAS_ID)
tryCatch({
  
  probabilities <- map(hybas_ids,~{
    preds <- merged %>%
      dplyr::filter(HYBAS_ID == .x) %>% 
      rename(pred=Consolidated_frcst)
    q_hist <- merged%>% 
      rename(pred=Consolidated_frcst) %>%
      dplyr::filter(HYBAS_ID == .x) %>% 
      dplyr::filter(YYYY>= 19910101 & YYYY<=20200101)
    
    error_sd <- sd(c(q_hist$Q-q_hist$pred),na.rm = TRUE)
    
    proba <- WASS2SHydroR::wass2s_class_from_forecast(df = preds,
                                                      q_hist = q_hist$Q,
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


