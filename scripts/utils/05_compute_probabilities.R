# ==============================================================================
# 5) COMPUTE PROBABILITIES
# ==============================================================================

message("Computing class probabilities ...")
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

message("END")