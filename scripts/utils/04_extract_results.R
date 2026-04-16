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
  
  fused_all <- imap(res_forecast, ~ purrr::pluck(.x, 1, "fused_by_model", .default = NULL)) %>%
    discard(is.null) %>%
    imap_dfr(~ mutate(.x, HYBAS_ID = .y, .before = 1))
  
  
  
  perf_tbl <- imap_dfr(res_forecast, function(basin_obj, basin_id) {
    
    # si structure imbriquée
    x <- basin_obj[[basin_id]]
    if (is.null(x)) x <- basin_obj
    
    lbs <- x$scores 
    if (is.null(lbs)) return(NULL)
    
    lbs %>%
      mutate(
        approach = APPROACH,
        predictor = PREDICTOR_VARS,
        fuser = FINAL_FUSER,
        .before = 1
      )
  })
  
  leaderboards <- extract_leaderboards_long(res_forecast,meta)
  res_forecast2 <- map(names(res_forecast), function(id) {
    res_forecast[[id]][[id]]
  }) %>%
    set_names(names(res_forecast))
  
  predictions <- map2(res_forecast2,names(res_forecast2), function(.x,.y){
    fused_by_model <- .x$fused_by_model
    
    if(!all(c("YYYY","Q" ,"pred_final") %in% names(fused_by_model))){
      fused_by_model <- data.frame()
      return(fused_by_model)
    }
    fused_by_model <- fused_by_model %>%
      dplyr::select("YYYY","Q" ,"pred_final") %>%
      rename(pred = pred_final) %>%
      mutate(HYBAS_ID = .y)
  }) %>% bind_rows()
  
  hybas_ids <- unique(predictions$HYBAS_ID)
  
  
  message("✔ STEP 4: Extract model outputs — COMPLETED SUCCESSFULLY")
  
}, error = function(e) {
  message("❌ STEP 4: Extract model outputs — FAILED")
  stop(e)
})

