# ==============================================================================
# STEP 3: RUN ML MODELS
# ==============================================================================
# This step trains ML models for each sub-basin:
# - WASS2SHydroR::SUPPORTED_MODELS
# - Ensemble fusion (meta-model)
#
# The models are calibrated using historical data
#
# Expected result:
# Model predictions and performance scores for each basin
# ==============================================================================

message("▶ STEP 3: Run models — START")

tryCatch({
  
  with_progress({
    p <- progressor(along = data_by_products)
    res_forecast <- future_map(
      data_by_products,
      function(.x) {
        # incrémente la progression à chaque bassin terminé
        p(sprintf("Done: %s", .x$HYBAS_ID[1] %||% ""))
        wass2s_run_basins_ml(data_by_product = .x,
                             hybas_id = "HYBAS_ID",
                             pred_pattern_by_product =pred_pattern_by_product,
                             models = tolower(MODELS) ,
                             topK = 5,
                             sub_fuser="rf",
                             product_fusion_method ="median",
                             fusion_method ="meta",
                             sub_grid_levels=3,
                             min_kge_model =-Inf ,
                             grid_levels = 5,
                             prediction_years =fyears,
                             verbose_tune = FALSE,quiet =  FALSE,
                             final_fuser = tolower(FINAL_FUSER),
                             parallel = FALSE,workers = 4,
                             auto_pca = auto_pca,
                             apply_corr = apply_corr,
                             apply_normalize = apply_normalize,
                             apply_impute = apply_impute,
                             impute_nominal = impute_nominal
        )
      },
      .options = furrr_options(seed = TRUE)
    )
  })
  
  plan(sequential)
  
  message("✔ STEP 3: Run models — COMPLETED SUCCESSFULLY")
  
}, error = function(e) {
  message("❌ STEP 3: Run models — FAILED")
  stop(e)
})

