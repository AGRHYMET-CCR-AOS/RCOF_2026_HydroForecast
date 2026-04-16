# ==============================================================================
# 3) RUN STATISTICAL FORECASTS
# ==============================================================================
with_progress({
  p <- progressor(along = data_by_products)
  res_forecast <- future_map(
    data_by_products,
    function(.x) {
      # incrémente la progression à chaque bassin terminé
      p(sprintf("Done: %s", .x$HYBAS_ID[1] %||% ""))
      WASS2SHydroR::wass2s_run_basins_stat(data_by_product = .x,
                                           hybas_id ="HYBAS_ID",
                                           pred_pattern_by_product  = pred_pattern_by_product,
                                           final_fuser = tolower(FINAL_FUSER),
                                           grid_levels = 10,
                                           topK = 3,
                                           quiet = FALSE,
                                           verbose_tune = FALSE,
                                           min_kge_model =-Inf,
                                           use_sub_fuser =TRUE,
                                           fusion_method ="meta",
                                           sub_fuser="rf",
                                           sub_grid_levels=5,
                                           prediction_years =fyears,
                                           init_frac = 0.8,
                                           assess_frac = 0.2,
                                           auto_pca = auto_pca,
                                           apply_corr = apply_corr,
                                           apply_normalize = apply_normalize,
                                           apply_impute = apply_impute ,
                                           impute_nominal = impute_nominal,
                                           cumulative = TRUE)
    },
    .options = furrr_options(seed = TRUE)
  )
})
plan(sequential)