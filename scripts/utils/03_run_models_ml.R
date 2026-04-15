message("Running ML forecasts (per product) ...")
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
                           topK = 3,
                           sub_fuser="rf",
                           use_sub_fuser = TRUE,
                           fusion_method ="median",
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