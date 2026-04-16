# Filter by trainee choices
SELECTED_MODELS <- unique(pred_catalog$model)

message(c("Available models are : ", paste(SELECTED_MODELS, collapse = " ")))

pred_catalog <- pred_catalog %>% filter(var %in% PREDICTOR_VARS)

if (!is.null(SELECTED_MODELS)) {
  pred_catalog <- pred_catalog %>% filter(model %in% SELECTED_MODELS)
}
sum(unique(hist_df$HYBAS_ID)%in%unique(subs_sel$HYBAS_ID))

pca_prefix <- paste0(tolower(PREDICTOR_VARS),"_eof_")
training_list <- extract_predictors_nested(hybas_ids = unique(hist_df$HYBAS_ID),
                                           models = SELECTED_MODELS,
                                           hist_df = hist_df,
                                           predictor = PREDICTOR_VARS,
                                           predictors_root = PATH_PREDICTORS,
                                           subbasins_sf = subs_sel,
                                           init_year = FYEAR,
                                           use_pca=APPLY_PCA,
                                           pca_prefix = pca_prefix)


## 5) Quick sanity checks on the output list
# Show first subbasin's head
training_list_clean <- tryCatch({
  training_list |>
    map(~ keep(.x, ~ is.data.frame(.x) && NROW(.x) > 0)) |>
    discard(~ length(.x) == 0)
}, error = function(e){
  training_list
})

# if(APPLY_PCA || FORCE_PCA){
#   
#   res_eof <- reduce_data_by_products_eof(
#     training_list_clean,
#     sst_regex = "^sst",
#     num_comp = 15,
#     corr_threshold = 0.7,
#     corr_method = "pearson",
#     remove_linear_comb = FALSE,
#     eof_prefix = "sst_eof_",
#     verbose = TRUE
#   )
# }
## 6) Save the prepared list for modeling

#savePath <- file.path(PATH_OUTPUT, paste0(PREDICTOR_VARS,"_training_list_", if(is.null(COUNTRY_CODE)) "ALL" else COUNTRY_CODE, "_obs.rds"))
if(is.null(COUNTRY_CODE)) savePath <- file.path(PATH_OUTPUT, paste0(PREDICTOR_VARS,"_WAS_PRESASS2026.rds"))
if(!is.null(COUNTRY_CODE)) savePath <- file.path(PATH_OUTPUT, paste0(PREDICTOR_VARS,"_",COUNTRY_CODE,"_WAS_PRESASS2026.rds"))

saveRDS(training_list_clean, file =savePath )
message("Saved: ",savePath)
