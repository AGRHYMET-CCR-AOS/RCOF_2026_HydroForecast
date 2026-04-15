
source("scripts/reduce_data_by_products_eof.R")
source("scripts/utils_plots.R")

merge_prcp_sst_lists <- function(
    prcp,
    sst,
    id_cols = c("HYBAS_ID", "YYYY","Q"),
    prcp_prefix = "prcp_",
    sst_prefix  = "sst_"
) {
  # ---- validate basic structure ----
  if (!is.list(prcp) || !is.list(sst)) {
    stop("`prcp` and `sst` must be lists.", call. = FALSE)
  }
  
  basins <- union(names(prcp), names(sst))
  
  out <- map(set_names(basins), function(b) {
    
    prcp_b <- prcp[[b]]
    sst_b  <- sst[[b]]
    
    # if one basin is missing entirely in one list, just return the other
    if (is.null(prcp_b) && is.null(sst_b)) return(NULL)
    if (is.null(prcp_b)) return(sst_b)
    if (is.null(sst_b))  return(prcp_b)
    
    models <- union(names(prcp_b), names(sst_b))
    
    map(set_names(models), function(m) {
      p <- prcp_b[[m]]
      s <- sst_b[[m]]
      
      if (is.null(p) && is.null(s)) return(NULL)
      if (is.null(p)) return(s)
      if (is.null(s)) return(p)
      
      # Keep only expected blocks to avoid duplicating id cols
      p_x <- p %>% select(any_of(id_cols), starts_with(prcp_prefix))
      s_x <- s %>% select(any_of(id_cols), starts_with(sst_prefix))
      
      # Join (keeps the PRCP rows; if you prefer strict intersection, use inner_join)
      merged <- p_x %>%
        left_join(s_x, by = id_cols)
      
      # Optional: ensure id_cols exist even if factors/names vary
      merged
    })
  })
  
  # Drop NULL basins (if any) and keep names clean
  compact(out)
}

# safe_readRDS <- function(path) {
#   if (is.null(path)) return(NULL)
#   if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
#   readRDS(path)
# }
#
#
# prcp_data_by_products <- safe_readRDS(PRCP_PATH_INPUTS)
# sst_data_by_products  <- safe_readRDS(SST_PATH_INPUTS)
#
# auto_pca = TRUE
# apply_corr = TRUE
# apply_normalize = TRUE
# apply_impute = TRUE
#
# if(!is.null(sst_data_by_products)){
#   res_eof <- reduce_data_by_products_eof(
#     sst_data_by_products,
#     sst_regex = "^sst_",
#     num_comp = 15,
#     corr_threshold = 0.99,
#     corr_method = "pearson",
#     remove_linear_comb = FALSE,
#     eof_prefix = "sst_eof_",
#     verbose = TRUE
#   )
#
#   sst_data_by_products <- res_eof$data_reduced
#
#   auto_pca = FALSE
#   apply_corr = FALSE
#   apply_normalize = FALSE
#   apply_impute = FALSE
# }

res <- validate_and_load_predictors(PRCP_PATH_INPUTS, SST_PATH_INPUTS)
prcp_data_by_products <- res$prcp_data_by_products
sst_data_by_products <- res$sst_data_by_products
auto_pca <- res$flags$auto_pca
apply_corr <- res$flags$apply_corr
apply_normalize <- res$flags$apply_normalize
apply_impute <- res$flags$apply_impute
impute_nominal <- res$flags$impute_nominal
if (is.null(prcp_data_by_products) && is.null(sst_data_by_products)) {
  stop("You must provide at least one input: PRCP_PATH_INPUTS or SST_PATH_INPUTS.", call. = FALSE)
}

data_by_products <- if (is.null(prcp_data_by_products)){
  sst_data_by_products
}else if(is.null(sst_data_by_products)) {prcp_data_by_products
} else{
  merge_prcp_sst_lists(prcp_data_by_products, sst_data_by_products)
  PREDICTOR_VARS <-"PRCP-SST"
}





timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
PATH_OUTPUT_ <- if(is.null(WASS2S_ROOT_PARENT)) "WASS2S_Operational_Runs" else WASS2S_ROOT_PARENT
PATH_OUTPUT <- file.path(PATH_OUTPUT_,tolower(APPROACH),PREDICTOR_VARS,RUN_ID)


get_basin_payload <- function(basin_obj, basin_id) {
  # Try the common nested structure first
  x <- basin_obj[[basin_id]]
  if (!is.null(x)) return(x)
  
  # If already "flat" (basin_obj itself contains leaderboards)
  basin_obj
}

meta <- tibble(
  approach = APPROACH,
  predictors = PREDICTOR_VARS,
  final_fuser = FINAL_FUSER
)
extract_leaderboards_long <- function(rds_obj, meta) {
  
  # The outer object may already be a list of basins,
  # or a list with a basins list inside. We assume named list by HYBAS_ID.
  basin_ids <- names(rds_obj)
  if (is.null(basin_ids) || length(basin_ids) == 0) {
    warning("RDS object has no names(); cannot infer basin IDs for file: ", meta$file)
    return(tibble())
  }
  
  imap_dfr(rds_obj, function(basin_obj, basin_id) {
    
    payload <- get_basin_payload(basin_obj, basin_id)
    
    lbs <- payload$leaderboards
    if (is.null(lbs) || length(lbs) == 0) return(tibble())
    
    imap_dfr(lbs, function(lb_df, model_name) {
      if (is.null(lb_df) || !is.data.frame(lb_df) || nrow(lb_df) == 0) return(tibble())
      
      # Expected columns: product, kge, weight
      out <- as_tibble(lb_df)
      
      # Defensive: ensure columns exist / standardize names
      nms <- names(out)
      if (!("product" %in% nms)) out$product <- NA_character_
      if (!("kge" %in% nms))     out$kge     <- NA_real_
      if (!("weight" %in% nms))  out$weight  <- NA_real_
      
      out %>%
        transmute(
          HYBAS_ID = as.character(basin_id),
          model = as.character(model_name),
          product = as.character(product),
          kge = as.numeric(kge),
          weight = as.numeric(weight)
        ) %>%
        mutate(
          predictors = meta$predictors,
          approach = meta$approach,
          final_fuser = meta$final_fuser,
          .before = 1
        )
    })
  })
}
