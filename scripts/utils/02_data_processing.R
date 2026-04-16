# ==============================================================================
# STEP 2: DATA PROCESSING
# ==============================================================================
# This step prepares the input data:
# - Loads predictor datasets (SST, PRCP)
# - Filters data for the selected country
# - Organizes data by sub-basin (HYBAS_ID)
#
# Expected result:
# Clean and structured dataset ready for modeling
# ==============================================================================

## Geo Data Processing
message("▶ STEP 2: Prepare input data — START")

tryCatch({
  
  pred_pattern_by_product <- "^(prcp|sst)"
  safe_read_sf<- function(path) {
    if (is.null(path)) return(NULL)
    if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
    sf::st_read(path, quiet = TRUE) %>%
      sf::st_make_valid()
  }
  #------------------- 1) Clip subbasins by country polygon-----------------------------------
  
  # Read shapefiles
  a_countries <- safe_read_sf(PATH_COUNTRIES)
  a_subs      <- safe_read_sf(PATH_SUBBASINS)
  a_rivers      <-safe_read_sf(PATH_RIVERS)
  a_masque <- safe_read_sf(PATH_MASQUE)
  a_outlets <- safe_read_sf(PATH_OUTLETS)
  a_was <- safe_read_sf(PATH_WAS)
  # Ensure same CRS
  if (sf::st_crs(a_countries) != sf::st_crs(a_subs)) {
    a_subs <- sf::st_transform(a_subs, sf::st_crs(a_countries))
  }
  
  if (sf::st_crs(a_rivers) != sf::st_crs(a_subs) & !is.null(a_rivers)){
    a_rivers <- sf::st_transform(a_rivers, sf::st_crs(a_subs))
  }
  
  if (sf::st_crs(a_rivers) != sf::st_crs(a_masque) & !is.null(a_rivers) & !is.null(a_masque)){
    a_masque <- sf::st_transform(a_masque, sf::st_crs(a_rivers))
  }
  
  
  if (sf::st_crs(a_rivers) != sf::st_crs(a_outlets) & !is.null(a_rivers) & !is.null(a_outlets)){
    a_outlets <- sf::st_transform(a_outlets, sf::st_crs(a_rivers))
  }
  
  # Filter country
  
  country <- a_countries
  if(!is.null(COUNTRY_CODE)){
    country <- country %>% filter(.data$GMI_CNTRY == COUNTRY_CODE)
    if (nrow(country) == 0) stop("No country with GMI_CNTRY == ", COUNTRY_CODE)
  }
  
  # Intersections: subbasins partially or fully covered by the country polygon
  inter_idx <- sf::st_intersects(a_subs, country, sparse = TRUE)
  sel <- lengths(inter_idx) > 0
  subs_sel <- a_subs[sel, ]
  
  suppressWarnings(sf_basins <- sf::st_intersection(a_subs, country)%>%
                     mutate(HYBAS_ID = as.factor(HYBAS_ID)))
  
  subs_union <- a_subs %>%
    st_make_valid() %>%
    st_union() %>%
    st_as_sf()
  
  country_union <- country %>%
    st_make_valid() %>%
    st_union() %>%
    st_as_sf()
  
  sf_masque <- if(!is.null(a_masque) && !is.null(country_union)) sf::st_intersection(a_masque,country_union) else NULL
  
  if(!is.null(sf_masque)) masque_union <- sf_masque %>%
    st_make_valid() %>%
    st_union() %>%
    st_as_sf()
  if(!is.null(a_rivers)){
    sf_rivers_ <- sf::st_intersection(a_rivers, country_union)
    sf_rivers <- sf::st_intersection(sf_rivers_, subs_union)
  }
  
  
  if(!is.null(a_outlets)){
    sf_outlets_ <- sf::st_intersection(a_outlets,country_union)
    sf_outlets <- sf::st_intersection(sf_outlets_,subs_union)
  }
  if(!is.null(sf_masque)){
    suppressMessages(
      sf_basins <- sf::st_intersection(sf_basins,masque_union)
    )
    suppressWarnings(sf_rivers <- sf::st_intersection(sf_rivers,masque_union))
  }
  
  
  layers <- list()
  
  if (exists("sf_rivers") && !is.null(sf_rivers)) {
    layers <- append(layers, list(list(
      layer = geom_sf(data = sf_rivers, color = "blue"),
      position = "above"
    )))
  }
  
  if (exists("sf_masque") && !is.null(sf_masque)) {
    layers <- append(layers, list(list(
      layer = geom_sf(data = sf_masque, fill = NA, color = "white"),
      position = "above"
    )))
  }
  
  if (exists("country") && !is.null(country)) {
    layers <- append(layers, list(list(
      layer = geom_sf(data = country, fill = NA, color = "black", size = 0.4),
      position = "above"
    )))
  }
  
  ## Inputs data processing
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
  
  
  message("✔ STEP 2: Prepare input data — COMPLETED SUCCESSFULLY")
  
}, error = function(e) {
  message("❌ STEP 2: Prepare input data — FAILED")
  stop(e)
})

