# Read shapefiles
a_countries <- sf::st_read(PATH_COUNTRIES, quiet = TRUE) %>%
  sf::st_make_valid()

a_subs <- sf::st_read(PATH_SUBBASINS, quiet = TRUE) %>%
  sf::st_make_valid() %>%
  dplyr::rename(HYBAS_ID = dplyr::all_of(SUBBASINS_ID_COL)) %>%
  dplyr::mutate(HYBAS_ID = as.character(HYBAS_ID))

# Ensure same CRS
if (sf::st_crs(a_countries) != sf::st_crs(a_subs)) {
  a_subs <- sf::st_transform(a_subs, sf::st_crs(a_countries))
}

# Predefined country list
countries <- c(
  "BEN", "GMB", "GHA", "GIN", "CIV", "LBR", "MLI", "MRT",
  "NER", "NGA", "GNB", "SEN", "SLE", "TGO", "BFA", "TCD", "CPV"
)

# Filter country
if (!is.null(COUNTRY_CODE) && COUNTRY_CODE %in% countries) {
  
  country <- a_countries %>%
    dplyr::filter(.data$GMI_CNTRY == COUNTRY_CODE) %>%
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")
  
} else {
  
  message(
    if (is.null(COUNTRY_CODE)) {
      "COUNTRY_CODE is NULL; entire country shapefile will be used."
    } else {
      paste0(COUNTRY_CODE, " code not found in the predefined list; entire shapefile will be used.")
    }
  )
  
  country <- a_countries %>%
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")
}


if (nrow(country) == 0) stop("No country with GMI_CNTRY == ", COUNTRY_CODE)


if (sf::st_crs(a_countries)$epsg != sf::st_crs(a_subs)$epsg) {
  a_subs <- sf::st_transform(a_subs, sf::st_crs(a_countries))
}

# Intersections: subbasins partially or fully covered by the country polygon
# inter_idx <- sf::st_intersects(a_subs, country, sparse = TRUE)
# sel <- lengths(inter_idx) > 0
# subs_sel <- a_subs[sel, ]
subs_sel <- sf::st_filter(a_subs%>%
                            sf::st_make_valid() , country%>%
                            sf::st_make_valid() , .predicate = sf::st_intersects)

# Classify as FULL vs PARTIAL coverage (by area ratio of intersection)

if(!is.null(COUNTRY_CODE)){
  inter_geom <- sf::st_intersection(sf::st_make_valid(subs_sel), sf::st_make_valid(country))
  
}else{
  inter_geom <- sf::st_intersection(sf::st_make_valid(subs_sel), sf::st_make_valid(subs_sel))
  
}
area_sub   <- sf::st_area(subs_sel%>%
                            sf::st_make_valid() )
area_int   <- sf::st_area(inter_geom%>%
                            sf::st_make_valid() )
cover      <- as.numeric(area_int) / as.numeric(area_sub)

subs_sel$coverage_class <- ifelse(cover >= 0.5, "FULL", "PARTIAL")
subs_sel$coverage_ratio <- cover

HYBAS_IDS <- as.factor(subs_sel$HYBAS_ID)
length(HYBAS_IDS)



# 1) Select the user's country and find covered subbasins
domain_plot <- ggplot2::ggplot()+
  geom_sf(data=subs_sel)+
  geom_sf(data = country, fill=NA, alpha=0.2,color="red")+
  theme_minimal()+
  ggspatial::annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    height = unit(1.2, "cm"),
    width = unit(1.2, "cm"),
    pad_x = unit(-0.1, "cm"),
    pad_y = unit(0.1, "cm")
  )+ ggspatial::annotation_scale(
    location = "br",
    width_hint = 0.3
  )
print(domain_plot)
# 2) Load historical data for selected subbasins
hist_df <- read_historical_df_yearly(path = PATH_HISTORICAL,
                                     sep = FIELD_SEPERATOR,
                                     id_col =HISTORICAL_DATA_ID_COL,
                                     hybas_ids = HYBAS_IDS,
                                     missing_value_code =MISSING_VALUE_CODE,
                                     check_warn =  TRUE,fyear = FYEAR) %>%
  rename(DATE = YYYY) %>%
  distinct(HYBAS_ID, DATE, .keep_all = TRUE) %>%
  dplyr::filter(DATE>=start_year,DATE<=end_year)

# Sanity check
hist_df %>% group_by(HYBAS_ID) %>% summarise(n = n(), .groups = "drop") %>% head(10)


# 3) Catalog available predictor files (PRCP / SST)

pred_catalog <- catalog_predictors(base_dir = PATH_PREDICTORS,
                                   vars_keep = PREDICTOR_VARS) %>%
  dplyr::filter(init_year==FYEAR)
unique(pred_catalog$model)
