# ==============================================================================
# STEP 7: GENERATE MAPS
# ==============================================================================
# This step creates visualization maps:
# - Probability maps
# - Class maps
# - Entropy maps
#
# Expected result:
# PNG figures ready for reporting and communication
# ==============================================================================

message("▶ STEP 7: Generate and save maps — START")


tryCatch({
  res <- plot_entropy_map_facet(
    df_entropy = probabilities %>% mutate(HYBAS_ID = as.factor(HYBAS_ID)),
    sf_bassins = sf_basins,
    period_start = min(fyears) ,
    period_end   = max(fyears),
    facet_by = "year",
    agg = "mean"   # utile si plusieurs lignes par bassin-année
  )
  entropy_plot <- res$plot+
    geom_sf(data=country, fill=NA)
  
  print(entropy_plot)
  proba_plot <- WASS2SHydroR::wass2s_plot_map(sf_basins =sf_basins,
                                              data = yprobas,
                                              basin_col = "HYBAS_ID",
                                              layers = layers) + annotation_north_arrow(
                                                location = "tr",
                                                which_north = "true",
                                                style = north_arrow_fancy_orienteering,
                                                height = unit(1.2, "cm"),
                                                width = unit(1.2, "cm"),
                                                pad_x = unit(-0.1, "cm"),
                                                pad_y = unit(0.1, "cm")
                                              )+ annotation_scale(
                                                location = "br",
                                                width_hint = 0.3
                                              )+
    scale_fill_gradient(
      low = "#deebf7", high = "#08519c",
      name = "Probability",
      limits = c(0, 1)
    )
  
  print(proba_plot)
  
  
  message("Building class map ...")

  class_plot <- WASS2SHydroR::wass2s_plot_map(sf_basins =sf_basins,
                                              data = yprobas,
                                              basin_col = "HYBAS_ID",
                                              type = "class",
                                              layers = layers)+
    theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5) )+
    annotation_north_arrow(
      location = "tr",
      which_north = "true",
      style = north_arrow_fancy_orienteering,
      height = unit(1.2, "cm"),
      width = unit(1.2, "cm"),
      pad_x = unit(-0.1, "cm"),
      pad_y = unit(0.1, "cm")
    )+ annotation_scale(
      location = "br",
      width_hint = 0.3
    )
  print(class_plot)
  # ==============================================================================
  # 8) SAVE MAPS
  # ==============================================================================
  prefix <- paste0()
  filename_proba <- paste0(COUNTRY_CODE, "_", PREDICTOR_VARS,"_",fyear,"_cons_probas_", FINAL_FUSER, "_", timestamp, ".png")
  ggsave(filename = filename_proba,
         plot = proba_plot,
         path = file.path(PATH_OUTPUT,"figures"),
         width = 9.5,
         height = 6.5,
         dpi = 600,
         bg = "white")
  
  
  filename_class <- paste0(COUNTRY_CODE, "_", PREDICTOR_VARS,"_",fyear,"_cons_class_", FINAL_FUSER, "_", timestamp, ".png")
  ggsave(filename = filename_class,
         plot = class_plot,
         path = file.path(PATH_OUTPUT,"figures"),
         width = 9.5,
         height = 6.5,
         dpi = 600,
         bg = "white")
  
  
  filename_entropy <- paste0(COUNTRY_CODE, "_", PREDICTOR_VARS,"_",fyear,"_",paste0(tolower(APPROACH)),"_cons_entropy_", FINAL_FUSER, "_", timestamp, ".png")
  ggsave(filename = filename_entropy,
         plot = entropy_plot,
         path = file.path(PATH_OUTPUT,"figures"),
         width = 9.5,
         height = 6.5,
         dpi = 600,
         bg = "white")
  
  message("✔ STEP 7: Generate and save maps — COMPLETED SUCCESSFULLY")
  
}, error = function(e) {
  message("❌ STEP 7: Generate and save maps — FAILED")
  stop(e)
})

