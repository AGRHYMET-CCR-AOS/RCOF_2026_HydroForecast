paths <- list(
  ML = list(
    PRCP = ML_PRCP_PATH,
    SST  = ML_SST_PATH
  ),
  
  STAT = list(
    PRCP = STAT_PRCP_PATH,
    SST  =STAT_SST_PATH 
  ),
  
  HYDRO = list(
    Q = HYDRO_Q_PATH
  )
)


# -----------------------------
# Fonction simple
# -----------------------------
read_and_prepare <- function(path, approach, variable) {
  
  if (is.null(path) || path == "") return(NULL)
  
  df <- read.csv(path, stringsAsFactors = FALSE)
  
  # Colonnes modèles
  #pred_cols <- setdiff(names(df), c("HYBAS_ID", "YYYY", "Q"))
  pred_cols <- "pred_final"
  # Préfixe propre
  new_names <- paste0(approach, "_", variable, "_", pred_cols)
  names(df)[match(pred_cols, names(df))] <- new_names
  df <- df[,c("HYBAS_ID", "YYYY", "Q",new_names)]
  df
}

# -----------------------------
# Construire la liste des dataframes
# -----------------------------
data_list <- list()

for (approach in names(paths)) {
  for (variable in names(paths[[approach]])) {
    
    path <- paths[[approach]][[variable]]
    
    df <- read_and_prepare(path, approach, variable)
    
    if (!is.null(df)) {
      data_list[[length(data_list) + 1]] <- df
    }
  }
}

# -----------------------------
# Fusion horizontale
# -----------------------------
merged <- data_list[[1]]

for (i in 2:length(data_list)) {
  df_i <- data_list[[i]]
  
  # enlever Q pour éviter duplication
  if ("Q" %in% names(df_i)) {
    df_i <- df_i %>% select(-Q)
  }
  
  merged <- merged %>%
    full_join(df_i, by = c("HYBAS_ID", "YYYY"))
}

# -----------------------------
# (Optionnel) Médiane finale
# -----------------------------
merged <- merged %>%
  rowwise() %>%
  mutate(
    Consolidated_frcst = median(
      c_across(-c(HYBAS_ID, YYYY, Q)),
      na.rm = TRUE
    )
  ) %>%
  ungroup()

# -----------------------------

