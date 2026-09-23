# --- 1. Récupération des métadonnées des stations ---

url_meta <- "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn-precip/ogd-smn-precip_meta_stations.csv"


stations <- read_delim(

  url_meta,

  delim = ";",

  locale = locale(encoding = "Windows-1252"),

  show_col_types = FALSE

) %>%

  select(

    station_id = station_abbr,

    station_name = station_name,

    canton = station_canton,

    elevation = station_height_masl,

    lat = station_coordinates_wgs84_lat,

    lon = station_coordinates_wgs84_lon

  ) %>%

  filter(!is.na(lat) & !is.na(lon))


# --- 2. Fonction de téléchargement utilisant la bonne structure d'URL ---

fetch_nbcn_station <- function(stn_id) {

  stn_code <- tolower(stn_id)



  # Construction du lien avec le sous-dossier de la station

  url_station <- sprintf(

    "https://data.geo.admin.ch/ch.meteoschweiz.ogd-nbcn-precip/%s/ogd-nbcn-precip_%s_m.csv",

    stn_code, stn_code

  )



  temp_loc <- tempfile(fileext = ".csv")



  # Téléchargement via cURL pour éviter les blocages 403

  cmd_curl <- sprintf(

    'curl -s -L -H "User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64)" "%s" -o "%s"',

    url_station, temp_loc

  )



  system(cmd_curl)



  # Vérification que le fichier est valide (non vide)

  if (!file.exists(temp_loc) || file.info(temp_loc)$size < 200) {

    if (file.exists(temp_loc)) unlink(temp_loc)

    return(NULL)

  }



  # Lecture du fichier CSV

  df <- tryCatch({

    read_delim(

      temp_loc,

      delim = ";",

      locale = locale(encoding = "Windows-1252"),

      show_col_types = FALSE,

      progress = FALSE

    )

  }, error = function(e) NULL)



  unlink(temp_loc)



  if (is.null(df) || nrow(df) == 0) return(NULL)



  names(df) <- tolower(names(df))



  # Identification de la colonne de précipitations

  target_col <- names(df)[str_detect(names(df), "rhs150m0|rre150m0|precip")]

  if (length(target_col) == 0) return(NULL)



  # Extraction de la dernière ligne disponible

  latest_val <- df %>%

    filter(!is.na(.data[[target_col[1]]]) & .data[[target_col[1]]] != "") %>%

    tail(1) %>%

    select(

      date = reference_timestamp,

      precip_mensuelle_mm = all_of(target_col[1])

    ) %>%

    mutate(

      station_id = toupper(stn_id),

      precip_mensuelle_mm = as.numeric(precip_mensuelle_mm)

    )



  return(latest_val)

}


# --- 3. Exécution pour l'ensemble des stations ---

message("Téléchargement des données mensuelles pour les stations NBCN...")


all_results <- map(stations$station_id, fetch_nbcn_station, .progress = TRUE) %>%

  bind_rows()


# --- 4. Fusion avec les coordonnées géographiques ---

final_df <- stations %>%

  inner_join(all_results, by = "station_id") %>%

  select(station_id, station_name, canton, elevation, lat, lon, date, precip_mensuelle_mm)


# Affichage du résultat dans la console

glimpse(final_df)


# Exportation du CSV prêt pour Datawrapper / Flourish

write_csv(final_df, "precipitations_mensuelles_nbcn_suisse.csv")

message("Export réussi ! Le fichier 'precipitations_mensuelles_nbcn_suisse.csv' est prêt.")
