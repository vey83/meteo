library(tidyverse)
library(lubridate)
library(janitor)

# --- 1. METADONNEES COMPLÈTES DES STATIONS ---
url_meta <- "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn-precip/ogd-smn-precip_meta_stations.csv"

stations <- read_delim(
  url_meta,
  delim = ";",
  locale = locale(encoding = "Windows-1252"),
  show_col_types = FALSE
) %>%
  clean_names() %>%
  mutate(station_id = toupper(station_abbr)) %>%
  filter(!is.na(station_coordinates_wgs84_lat) & !is.na(station_coordinates_wgs84_lon))

# --- 2. FONCTION POUR EXTRAIRE ET AGREGATION DE SEPTEMBRE (1 AU 23) ---
fetch_september_daily <- function(stn_id) {
  stn_code <- tolower(stn_id)

  # Utilisation du fichier quotidien 'recent' (qui contient l'année en cours jour par jour)
  url_station <- sprintf(
    "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn-precip/%s/ogd-smn-precip_%s_d_recent.csv",
    stn_code, stn_code
  )

  temp_loc <- tempfile(fileext = ".csv")

  cmd_curl <- sprintf(
    'curl -s -L -H "User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64)" "%s" -o "%s"',
    url_station, temp_loc
  )
  system(cmd_curl)

  if (!file.exists(temp_loc) || file.info(temp_loc)$size < 500) {
    if (file.exists(temp_loc)) unlink(temp_loc)
    return(NULL)
  }

  df_summary <- tryCatch({
    data_day <- read_delim(
      temp_loc,
      delim = ";",
      locale = locale(encoding = "Windows-1252"),
      show_col_types = FALSE,
      progress = FALSE
    ) %>% clean_names()

    # Identification de la colonne de précipitation quotidienne (rre150d0)
    col_precip <- names(data_day)[str_detect(names(data_day), "rre150d0|rhs150d0|precip")][1]

    if (is.na(col_precip)) return(NULL)

    # Filtrage et calculs du 1er au 23 septembre 2026
    data_day %>%
      mutate(
        date_clean = dmy_hm(reference_timestamp),
        precip_mm = as.numeric(.data[[col_precip]])
      ) %>%
      # Filtre sur le mois de septembre 2026 jusqu'au 23
      filter(
        year(date_clean) == 2026 &
          month(date_clean) == 9 &
          day(date_clean) <= 23 &
          !is.na(precip_mm) & precip_mm >= 0
      ) %>%
      summarise(
        nb_jours_observes = n(),
        debut_periode = min(as.Date(date_clean)),
        fin_periode = max(as.Date(date_clean)),
        cumul_septembre_mm = round(sum(precip_mm, na.rm = TRUE), 2),
        moyenne_quotidienne_mm = round(mean(precip_mm, na.rm = TRUE), 2)
      ) %>%
      mutate(station_id = toupper(stn_id))

  }, error = function(e) NULL)

  unlink(temp_loc)
  return(df_summary)
}

# --- 3. EXECUTION SUR TOUTES LES STATIONS ---
message("Calcul des bilans du 1er au 23 septembre 2026...")

september_results <- map(stations$station_id, fetch_september_daily, .progress = TRUE) %>%
  bind_rows()

# --- 4. FUSION AVEC L'ENSEMBLE DES COLONNES DE METADONNEES ---
final_september_df <- stations %>%
  inner_join(september_results, by = "station_id")

# Affichage du tableau résultat
glimpse(final_september_df)

# Exportation CSV pour Datawrapper / Flourish
write_csv(final_september_df, "precipitations_septembre_1au23.csv")
message("Succès ! Le fichier 'precipitations_septembre_1au23.csv' est prêt.")
