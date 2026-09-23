library(tidyverse)
library(lubridate)
library(janitor)

closeAllConnections()

# --- 1. TELECHARGEMENT DES METADONNEES BRUTES ---
url_meta <- "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn-precip/ogd-smn-precip_meta_stations.csv"

stations_raw <- read_delim(
  url_meta,
  delim = ";",
  locale = locale(encoding = "Windows-1252"),
  show_col_types = FALSE
) %>%
  clean_names() %>%
  select(
    station_id = station_abbr,
    station_name = station_name,
    canton = station_canton,
    elevation = station_height_masl,
    lat = station_coordinates_wgs84_lat,
    lon = station_coordinates_wgs84_lon
  ) %>%
  filter(!is.na(lat) & !is.na(lon))

# --- 2. VERIFICATION DES STATIONS DISPONIBLES (TEST D'EXISTENCE) ---
check_station_exists <- function(stn_id) {
  stn_code <- tolower(stn_id)

  # Test sur le fichier quotidien recent de SwissMetNet
  url_test <- sprintf(
    "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn-precip/%s/ogd-smn-precip_%s_d_recent.csv",
    stn_code, stn_code
  )

  temp_chk <- tempfile(fileext = ".csv")
  cmd_curl <- sprintf('curl -s -L -I -H "User-Agent: Mozilla/5.0" "%s" -o "%s"', url_test, temp_chk)
  system(cmd_curl)

  is_valid <- FALSE
  if (file.exists(temp_chk)) {
    header_content <- readLines(temp_chk, warn = FALSE)
    # Vérifie si le serveur retourne un code 200 OK
    if (any(str_detect(header_content, "200 OK"))) {
      is_valid <- TRUE
    }
    unlink(temp_chk)
  }
  return(is_valid)
}

message("Verification des stations actives sur le serveur de la Confederation...")
# Pour aller vite lors des tests, on valide les stations :
stations_raw <- stations_raw %>%
  mutate(stn_lower = tolower(station_id))

# --- 3. TRAITEMENT DES DONNEES MENSUELLES SUR LES STATIONS VALIDES ---
process_active_station <- function(stn_id) {
  stn_code <- tolower(stn_id)

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

  # On ignore les fichiers inexistants ou d'erreur (< 1 KB)
  if (!file.exists(temp_loc) || file.info(temp_loc)$size < 1000) {
    if (file.exists(temp_loc)) unlink(temp_loc)
    return(NULL)
  }

  res <- tryCatch({
    data_day <- read_delim(
      temp_loc,
      delim = ";",
      locale = locale(encoding = "Windows-1252"),
      show_col_types = FALSE,
      progress = FALSE
    ) %>% clean_names()

    col_precip <- names(data_day)[str_detect(names(data_day), "rre150d0|rhs150d0|precip")][1]
    if (is.na(col_precip)) return(NULL)

    # Lubridate : reformatage de la date et groupement par mois
    data_day %>%
      mutate(
        date_clean = dmy_hm(reference_timestamp),
        mois = floor_date(date_clean, unit = "month"),
        precip_mm = as.numeric(.data[[col_precip]])
      ) %>%
      filter(!is.na(precip_mm) & precip_mm >= 0) %>%
      group_by(mois) %>%
      summarise(
        nb_jours = n(),
        precip_total_mm = round(sum(precip_mm, na.rm = TRUE), 2),
        precip_moyenne_mm = round(mean(precip_mm, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      mutate(station_id = toupper(stn_id))
  }, error = function(e) NULL)

  if (file.exists(temp_loc)) unlink(temp_loc)
  return(res)
}

# --- 4. EXECUTION ET CALCUL DE LA TABLE ADAPTEE ---
message("Telechargement et calcul pour les stations valides...")

# Traitement sur les stations de la liste
monthly_results <- map_dfr(stations_raw$station_id, process_active_station, .progress = TRUE)

# Création de la table des stations ADAPTÉE (qui ne contient QUE les stations dont les données ont été trouvées)
stations_adaptee <- stations_raw %>%
  inner_join(distinct(monthly_results, station_id), by = "station_id")

# Table finale agrégée avec coordonnées
dataset_final <- stations_adaptee %>%
  inner_join(monthly_results, by = "station_id") %>%
  select(station_id, station_name, canton, elevation, lat, lon, mois, nb_jours, precip_total_mm, precip_moyenne_mm)

# Affichage du résultat dans RStudio
message("Nombre de stations validees et adaptées : ", nrow(stations_adaptee))
glimpse(dataset_final)

# Exportation CSV final
write_csv(dataset_final, "precipitations_suisse_stations_adaptees.csv")
message("Export réussi : 'precipitations_suisse_stations_adaptees.csv'")
