
# -----------------------------------------------------------------------------
# FICHIER 2 : Précipitations mensuelles cumulées par station en septembre 2025
# (Septembre 2025 complet : 1er au 30)
# -----------------------------------------------------------------------------
message("Génération du fichier 2 : Précipitations par station en Septembre 2025...")

sept_2025_by_station <- all_daily_data %>%
  filter(year(date_clean) == 2025 & month(date_clean) == 9) %>%
  group_by(station_id) %>%
  summarise(
    nb_jours_observes = n(),
    cumul_septembre_2025_mm = round(sum(precip_mm, na.rm = TRUE), 2),
    moyenne_quotidienne_mm = round(mean(precip_mm, na.rm = TRUE), 2),
    .groups = "drop"
  )

file_sept_2025 <- stations %>%
  inner_join(sept_2025_by_station, by = "station_id")

write_csv(file_sept_2025, "2_precipitations_stations_septembre_2025.csv")
