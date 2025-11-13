#' @title Generar resumen estadístico de datos de biodiversidad
#'
#' @param data Data frame con registros de biodiversidad
#' @return Lista con estadísticas descriptivas
#' @export
summarize_biodiversity_data <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(
      total_records = 0,
      unique_species = 0,
      sources = character(0),
      year_range = c(NA, NA),
      coord_range = list(lon = c(NA, NA), lat = c(NA, NA))
    ))
  }

  summary <- list(
    total_records = nrow(data),
    unique_species = length(unique(data$species[!is.na(data$species)])),
    sources = table(data$source),
    year_range = if(any(!is.na(data$year)))
      range(data$year, na.rm = TRUE) else c(NA, NA),
    coord_range = list(
      lon = range(data$lon, na.rm = TRUE),
      lat = range(data$lat, na.rm = TRUE)
    ),
    records_with_date = sum(!is.na(data$date_recorded)),
    records_with_taxonRank = sum(!is.na(data$taxonRank))
  )

  return(summary)
}
