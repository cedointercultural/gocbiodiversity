#' @title Calcular estadísticas espaciales
#' @description Genera resumen estadístico de la distribución espacial de los puntos
#'
#' @param data Data frame con columnas lon y lat
#' @return Lista con estadísticas espaciales
#' @export
calculate_spatial_statistics <- function(data) {
  if (!"lon" %in% names(data) || !"lat" %in% names(data)) {
    stop("El data frame debe contener columnas 'lon' y 'lat'")
  }

  # Filtrar coordenadas válidas
  valid_coords <- validate_coordinates(data$lon, data$lat)
  data_valid <- data[valid_coords, ]

  if (nrow(data_valid) == 0) {
    return(list(
      n_points = 0,
      lon_range = c(NA, NA),
      lat_range = c(NA, NA),
      centroid = c(lon = NA, lat = NA)
    ))
  }

  stats <- list(
    n_points = nrow(data_valid),
    lon_range = range(data_valid$lon, na.rm = TRUE),
    lat_range = range(data_valid$lat, na.rm = TRUE),
    centroid = c(
      lon = mean(data_valid$lon, na.rm = TRUE),
      lat = mean(data_valid$lat, na.rm = TRUE)
    ),
    lon_sd = sd(data_valid$lon, na.rm = TRUE),
    lat_sd = sd(data_valid$lat, na.rm = TRUE)
  )

  return(stats)
}
