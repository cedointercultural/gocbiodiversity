#' @title Crear objeto espacial sf desde coordenadas
#'
#' @description Convierte un data frame con coordenadas a objeto sf
#'
#' @param data Data frame con columnas lon y lat
#' @param crs Sistema de coordenadas (por defecto 4326)
#' @return Objeto sf
#' @export
create_spatial_points <- function(data, crs = 4326) {
  if (!"lon" %in% names(data) || !"lat" %in% names(data)) {
    stop("El data frame debe contener columnas 'lon' y 'lat'")
  }

  # Filtrar coordenadas válidas
  valid_coords <- validate_coordinates(data$lon, data$lat)
  data_valid <- data[valid_coords, ]

  if (nrow(data_valid) == 0) {
    stop("No hay coordenadas válidas en los datos")
  }

  # Crear objeto sf
  spatial_data <- sf::st_as_sf(
    data_valid,
    coords = c("lon", "lat"),
    crs = crs,
    remove = FALSE
  )

  cat("✓ Objeto espacial creado:", nrow(spatial_data), "puntos\n")

  return(spatial_data)
}
