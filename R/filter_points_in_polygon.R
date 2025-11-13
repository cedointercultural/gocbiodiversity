
#' @title Filtrar puntos dentro de polígono
#'
#' @description Selecciona solo los puntos que caen dentro de un polígono dado
#'
#' @param points_data Data frame con columnas lon y lat
#' @param polygon Objeto sf con polígono
#' @return Data frame filtrado con puntos dentro del polígono
#' @export
filter_points_in_polygon <- function(points_data, polygon) {

  # Crear puntos espaciales
  points_sf <- create_spatial_points(points_data)

  # Asegurar mismo CRS
  if (sf::st_crs(points_sf) != sf::st_crs(polygon)) {
    polygon <- sf::st_transform(polygon, sf::st_crs(points_sf))
  }

  # Realizar intersección espacial
  points_in_polygon <- sf::st_intersection(points_sf, polygon)

  # Convertir de vuelta a data frame
  result <- as.data.frame(points_in_polygon)
  result$geometry <- NULL  # Eliminar columna de geometría

  n_original <- nrow(points_data)
  n_filtered <- nrow(result)
  n_removed <- n_original - n_filtered

  cat("✓ Filtrado espacial completado\n")
  cat("  - Puntos originales:", n_original, "\n")
  cat("  - Puntos dentro del polígono:", n_filtered, "\n")
  cat("  - Puntos removidos:", n_removed, "\n")

  return(result)
}
