#' @title Obtener estadísticas del grid optimizado
#'
#' @param grid Grid sf object
#' @param original_polygon Polígono original
#' @return Lista con estadísticas del grid
#' @export
get_grid_statistics <- function(grid, original_polygon) {

  # Área del polígono original
  polygon_area_km2 <- sum(as.numeric(sf::st_area(original_polygon))) / 1e6

  # Área total del grid
  if (nrow(grid) > 0) {
    cell_area_km2 <- as.numeric(sf::st_area(grid$geometry[1])) / 1e6
    total_grid_area_km2 <- cell_area_km2 * nrow(grid)

    # Bounding box del grid
    grid_bbox <- sf::st_bbox(grid$geometry)

    list(
      n_cells = nrow(grid),
      cell_area_km2 = cell_area_km2,
      total_grid_area_km2 = total_grid_area_km2,
      polygon_area_km2 = polygon_area_km2,
      coverage_ratio = total_grid_area_km2 / polygon_area_km2,
      grid_bbox = grid_bbox,
      efficiency = polygon_area_km2 / total_grid_area_km2
    )
  } else {
    list(
      n_cells = 0,
      cell_area_km2 = 0,
      total_grid_area_km2 = 0,
      polygon_area_km2 = polygon_area_km2,
      coverage_ratio = 0,
      grid_bbox = c(xmin = NA, ymin = NA, xmax = NA, ymax = NA),
      efficiency = 0
    )
  }
}
