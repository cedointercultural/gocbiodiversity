#' @title Simplificar geometría de polígono
#'
#' @description Reduce la complejidad de un polígono manteniendo su forma general
#'
#' @param polygon Objeto sf con geometría
#' @param tolerance Tolerancia para simplificación (grados)
#' @return Objeto sf simplificado
#' @export
simplify_polygon <- function(polygon, tolerance = 0.01) {

  original_vertices <- sum(sapply(sf::st_geometry(polygon), function(x) nrow(sf::st_coordinates(x))))

  simplified <- sf::st_simplify(polygon, dTolerance = tolerance)

  simplified_vertices <- sum(sapply(sf::st_geometry(simplified), function(x) nrow(sf::st_coordinates(x))))

  reduction <- round((1 - simplified_vertices/original_vertices) * 100, 1)

  cat("✓ Polígono simplificado\n")
  cat("  - Vértices originales:", original_vertices, "\n")
  cat("  - Vértices simplificados:", simplified_vertices, "\n")
  cat("  - Reducción:", reduction, "%\n")

  return(simplified)
}
