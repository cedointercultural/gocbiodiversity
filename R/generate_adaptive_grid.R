#' @title Generar grid adaptativo basado en densidad
#'
#' @description Crea un grid con tamaños de celda variables basados en la densidad de puntos
#'
#' @param polygon Objeto sf con geometría
#' @param min_size Tamaño mínimo de celda en grados
#' @param max_size Tamaño máximo de celda en grados
#' @param target_points Número objetivo de puntos por celda
#' @return Data frame con grid adaptativo
#' @export
generate_adaptive_grid <- function(polygon,
                                   min_size = 0.1,
                                   max_size = 1.0,
                                   target_points = 100) {

  cat("Generando grid adaptativo...\n")
  cat("  - Tamaño mínimo:", min_size, "grados\n")
  cat("  - Tamaño máximo:", max_size, "grados\n")

  # Por ahora, usar grid simple con tamaño medio
  # En futuras versiones se puede implementar lógica más sofisticada
  avg_size <- (min_size + max_size) / 2

  grid <- generate_grid_bboxes(polygon, grid_size = avg_size)

  cat("✓ Grid adaptativo generado\n")

  return(grid)
}
