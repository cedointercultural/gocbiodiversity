#' @title Generar grid optimizado para polígono simplificado
#' @description Crea un grid eficiente basado en las características del polígono
#'
#' @param api_polygon Resultado de create_api_friendly_polygon()
#' @param target_cells Número objetivo de celdas (default: 20)
#' @param max_cells Número máximo de celdas (default: 50)
#' @return Data frame con grid optimizado
#' @export
generate_optimized_grid <- function(api_polygon, target_cells = 20, max_cells = 50) {

  cat(sprintf("\n🔢 GENERANDO GRID OPTIMIZADO (objetivo: %d celdas, máximo: %d):\n", target_cells, max_cells))
  cat("═══════════════════════════════════════════════════════════════\n")

  if (api_polygon$method == "grid_friendly" && !is.null(api_polygon$grid_info)) {
    # Usar información del grid calculada
    grid_info <- api_polygon$grid_info

    # Calcular tamaño de grid basado en número objetivo de celdas
    total_area_deg2 <- grid_info$lon_range * grid_info$lat_range
    target_cell_size <- sqrt(total_area_deg2 / target_cells)

    # Ajustar a valores estándar de grid
    standard_sizes <- c(0.1, 0.25, 0.5, 1.0, 2.0, 5.0)
    grid_size <- standard_sizes[which.min(abs(standard_sizes - target_cell_size))]

    # Verificar que no exceda el máximo de celdas
    estimated_cells <- ceiling(grid_info$lon_range / grid_size) * ceiling(grid_info$lat_range / grid_size)

    if (estimated_cells > max_cells) {
      # Aumentar tamaño de grid para reducir número de celdas
      grid_size <- standard_sizes[which(ceiling(grid_info$lon_range / standard_sizes) *
                                          ceiling(grid_info$lat_range / standard_sizes) <= max_cells)[1]]
      estimated_cells <- ceiling(grid_info$lon_range / grid_size) * ceiling(grid_info$lat_range / grid_size)
    }

    cat(sprintf("✓ Tamaño de grid optimizado: %.2f°\n", grid_size))
    cat(sprintf("✓ Celdas estimadas: %d\n", estimated_cells))
    cat(sprintf("✓ Eficiencia: %.1f celdas/grado²\n", estimated_cells / total_area_deg2))

  } else {
    # Fallback para otros métodos
    bbox_parts <- as.numeric(strsplit(api_polygon$bbox_string, ",")[[1]])
    lon_range <- bbox_parts[3] - bbox_parts[1]
    lat_range <- bbox_parts[4] - bbox_parts[2]

    total_area_deg2 <- lon_range * lat_range
    target_cell_size <- sqrt(total_area_deg2 / target_cells)

    standard_sizes <- c(0.1, 0.25, 0.5, 1.0, 2.0, 5.0)
    grid_size <- standard_sizes[which.min(abs(standard_sizes - target_cell_size))]

    estimated_cells <- ceiling(lon_range / grid_size) * ceiling(lat_range / grid_size)

    cat(sprintf("✓ Tamaño de grid calculado: %.2f°\n", grid_size))
    cat(sprintf("✓ Celdas estimadas: %d\n", estimated_cells))
  }

  return(list(
    grid_size = grid_size,
    estimated_cells = estimated_cells,
    bbox_string = api_polygon$bbox_string,
    wkt = api_polygon$wkt,
    method_used = api_polygon$method
  ))
}
