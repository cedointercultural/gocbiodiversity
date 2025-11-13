#' @title Validar y optimizar grid para área de estudio
#' @description
#' Aplica filtros inteligentes al grid para optimizar consultas, preservando
#' áreas oceánicas relevantes para biodiversidad marina
#'
#' @param grid Grid sf object generado por generate_grid_bboxes
#' @param original_polygon Polígono original del área de estudio
#' @param validation_type Tipo de validación: "intersects", "contains", "buffer", "none"
#' @param ocean_buffer_km Buffer en km para incluir áreas oceánicas adyacentes
#' @return Grid validado y optimizado
#' @export
validate_grid_spatial <- function(grid, original_polygon,
                                  validation_type = "intersects",
                                  ocean_buffer_km = 10) {

  cat("🔍 Aplicando validación espacial del grid...\n")

  if (validation_type == "none") {
    cat("  - Validación deshabilitada: conservando todas las celdas\n")
    return(grid)
  }

  tryCatch({
    # Crear buffer del polígono original para incluir áreas oceánicas
    if (ocean_buffer_km > 0) {
      # Convertir km a grados aproximadamente (1 grado ≈ 111 km)
      buffer_degrees <- ocean_buffer_km / 111
      polygon_buffered <- sf::st_buffer(original_polygon, buffer_degrees)
      cat("  - Buffer oceánico aplicado:", ocean_buffer_km, "km\n")
    } else {
      polygon_buffered <- original_polygon
    }

    # Aplicar validación según el tipo
    valid_cells <- switch(validation_type,
                          "intersects" = {
                            # Celdas que intersectan con el polígono (más permisivo)
                            intersections <- sf::st_intersects(grid$geometry, polygon_buffered, sparse = FALSE)
                            apply(intersections, 1, any)
                          },
                          "contains" = {
                            # Solo celdas completamente dentro del polígono (más restrictivo)
                            within_checks <- sf::st_within(grid$geometry, polygon_buffered, sparse = FALSE)
                            apply(within_checks, 1, any)
                          },
                          "buffer" = {
                            # Celdas dentro del polígono + buffer oceánico
                            intersections <- sf::st_intersects(grid$geometry, polygon_buffered, sparse = FALSE)
                            apply(intersections, 1, any)
                          },
                          {
                            # Default: todas las celdas son válidas
                            rep(TRUE, nrow(grid))
                          }
    )

    original_count <- nrow(grid)
    filtered_grid <- grid[valid_cells, ]
    final_count <- nrow(filtered_grid)

    cat("  - Validación:", validation_type, "\n")
    cat("  - Celdas originales:", original_count, "\n")
    cat("  - Celdas válidas:", final_count, sprintf("(%.1f%%)\n",
                                                    100 * final_count / original_count))
    cat("  - Celdas removidas:", original_count - final_count, "\n")

    # Recalcular box_ids para mantener secuencia
    if (final_count > 0) {
      filtered_grid$box_id <- 1:final_count
    }

    cat("✅ Validación espacial completada\n")

    return(filtered_grid)

  }, error = function(e) {
    warning("Error en validación espacial: ", e$message,
            ". Conservando grid completo.")
    return(grid)
  })
}
