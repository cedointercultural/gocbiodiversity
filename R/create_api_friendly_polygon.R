#' @title Crear polígono apropiado para consultas a bases de datos
#' @description Genera diferentes versiones de polígono optimizadas para APIs específicas
#'
#' @param vertices Matrix o data frame con columnas lon, lat
#' @param method Método de simplificación: "bbox", "convex_hull", "simplified", "grid_friendly"
#' @return Lista con polígono y parámetros para APIs
#' @export
create_api_friendly_polygon <- function(vertices, method = "grid_friendly") {

  cat(sprintf("\n🎯 CREANDO POLÍGONO PARA APIs (método: %s):\n", method))
  cat("═══════════════════════════════════════════════════════════════\n")

  result <- list(method = method)

  if (method == "bbox") {
    # Método 1: Bounding box simple (rectángulo)
    min_lon <- min(vertices[, "lon"])
    max_lon <- max(vertices[, "lon"])
    min_lat <- min(vertices[, "lat"])
    max_lat <- max(vertices[, "lat"])

    # Crear rectángulo
    bbox_coords <- matrix(c(
      min_lon, min_lat,
      max_lon, min_lat,
      max_lon, max_lat,
      min_lon, max_lat,
      min_lon, min_lat
    ), ncol = 2, byrow = TRUE)

    result$coordinates <- bbox_coords
    result$wkt <- sprintf("POLYGON((%s))",
                          paste(apply(bbox_coords, 1, function(x) paste(x[1], x[2])), collapse = ","))
    result$bbox_string <- paste(min_lon, min_lat, max_lon, max_lat, sep = ",")

    cat("✓ Polígono rectangular (bounding box)\n")
    cat(sprintf("  Coordenadas: [%.4f, %.4f] a [%.4f, %.4f]\n", min_lon, min_lat, max_lon, max_lat))

  } else if (method == "convex_hull") {
    # Método 2: Convex hull de los vértices
    vertices_sf <- st_sfc(st_multipoint(vertices), crs = 4326)
    hull <- st_convex_hull(vertices_sf)
    hull_coords <- st_coordinates(hull)[, c("X", "Y")]

    result$coordinates <- hull_coords
    result$wkt <- st_as_text(hull)

    # Calcular bbox del convex hull
    hull_bbox <- st_bbox(hull)
    result$bbox_string <- paste(hull_bbox[1], hull_bbox[2], hull_bbox[3], hull_bbox[4], sep = ",")

    cat("✓ Polígono convex hull (envolvente convexa)\n")
    cat(sprintf("  Vértices: %d\n", nrow(hull_coords)))

  } else if (method == "simplified") {
    # Método 3: Polígono simplificado manteniendo forma aproximada
    # Tomar cada N vértices para reducir complejidad
    n_vertices <- nrow(vertices)
    step <- max(1, round(n_vertices / 50))  # Máximo 50 vértices
    simplified_indices <- seq(1, n_vertices, by = step)

    simplified_coords <- vertices[simplified_indices, ]

    # Asegurar que el polígono esté cerrado
    if (!all(simplified_coords[1, ] == simplified_coords[nrow(simplified_coords), ])) {
      simplified_coords <- rbind(simplified_coords, simplified_coords[1, ])
    }

    result$coordinates <- simplified_coords
    result$wkt <- sprintf("POLYGON((%s))",
                          paste(apply(simplified_coords, 1, function(x) paste(x[1], x[2])), collapse = ","))

    # Calcular bbox
    bbox <- c(min(simplified_coords[, "lon"]), min(simplified_coords[, "lat"]),
              max(simplified_coords[, "lon"]), max(simplified_coords[, "lat"]))
    result$bbox_string <- paste(bbox, collapse = ",")

    cat("✓ Polígono simplificado\n")
    cat(sprintf("  Vértices: %d → %d (cada %d puntos)\n", n_vertices, nrow(simplified_coords), step))

  } else if (method == "grid_friendly") {
    # Método 4: Optimizado para generar grids efectivos
    # Usar bounding box pero expandido ligeramente para asegurar cobertura
    min_lon <- min(vertices[, "lon"])
    max_lon <- max(vertices[, "lon"])
    min_lat <- min(vertices[, "lat"])
    max_lat <- max(vertices[, "lat"])

    # Expandir 1% en cada dirección para asegurar cobertura
    lon_range <- max_lon - min_lon
    lat_range <- max_lat - min_lat
    expansion <- 0.01  # 1%

    min_lon <- min_lon - lon_range * expansion
    max_lon <- max_lon + lon_range * expansion
    min_lat <- min_lat - lat_range * expansion
    max_lat <- max_lat + lat_range * expansion

    # Crear rectángulo expandido
    bbox_coords <- matrix(c(
      min_lon, min_lat,
      max_lon, min_lat,
      max_lon, max_lat,
      min_lon, max_lat,
      min_lon, min_lat
    ), ncol = 2, byrow = TRUE)

    result$coordinates <- bbox_coords
    result$wkt <- sprintf("POLYGON((%s))",
                          paste(apply(bbox_coords, 1, function(x) paste(x[1], x[2])), collapse = ","))
    result$bbox_string <- paste(min_lon, min_lat, max_lon, max_lat, sep = ",")

    # Calcular dimensiones para grid
    result$grid_info <- list(
      lon_range = max_lon - min_lon,
      lat_range = max_lat - min_lat,
      recommended_grid_size = min(2.0, max(0.5, min(lon_range, lat_range) / 10)),
      max_cells_1deg = ceiling((max_lon - min_lon)) * ceiling((max_lat - min_lat)),
      max_cells_05deg = ceiling((max_lon - min_lon) * 2) * ceiling((max_lat - min_lat) * 2)
    )

    cat("✓ Polígono optimizado para grid\n")
    cat(sprintf("  Coordenadas expandidas: [%.4f, %.4f] a [%.4f, %.4f]\n", min_lon, min_lat, max_lon, max_lat))
    cat(sprintf("  Expansión aplicada: %.1f%%\n", expansion * 100))
    cat(sprintf("  Dimensiones: %.2f° × %.2f°\n", lon_range, lat_range))
    cat(sprintf("  Grid recomendado: %.1f°\n", result$grid_info$recommended_grid_size))
    cat(sprintf("  Celdas estimadas (1°): %d\n", result$grid_info$max_cells_1deg))
    cat(sprintf("  Celdas estimadas (0.5°): %d\n", result$grid_info$max_cells_05deg))
  }

  return(result)
}

