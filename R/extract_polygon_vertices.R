#' @title Extraer coordenadas de vértices de un polígono complejo
#' @description Convierte un MULTIPOLYGON complejo en coordenadas de vértices simples apropiadas para consultas a bases de datos
#'
#' @param polygon Objeto sf con geometría compleja
#' @param simplify_tolerance Tolerancia para simplificación (grados, default: 0.01)
#' @param convex_hull Usar convex hull para simplificar (default: FALSE)
#' @return Lista con coordenadas de vértices y polígono simplificado
#' @export
extract_polygon_vertices <- function(polygon, simplify_tolerance = 0.01, convex_hull = FALSE) {

  cat("📐 ANÁLISIS DEL POLÍGONO ORIGINAL:\n")
  cat("═══════════════════════════════════════════════════════════════\n")

  # Información básica
  geom_type <- st_geometry_type(polygon)[1]
  n_features <- nrow(polygon)
  bbox_orig <- st_bbox(polygon)
  coords_orig <- st_coordinates(polygon)
  n_points_orig <- nrow(coords_orig)

  cat(sprintf("• Tipo de geometría: %s\n", geom_type))
  cat(sprintf("• Número de features: %d\n", n_features))
  cat(sprintf("• Número de puntos: %d\n", n_points_orig))
  cat(sprintf("• Bounding box: [%.4f, %.4f] a [%.4f, %.4f]\n",
              bbox_orig[1], bbox_orig[2], bbox_orig[3], bbox_orig[4]))

  area_orig <- sum(as.numeric(st_area(polygon))) / 1e6
  cat(sprintf("• Área total: %.2f km²\n\n", area_orig))

  # Unir todas las geometrías en una sola
  cat("🔄 PROCESANDO POLÍGONO:\n")
  cat("═══════════════════════════════════════════════════════════════\n")

  # Unir todos los polígonos en uno solo
  polygon_union <- st_union(polygon)
  cat("✓ Polígonos unidos en geometría única\n")

  # Aplicar simplificación si se especifica
  if (simplify_tolerance > 0) {
    polygon_simplified <- st_simplify(polygon_union, dTolerance = simplify_tolerance)
    cat(sprintf("✓ Polígono simplificado (tolerancia: %.3f grados)\n", simplify_tolerance))
  } else {
    polygon_simplified <- polygon_union
  }

  # Aplicar convex hull si se especifica
  if (convex_hull) {
    polygon_simplified <- st_convex_hull(polygon_simplified)
    cat("✓ Aplicado convex hull (envolvente convexa)\n")
  }

  # Extraer coordenadas del polígono simplificado
  coords_simplified <- st_coordinates(polygon_simplified)
  n_points_simplified <- nrow(coords_simplified)

  cat(sprintf("✓ Puntos reducidos: %d → %d (reducción: %.1f%%)\n",
              n_points_orig, n_points_simplified,
              100 * (1 - n_points_simplified / n_points_orig)))

  # Calcular nueva área
  area_simplified <- as.numeric(st_area(polygon_simplified)) / 1e6
  cat(sprintf("✓ Área conservada: %.2f km² (%.1f%% del original)\n\n",
              area_simplified, 100 * area_simplified / area_orig))

  # Extraer vértices únicos
  vertices <- unique(coords_simplified[, c("X", "Y")])
  colnames(vertices) <- c("lon", "lat")

  # Obtener bounding box del polígono simplificado
  bbox_simplified <- st_bbox(polygon_simplified)

  cat("📊 RESULTADOS:\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat(sprintf("• Vértices únicos extraídos: %d\n", nrow(vertices)))
  cat(sprintf("• Rango longitudinal: %.4f° a %.4f° (amplitud: %.4f°)\n",
              min(vertices[, "lon"]), max(vertices[, "lon"]),
              diff(range(vertices[, "lon"]))))
  cat(sprintf("• Rango latitudinal: %.4f° a %.4f° (amplitud: %.4f°)\n",
              min(vertices[, "lat"]), max(vertices[, "lat"]),
              diff(range(vertices[, "lat"]))))

  return(list(
    vertices = vertices,
    polygon_simplified = polygon_simplified,
    bbox_original = bbox_orig,
    bbox_simplified = bbox_simplified,
    coordinates_original = coords_orig,
    coordinates_simplified = coords_simplified,
    stats = list(
      n_features_original = n_features,
      n_points_original = n_points_orig,
      n_points_simplified = n_points_simplified,
      area_original_km2 = area_orig,
      area_simplified_km2 = area_simplified,
      reduction_percentage = 100 * (1 - n_points_simplified / n_points_orig),
      area_conservation_percentage = 100 * area_simplified / area_orig
    )
  ))
}
