#' @title Cargar polígono desde shapefile o GeoPackage
#'
#' @description Lee un archivo espacial y asegura que esté en el sistema de coordenadas
#' correcto (WGS84, EPSG:4326)
#'
#' @param shp_path Ruta al archivo shapefile (.shp) o GeoPackage (.gpkg)
#' @param target_crs CRS objetivo (por defecto 4326 - WGS84)
#' @return Objeto sf con el polígono en WGS84
#' @export
load_polygon_from_shapefile <- function(shp_path, target_crs = 4326) {
  if (!file.exists(shp_path)) {
    stop("Archivo espacial no encontrado: ", shp_path)
  }

  tryCatch({
    # Leer archivo espacial
    polygon <- sf::st_read(shp_path, quiet = TRUE)

    cat("✓ Archivo espacial cargado:", shp_path, "\n")
    cat("  - Geometría:", as.character(sf::st_geometry_type(polygon)[1]), "\n")

    # Obtener información del CRS
    crs_info <- sf::st_crs(polygon)$input
    cat("  - CRS original:", if (!is.null(crs_info)) crs_info else "Sin especificar", "\n")

    # Asegurar que está en el CRS objetivo
    if (is.na(sf::st_crs(polygon))) {
      sf::st_crs(polygon) <- target_crs
      cat("  - CRS asignado a EPSG:", target_crs, "\n")
    } else if (sf::st_crs(polygon)$epsg != target_crs) {
      polygon <- sf::st_transform(polygon, target_crs)
      cat("  - CRS transformado a EPSG:", target_crs, "\n")
    }

    # Validar que es una geometría válida
    if (!all(sf::st_is_valid(polygon))) {
      cat("  - Corrigiendo geometrías inválidas...\n")
      polygon <- sf::st_make_valid(polygon)
    }

    # Mostrar información del bounding box
    bbox <- sf::st_bbox(polygon)
    cat("  - Bounding box:\n")
    cat("    Longitud:", round(bbox["xmin"], 4), "a", round(bbox["xmax"], 4), "\n")
    cat("    Latitud:", round(bbox["ymin"], 4), "a", round(bbox["ymax"], 4), "\n")

    # Calcular área aproximada
    area_km2 <- sum(as.numeric(sf::st_area(polygon))) / 1e6
    n_coords <- nrow(sf::st_coordinates(polygon))

    cat("  - Área aproximada:", round(area_km2, 2), "km²\n")
    cat("  - Número de coordenadas:", n_coords, "\n")

    # Simplificar automáticamente si el polígono es muy complejo
    if (n_coords > 50000) {
      cat("  - Polígono muy complejo, aplicando simplificación...\n")

      # Unir geometrías múltiples
      polygon_union <- sf::st_union(polygon)

      # Aplicar simplificación (tolerancia de 0.01 grados ≈ 1 km)
      polygon_simplified <- sf::st_simplify(polygon_union, dTolerance = 0.01)

      # Convertir de nuevo a sf dataframe
      polygon <- sf::st_sf(geometry = polygon_simplified)

      n_coords_new <- nrow(sf::st_coordinates(polygon))
      cat("    ✓ Coordenadas reducidas:", n_coords, "→", n_coords_new,
          sprintf("(%.1f%% reducción)\n", 100 * (1 - n_coords_new / n_coords)))
    }

    return(polygon)

  }, error = function(e) {
    stop("Error al cargar archivo espacial: ", e$message)
  })
}
