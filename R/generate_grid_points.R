#' @title Generar grid de bounding boxes desde polígono
#'
#' @description Divide un polígono en un grid regular de celdas y genera sus bounding boxes y representaciones WKT para consultas espaciales.
#' Optimizado para cobertura completa del área de estudio.
#'
#' @param polygon Objeto sf con geometría
#' @param grid_size Tamaño de celda en grados (por defecto 2.0)
#' @param buffer_percent Porcentaje de buffer para extender grid (por defecto 2%)
#' @return Data frame con columnas: box_id, bbox, wkt, bbox_array, geometry
#' @export
generate_grid_points <- function(polygon, grid_size = 2.0, buffer_percent = 2) {

    # Obtener bounding box del polígono

    bbox <- min_bounding_polygon(polygon)

    # Aplicar buffer para asegurar cobertura completa

    buffer_factor <- buffer_percent / 100

    bbox.buffer <- sf::st_buffer(bbox, 1)

    grid_square <- sf::st_make_grid(bbox, cellsize = c(grid_size, grid_size), what = "centers")


    grid_intersects <- grid_square[polygon,]

    return(grid_intersects)
}
