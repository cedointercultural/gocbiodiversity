#' @title Generar bounding box simple de polígono
#' @description
#' Función auxiliar para generar un único bounding box cuando no se usa grid
#'
#' @param polygon Objeto sf con geometría
#' @return Data frame con una fila: box_id, bbox, wkt
#' @export
generate_simple_bbox <- function(polygon) {
  bbox <- sf::st_bbox(polygon)

  grid_df <- data.frame(
    box_id = 1,
    bbox = paste(bbox[["xmin"]], bbox[["ymin"]],
                 bbox[["xmax"]], bbox[["ymax"]], sep = ","),
    wkt = sf::st_as_text(sf::st_as_sfc(bbox), digits = 8),
    stringsAsFactors = FALSE
  )

  cat("✓ Usando bounding box completo (1 caja)\n")
  cat("  - Extensión:", grid_df$bbox, "\n")

  return(grid_df)
}
