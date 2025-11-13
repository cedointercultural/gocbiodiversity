#' @title Generar metadatos de la consulta
#'
#' @param config Lista de configuración
#' @param results Data frame con resultados
#' @param grid Data frame con grid usado
#' @param execution_time Tiempo de ejecución (opcional)
#' @return Lista con metadatos
#' @export
generate_metadata <- function(config, results, grid, execution_time = NULL) {
  metadata <- list(
    execution_date = as.character(Sys.time()),
    execution_time_seconds = execution_time,
    total_records = nrow(results),
    unique_species = length(unique(results$species[!is.na(results$species)])),
    databases_queried = names(config$databases)[
      sapply(config$databases, function(x) x$enabled)
    ],
    grid_cells_used = nrow(grid),
    records_by_source = as.list(table(results$source)),
    year_range = if(any(!is.na(results$year)))
      range(results$year, na.rm = TRUE) else c(NA, NA),
    spatial_extent = list(
      lon_range = range(results$lon, na.rm = TRUE),
      lat_range = range(results$lat, na.rm = TRUE)
    ),
    config_summary = list(
      grid_enabled = config$spatial$grid_enabled,
      grid_size = config$spatial$grid_size_degrees,
      max_boxes = config$spatial$max_boxes
    )
  )

  return(metadata)
}
