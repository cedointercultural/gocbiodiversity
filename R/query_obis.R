#' @title Consultar OBIS (Ocean Biodiversity Information System)
#'
#' @param grid_row Fila del grid con información espacial completa
#' @param config Lista de configuración OBIS con parámetros de consulta
#' @param box_id ID de la caja de búsqueda (para logging)
#' @param this_source Nombre de la fuente
#' @return Guarda los registros y reporta terminado
#' @export
query_obis <- function(grid_row, config, box_id, this_source) {
  #check if file already exists

  results.file <- here::here(
    "data",
    "query_results",
    this_source,
    paste0(this_source, "_results_", box_id, ".csv")
  )

  if (file.exists(results.file) == FALSE) {
    tryCatch({
      print(paste0("Extracting ", this_source, " ", box_id))

      # Obtener formato espacial optimizado para OBIS
      spatial_format <- get_spatial_format_for_api(api_type = this_source, grid_row)

      if (is.null(spatial_format$value) ||
          nchar(trimws(spatial_format$value)) == 0) {
        log_function(paste("✗ Error OBIS - Box", box_id, ": geometría vacía"),
                     log_file,
                     level = "INFO")
        return(data.frame())
      }

      # Extraer parámetros de OBIS de la configuración
      obis_params_config <- config$databases$obis$params

      # Ejecutar consulta a OBIS con parámetros correctos
      obis_params <- list(
        geometry = spatial_format$value
        # NOTA: OBIS no acepta parámetro 'size', usa paginación automática
      )

      # Agregar filtro de años si está configurado
      if (!is.null(obis_params_config$year_start) &&
          !is.null(obis_params_config$year_end)) {
        obis_params$startdate <- paste0(obis_params_config$year_start, "-01-01")
        obis_params$enddate <- paste0(obis_params_config$year_end, "-12-31")
      }

      obis_result <- do.call(robis::occurrence, obis_params)

      if (("species" %in% names(obis_result)) == TRUE) {
        formatted_result_df <-  obis_result %>%
          dplyr::filter(!is.na(species)) %>%
          dplyr::mutate(source = this_source)

        readr::write_csv(formatted_result_df, results.file, append = FALSE)


      }


    })
  }

  result <- "done"
  return(result)
}
