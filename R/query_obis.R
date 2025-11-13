#' @title Consultar OBIS (Ocean Biodiversity Information System)
#'
#' @param grid_row Fila del grid con información espacial completa
#' @param config Lista de configuración OBIS con parámetros de consulta
#' @param box_id ID de la caja de búsqueda (para logging)
#' @param log_function Función para registrar mensajes (opcional)
#' @return Data frame con registros de OBIS formateados
#' @export
query_obis <- function(grid_row, config, box_id = 1, log_function) {

  tryCatch({
    log_function(paste("Consultando OBIS - Box", box_id, "..."))

    # Obtener formato espacial optimizado para OBIS
    spatial_format <- get_spatial_format_for_api("obis", grid_row)

    if (is.null(spatial_format$value) || nchar(trimws(spatial_format$value)) == 0) {
      log_function(paste("✗ Error OBIS - Box", box_id, ": geometría vacía"), log_file, level="INFO")
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
    if (!is.null(obis_params_config$year_start) && !is.null(obis_params_config$year_end)) {
      obis_params$startdate <- paste0(obis_params_config$year_start, "-01-01")
      obis_params$enddate <- paste0(obis_params_config$year_end, "-12-31")
    }

    obis_result <- do.call(robis::occurrence, obis_params)

    # Verificar resultados
    if (is.null(obis_result) ||
        !is.data.frame(obis_result) ||
        nrow(obis_result) == 0) {
      log_function(paste("⚠ OBIS - Box", box_id, ": sin resultados"), log_file, level="INFO")
      return(data.frame())
    }

    # Procesar datos
    data <- obis_result

    result_df <- data.frame(
      species = sapply(data$scientificName, function(x)
        if (!is.na(x)) clean_species_name(x) else NA_character_),
      lon = as.numeric(data$decimalLongitude),
      lat = as.numeric(data$decimalLatitude),
      year = if("year" %in% names(data)) as.numeric(data$year) else NA_real_,
      month = if("month" %in% names(data)) as.numeric(data$month) else NA_real_,
      day = if("day" %in% names(data)) as.numeric(data$day) else NA_real_,
      date_recorded = if("eventDate" %in% names(data)) as.character(data$eventDate) else NA_character_,
      taxonRank = if("taxonRank" %in% names(data)) as.character(data$taxonRank) else NA_character_,
      stringsAsFactors = FALSE
    )

    log_function(paste("✓ OBIS - Box", box_id, ":", nrow(result_df), "registros"), log_file, level="INFO")

    return(format_biodiversity_data(result_df, "OBIS"))

  }, error = function(e) {
    log_function(paste("✗ Error OBIS - Box", box_id, ":", e$message), log_file, level="INFO")
    return(data.frame())
  })
}
