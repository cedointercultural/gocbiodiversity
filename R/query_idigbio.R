#' @title Consultar iDigBio (Integrated Digitized Biocollections)
#'
#' @param bbox Bounding box como string "min_lng,min_lat,max_lng,max_lat"
#' @param config Lista de configuración iDigBio con parámetros de consulta
#' @param box_id ID de la caja de búsqueda (para logging)
#' @param log_function Función para registrar mensajes (opcional)
#' @return Data frame con registros de iDigBio formateados
#' @export
query_idigbio <- function(grid_row, config, box_id = 1, log_function) {

  tryCatch({
    log_function(paste("Consultando iDigBio - Box", box_id, "..."))

    # Obtener formato espacial optimizado para iDigBio
    spatial_format <- get_spatial_format_for_api("idigbio", grid_row)

    if (is.null(spatial_format$value)) {
      log_function(paste("✗ Error iDigBio - Box", box_id, ": parámetros espaciales vacíos"), log_file, level="INFO")
      return(data.frame())
    }

    # Extraer parámetros de iDigBio de la configuración
    idigbio_params_config <- config$databases$idigbio$params

    # Construcción del query para iDigBio con formato correcto
    # iDigBio usa 'geopoint' (minúscula) y formato específico
    idigbio_result <- ridigbio::idig_search_records(
      rq = list(
        geopoint = spatial_format$value
      ),
      limit = min(idigbio_params_config$records_per_box, 100000)  # iDigBio permite hasta 100k
    )

    if (is.null(idigbio_result) ||
        !is.data.frame(idigbio_result) ||
        nrow(idigbio_result) == 0) {
      log_function(paste("⚠ iDigBio - Box", box_id, ": sin resultados"), log_file, level="INFO")
      return(data.frame())
    }

    # Procesar datos
    data <- idigbio_result

    result_df <- data.frame(
      species = sapply(data$scientificname, function(x)
        if (!is.na(x)) clean_species_name(x) else NA_character_),
      lon = as.numeric(data$geopoint_lon),
      lat = as.numeric(data$geopoint_lat),
      year = if("yearcollected" %in% names(data)) as.numeric(data$yearcollected) else NA_real_,
      month = NA_real_,
      day = NA_real_,
      date_recorded = if("datecollected" %in% names(data)) as.character(data$datecollected) else NA_character_,
      taxonRank = NA_character_,
      stringsAsFactors = FALSE
    )

    log_function(paste("✓ iDigBio - Box", box_id, ":", nrow(result_df), "registros"), log_file, level="INFO")

    return(format_biodiversity_data(result_df, "iDigBio"))

  }, error = function(e) {
    log_function(paste("✗ Error iDigBio - Box", box_id, ":", e$message), log_file, level="INFO")
    return(data.frame())
  })
}
