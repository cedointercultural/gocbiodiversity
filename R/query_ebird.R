#' @title Consultar eBird
#'
#' @param bbox Bounding box como string "min_lng,min_lat,max_lng,max_lat"
#' @param config Lista de configuración eBird con parámetros de consulta
#' @param box_id ID de la caja de búsqueda (para logging)
#' @param log_function Función para registrar mensajes (opcional)
#' @return Data frame con registros de eBird formateados
#' @export
#' @note eBird requiere API key configurada en config$params$api_key
query_ebird <- function(bbox, config, box_id = 1, log_function) {

  tryCatch({
    log_function(paste("Consultando eBird - Box", box_id, "..."))

    # Verificar API key
    if (is.null(config$params$api_key) || config$params$api_key == "") {
      log_function("⚠ eBird: API key no configurada. Configure una API key en el archivo de configuración.", log_file, level="INFO")
      return(data.frame())
    }

    # Validar bbox
    if (is.null(bbox) || nchar(trimws(bbox)) == 0) {
      log_function(paste("✗ Error eBird - Box", box_id, ": bbox vacío"), log_file, level="INFO")
      return(data.frame())
    }

    bbox_parts <- as.numeric(strsplit(bbox, ",")[[1]])

    if (length(bbox_parts) != 4 || any(is.na(bbox_parts))) {
      log_function(paste("✗ Error eBird - Box", box_id, ": formato de bbox inválido"), log_file, level="INFO")
      return(data.frame())
    }

    # Nota: eBird tiene una API compleja que requiere configuración específica
    # Esta es una implementación básica que puede necesitar ajustes
    # Para consultas por región, usar ebirdregioncheck
    # Para consultas por coordenadas, usar ebirdgeo

    if (!is.null(config$params$region_code) && config$params$region_code != "") {
      ebird_result <- rebird::ebirdregioncheck(
        region = config$params$region_code,
        species = NULL
      )
    } else {
      # Usar punto central del bbox
      center_lon <- mean(c(bbox_parts[1], bbox_parts[3]))
      center_lat <- mean(c(bbox_parts[2], bbox_parts[4]))

      ebird_result <- rebird::ebirdgeo(
        lat = center_lat,
        lng = center_lon,
        dist = 50  # Radio en km
      )
    }

    if (is.null(ebird_result) ||
        !is.data.frame(ebird_result) ||
        nrow(ebird_result) == 0) {
      log_function(paste("⚠ eBird - Box", box_id, ": sin resultados"), log_file, level="INFO")
      return(data.frame())
    }

    log_function(paste("✓ eBird - Box", box_id, ":", nrow(ebird_result), "registros"), log_file, level="INFO")

    return(format_biodiversity_data(ebird_result, "eBird"))

  }, error = function(e) {
    log_function(paste("✗ Error eBird - Box", box_id, ":", e$message), log_file, level="INFO")
    return(data.frame())
  })
}
