#' @title Consultar iNaturalist
#'
#' @param grid_row Fila del grid con información espacial completa
#' @param config Lista de configuración iNaturalist con parámetros de consulta
#' @param box_id ID de la caja de búsqueda (para logging)
#' @param log_function Función para registrar mensajes (opcional)
#' @return Data frame con registros de iNaturalist formateados
#' @export
query_inat <- function(grid_row, config, box_id = 1, log_function) {


  tryCatch({
    log_function(paste("Consultando iNaturalist - Box", box_id, "..."), log_file, level="INFO")

    # Obtener formato espacial optimizado para iNaturalist
    spatial_format <- get_spatial_format_for_api("inat", grid_row)

    if (is.null(spatial_format$value)) {
      log_function(paste("✗ Error iNaturalist - Box", box_id, ": parámetros espaciales vacíos"), log_file, level="INFO")
      return(data.frame())
    }

    # Extraer parámetros de iNaturalist de la configuración
    inat_params_config <- config$databases$inat$params

    # Construir URL de iNaturalist API
    base_url <- "https://api.inaturalist.org/v1/observations"
    params <- spatial_format$value  # Ya contiene swlat, swlng, nelat, nelng
    params$per_page <- min(inat_params_config$records_per_box, 200)  # Límite de iNat
    params$quality_grade <- "research"

    # Agregar filtro de años si está configurado
    if (!is.null(inat_params_config$year_start) && !is.null(inat_params_config$year_end)) {
      params$d1 <- paste0(inat_params_config$year_start, "-01-01")
      params$d2 <- paste0(inat_params_config$year_end, "-12-31")
    }

    # Construir query string
    query_string <- paste(names(params), params, sep = "=", collapse = "&")
    url <- paste(base_url, query_string, sep = "?")

    # Hacer petición HTTP
    response <- httr::GET(url)

    if (httr::status_code(response) != 200) {
      log_function(paste("⚠ iNaturalist - Box", box_id, ": error de API"), log_file, level="INFO")
      return(data.frame())
    }

    json_data <- httr::content(response, "text", encoding = "UTF-8")
    data_list <- jsonlite::fromJSON(json_data)

    if (is.null(data_list$results) || length(data_list$results) == 0) {
      log_function(paste("⚠ iNaturalist - Box", box_id, ": sin resultados"), log_file, level="INFO")
      return(data.frame())
    }

    # Procesar datos
    data <- data_list$results

    # Extraer especies de la estructura JSON de iNaturalist
    species_names <- sapply(1:nrow(data), function(i) {
      if (!is.null(data$taxon[[i]]) && !is.null(data$taxon[[i]]$name)) {
        return(clean_species_name(data$taxon[[i]]$name))
      } else {
        return(NA_character_)
      }
    })

    result_df <- data.frame(
      species = species_names,
      lon = as.numeric(data$location),  # iNat devuelve "lat,lng" en location
      lat = as.numeric(data$location),  # Necesita parsing especial
      year = as.numeric(substr(data$observed_on, 1, 4)),
      month = as.numeric(substr(data$observed_on, 6, 7)),
      day = as.numeric(substr(data$observed_on, 9, 10)),
      date_recorded = as.character(data$observed_on),
      taxonRank = sapply(1:nrow(data), function(i) {
        if (!is.null(data$taxon[[i]]) && !is.null(data$taxon[[i]]$rank)) {
          return(toupper(data$taxon[[i]]$rank))
        } else {
          return(NA_character_)
        }
      }),
      stringsAsFactors = FALSE
    )

    # Parsear coordenadas de location string "lat,lng"
    if ("location" %in% names(data) && any(!is.na(data$location))) {
      coords <- strsplit(as.character(data$location), ",")
      result_df$lat <- as.numeric(sapply(coords, function(x) if(length(x) >= 2) x[1] else NA))
      result_df$lon <- as.numeric(sapply(coords, function(x) if(length(x) >= 2) x[2] else NA))
    }

    log_function(paste("✓ iNaturalist - Box", box_id, ":", nrow(result_df), "registros"), log_file, level="INFO")

    return(format_biodiversity_data(result_df, "iNaturalist"))

  }, error = function(e) {
    log_function(paste("✗ Error iNaturalist - Box", box_id, ":", e$message), log_file, level="INFO")
    return(data.frame())
  })
}
