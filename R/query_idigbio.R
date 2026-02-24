#' @title Consultar iDigBio (Integrated Digitized Biocollections)
#'
#' @param bbox Bounding box como string "min_lng,min_lat,max_lng,max_lat"
#' @param config Lista de configuración iDigBio con parámetros de consulta
#' @param box_id ID de la caja de búsqueda (para logging)
#' @param log_function Función para registrar mensajes (opcional)
#' @return Data frame con registros de iDigBio formateados
#' @export
query_idigbio <- function(grid_row, config, box_id, this_source) {

  results.file <- here::here(
    "data",
    "query_results",
    this_source,
    paste0(this_source, "_results_", box_id, ".csv")
  )

  print(box_id)
  if(file.exists(results.file)==FALSE) {

print(paste0("Extracting ",this_source, " ", box_id))
  tryCatch({

    # Obtener formato espacial optimizado para iDigBio
    coords <- sf::st_coordinates(grid_row) %>%
      as.numeric()

    spatial_format <- list(
      geopoint=list(
        type = "geo_bounding_box",
        top_left = list( lat = coords[8], lon = coords[1]),
        bottom_right = list(lat = coords[6], lon = coords[2])
      ))

     # Extraer parámetros de iDigBio de la configuración
    idigbio_params_config <- config$databases$idigbio$params

    # Construcción del query para iDigBio con formato correcto
    # iDigBio usa 'geopoint' (minúscula) y formato específico
    idigbio_result <- ridigbio::idig_search_records(
      rq = spatial_format,
      limit = min(idigbio_params_config$records_per_box, 100000)  # iDigBio permite hasta 100k
    )


    # Procesar datos
    result_df <- idigbio_result %>%
      dplyr::filter(!is.na(scientificname)) %>%
      dplyr::mutate(source=this_source)

    readr::write_csv(result_df, results.file, append = FALSE)

    Sys.sleep(3)


  })


  result <- "done"

  return(result)
  }
}
