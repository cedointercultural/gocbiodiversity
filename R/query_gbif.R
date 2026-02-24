#' @title Consultar GBIF (Global Biodiversity Information Facility)
#'
#' @param grid_row WKT (Well-Known Text) string para la geometría de búsqueda
#' @param config Lista de configuración GBIF con parámetros de consulta
#' @param box_id ID de la caja de búsqueda (para logging)
#' @param this_source database name
#' @return Saves as cvs records found
#' @export
query_gbif <- function(grid_row, config, box_id, this_source) {


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
    # Obtener formato espacial optimizado para GBIF
    spatial_format <- get_spatial_format_for_api("gbif", grid_row)

  #  print(spatial_format)

     # Extraer parámetros de GBIF de la configuración
    gbif_params <- config$databases$gbif$params

    # Construir parámetros de consulta
    params <- list(
      geometry = spatial_format$value,
      limit = gbif_params$records_per_box,
      hasCoordinate = gbif_params$has_coordinate
    )

    # Agregar filtro de año si está configurado
    if (!is.null(gbif_params$year_start) && !is.null(gbif_params$year_end)) {
      params$year <- paste0(gbif_params$year_start, ",", gbif_params$year_end)
    }

    # Agregar filtros opcionales
    if (!is.null(gbif_params$rank) && gbif_params$rank != "") {
      params$rank <- gbif_params$rank
    }

    # Ejecutar consulta con timeout implícito
   # gbif_result <- do.call(rgbif::occ_download, params)


    request_info <- rgbif::occ_download(rgbif::pred_within(spatial_format$value),
                                        format = "SIMPLE_CSV")

    rgbif::occ_download_wait(request_info)

    request_result <- rgbif::occ_download_get(request_info)

    rgbif_download<- rgbif::occ_download_import(request_result)

    zip.file <- list.files(path=here::here(), pattern="*.zip", full.names = FALSE)
    #print(zip.file)

    save.path <- here::here("data","query_results","gbif_raw")
    system(paste0("mv ", zip.file," ", save.path))

    # Procesar datos

    formatted_result_df <-  rgbif_download %>%
      dplyr::filter(taxonRank=="SPECIES") %>%
      dplyr::filter(!is.na(species)) %>%
      dplyr::mutate(source=this_source)

    readr::write_csv(formatted_result_df,results.file, append = FALSE)


    })
    }

result <- "done"

return(result)

}
