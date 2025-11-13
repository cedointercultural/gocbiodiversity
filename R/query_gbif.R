#' @title Consultar GBIF (Global Biodiversity Information Facility)
#'
#' @param grid_row WKT (Well-Known Text) string para la geometría de búsqueda
#' @param config Lista de configuración GBIF con parámetros de consulta
#' @param box_id ID de la caja de búsqueda (para logging)
#' @param this_source database name
#' @return Saves as cvs records found
#' @export
query_gbif <- function(grid_row, config, box_id, this_source) {


    # Obtener formato espacial optimizado para GBIF
    spatial_format <- get_spatial_format_for_api("gbif", grid_row)

    print(spatial_format)

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


    request_info <- rgbif::occ_download(rgbif::pred_within(spatial_format$value),format = "SIMPLE_CSV")

    rgbif::occ_download_wait(request_info)

    request_result <- rgbif::occ_download_get(request_info) %>%
      rgbif::occ_download_import()

    zip.file <- list.files(pattern="*.zip", full.names = TRUE)

    unzip(zip.file, exdir = here::here("data","query_results","gbif_raw"))

    new.name <- gsub(".zip",".csv",zip.file) %>%
      gsub("./","", .)

    save.path <- here::here("data","query_results","gbif_raw")
    system(paste0("mv ", zip.file," ", save.path))

    rec.data <- readr::read_delim(file=here::here("data","query_results","gbif_raw",new.name), delim="\t", col_select = c("species", "decimalLatitude", "decimalLongitude", "year", "month", "day", "eventDate", "taxonRank"),
                                  col_types = readr::cols(
                                    species = readr::col_character(),
                                    decimalLatitude = readr::col_double(),
                                    decimalLongitude = readr::col_double(),
                                    year = readr::col_character(),
                                    month = readr::col_character(),
                                    date = readr::col_character(),
                                    eventDate = readr::col_date(format = "%Y-%m-%d"),
                                    taxonRank = readr::col_character())) %>%
      dplyr::rename(lat=decimalLatitude, lon = decimalLongitude, date_recorded = eventDate)
    # Procesar datos

    formatted_result_df <-  rec.data %>%
      dplyr::filter(taxonRank=="SPECIES") %>%
      dplyr::filter(!is.na(species)) %>%
      dplyr::mutate(source=this_source)

    readr::write_csv(formatted_result_df,here::here("data","query_results","gbif", paste0("gbif_results_", box_id,".csv")), append = FALSE)

    result <- "done"

return(result)

}
