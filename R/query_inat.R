#' @title Consultar iNaturalist
#'
#' @param grid_row Fila del grid con información espacial completa
#' @param config Lista de configuración iNaturalist con parámetros de consulta
#' @param box_id ID de la caja de búsqueda (para logging)
#' @param this_source Nombre de la fuente
#' @return Data frame con registros de iNaturalist formateados
#' @export
query_inat <- function(grid_row, config, box_id, this_source) {

  results.file <- here::here(
    "data",
    "query_results",
    this_source,
    paste0(this_source, "_results_", box_id, ".csv")
  )

  if (file.exists(results.file) == FALSE) {
    print(paste0("Extracting ", this_source, " ", box_id))

       # Obtener formato espacial optimizado para iNaturalist
      spatial_format <- get_spatial_format_for_api("inat", grid_row)

      # Extraer parámetros de iNaturalist de la configuración
      inat_params_config <- config$databases$inat$params

      # value = c(sf::st_bbox(grid_row)$ymin, sf::st_bbox(grid_row)$xmin, sf::st_bbox(grid_row)$ymax, sf::st_bbox(grid_row)$xmax) %>% as.numeric
      #
      # bounds <- c(38.44047, -125, 40.86652, -121.837)
      # deer <- rinat::get_inat_obs(bounds = value, quality = "research",
      #                      maxresults = 1000)


      # Construir URL de iNaturalist API
      base_url <- "https://api.inaturalist.org/v1/observations"
      params <- spatial_format$value  # Ya contiene swlat, swlng, nelat, nelng
      params$per_page <- min(inat_params_config$records_per_box, 200)  # Límite de iNat
      params$quality_grade <- "research"

      # Agregar filtro de años si está configurado
      if (!is.null(inat_params_config$year_start) &&
          !is.null(inat_params_config$year_end)) {
        params$d1 <- paste0(inat_params_config$year_start, "-01-01")
        params$d2 <- paste0(inat_params_config$year_end, "-12-31")
      }

      # Construir query string
      query_string <- paste(names(params),
                            params,
                            sep = "=",
                            collapse = "&")
      url <- paste(base_url, query_string, sep = "?")

      # Hacer petición HTTP

        response <- httr::GET(url)

      json_data <- httr::content(response, "text", encoding = "UTF-8")


      if (grepl("total_results\":0", json_data) == FALSE) {

        tryCatch({ data_list <- jsonlite::fromJSON(json_data)}, error = function(e) {
          warning("Lexical error: ", e$message,
                  ". No results.")
          return("skip")
        }
        )

        if(exists("data_list")==TRUE) {

          json_frame <- as.data.frame(data_list)

          results.taxon <- json_frame$results.taxon %>%
            dplyr::rename(record_id = id) %>%
            dplyr::select(-default_photo, -flag_counts, -ancestor_ids)

          col.data <- as.data.frame(json_frame$results.observed_on_details)
          taxa.frame <- list()

          for (eachtaxa in 1:length(json_frame$results.non_owner_ids)) {
            taxa.data <- as.data.frame(json_frame$results.non_owner_ids[[eachtaxa]]$taxon$ancestors[[1]])

            if ("rank" %in% names(taxa.data)) {
              print(eachtaxa)
              taxa.data.rev <- taxa.data %>%
                dplyr::select(rank, name) %>%
                tidyr::pivot_wider(names_from = rank, values_from = name)

            } else {
              taxa.data.rev <- tidyr::tibble(
                kingdom = "NA",
                phylum = "NA",
                subphylum = "NA",
                class = "NA",
                order = "NA",
                family = "NA",
                genus = "NA"
              )
            }


            taxa.frame[[eachtaxa]] <- taxa.data.rev

          }

          taxa.frame <- dplyr::bind_rows(taxa.frame)

          if (!"species" %in% names(taxa.frame)) {
            results.taxon <- results.taxon %>%
              dplyr::rename(species = name)
          }

          if ("conservation_status" %in% names(results.taxon)) {
            cons.status <- results.taxon$conservation_status %>% tidyr::as_tibble()

            json_tibble <- results.taxon %>%
              dplyr::select(-conservation_status) %>%
              dplyr::bind_cols(cons.status) %>%
              dplyr::bind_cols(col.data) %>%
              dplyr::bind_cols(taxa.frame) %>%
              dplyr::mutate(source = this_source,
                            location = json_frame$results.location) %>%
              tidyr::separate_wider_delim(location,
                                          delim = ",",
                                          names = c("lat", "lon"))

          } else {
            json_tibble <- results.taxon %>%
              dplyr::bind_cols(col.data) %>%
              dplyr::bind_cols(taxa.frame) %>%
              dplyr::mutate(source = this_source,
                            location = json_frame$results.location) %>%
              tidyr::separate_wider_delim(location,
                                          delim = ",",
                                          names = c("lat", "lon"))

          }



          readr::write_csv(json_tibble, results.file, append = FALSE)

          Sys.sleep(3)

        }

      }

  }
  result <- "done"

  return(result)
}
