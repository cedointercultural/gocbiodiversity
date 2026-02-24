#' @title Consultar eBird
#'
#' @param grid_point Bounding box como string "min_lng,min_lat,max_lng,max_lat"
#' @param config Lista de configuración eBird con parámetros de consulta
#' @param box_id ID de la caja de búsqueda (para logging)
#' @param this_source ID de la caja de búsqueda (para logging)
#' @return Data frame con registros de eBird formateados
#' @export
#' @note eBird requiere API key configurada en .Renviron
#'
query_ebird <- function(grid_point, config, box_id, this_source) {


  #check if file already exists

  results.file <- here::here("data","query_results",this_source, paste0(this_source,"_results_", box_id,".csv"))

  if(file.exists(results.file)==FALSE){
    tryCatch({

      if(box_id==1){

        # Extraer parámetros de OBIS de la configuración
        params_config <- config$databases$ebird$params

        api_key <- paste0("EBIRD_KEY = ",params_config$api_key)

        renviron_path = "~/.Renviron"

        writeLines(api_key, renviron_path)
      }

      # Ejecutar consulta a ebird por puntos y 50 km de radio

      grid_coords <- sf::st_coordinates(grid_point) %>% as.character()

      hist_occ_results <- rebird::ebirdhistorical(loc='MX-BCN',date='2000-01-01', key = params_config$api_key)


       rebird::ebirdgeo(species=NULL, lat=as.numeric(grid_coords[2]), lng = as.numeric(grid_coords[1]), dist = 50, key = params_config$api_key)

      # Agregar filtro de años si está configurado
      if (!is.null(obis_params_config$year_start) && !is.null(obis_params_config$year_end)) {
        obis_params$startdate <- paste0(obis_params_config$year_start, "-01-01")
        obis_params$enddate <- paste0(obis_params_config$year_end, "-12-31")
      }

      obis_result <- do.call(robis::occurrence, obis_params)

      if(("species" %in% names(obis_result))==TRUE) {

        formatted_result_df <-  obis_result %>%
          dplyr::filter(!is.na(species)) %>%
          dplyr::mutate(source=this_source)

        readr::write_csv(formatted_result_df,results.file, append = FALSE)


      }


    })
  }

  result <- "done"
  return(result)
}
