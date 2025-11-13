#' @title Ejecutar consultas en for loop para GBIF, su servidor rechaza las consultas paralelas
#' @description Versión optimizada que usa paralelización para acelerar las consultas cuando hay múltiples celdas de grid
#'
#' @param grid Data frame con columnas: box_id, bbox, wkt
#' @param config Lista de configuración con parámetros de consulta
#' @param log_function Función para logging (opcional)
#'
#' @return Data frame consolidado con todos los resultados
#' @export
execute_gbif_queries_loop <- function(grid,
                                          config,
                                          log_function,
                                          log_file) {
  # Detectar número de núcleos disponibles
  log_function(paste0("   • Celdas del grid: ", nrow(grid)), log_file, level =
                 "INFO")

  all_results <- data.frame()

  # ============================================================================
  # GBIF - Loop
  # ============================================================================

  if(dir.exists(here::here("data","query_results","gbif_raw"))==FALSE) {

    dir.create(here::here("data","query_results","gbif_raw"))

  }

  if(dir.exists(here::here("data","query_results","gbif"))==FALSE) {

    dir.create(here::here("data","query_results","gbif"))

  }

  if (config$databases$gbif$enabled) {
    log_function("Iniciando consultas paralelas a GBIF...", log_file, level =
                   "INFO")
    start_time <- Sys.time()

    no_boxes <- 1:length(sf::st_geometry(grid))

  for(i in no_boxes){

  grid_row <- grid[i]
  box_id <- i

  log_fn(paste0("Buscando box ",box_id), log_file, level="INFO")

  results.file <- here::here("data","query_results","gbif", paste0("gbif_results_", box_id,".csv"))

  if(file.exists(results.file)==FALSE){

    result <- query_gbif(grid_row, config, box_id, this_source = "GBIF")

  } else {

    result <- "done"
  }

  print(paste0("Box ",box_id," ", result))
}


  }
}
