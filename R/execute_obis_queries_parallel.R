#' @title Ejecutar consultas en paralelo para OBIS
#' @description Versión optimizada que usa paralelización para acelerar las consultas cuando hay múltiples celdas de grid
#'
#' @param grid Data frame con columnas: box_id, bbox, wkt
#' @param config Lista de configuración con parámetros de consulta
#' @param log_function Función para logging (opcional)
#' @param n_cores Número de núcleos a usar (por defecto: detecta automáticamente)
#'
#' @return Data frame consolidado con todos los resultados
#' @export
execute_obis_queries_parallel <- function(grid,
                                          config,
                                          log_function,
                                          log_file,
                                          n_cores = NULL) {
  # Detectar número de núcleos disponibles
  if (is.null(n_cores)) {
    n_cores <- max(1, parallel::detectCores() - 1)  # Dejar 1 núcleo libre
  }

  log_function(paste0("🚀 Configuración de paralelización:"),
               log_file,
               level = "INFO")
  log_function(paste0("   • Núcleos disponibles: ", parallel::detectCores()),
               log_file,
               level = "INFO")
  log_function(paste0("   • Núcleos a usar: ", n_cores), log_file, level =
                 "INFO")
  log_function(paste0("   • Celdas del grid: ", nrow(grid)), log_file, level =
                 "INFO")
  log_function(paste0(
    "   • Estimación: ~",
    round(nrow(grid) / n_cores, 1),
    " celdas por núcleo\n"
  ),
  log_file,
  level = "INFO")


  all_results <- data.frame()

  # ============================================================================
  # OBIS - Paralelizado
  # ============================================================================

  if(dir.exists(here::here("data","query_results","obis_raw"))==FALSE) {

    dir.create(here::here("data","query_results","obis_raw"))

  }

  if(dir.exists(here::here("data","query_results","obis"))==FALSE) {

    dir.create(here::here("data","query_results","obis"))

  }

  if (config$databases$gbif$enabled) {
    log_function("Iniciando consultas paralelas a OBIS...", log_file, level =
                   "INFO")
    start_time <- Sys.time()

    no_boxes <- 1:length(sf::st_geometry(grid))

    # Initiate cluster
    cl <- parallel::makeCluster(n_cores, outfile = "cluster_log.txt")
    doSNOW::registerDoSNOW(cl)

    parallel::clusterExport(cl, c("query_obis", "get_spatial_format_for_api"))
    parallel::clusterEvalQ(cl, library(dplyr))

    # Run this for loop for one call of model from each cluster, assuming cluster is already initiated.
    atlantis.scenarios <- foreach::foreach(i = no_boxes, grid, config, .verbose = TRUE) %dopar% {

      grid_row <- grid[i]
      box_id <- i

      results.file <- here::here("data","query_results","obis", paste0("obis_results_", box_id,".csv"))

      if(file.exists(results.file)==FALSE){

        result <- query_obis(grid_row, config, box_id, this_source= "obis")

      } else {

        result <- "done"
      }

    }

  }

  stopCluster(cl)
}
