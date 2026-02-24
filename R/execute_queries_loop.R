#' @title Ejecutar consultas en for loop
#' @description Varios rechazan las consultas paralelas
#'
#' @param grid Data frame con columnas: box_id, bbox, wkt
#' @param config Lista de configuración con parámetros de consulta
#' @param log_function Función para logging (opcional)
#' @param log_file Nombre logging file
#' @param this_source Fuente de datos description
#'
#' @return Data frame consolidado con todos los resultados
#' @export
execute_queries_loop <- function(grid,
                                 config,
                                 log_function,
                                 log_file,
                                 this_source) {
  # Detectar número de núcleos disponibles
  log_function(paste0("   • Celdas del grid: ", nrow(grid)), log_file, level =
                 "INFO")

  all_results <- data.frame()

  # ============================================================================
  # databases - Loop
  # ============================================================================

  if (dir.exists(here::here("data", "query_results", paste0(this_source, "_raw"))) ==
      FALSE) {
    dir.create(here::here("data", "query_results", paste0(this_source, "_raw")))

  }

  if (dir.exists(here::here("data", "query_results", this_source)) == FALSE) {
    dir.create(here::here("data", "query_results", this_source))

  }

    log_function(paste0("Iniciando consultas paralelas a ", this_source),
                 log_file,
                 level =
                   "INFO")
    start_time <- Sys.time()


    if (this_source == "fishbase") {
      query_fishbase(grid, this_source)

    }

      no_boxes <- 1:length(sf::st_geometry(grid))

      for (i in no_boxes) {
        grid_row <- grid[i]
        box_id <- i

        log_fn(paste0("Buscando box ", box_id), log_file, level = "INFO")

        if (this_source == "obis") {
          query_obis(grid_row, config, box_id, this_source)

        }

        if (this_source == "gbif") {
          query_gbif(grid_row, config, box_id, this_source)

        }

        if (this_source == "inat") {
          query_inat(grid_row, config, box_id, this_source)

        }

        if (this_source == "idigbio") {
          query_idigbio(grid_row, config, box_id, this_source)

        }


        print(paste0("Box ", box_id, " done"))


    }


}
