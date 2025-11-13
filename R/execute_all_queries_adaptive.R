#' @title Ejecutar consultas con estrategia adaptativa (automática o paralela)
#' @description Decide automáticamente si usar paralelización basándose en el número de celdas
#'

#' @param grid Data frame con columnas: box_id, bbox, wkt
#' @param config Lista de configuración con parámetros de consulta
#' @param log_function Función para logging (opcional)
#' @param parallel_threshold Número mínimo de celdas para usar paralelización (default: 5)
#' @param n_cores Número de núcleos a usar (por defecto: detecta automáticamente)
#'
#' @return Data frame consolidado con todos los resultados
#' @export
execute_all_queries_adaptive <- function(grid, config, log_function,
                                         parallel_threshold = 5, n_cores = NULL) {

  n_boxes <- nrow(grid)

  # Decidir estrategia
  if (n_boxes >= parallel_threshold) {
    log_function(paste0("📊 Estrategia: PARALELA (", n_boxes, " celdas)\n"), log_file, level="INFO")
    return(execute_all_queries_parallel(grid, config, log_function, n_cores))
  } else {
    log_function(paste0("📊 Estrategia: SECUENCIAL (", n_boxes, " celdas)\n"), log_file, level="INFO")
    # Usar la función original del archivo query_functions.R
    return(execute_all_queries(grid, config, log_function))
  }
}
