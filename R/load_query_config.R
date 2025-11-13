#' @title Cargar configuración desde archivo JSON
#'
#' @param config_file Ruta al archivo JSON de configuración
#' @return Lista con la configuración cargada
#' @export
load_query_config <- function(config_file) {
  if (!file.exists(config_file)) {
    stop("Archivo de configuración no encontrado: ", config_file)
  }

  tryCatch({
    config <- jsonlite::fromJSON(config_file)
    cat("✓ Configuración cargada desde:", config_file, "\n")
    return(config)
  }, error = function(e) {
    stop("Error al cargar configuración: ", e$message)
  })
}
