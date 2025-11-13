#' @title Registrar mensaje en log con timestamp
#' @description Función para registrar mensajes tanto en consola como en archivo de log
#'
#' @param message Mensaje a registrar
#' @param log_file Ruta del archivo de log (opcional)
#' @param level Nivel del mensaje: "INFO", "WARNING", "ERROR"
#' @export
log_message <- function(message, log_file = NULL, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- paste0("[", timestamp, "] [", level, "] ", message)

  # Imprimir en consola
  cat(log_entry, "\n")

  # Escribir en archivo si se especifica
  if (!is.null(log_file)) {
    tryCatch({
      cat(log_entry, "\n", file = log_file, append = TRUE)
    }, error = function(e) {
      warning(paste("No se pudo escribir en el archivo de log:", e$message))
    })
  }

  invisible(log_entry)
}
