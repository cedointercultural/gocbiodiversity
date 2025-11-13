#' @title Función de logging que escribe en archivo y consola
#'
#' @param msg message
#' @param level information
log_fn <- function(msg, log_file, level = "INFO") {
  log_message(msg, log_file, level)
}
