#' @title Wrapper para función de logging que escribe en archivo y consola
#'
#' @param msg message
#' @param level information
log_function <- function(msg, log_file, level = "INFO") {
  log_message(msg, log_file, level)
}
