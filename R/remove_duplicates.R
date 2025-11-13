#' @title   Eliminar registros duplicados de biodiversidad
#' @description Elimina duplicados basándose en combinación de especies, coordenadas, año y fuente
#'
#' @param data Data frame con registros de biodiversidad
#' @param keep_first Lógico, mantener primera ocurrencia (TRUE) o última (FALSE)
#' @return Data frame sin duplicados
#' @export
remove_duplicates <- function(data, keep_first = TRUE) {
  if (is.null(data) || nrow(data) == 0) {
    return(data)
  }

  # Usar dplyr para eliminación eficiente de duplicados
  data_unique <- data %>%
    dplyr::distinct(species, lon, lat, year, source, .keep_all = TRUE)

  n_removed <- nrow(data) - nrow(data_unique)

  if (n_removed > 0) {
    cat("✓ Eliminados", n_removed, "registros duplicados\n")
  }

  return(data_unique)
}
