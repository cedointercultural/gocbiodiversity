#' @title Validar coordenadas geográficas
#' @description Verifica que las coordenadas estén dentro de rangos válidos
#'
#' @param lon Vector de longitudes
#' @param lat Vector de latitudes
#' @return Vector lógico indicando coordenadas válidas
#' @export
validate_coordinates <- function(lon, lat) {
  valid <- !is.na(lon) & !is.na(lat) &
    lon >= -180 & lon <= 180 &
    lat >= -90 & lat <= 90

  n_invalid <- sum(!valid)
  if (n_invalid > 0) {
    cat("⚠ Encontradas", n_invalid, "coordenadas inválidas\n")
  }

  return(valid)
}
