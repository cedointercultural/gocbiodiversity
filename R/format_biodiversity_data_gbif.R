#' @title Formatear datos de biodiversidad a estructura estándar
#'
#' @description Convierte datos de diferentes fuentes a un formato unificado con
#' validación de tipos y eliminación de registros inválidos
#'
#' @param data Data frame con datos brutos de biodiversidad
#' @param source Nombre de la fuente de datos (ej: "GBIF", "OBIS")
#' @param config Nombre de la fuente de datos (ej: "GBIF", "OBIS")
#' @return Data frame formateado y validado
#' @export
format_biodiversity_data_gbif <- function(data, source, config) {

   formatted_df <- data %>%
    dplyr::filter(year>=config$databases$gbif$params$year_start) %>%
    dplyr::filter(taxonRank=="SPECIES") %>%
    dplyr::filter(!is.na(species)) %>%
    dplyr::mutate(source=source)

  return(formatted_df)
}
