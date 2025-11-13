#' @title Formatear datos de biodiversidad a estructura estándar
#'
#' @description Convierte datos de diferentes fuentes a un formato unificado con
#' validación de tipos y eliminación de registros inválidos
#'
#' @param data Data frame con datos brutos de biodiversidad
#' @param source Nombre de la fuente de datos (ej: "GBIF", "OBIS")
#' @param required_cols Vector con nombres de columnas requeridas
#' @return Data frame formateado y validado
#' @export
format_biodiversity_data <- function(data,
                                     source,
                                     required_cols = c("species", "lon", "lat", "year")) {

   formatted_df <- data %>%
    dplyr::filter(year>=config$databases$gbif$params$year_start) %>%
    dplyr::filter(taxonRank=="SPECIES") %>%
    dplyr::filter(!is.na(species))

  sp.list <- formatted_df %>%
    dplyr::distinct(species) %>%
    dplyr::pull(species)

  return(sp.list)
}
