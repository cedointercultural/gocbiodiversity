#' @title Limpiar nombre de especie
#' @description Elimina información adicional del nombre científico (autor, año, etc.) y convierte a minúsculas para normalización
#'
#' @param species_name Nombre de la especie (puede incluir autor, año, etc.)
#' @return Nombre limpio en minúsculas
#' @export
#' @examples
#' clean_species_name("Homo sapiens Linnaeus, 1758")  # "homo sapiens"
#' clean_species_name("Canis lupus (L.)")             # "canis lupus"
clean_species_name <- function(species_name) {
  if (is.na(species_name) || is.null(species_name)) {
    return(NA_character_)
  }

  # Convertir a character si no lo es
  species_name <- as.character(species_name)

  # Eliminar contenido entre paréntesis (autores abreviados)
  cleaned_name <- gsub("\\s*\\(.*?\\)", "", species_name)

  # Eliminar contenido después de coma (autor y año)
  cleaned_name <- gsub("\\s*,.*$", "", cleaned_name)

  # Eliminar año (4 dígitos) y texto posterior
  cleaned_name <- gsub("\\s*\\d{4}.*$", "", cleaned_name)

  # Eliminar espacios múltiples
  cleaned_name <- gsub("\\s+", " ", cleaned_name)

  # Eliminar espacios al inicio y final
  cleaned_name <- trimws(cleaned_name)

  # Convertir a minúsculas para normalización
  cleaned_name <- tolower(cleaned_name)

  return(cleaned_name)
}
