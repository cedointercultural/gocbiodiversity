#' @title Convertir bbox a formato WKT optimizado para API específica
#'
#' @param bbox String "min_lon,min_lat,max_lon,max_lat" o vector numérico
#' @param api_type Tipo de API: "gbif", "obis", "inat", "idigbio"
#' @return String WKT optimizado para la API específica
#' @export
bbox_to_wkt <- function(bbox, api_type = "gbif") {
  # Parsear bbox si es string
  if (is.character(bbox)) {
    bbox_parts <- as.numeric(strsplit(bbox, ",")[[1]])
  } else {
    bbox_parts <- as.numeric(bbox)
  }

  if (length(bbox_parts) != 4 || any(is.na(bbox_parts))) {
    stop("Formato de bbox inválido. Debe ser: min_lon,min_lat,max_lon,max_lat")
  }

  min_lon <- bbox_parts[1]
  min_lat <- bbox_parts[2]
  max_lon <- bbox_parts[3]
  max_lat <- bbox_parts[4]

  # Formato estándar WKT (antihorario)
  wkt <- sprintf(
    "POLYGON((%f %f,%f %f,%f %f,%f %f,%f %f))",
    min_lon, min_lat,  # SW
    max_lon, min_lat,  # SE
    max_lon, max_lat,  # NE
    min_lon, max_lat,  # NW
    min_lon, min_lat   # SW (cierre)
  )

  return(wkt)
}
