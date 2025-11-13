
#' @title Obtener formato espacial apropiado para API específica
#'
#' @param api_type Tipo de API: "gbif", "obis", "inat", "idigbio", "ebird"
#' @param grid_row Fila del grid (debe tener columnas bbox, wkt, min_lon, etc.)
#' @return Lista con formato apropiado para la API
#' @export
get_spatial_format_for_api <- function(api_type, grid_row) {

  switch(api_type,
         "gbif" = {
           # GBIF prefiere WKT directo
           list(
             type = "wkt",
             value = sf::st_as_text(grid_row),
             param_name = "geometry"
           )
         },
         "obis" = {
           # OBIS requiere WKT pero desde bbox
           list(
             type = "wkt",
             value = bbox_to_wkt(grid_row$bbox, "obis"),
             param_name = "geometry"
           )
         },
         "inat" = {
           # iNaturalist usa parámetros de bbox nativos
           list(
             type = "bbox_params",
             value = list(
               swlat = grid_row$min_lat,
               swlng = grid_row$min_lon,
               nelat = grid_row$max_lat,
               nelng = grid_row$max_lon
             ),
             param_name = "bbox_params"
           )
         },
         "idigbio" = {
           # iDigBio usa formato geopoint específico
           list(
             type = "geopoint",
             value = list(
               type = "geo_bounding_box",
               top_left = list(
                 lon = grid_row$min_lon,
                 lat = grid_row$max_lat
               ),
               bottom_right = list(
                 lon = grid_row$max_lon,
                 lat = grid_row$min_lat
               )
             ),
             param_name = "geopoint"
           )
         },
         "ebird" = {
           # eBird usa punto central y radio
           center_lon <- (grid_row$min_lon + grid_row$max_lon) / 2
           center_lat <- (grid_row$min_lat + grid_row$max_lat) / 2
           # Calcular radio aproximado para cubrir el bbox
           radius_km <- max(
             111 * abs(grid_row$max_lon - grid_row$min_lon),
             111 * abs(grid_row$max_lat - grid_row$min_lat)
           ) / 2 * 1.1  # 10% extra para asegurar cobertura

           list(
             type = "point_radius",
             value = list(
               lat = center_lat,
               lng = center_lon,
               dist = min(radius_km, 50)  # eBird limita a 50km
             ),
             param_name = "point_radius"
           )
         },
         {
           # Formato por defecto: bbox estándar
           list(
             type = "bbox",
             value = grid_row$bbox,
             param_name = "bbox"
           )
         }
  )
}
