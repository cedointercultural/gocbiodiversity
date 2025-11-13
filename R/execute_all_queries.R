

#' Ejecutar consultas para todas las bases de datos habilitadas
#'
#' @param grid Data frame con columnas box_id, bbox, wkt
#' @param config Lista de configuración completa
#' @param log_function Función para registrar mensajes
#' @return Data frame con todos los resultados consolidados
#' @export
execute_all_queries <- function(grid, config, log_function) {

    all_results <- data.frame()

  # Consultar GBIF
  if (config$databases$gbif$enabled) {
    log_function("Iniciando consultas a GBIF...", log_file, level="INFO")
    gbif_results <- data.frame()

    for (i in 1:nrow(grid)) {
      box_result <- query_gbif(
        grid_row = grid[i, ],
        config = config,
        box_id = i,
        log_function
      )
      if (nrow(box_result) > 0) {
        gbif_results <- rbind(gbif_results, box_result)
      }
    }

    if (nrow(gbif_results) > 0) {
      all_results <- rbind(all_results, gbif_results)
      log_function(paste("GBIF - Total:", nrow(gbif_results), "registros"), log_file, level="INFO")
    }
  }

  # Consultar OBIS
  if (config$databases$obis$enabled) {
    log_function("Iniciando consultas a OBIS...", log_file, level="INFO")
    obis_results <- data.frame()

    for (i in 1:nrow(grid)) {
      box_result <- query_obis(
        grid_row = grid[i, ],
        config = config,
        box_id = i,
        log_function
      )
      if (nrow(box_result) > 0) {
        obis_results <- rbind(obis_results, box_result)
      }
    }

    if (nrow(obis_results) > 0) {
      all_results <- rbind(all_results, obis_results)
      log_function(paste("OBIS - Total:", nrow(obis_results), "registros"), log_file, level="INFO")
    }
  }

  # Consultar iNaturalist
  if (config$databases$inat$enabled) {
    log_function("Iniciando consultas a iNaturalist...")
    inat_results <- data.frame()

    for (i in 1:nrow(grid)) {
      box_result <- query_inat(
        grid_row = grid[i, ],
        config = config,
        box_id = i,
        log_function
      )
      if (nrow(box_result) > 0) {
        inat_results <- rbind(inat_results, box_result)  # ✅ CORREGIDO: box_result en vez de inat_results
      }
    }

    if (nrow(inat_results) > 0) {
      all_results <- rbind(all_results, inat_results)
      log_function(paste("iNaturalist - Total:", nrow(inat_results), "registros"), log_file, level="INFO")
    }
  }

  # Consultar eBird
  if (config$databases$ebird$enabled) {
    log_function("Iniciando consultas a eBird...")
    ebird_results <- data.frame()

    for (i in 1:nrow(grid)) {
      box_result <- query_ebird(
        bbox = grid$bbox[i],
        config = config,
        box_id = i,
        log_function
      )
      if (nrow(box_result) > 0) {
        ebird_results <- rbind(ebird_results, box_result)
      }
    }

    if (nrow(ebird_results) > 0) {
      all_results <- rbind(all_results, ebird_results)
      log_function(paste("eBird - Total:", nrow(ebird_results), "registros"), log_file, level="INFO")
    }
  }

  # Consultar iDigBio
  if (config$databases$idigbio$enabled) {
    log_function("Iniciando consultas a iDigBio...")
    idigbio_results <- data.frame()

    for (i in 1:nrow(grid)) {
      box_result <- query_idigbio(
        grid_row = grid[i, ],
        config = config,
        box_id = i,
        log_function
      )
      if (nrow(box_result) > 0) {
        idigbio_results <- rbind(idigbio_results, box_result)
      }
    }

    if (nrow(idigbio_results) > 0) {
      all_results <- rbind(all_results, idigbio_results)
      log_function(paste("iDigBio - Total:", nrow(idigbio_results), "registros"), log_file, level="INFO")
    }
  }

  log_function(paste("Total de registros consolidados:", nrow(all_results)), log_file, level="INFO")

  return(all_results)
}
