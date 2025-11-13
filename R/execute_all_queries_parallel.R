#' @title Ejecutar consultas en paralelo para todas las bases de datos habilitadas
#' @description Versión optimizada que usa paralelización para acelerar las consultas cuando hay múltiples celdas de grid
#'
#' @param grid Data frame con columnas: box_id, bbox, wkt
#' @param config Lista de configuración con parámetros de consulta
#' @param log_function Función para logging (opcional)
#' @param n_cores Número de núcleos a usar (por defecto: detecta automáticamente)
#'
#' @return Data frame consolidado con todos los resultados
#' @export
execute_all_queries_parallel <- function(grid, config, log_function = cat, n_cores = NULL) {

  # Detectar número de núcleos disponibles
  if (is.null(n_cores)) {
    n_cores <- max(1, parallel::detectCores() - 1)  # Dejar 1 núcleo libre
  }

  log_function(paste0("🚀 Configuración de paralelización:"), log_file, level="INFO")
  log_function(paste0("   • Núcleos disponibles: ", parallel::detectCores()), log_file, level="INFO")
  log_function(paste0("   • Núcleos a usar: ", n_cores), log_file, level="INFO")
  log_function(paste0("   • Celdas del grid: ", nrow(grid)), log_file, level="INFO")
  log_function(paste0("   • Estimación: ~", round(nrow(grid) / n_cores, 1), " celdas por núcleo\n"), log_file, level="INFO")

  # Configurar plan de paralelización
  future::plan("future::multisession", workers = n_cores)

  all_results <- data.frame()

  # ============================================================================
  # GBIF - Paralelizado
  # ============================================================================
  if (config$databases$gbif$enabled) {
    log_function("Iniciando consultas paralelas a GBIF...", log_file, level="INFO")
    start_time <- Sys.time()

    tryCatch({
      # Ejecutar consultas en paralelo
      gbif_results_list <- future_map(
        1:nrow(grid),
        function(i) {
          tryCatch({
            query_gbif(
              grid_row = grid[i, ],
              config = config,
              box_id = i,
              log_function = function(msg) {}
            )
          }, error = function(e) {
            log_function(sprintf("✗ Error GBIF - Box %d: %s", i, e$message), log_file, level="INFO")
            data.frame()
          })
        },
        .options = furrr_options(seed = TRUE)
      )

      # Combinar resultados
      gbif_results <- do.call(rbind, gbif_results_list)

      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

      if (!is.null(gbif_results) && nrow(gbif_results) > 0) {
        all_results <- rbind(all_results, gbif_results)
        log_function(sprintf("✓ GBIF - Total: %d registros en %.1f segundos (%.1f celdas/seg)",
                             nrow(gbif_results), elapsed, nrow(grid)/elapsed), log_file, level="INFO")
      } else {
        log_function(sprintf("⚠ GBIF - Sin resultados (%.1f segundos)", elapsed), log_file, level="INFO")
      }
    }, error = function(e) {
      log_function(paste("✗ Error en consultas GBIF:", e$message), log_file, level="INFO")
    })
  }

  # ============================================================================
  # OBIS - Paralelizado
  # ============================================================================
  if (config$databases$obis$enabled) {
    log_function("Iniciando consultas paralelas a OBIS...")
    start_time <- Sys.time()

    tryCatch({
      obis_results_list <- future_map(
        1:nrow(grid),
        function(i) {
          tryCatch({
            query_obis(
              grid_row = grid[i, ],
              config = config,
              box_id = i,
              log_function = function(msg) {}
            )
          }, error = function(e) {
            log_function(sprintf("✗ Error OBIS - Box %d: %s", i, e$message), log_file, level="INFO")
            data.frame()
          })
        },
        .options = furrr_options(seed = TRUE)
      )

      obis_results <- do.call(rbind, obis_results_list)
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

      if (!is.null(obis_results) && nrow(obis_results) > 0) {
        all_results <- rbind(all_results, obis_results)
        log_function(sprintf("✓ OBIS - Total: %d registros en %.1f segundos (%.1f celdas/seg)",
                             nrow(obis_results), elapsed, nrow(grid)/elapsed), log_file, level="INFO")
      } else {
        log_function(sprintf("⚠ OBIS - Sin resultados (%.1f segundos)", elapsed), log_file, level="INFO")
      }
    }, error = function(e) {
      log_function(paste("✗ Error en consultas OBIS:", e$message), log_file, level="INFO")
    })
  }

  # ============================================================================
  # iNaturalist - Paralelizado
  # ============================================================================
  if (config$databases$inat$enabled) {
    log_function("Iniciando consultas paralelas a iNaturalist...")
    start_time <- Sys.time()

    tryCatch({
      inat_results_list <- future_map(
        1:nrow(grid),
        function(i) {
          tryCatch({
            query_inat(
              grid_row = grid[i, ],
              config = config,
              box_id = i,
              log_function = function(msg) {}
            )
          }, error = function(e) {
            log_function(sprintf("✗ Error iNaturalist - Box %d: %s", i, e$message))
            data.frame()
          })
        },
        .options = furrr_options(seed = TRUE)
      )

      inat_results <- do.call(rbind, inat_results_list)
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

      if (!is.null(inat_results) && nrow(inat_results) > 0) {
        all_results <- rbind(all_results, inat_results)
        log_function(sprintf("✓ iNaturalist - Total: %d registros en %.1f segundos (%.1f celdas/seg)",
                             nrow(inat_results), elapsed, nrow(grid)/elapsed), log_file, level="INFO")
      } else {
        log_function(sprintf("⚠ iNaturalist - Sin resultados (%.1f segundos)", elapsed), log_file, level="INFO")
      }
    }, error = function(e) {
      log_function(paste("✗ Error en consultas iNaturalist:", e$message), log_file, level="INFO")
    })
  }

  # ============================================================================
  # iDigBio - Paralelizado
  # ============================================================================
  if (config$databases$idigbio$enabled) {
    log_function("Iniciando consultas paralelas a iDigBio...", log_file, level="INFO")
    start_time <- Sys.time()

    tryCatch({
      idigbio_results_list <- future_map(
        1:nrow(grid),
        function(i) {
          tryCatch({
            query_idigbio(
              grid_row = grid[i, ],
              config = config,
              box_id = i,
              log_function = function(msg) {}
            )
          }, error = function(e) {
            log_function(sprintf("✗ Error iDigBio - Box %d: %s", i, e$message), log_file, level="INFO")
            data.frame()
          })
        },
        .options = furrr_options(seed = TRUE)
      )

      idigbio_results <- do.call(rbind, idigbio_results_list)
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

      if (!is.null(idigbio_results) && nrow(idigbio_results) > 0) {
        all_results <- rbind(all_results, idigbio_results)
        log_function(sprintf("✓ iDigBio - Total: %d registros en %.1f segundos (%.1f celdas/seg)",
                             nrow(idigbio_results), elapsed, nrow(grid)/elapsed),, log_file, level="INFO")
      } else {
        log_function(sprintf("⚠ iDigBio - Sin resultados (%.1f segundos)", elapsed), log_file, level="INFO")
      }
    }, error = function(e) {
      log_function(paste("✗ Error en consultas iDigBio:", e$message), log_file, level="INFO")
    })
  }

  # Restaurar plan secuencial
  plan(sequential)

  return(all_results)
}

