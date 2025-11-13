#' @title Ejecutar consultas con balanceo de carga por chunks
#' @description Divide el grid en chunks y los procesa en paralelo para mejor control y recuperación ante errores
#'
#' @param grid Data frame con columnas: box_id, bbox, wkt
#' @param config Lista de configuración con parámetros de consulta
#' @param log_function Función para logging (opcional)
#' @param chunk_size Tamaño de cada chunk (default: 10 celdas)
#' @param n_cores Número de núcleos a usar (por defecto: detecta automáticamente)
#' @return Data frame consolidado con todos los resultados
#' @export
execute_all_queries_chunked <- function(grid, config, log_function,
                                        chunk_size = 10, n_cores = NULL) {

  # Detectar núcleos
  if (is.null(n_cores)) {
    n_cores <- max(1, parallel::detectCores() - 1)
  }

  # Dividir grid en chunks
  n_boxes <- nrow(grid)
  n_chunks <- ceiling(n_boxes / chunk_size)

  log_function(paste0("🔄 Estrategia por chunks:"), log_file, level="INFO")
  log_function(paste0("   • Total de celdas: ", n_boxes), log_file, level="INFO")
  log_function(paste0("   • Tamaño de chunk: ", chunk_size), log_file, level="INFO")
  log_function(paste0("   • Total de chunks: ", n_chunks), log_file, level="INFO")
  log_function(paste0("   • Núcleos: ", n_cores, "\n"), log_file, level="INFO")

  future::plan("future::multisession", workers = n_cores)

  all_results <- data.frame()

  # Procesar por base de datos
  for (db_name in names(config$databases)) {
    db_config <- config$databases[[db_name]]

    if (!db_config$enabled) next

    log_function(paste0("Procesando ", toupper(db_name), " en ", n_chunks, " chunks..."), log_file, level="INFO")
    start_time <- Sys.time()

    # Función de consulta según base de datos
    query_func <- switch(db_name,
                         "gbif" = query_gbif,
                         "obis" = query_obis,
                         "inat" = query_inat,
                         "ebird" = query_ebird,
                         "idigbio" = query_idigbio,
                         NULL
    )

    if (is.null(query_func)) {
      log_function(paste("⚠ Base de datos no reconocida:", db_name), log_file, level="INFO")
      next
    }

    # Procesar chunks en paralelo
    chunk_results <- list()

    for (chunk_idx in 1:n_chunks) {
      start_idx <- (chunk_idx - 1) * chunk_size + 1
      end_idx <- min(chunk_idx * chunk_size, n_boxes)
      chunk_indices <- start_idx:end_idx

      log_function(sprintf("  Chunk %d/%d (celdas %d-%d)...",
                           chunk_idx, n_chunks, start_idx, end_idx), log_file, level="INFO")

      tryCatch({
        chunk_result_list <- future_map(
          chunk_indices,
          function(i) {
            tryCatch({
              if (identical(query_func, query_ebird)) {
                return(query_ebird(
                  bbox = grid$bbox[i],
                  config = config,
                  box_id = i,
                  log_function = function(msg) {}
                ))
              }
              query_func(
                grid_row = grid[i, ],
                config = config,
                box_id = i,
                log_function = function(msg) {}
              )
            }, error = function(e) {
              log_function(sprintf("    ✗ Error %s - Box %d: %s", toupper(db_name), i, e$message), log_file, level="INFO")
              data.frame()
            })
          },
          .options = furrr_options(seed = TRUE)
        )

        chunk_result <- do.call(rbind, chunk_result_list)
        if (!is.null(chunk_result) && nrow(chunk_result) > 0) {
          chunk_results[[chunk_idx]] <- chunk_result
          log_function(sprintf("    ✓ %d registros", nrow(chunk_result), log_file, level="INFO"))
        } else {
          log_function("    ⚠ Sin resultados", log_file, level="INFO")
        }

      }, error = function(e) {
        log_function(sprintf("    ✗ Error: %s", e$message), log_file, level="INFO")
      })
    }

    # Consolidar resultados del database
    if (length(chunk_results) > 0) {
      db_results <- do.call(rbind, chunk_results)
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

      if (nrow(db_results) > 0) {
        all_results <- rbind(all_results, db_results)
        log_function(sprintf("✓ %s completado: %d registros en %.1f segundos\n",
                             toupper(db_name), nrow(db_results), elapsed), log_file, level="INFO")
      }
    }
  }

  plan(sequential)
  return(all_results)
}
