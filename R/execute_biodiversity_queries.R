#' @title Ejecutar consultas a bases de datos de biodiversidad
#'
#' @description
#' Función principal que coordina todo el proceso de consulta, desde la carga
#' de configuración hasta la exportación de resultados
#'
#' @param config_file Ruta al archivo JSON de configuración
#' @param polygon_file Ruta al shapefile (opcional, si no está en config)
#' @param output_dir Directorio de salida (opcional)
#' @param parallel Usar paralelización (TRUE/FALSE, default: "auto" decide automáticamente)
#' @param n_cores Número de núcleos para paralelización (NULL = auto-detectar)
#' @return Lista con resultados de todas las consultas y metadatos
#' @export
execute_biodiversity_queries <- function(config_file,
                                         polygon_file = NULL,
                                         output_dir = NULL,
                                         parallel = "auto",
                                         n_cores = NULL) {

  # Registrar tiempo de inicio
  start_time <- Sys.time()

  # ========================================================================
  # PASO 1: Cargar configuración
  # ========================================================================
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("  SISTEMA DE CONSULTAS DE BIODIVERSIDAD\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")

  config <- load_query_config(config_file)

  # Determinar directorio de salida
  if (is.null(output_dir)) {
    output_dir <- config$general$output_dir
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("✓ Directorio de salida creado:", output_dir, "\n")
  }

  # Configurar archivo de log
  log_file <- file.path(config$logging$log_file)
  dir.create(config$general$log_dir, recursive = TRUE)
  file.create(log_file)

  log_fn("═══════════════════════════════════════════════════════════════", log_file, level="INFO")
  log_fn("INICIANDO CONSULTAS DE BIODIVERSIDAD", log_file, level="INFO")
  log_fn("═══════════════════════════════════════════════════════════════", log_file, level="INFO")
  log_fn(paste("Configuración:", config_file), log_file, level="INFO")
  log_fn(paste("Directorio de salida:", output_dir), log_file, level="INFO")

  # ========================================================================
  # PASO 2: Cargar y procesar polígono
  # ========================================================================
  log_fn("Cargando área de estudio...", log_file, level="INFO")

  if (is.null(polygon_file)) {
    polygon_file <- here::here(config$general$polygon_path)
  }

  polygon <- load_polygon_from_shapefile(polygon_file)
  log_fn(paste("Polígono cargado desde:", polygon_file), log_file, level="INFO")

  # ========================================================================
  # PASO 3: Generar grid de búsqueda
  # ========================================================================
  log_fn("Generando grid de búsqueda...", log_file, level="INFO")

  if (config$spatial$grid_enabled) {
    grid <- generate_grid_bboxes(
      polygon = polygon,
      grid_size = config$spatial$grid_size_degrees,
      square = TRUE,
      buffer_percent = if(!is.null(config$spatial$buffer_percent)) config$spatial$buffer_percent else 2
    )
    log_fn(paste("Grid optimizado generado:", nrow(grid), "celdas de",
                 config$spatial$grid_size_degrees, "grados con buffer"), log_file, level="INFO")
  } else {
    grid <- generate_simple_bbox(polygon)
    log_fn("Usando bounding box completo (sin subdivisión)", log_file, level="INFO")
  }

  # Solo aplicar límite de boxes si está configurado explícitamente
  if (!is.null(config$spatial$max_boxes) &&
      is.numeric(config$spatial$max_boxes) &&
      config$spatial$max_boxes > 0 &&
      nrow(grid) > config$spatial$max_boxes) {
    log_fn(paste("⚠️ Limitando grid a", config$spatial$max_boxes, "celdas de",
                 nrow(grid), "disponibles"), log_file, level="INFO")
    grid <- grid[1:config$spatial$max_boxes, ]
    log_fn("✅ Grid limitado aplicado" ,log_file, level="INFO")
  }


  # Solo aplicar límite de boxes si está configurado explícitamente
  if (!is.null(config$spatial$max_boxes) &&
      is.numeric(config$spatial$max_boxes) &&
      config$spatial$max_boxes > 0 &&
      nrow(grid) > config$spatial$max_boxes) {
   # log_fn(paste("⚠️ Limitando grid a", config$spatial$max_boxes, "celdas de",
   #              nrow(grid), "disponibles"))
    grid <- grid[1:config$spatial$max_boxes, ]
    log_fn("✅ Grid limitado aplicado")
  }

  # ========================================================================
  # PASO 4: Ejecutar consultas a bases de datos
  # ========================================================================
  log_fn("───────────────────────────────────────────────────────────────", log_file, level="INFO")
  log_fn("EJECUTANDO CONSULTAS", log_file, level="INFO")
  log_fn("───────────────────────────────────────────────────────────────", log_file, level="INFO")

  # Decidir estrategia de ejecución (secuencial vs paralela)
  use_parallel <- FALSE

  if (parallel == "auto") {
    # Usar paralelización si hay más de 5 celdas y está disponible
    use_parallel <- (nrow(grid) >= 5 && exists("execute_all_queries_adaptive"))
  } else if (is.logical(parallel)) {
    use_parallel <- parallel && exists("execute_all_queries_adaptive")
  }

  # Ejecutar consultas con la estrategia apropiada
  if (use_parallel) {
    log_fn("🚀 Modo: PARALELO", log_file, level="INFO")
    all_results <- execute_all_queries_adaptive(
      grid = grid,
      config = config,
      log_function = log_fn,
      parallel_threshold = 5,
      n_cores = n_cores
    )
  } else {
    log_fn("📊 Modo: SECUENCIAL", log_file, level="INFO")
    all_results <- execute_all_queries(
      grid = grid,
      config = config,
      log_function = log_fn
    )
  }

  # ========================================================================
  # PASO 5: Procesar y limpiar resultados
  # ========================================================================
  log_fn("───────────────────────────────────────────────────────────────", log_file, level="INFO")
  log_fn("PROCESANDO RESULTADOS", log_file, level="INFO")
  log_fn("───────────────────────────────────────────────────────────────", log_file, level="INFO")
  log_fn(paste("Total de registros recolectados:", nrow(all_results)), log_file, level="INFO")

  # Inicializar variable para resultados únicos
  all_results_unique <- data.frame()

  if (nrow(all_results) > 0) {
    # Eliminar duplicados
    all_results_unique <- remove_duplicates(all_results)
    log_fn(paste("Registros únicos después de eliminar duplicados:",
                 nrow(all_results_unique)), log_file, level="INFO")

    # Generar resumen estadístico
    summary_stats <- summarize_biodiversity_data(all_results_unique)
    log_fn(paste("Especies únicas encontradas:", summary_stats$unique_species), log_file, level="INFO")
    log_fn(paste("Fuentes de datos:"), log_file, level="INFO")
    for (source in names(summary_stats$sources)) {
      log_fn(paste("  -", source, ":", summary_stats$sources[[source]], "registros"), log_file, level="INFO")
    }

    if (!all(is.na(summary_stats$year_range))) {
      log_fn(paste("Rango temporal:",
                   summary_stats$year_range[1], "-",
                   summary_stats$year_range[2]), log_file, level="INFO")
    }

    # ======================================================================
    # PASO 6: Exportar resultados
    # ======================================================================
    log_fn("───────────────────────────────────────────────────────────────", log_file, level="INFO")
    log_fn("EXPORTANDO RESULTADOS", log_file, level="INFO")
    log_fn("───────────────────────────────────────────────────────────────", log_file, level="INFO")

    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    base_name <- paste0("biodiversity_", timestamp)

    # Exportar datos en formatos configurados
    exported_files <- export_biodiversity_data(
      data = all_results_unique,
      output_dir = output_dir,
      base_name = base_name,
      formats = config$output$formats,
      log_function = log_fn
    )

    # ======================================================================
    # PASO 7: Generar y exportar metadatos
    # ======================================================================
    if (config$output$include_metadata) {
      execution_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

      metadata <- generate_metadata(
        config = config,
        results = all_results_unique,
        grid = grid,
        execution_time = execution_time
      )

      metadata$config_file <- config_file
      metadata$polygon_file <- polygon_file
      metadata$output_files <- exported_files

      metadata_file <- file.path(output_dir,
                                 paste0("metadata_", timestamp, ".json"))
      jsonlite::write_json(metadata, metadata_file, pretty = TRUE, auto_unbox = TRUE)
      log_fn(paste("✓ Metadatos exportados:", metadata_file), log_file, level="INFO")
    }

  } else {
    log_fn("⚠ No se encontraron registros en ninguna base de datos",log_file, level="WARNING")
  }

  # ========================================================================
  # FINALIZACIÓN
  # ========================================================================
  end_time <- Sys.time()
  execution_time <- difftime(end_time, start_time, units = "secs")

  log_fn("───────────────────────────────────────────────────────────────", log_file, level="INFO")
  log_fn("PROCESO COMPLETADO", log_file, level="INFO")
  log_fn("───────────────────────────────────────────────────────────────", log_file, level="INFO")
  log_fn(paste("Tiempo de ejecución:", round(execution_time, 2), "segundos"), log_file, level="INFO")
  log_fn(paste("Registros finales:", nrow(all_results_unique)), log_file, level="INFO")
  log_fn(paste("Archivo de log:", log_file), log_file, level="INFO")
  log_fn("═══════════════════════════════════════════════════════════════", log_file, level="INFO")

  cat("\n✓ Proceso finalizado exitosamente\n")
  cat("  Resultados guardados en:", output_dir, "\n\n")

  # Retornar objeto con todos los resultados
  return(list(
    results = all_results_unique,
    grid = grid,
    polygon = polygon,
    config = config,
    execution_time = execution_time,
    summary = if(nrow(all_results_unique) > 0)
      summarize_biodiversity_data(all_results_unique) else NULL
  ))
}
