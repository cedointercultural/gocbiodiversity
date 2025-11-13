#' @title Ejecutar consultas a bases de datos de biodiversidad
#'
#' @description
#' Función principal que coordina todo el proceso de consulta, desde la carga
#' de configuración hasta la exportación de resultados
#'
#' @param config_file Ruta al archivo JSON de configuración
#' @param polygon_file Ruta al shapefile (opcional, si no está en config)
#' @param output_dir Directorio de salida (opcional)
#' @return Lista con resultados de todas las consultas y metadatos
#' @export
execute_bio_gbif_queries <- function(config_file,
                                         polygon_file = NULL,
                                         output_dir = NULL) {

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

    log_fn("🚀 Modo: Loop", log_file, level="INFO")
    execute_gbif_queries_loop(
      grid = grid,
      config = config,
      log_function,
      log_file
    )


}
