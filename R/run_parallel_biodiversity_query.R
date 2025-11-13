#' @title Ejecutar consulta paralela mejorada para todas las bases de datos
#' @param polygon_file Ruta al archivo del polígono de estudio
#' @param config_file Ruta al archivo de configuración
#' @return Lista con resultados de la consulta
run_parallel_biodiversity_query <- function(polygon_file = "shapefiles/study_zone.gpkg",
                                            config_file = "scripts/config/query_config_maxima.json") {

  cat("\n")
  cat("╔═══════════════════════════════════════════════════════════════════╗\n")
  cat("║                 CONSULTA PARALELA MEJORADA                        ║\n")
  cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")

  # Cargar funciones necesarias
  cat("📦 Cargando sistema de consultas...\n")
  source("scripts/database_queries.R")

  # Verificar que tenemos las funciones necesarias
  if (!exists("execute_biodiversity_queries")) {
    stop("❌ Error: No se pudo cargar execute_biodiversity_queries")
  }

  cat("✅ Sistema cargado correctamente\n\n")

  # Configurar consulta con modo paralelo forzado
  cat("🚀 Iniciando consulta paralela optimizada...\n")
  cat("📍 Polígono:", polygon_file, "\n")
  cat("⚙️  Configuración:", config_file, "\n\n")

  # Ejecutar con paralelización habilitada
  start_time <- Sys.time()

  results <- execute_biodiversity_queries(
    config_file = config_file,
    polygon_file = polygon_file,
    output_dir = "data/query_results",
    parallel = TRUE,  # Forzar modo paralelo
    n_cores = 3       # Usar 3 núcleos
  )

  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "mins"))

  # Mostrar resultados
  if (!is.null(results) && !is.null(results$results) && nrow(results$results) > 0) {
    cat("\n")
    cat("╔═══════════════════════════════════════════════════════════════════╗\n")
    cat("║                    CONSULTA PARALELA COMPLETADA                   ║\n")
    cat("╚═══════════════════════════════════════════════════════════════════╝\n\n")

    cat("📊 RESUMEN DE RESULTADOS:\n")
    cat("   • Registros totales:", nrow(results$results), "\n")
    cat("   • Tiempo transcurrido:", sprintf("%.1f minutos", duration), "\n")
    cat("   • Modo utilizado: PARALELO MEJORADO\n")
    cat("   • Núcleos utilizados: 3\n\n")

    # Análisis por base de datos
    if ("database" %in% colnames(results$results)) {
      db_summary <- table(results$results$database)
      cat("📈 REGISTROS POR BASE DE DATOS:\n")
      for (db in names(db_summary)) {
        cat(sprintf("   • %-15s: %s registros\n", db, format(db_summary[db], big.mark = ",")))
      }
      cat("\n")
    }

    # Información de archivos generados
    cat("📁 ARCHIVOS GENERADOS:\n")
    output_files <- list.files("data/query_results",
                               pattern = paste0("biodiversity.*", format(Sys.Date(), "%Y%m%d")),
                               full.names = FALSE)
    if (length(output_files) > 0) {
      for (file in tail(output_files, 3)) {  # Mostrar solo los últimos 3
        cat("   • data/query_results/", file, "\n")
      }
    }

    cat("\n✅ Consulta paralela completada exitosamente!\n")

  } else {
    cat("\n")
    cat("⚠️  No se obtuvieron resultados. Verificando posibles causas...\n\n")

    # Diagnóstico básico
    cat("🔍 DIAGNÓSTICO:\n")
    cat("   • Archivo de polígono:", ifelse(file.exists(polygon_file), "✅ Existe", "❌ No existe"), "\n")
    cat("   • Archivo de configuración:", ifelse(file.exists(config_file), "✅ Existe", "❌ No existe"), "\n")

    # Probar conectividad básica
    cat("   • Probando conectividad a GBIF...\n")
    test_result <- tryCatch({
      rgbif::occ_search(country = "MX", limit = 1, hasCoordinate = TRUE)
      "✅ Conectado"
    }, error = function(e) {
      paste("❌ Error:", e$message)
    })
    cat("   • Conectividad GBIF:", test_result, "\n")
  }

  return(results)
}
