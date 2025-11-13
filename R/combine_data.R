#' @title Combine data
#'
#' @param file.list WKT (Well-Known Text) string para la geometría de búsqueda
#' @param all_results Lista de configuración GBIF con parámetros de consulta
#' @return Data frame con registros de GBIF formateados
#' @export
combine_data <- function(file.list, all_results) {
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
