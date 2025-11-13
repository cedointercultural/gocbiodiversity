#' @title  Exportar datos en múltiples formatos
#'
#' @param data Data frame a exportar
#' @param output_dir Directorio de salida
#' @param base_name Nombre base del archivo (sin extensión)
#' @param formats Vector de formatos: "csv", "json", "xlsx"
#' @param log_function Función para logging (opcional)
#' @export
export_biodiversity_data <- function(data,
                                     output_dir,
                                     base_name,
                                     formats = c("csv", "json"),
                                     log_function) {



  exported_files <- list()

  for (format in formats) {
    output_file <- file.path(output_dir, paste0(base_name, ".", format))

    tryCatch({
      if (format == "csv") {
        write.csv(data, output_file, row.names = FALSE)
        log_function(paste("✓ Exportado CSV:", output_file), log_file, level="INFO")
        exported_files$csv <- output_file

      } else if (format == "json") {
        jsonlite::write_json(data, output_file, pretty = TRUE)
        log_function(paste("✓ Exportado JSON:", output_file), log_file, level="INFO")
        exported_files$json <- output_file

      } else if (format == "xlsx") {
        if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(data, output_file)
          log_function(paste("✓ Exportado XLSX:", output_file), log_file, level="INFO")
          exported_files$xlsx <- output_file
        } else {
          log_function("⚠ Paquete 'openxlsx' no disponible, omitiendo formato XLSX")
        }
      } else {
        log_function(paste("⚠ Formato no soportado:", format), log_file, level="INFO")
      }
    }, error = function(e) {
      log_function(paste("✗ Error al exportar", format, ":", e$message), log_file, level="INFO")
    })
  }

  return(exported_files)
}
