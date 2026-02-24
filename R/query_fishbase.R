#' Extraer datos de Fishbase
#' https://docs.ropensci.org/rfishbase/
#' @param grid Complete sampling grid
#' @param this_source Database name
#'
#' @returns result when done
#' @export
#'
#' @examples

query_fishbase <- function(grid, this_source) {

  results.file <- here::here(
    "data",
    "query_results",
    paste0(this_source, "_raw"),
    paste0(this_source, "_results.csv")
  )

  if (file.exists(results.file) == FALSE) {

    occ_tbl <- rfishbase::fb_tbl("occurrence")

    bb_box <- sf::st_bbox(grid)

    occ_selecc <- occ_tbl %>%
      dplyr::filter(LatitudeDec > bb_box[2]) %>%  #select the appropriate latitude
      dplyr::filter(LatitudeDec < bb_box[4]) %>%
      dplyr::filter(LongitudeDec > bb_box[1]) %>%
      dplyr::filter(LongitudeDec < bb_box[3])


    tax_table <- rfishbase::fb_tbl("classification_tree")


    taxa_occ <- occ_selecc %>%
      dplyr::rename(Species = SpeciesCol, Genus = GenusCol) %>%
      dplyr::left_join(tax_table, by = c("Species", "Genus"))


    readr::write_csv(taxa_occ,
                     here::here(
                       "data",
                       "query_results",
                       paste0(this_source, "_raw"),
                       paste0(this_source, "_results.csv")
                     ))

  }

  result <- "done"

  return(result)
}
