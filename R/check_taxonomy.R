#' Check taxonomy using WORMS
#'
#' @param thisspecies
#'
#' @returns sp_data
#' @export
#'
#' @examples
check_taxonomy <- function(thisspecies){

  tryCatch(taxa.list <- worrms::wm_records_name(thisspecies), error = function(e) e, finally = taxa.list <- "No result")

if(taxa.list!="no result"){

  formatted_df <- taxize::gna_verifier(taxa.list) %>%
    dplyr::select(submittedName, currentCanonicalSimple) %>%
    dplyr::rename(species=submittedName)

}

}
