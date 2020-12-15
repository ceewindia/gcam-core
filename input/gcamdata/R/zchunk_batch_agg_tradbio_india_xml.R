#' module_gcamindia_batch_agg_tradbio_xml
#'
#' Construct XML data structure for \code{comm_india.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{agg_tradbio_india.xml}. The corresponding file in the
#' original data system was \code{batch_agg_elect_td_ind_india_xml.R} (gcamindia XML).
#' @author PNK Nov20
module_gcamindia_batch_agg_tradbio_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L242.india_state_Supplysector_agg_tradbio",
             "L242.india_state_SubsectorLogit_agg_tradbio",
             "L242.india_state_SubsectorShrwtFllt_agg_tradbio",
             "L242.india_state_TechShrwt_agg_tradbio",
             "L242.india_state_TechEff_agg_tradbio"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "agg_tradbio_india.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L242.india_state_Supplysector_agg_tradbio <- get_data(all_data, "L242.india_state_Supplysector_agg_tradbio")
    L242.india_state_SubsectorLogit_agg_tradbio <- get_data(all_data, "L242.india_state_SubsectorLogit_agg_tradbio")
    L242.india_state_SubsectorShrwtFllt_agg_tradbio <- get_data(all_data, "L242.india_state_SubsectorShrwtFllt_agg_tradbio")
    L242.india_state_TechShrwt_agg_tradbio <- get_data(all_data, "L242.india_state_TechShrwt_agg_tradbio")
    L242.india_state_TechEff_agg_tradbio <- get_data(all_data, "L242.india_state_TechEff_agg_tradbio")

    # ===================================================

    # Produce outputs
    create_xml("agg_tradbio_india.xml") %>%
      add_logit_tables_xml(L242.india_state_Supplysector_agg_tradbio, "Supplysector") %>%
      add_logit_tables_xml(L242.india_state_SubsectorLogit_agg_tradbio, "SubsectorLogit") %>%
      add_xml_data(L242.india_state_SubsectorShrwtFllt_agg_tradbio, "SubsectorShrwtFllt") %>%
      add_xml_data(L242.india_state_TechShrwt_agg_tradbio, "TechShrwt") %>%
      add_xml_data(L242.india_state_TechEff_agg_tradbio, "TechEff") %>%



      add_precursors("L242.india_state_Supplysector_agg_tradbio",
                     "L242.india_state_SubsectorLogit_agg_tradbio",
                     "L242.india_state_SubsectorShrwtFllt_agg_tradbio",
                     "L242.india_state_TechShrwt_agg_tradbio",
                     "L242.india_state_TechEff_agg_tradbio") ->
      agg_tradbio_india.xml

    return_data(agg_tradbio_india.xml)
  } else {
    stop("Unknown command")
  }
}
