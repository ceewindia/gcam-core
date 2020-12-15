#' module_gcamindia_batch_agg_elect_td_ind_xml
#'
#' Construct XML data structure for \code{elect_td_ind.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{agg_elect_td_ind_india.xml}. The corresponding file in the
#' original data system was \code{batch_agg_elect_td_ind_india_xml.R} (gcamindia XML).
#' @author AM Nov20
module_gcamindia_batch_agg_elect_td_ind_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L223.india_state_Supplysector_agg_elect_td_ind",
             "L223.india_state_SubsectorLogit_agg_elect_td_ind",
             "L223.india_state_SubsectorShrwtFllt_agg_elect_td_ind",
             "L223.india_state_TechShrwt_agg_elect_td_ind",
             "L223.india_state_TechCoeff_agg_elect_td_ind"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "agg_elect_td_ind_india.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L223.india_state_Supplysector_agg_elect_td_ind <- get_data(all_data, "L223.india_state_Supplysector_agg_elect_td_ind")
    L223.india_state_SubsectorLogit_agg_elect_td_ind <- get_data(all_data, "L223.india_state_SubsectorLogit_agg_elect_td_ind")
    L223.india_state_SubsectorShrwtFllt_agg_elect_td_ind <- get_data(all_data, "L223.india_state_SubsectorShrwtFllt_agg_elect_td_ind")
    L223.india_state_TechShrwt_agg_elect_td_ind <- get_data(all_data, "L223.india_state_TechShrwt_agg_elect_td_ind")
    L223.india_state_TechCoeff_agg_elect_td_ind <- get_data(all_data, "L223.india_state_TechCoeff_agg_elect_td_ind")

    # ===================================================

    # Produce outputs
    create_xml("agg_elect_td_ind_india.xml") %>%
      add_logit_tables_xml(L223.india_state_Supplysector_agg_elect_td_ind, "Supplysector") %>%
      add_logit_tables_xml(L223.india_state_SubsectorLogit_agg_elect_td_ind, "SubsectorLogit") %>%
      add_xml_data(L223.india_state_SubsectorShrwtFllt_agg_elect_td_ind, "SubsectorShrwtFllt") %>%
      add_xml_data(L223.india_state_TechShrwt_agg_elect_td_ind, "TechShrwt") %>%
      add_xml_data(L223.india_state_TechCoeff_agg_elect_td_ind, "TechCoef") %>%



      add_precursors("L223.india_state_Supplysector_agg_elect_td_ind",
                     "L223.india_state_SubsectorLogit_agg_elect_td_ind",
                     "L223.india_state_SubsectorShrwtFllt_agg_elect_td_ind",
                     "L223.india_state_TechShrwt_agg_elect_td_ind",
                     "L223.india_state_TechCoeff_agg_elect_td_ind") ->
      agg_elect_td_ind_india.xml

    return_data(agg_elect_td_ind_india.xml)
  } else {
    stop("Unknown command")
  }
}
