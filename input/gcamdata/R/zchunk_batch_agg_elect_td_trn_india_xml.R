#' module_gcamindia_batch_agg_elect_td_trn_xml
#'
#' Construct XML data structure for \code{elect_td_trn.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{agg_elect_td_trn_india.xml}. The corresponding file in the
#' original data system was \code{batch_agg_elect_td_trn_india_xml.R} (gcamindia XML).
#' @author AM Nov20
module_gcamindia_batch_agg_elect_td_trn_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L223.india_state_Supplysector_agg_elect_td_trn",
             "L223.india_state_SubsectorLogit_agg_elect_td_trn",
             "L223.india_state_SubsectorShrwtFllt_agg_elect_td_trn",
             "L223.india_state_TechShrwt_agg_elect_td_trn",
             "L223.india_state_TechCoeff_agg_elect_td_trn"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "agg_elect_td_trn_india.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L223.india_state_Supplysector_agg_elect_td_trn <- get_data(all_data, "L223.india_state_Supplysector_agg_elect_td_trn")
    L223.india_state_SubsectorLogit_agg_elect_td_trn <- get_data(all_data, "L223.india_state_SubsectorLogit_agg_elect_td_trn")
    L223.india_state_SubsectorShrwtFllt_agg_elect_td_trn <- get_data(all_data, "L223.india_state_SubsectorShrwtFllt_agg_elect_td_trn")
    L223.india_state_TechShrwt_agg_elect_td_trn <- get_data(all_data, "L223.india_state_TechShrwt_agg_elect_td_trn")
    L223.india_state_TechCoeff_agg_elect_td_trn <- get_data(all_data, "L223.india_state_TechCoeff_agg_elect_td_trn")

    # ===================================================

    # Produce outputs
    create_xml("agg_elect_td_trn_india.xml") %>%
      add_logit_tables_xml(L223.india_state_Supplysector_agg_elect_td_trn, "Supplysector") %>%
      add_logit_tables_xml(L223.india_state_SubsectorLogit_agg_elect_td_trn, "SubsectorLogit") %>%
      add_xml_data(L223.india_state_SubsectorShrwtFllt_agg_elect_td_trn, "SubsectorShrwtFllt") %>%
      add_xml_data(L223.india_state_TechShrwt_agg_elect_td_trn, "TechShrwt") %>%
      add_xml_data(L223.india_state_TechCoeff_agg_elect_td_trn, "TechCoef") %>%



      add_precursors("L223.india_state_Supplysector_agg_elect_td_trn",
                     "L223.india_state_SubsectorLogit_agg_elect_td_trn",
                     "L223.india_state_SubsectorShrwtFllt_agg_elect_td_trn",
                     "L223.india_state_TechShrwt_agg_elect_td_trn",
                     "L223.india_state_TechCoeff_agg_elect_td_trn") ->
      agg_elect_td_trn_india.xml

    return_data(agg_elect_td_trn_india.xml)
  } else {
    stop("Unknown command")
  }
}
