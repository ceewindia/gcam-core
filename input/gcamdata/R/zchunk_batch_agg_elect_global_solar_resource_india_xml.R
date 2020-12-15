#' module_gcamindia_batch_agg_elect_global_solar_resource_xml
#'
#' Construct XML data structure for \code{comm_india.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{agg_elect_global_solar_resource_india.xml}. The corresponding file in the
#' (gcamindia XML).
#' @author PNK Nov20
module_gcamindia_batch_agg_elect_global_solar_resource_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L210.india_state_UnlimitRsrc_agg_india",
             "L210.india_state_UnlimitRsrcPrice_agg_india"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "agg_elect_global_solar_resource_india.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L210.india_state_UnlimitRsrc_agg_india <- get_data(all_data, "L210.india_state_UnlimitRsrc_agg_india")
    L210.india_state_UnlimitRsrcPrice_agg_india <- get_data(all_data, "L210.india_state_UnlimitRsrcPrice_agg_india")

    # ===================================================

    # Produce outputs
    create_xml("agg_elect_global_solar_resource_india.xml") %>%
      add_xml_data(L210.india_state_UnlimitRsrc_agg_india, "UnlimitRsrc") %>%
      add_xml_data(L210.india_state_UnlimitRsrcPrice_agg_india, "UnlimitRsrcPrice") %>%


      add_precursors("L210.india_state_UnlimitRsrc_agg_india",
                     "L210.india_state_UnlimitRsrcPrice_agg_india") ->
      agg_elect_global_solar_resource_india.xml

    return_data(agg_elect_global_solar_resource_india.xml)
  } else {
    stop("Unknown command")
  }
}
