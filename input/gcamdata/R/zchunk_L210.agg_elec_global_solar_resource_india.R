#' module_gcamindia_L210.agg_global_solar_resource_india
#'
#' Calculate supply sector, subsector, and technology information for the agg_elect_td_ind sector
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L210.india_state_UnlimitRsrc_agg_india}, \code{L210.india_state_UnlimitRsrcPrice_agg_india}
#' original data system was \code{L223.building_agg.R} (energy level2).
#' @details Calculate shareweights, cost, price elasticity, calibrated, and other data for the building sector
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author PNK Nov20
module_gcamindia_L210.agg_global_solar_resource_india <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L210.india_state_UnlimitRsrc",
             "L210.india_state_UnlimitRsrcPrice"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L210.india_state_UnlimitRsrc_agg_india",
             "L210.india_state_UnlimitRsrcPrice_agg_india"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package notes
     region <- renewresource <- smooth.renewable.subresource <- year.fillout <- maxSubResource <- mid.price <-
       curve.exponent <- output.unit <- price.unit <- market<- NULL


    # Load required inputs
     L210.india_state_UnlimitRsrc <- get_data(all_data, "L210.india_state_UnlimitRsrc")
     L210.india_state_UnlimitRsrcPrice <- get_data(all_data, "L210.india_state_UnlimitRsrcPrice")

    # ===================================================

     L210.india_state_UnlimitRsrc_agg_india <- L210.india_state_UnlimitRsrc %>%
       rename(state = region) %>%
       mutate (region = "India") %>%
       select(region, unlimited.resource, output.unit, price.unit, market) %>%
       unique()


     L210.india_state_UnlimitRsrcPrice_agg_india <- L210.india_state_UnlimitRsrcPrice %>%
       rename(state = region) %>%
       mutate (region = "India") %>%
       select(region, unlimited.resource, year, price) %>%
       unique()



    # ===================================================


       L210.india_state_UnlimitRsrc_agg_india %>%
      add_title("Smth renew res infortation for india wind sector") %>%
      add_units("Unitless") %>%
      add_comments("Smth renew res infortation for india wind sector") %>%
      add_legacy_name("L210.india_state_UnlimitRsrc_agg_india") %>%
      add_precursors("L210.india_state_UnlimitRsrc") ->
         L210.india_state_UnlimitRsrc_agg_india

       L210.india_state_UnlimitRsrcPrice_agg_india %>%
      add_title("Smth renew res infortation for india wind sector") %>%
      add_units("Unitless") %>%
      add_comments("Smth renew res infortation for india wind sector") %>%
      add_legacy_name("L210.india_state_UnlimitRsrcPrice_agg_india") %>%
      add_precursors("L210.india_state_UnlimitRsrcPrice") ->
         L210.india_state_UnlimitRsrcPrice_agg_india



    return_data(L210.india_state_UnlimitRsrc_agg_india,
                L210.india_state_UnlimitRsrcPrice_agg_india)
  } else {
    stop("Unknown command")
  }
}
