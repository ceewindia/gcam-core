#' module_gcamindia_L210.agg_onshore_wind_resource_india
#'
#' Calculate supply sector, subsector, and technology information for the agg_elect_td_ind sector
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L210.india_state_RenewRsrc_agg_india}, \code{L210.india_state_SmthRenewRsrcCurves_wind_agg_india}
#' original data system was \code{L223.building_agg.R} (energy level2).
#' @details Calculate shareweights, cost, price elasticity, calibrated, and other data for the building sector
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author PNK Nov20
module_gcamindia_L210.agg_onshore_wind_resource_india <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L210.india_state_RenewRsrc",
             "L210.india_state_SmthRenewRsrcCurves_wind"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L210.india_state_RenewRsrc_agg_india",
             "L210.india_state_SmthRenewRsrcCurves_wind_agg_india"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package notes
     region <- renewresource <- smooth.renewable.subresource <- year.fillout <- maxSubResource <- mid.price <-
       curve.exponent <- output.unit <- price.unit <- market<- NULL


    # Load required inputs
     L210.india_state_RenewRsrc <- get_data(all_data, "L210.india_state_RenewRsrc")
     L210.india_state_SmthRenewRsrcCurves_wind <- get_data(all_data, "L210.india_state_SmthRenewRsrcCurves_wind")

    # ===================================================

     L210.india_state_RenewRsrc_agg_india <- L210.india_state_RenewRsrc %>%
       filter(renewresource == "onshore wind resource") %>%
       select(renewresource, output.unit, price.unit, market) %>%
       mutate(region = "India", market = region) %>%
       select(region, renewresource, output.unit, price.unit, market) %>%
       unique()

     L210.india_state_SmthRenewRsrcCurves_wind_agg_india <- L210.india_state_SmthRenewRsrcCurves_wind %>%
     rename(state = region) %>%
     mutate (region = "India") %>%
       group_by(region, renewresource, smooth.renewable.subresource, year.fillout) %>%
       summarise(maxSubResource = sum(maxSubResource), mid.price = median(mid.price), curve.exponent = median(curve.exponent)) %>%
      ungroup()



    # ===================================================


       L210.india_state_RenewRsrc_agg_india %>%
      add_title("Smth renew res infortation for india wind sector") %>%
      add_units("Unitless") %>%
      add_comments("Smth renew res infortation for india wind sector") %>%
      add_legacy_name("L210.india_state_RenewRsrc_agg_india") %>%
      add_precursors("L210.india_state_RenewRsrc") ->
         L210.india_state_RenewRsrc_agg_india

       L210.india_state_SmthRenewRsrcCurves_wind_agg_india %>%
      add_title("Smth renew res infortation for india wind sector") %>%
      add_units("Unitless") %>%
      add_comments("Smth renew res infortation for india wind sector") %>%
      add_legacy_name("L210.india_state_SmthRenewRsrcCurves_wind_agg_india") %>%
      add_precursors("L210.india_state_SmthRenewRsrcCurves_wind") ->
         L210.india_state_SmthRenewRsrcCurves_wind_agg_india



    return_data(L210.india_state_RenewRsrc_agg_india,
                L210.india_state_SmthRenewRsrcCurves_wind_agg_india)
  } else {
    stop("Unknown command")
  }
}
