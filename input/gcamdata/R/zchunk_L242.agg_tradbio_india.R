#' module_gcamindia_L242.agg_tradbio
#'
#' Calculate supply sector, subsector, and technology information for the agg_elect_td_ind sector
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L242.india_state_Supplysector_agg_tradbio}, \code{L242.india_state_SubsectorLogit_agg_tradbio}, \code{L242.india_state_SubsectorShrwtFllt_agg_tradbio},
#' \code{L242.india_state_TechShrwt_agg_tradbio}, \code{L242.india_state_TechEff_agg_tradbio},
#' original data system was \code{L223.building_agg.R} (energy level2).
#' @details Calculate shareweights, cost, price elasticity, calibrated, and other data for the building sector
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author PNK Nov20
module_gcamindia_L242.agg_tradbio <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-india/A42.india_state_sector_agg_tradbio",
             FILE = "gcam-india/A42.india_state_subsector_logit_agg_tradbio",
             FILE = "gcam-india/A42.india_state_subsector_shrwt_agg_tradbio",
             FILE = "gcam-india/A42.india_state_tech_shrwt_agg_tradbio"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L242.india_state_Supplysector_agg_tradbio",
             "L242.india_state_SubsectorLogit_agg_tradbio",
             "L242.india_state_SubsectorShrwtFllt_agg_tradbio",
             "L242.india_state_TechShrwt_agg_tradbio",
             "L242.india_state_TechEff_agg_tradbio"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package notes
    . <- GCAM_region_ID <- MgdFor_adj <- base.service <- technology <- to.value <-
      calibrated.value <- coefficient <- curr_table <- efficiency <- fuel <-
      has_district_heat <- input.cost <- logit.exponent <- minicam.energy.input <-
      minicam.non.energy.input <- output <- region <- region_subsector <- sector <-
      share.weight <- share.weight.year <- stub.technology <- subsector <- supplysector <-
      tradbio_region <- year <- year.fillout <- value <- apply.to <- from.year <- to.year <-
      interpolation.function <- sector.name <- subsector.name <- subs.share.weight <-
      tech.share.weight <- fuelprefElasticity <- perCapitaBased <- energy.final.demand <-
      price.elasticity <- logit.type <- output.unit <- input.unit <- price.unit <-
      logit.year.fillout <- final.energy <- NULL


    # Load required inputs
    A42.india_state_sector_agg_tradbio <- get_data(all_data, "gcam-india/A42.india_state_sector_agg_tradbio")
    A42.india_state_subsector_logit_agg_tradbio <- get_data(all_data, "gcam-india/A42.india_state_subsector_logit_agg_tradbio")
    A42.india_state_subsector_shrwt_agg_tradbio <- get_data(all_data, "gcam-india/A42.india_state_subsector_shrwt_agg_tradbio")
    A42.india_state_tech_shrwt_agg_tradbio <- get_data(all_data, "gcam-india/A42.india_state_tech_shrwt_agg_tradbio")

    # ===================================================

    A42.india_state_sector_agg_tradbio <- A42.india_state_sector_agg_tradbio %>% mutate(region = "India")
    A42.india_state_subsector_logit_agg_tradbio <- A42.india_state_subsector_logit_agg_tradbio %>% mutate(region = "India")
    A42.india_state_subsector_shrwt_agg_tradbio <- A42.india_state_subsector_shrwt_agg_tradbio %>% mutate(region = "India")
    A42.india_state_tech_shrwt_agg_tradbio <- A42.india_state_tech_shrwt_agg_tradbio %>% mutate(region = "India")


    # convert to long form


    A42.india_state_tech_shrwt_agg_tradbio <- A42.india_state_tech_shrwt_agg_tradbio %>%
      gather_years




    L242.india_state_Supplysector_agg_tradbio <- A42.india_state_sector_agg_tradbio %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(supplysector, output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent, logit.type) %>%
      mutate(region = "India") %>%
      select(region, supplysector, output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent, logit.type) %>%
      unique()

    L242.india_state_SubsectorLogit_agg_tradbio <- A42.india_state_subsector_logit_agg_tradbio %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(-region) %>%
      mutate(region = "India") %>%
      select(region, supplysector, subsector, logit.year.fillout, logit.exponent, logit.type)


    L242.india_state_SubsectorShrwtFllt_agg_tradbio <- A42.india_state_subsector_shrwt_agg_tradbio %>%
      mutate(year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(-region) %>%
      mutate(region = "India") %>%
      select(region, supplysector, subsector, year.fillout, share.weight)



    # Shareweights of agg_elect_td_ind technologies # OUTPUT
    L242.india_state_TechShrwt_agg_tradbio <- A42.india_state_tech_shrwt_agg_tradbio %>%
     # Expand table to include all model base and future years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, subsector, technology,region)) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years that may be outside of min-max range are assigned values from closest data, as opposed to NAs
      group_by(supplysector, subsector, technology, region) %>%
      mutate(share.weight = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>% # This will drop 1971
      mutate(technology = subsector) %>%
      select(supplysector, subsector, technology, year, share.weight) %>%
      mutate(region = "India") %>%
      select(region, supplysector, subsector, technology, year, share.weight)



    L242.india_state_TechEff_agg_tradbio <- A42.india_state_tech_shrwt_agg_tradbio %>%
      # Expand table to include all model base and future years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, subsector, technology,region)) %>%
      rename(market.name = region) %>%
    mutate (region = "India", minicam.energy.input = technology) %>%
    filter(year %in% MODEL_YEARS) %>% # This will drop 1971
      mutate(technology = subsector, efficiency = 1) %>%
      select(region, supplysector, subsector, technology, minicam.energy.input, market.name, year, efficiency)




    # ===================================================


    L242.india_state_Supplysector_agg_tradbio %>%
      add_title("Supply sector information for agg_elec sector") %>%
      add_units("Unitless") %>%
      add_comments("Supply sector information for agg_elec was written for all regions") %>%
      add_legacy_name("L242.india_state_Supplysector_agg_tradbio") %>%
      add_precursors("gcam-india/A42.india_state_sector_agg_tradbio") ->
      L242.india_state_Supplysector_agg_tradbio

    L242.india_state_SubsectorLogit_agg_tradbio %>%
      add_title("Supply subsector information for agg_elec sector") %>%
      add_units("Unitless") %>%
      add_comments("Supply subsector information for agg_elec was written for all regions") %>%
      add_legacy_name("L242.india_state_SubsectorLogit_agg_tradbio") %>%
      add_precursors("gcam-india/A42.india_state_subsector_logit_agg_tradbio") ->
      L242.india_state_SubsectorLogit_agg_tradbio

    L242.india_state_SubsectorShrwtFllt_agg_tradbio %>%
      add_title("Supply subsector information for agg_elec sector") %>%
      add_units("Unitless") %>%
      add_comments("Supply subsector information for agg_elec was written for all regions") %>%
      add_legacy_name("L242.india_state_SubsectorShrwtFllt_agg_tradbio") %>%
      add_precursors("gcam-india/A42.india_state_subsector_shrwt_agg_tradbio") ->
      L242.india_state_SubsectorShrwtFllt_agg_tradbio

    L242.india_state_TechShrwt_agg_tradbio %>%
      add_title("Supply subsector information for agg_elec sector") %>%
      add_units("Unitless") %>%
      add_comments("Supply subsector information for agg_elec was written for all regions") %>%
      add_legacy_name("L242.india_state_TechShrwt_agg_tradbio") %>%
      add_precursors("gcam-india/A42.india_state_tech_shrwt_agg_tradbio") ->
      L242.india_state_TechShrwt_agg_tradbio

    L242.india_state_TechEff_agg_tradbio %>%
      add_title("Supply subsector information for agg_elec sector") %>%
      add_units("Unitless") %>%
      add_comments("Supply subsector information for agg_elec was written for all regions") %>%
      add_legacy_name("L242.india_state_TechEff_agg_tradbio") %>%
      add_precursors("gcam-india/A42.india_state_tech_shrwt_agg_tradbio") ->
      L242.india_state_TechEff_agg_tradbio


    return_data(L242.india_state_Supplysector_agg_tradbio,
                L242.india_state_SubsectorLogit_agg_tradbio,
                L242.india_state_SubsectorShrwtFllt_agg_tradbio,
                L242.india_state_TechShrwt_agg_tradbio,
                L242.india_state_TechEff_agg_tradbio)
  } else {
    stop("Unknown command")
  }
}
