#' module_gcamindia_L223.agg_elect_td_trn
#'
#' Calculate supply sector, subsector, and technology information for the agg_elect_td_trn sector
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L223.india_state_Supplysector_agg_elect_td_trn}, \code{L223.india_state_SubsectorLogit_agg_elect_td_trn}, \code{L223.india_state_SubsectorShrwtFllt_agg_elect_td_trn},
#' \code{L223.india_state_TechShrwt_agg_elect_td_trn}, \code{L223.india_state_TechCoeff_agg_elect_td_trn},
#' original data system was \code{L223.building_agg.R} (energy level2).
#' @details Calculate shareweights, cost, price elasticity, calibrated, and other data for the building sector
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author AM Nov20
module_gcamindia_L223.agg_elect_td_trn <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-india/A23.india_state_sector_agg_elect_td_trn",
             FILE = "gcam-india/A23.india_state_subsector_logit_agg_elect_td_trn",
             FILE = "gcam-india/A23.india_state_subsector_shrwt_agg_elect_td_trn",
             FILE = "gcam-india/A23.india_state_tech_shrwt_agg_elect_td_trn"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L223.india_state_Supplysector_agg_elect_td_trn",
             "L223.india_state_SubsectorLogit_agg_elect_td_trn",
             "L223.india_state_SubsectorShrwtFllt_agg_elect_td_trn",
             "L223.india_state_TechShrwt_agg_elect_td_trn",
             "L223.india_state_TechCoeff_agg_elect_td_trn"))
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
    A23.india_state_sector_agg_elect_td_trn <- get_data(all_data, "gcam-india/A23.india_state_sector_agg_elect_td_trn")
    A23.india_state_subsector_logit_agg_elect_td_trn <- get_data(all_data, "gcam-india/A23.india_state_subsector_logit_agg_elect_td_trn")
    A23.india_state_subsector_shrwt_agg_elect_td_trn <- get_data(all_data, "gcam-india/A23.india_state_subsector_shrwt_agg_elect_td_trn")
    A23.india_state_tech_shrwt_agg_elect_td_trn <- get_data(all_data, "gcam-india/A23.india_state_tech_shrwt_agg_elect_td_trn")

    # ===================================================

    A23.india_state_sector_agg_elect_td_trn <- A23.india_state_sector_agg_elect_td_trn %>% mutate(region = "India")
    A23.india_state_subsector_logit_agg_elect_td_trn <- A23.india_state_subsector_logit_agg_elect_td_trn %>% mutate(region = "India")
    A23.india_state_subsector_shrwt_agg_elect_td_trn <- A23.india_state_subsector_shrwt_agg_elect_td_trn %>% mutate(region = "India")
    A23.india_state_tech_shrwt_agg_elect_td_trn <- A23.india_state_tech_shrwt_agg_elect_td_trn %>% mutate(region = "India")


    # convert to long form


    A23.india_state_tech_shrwt_agg_elect_td_trn <- A23.india_state_tech_shrwt_agg_elect_td_trn %>%
      gather_years




    # The agg_elect_td_trn_india_processing function is used in place of a for loop in the old data sytem.
    # This function checks to see if the input data needs to be expanded to all states or used as
    # is.

    # agg_elect_td_trn_india_processing: is a function that
    agg_elect_td_trn_india_processing <- function(data) {

      # Subset the input data frame for the india region. The subsetted data will be used
      # to check to see if the data frame needs to be processed, it's assumed that if the india
      # is not found in the region column that regions have already been processed.

      check_india <- filter(data, region == gcam.india_REGION)

      if(nrow(check_india) == 0) {

        # This does not change the entries of the data frame but will strip the attributes
        # from the input data frame.
        new_data <- mutate(data, region = region)

      } else {

        # If the input data frame contains india region information
        # then expand the input data to all states.

        data %>%
          filter(region == gcam.india_REGION) %>%
          write_to_all_india_states(names = names(data)) ->
          new_data

      }

      return(new_data)
    } # end of function

    A23.india_state_sector_agg_elect_td_trn <- agg_elect_td_trn_india_processing(A23.india_state_sector_agg_elect_td_trn)
    A23.india_state_subsector_logit_agg_elect_td_trn <- agg_elect_td_trn_india_processing(A23.india_state_subsector_logit_agg_elect_td_trn)
    A23.india_state_subsector_shrwt_agg_elect_td_trn <- agg_elect_td_trn_india_processing(A23.india_state_subsector_shrwt_agg_elect_td_trn)
    A23.india_state_tech_shrwt_agg_elect_td_trn <- agg_elect_td_trn_india_processing(A23.india_state_tech_shrwt_agg_elect_td_trn)


    L223.india_state_Supplysector_agg_elect_td_trn <- A23.india_state_sector_agg_elect_td_trn %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(supplysector, output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent, logit.type) %>%
      mutate(region = "India") %>%
      select(region, supplysector, output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent, logit.type) %>%
      unique()

    L223.india_state_SubsectorLogit_agg_elect_td_trn <- A23.india_state_subsector_logit_agg_elect_td_trn %>%
      mutate(subsector = paste(subsector,region), logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(-region) %>%
      mutate(region = "India") %>%
      select(region, supplysector, subsector, logit.year.fillout, logit.exponent, logit.type)


    L223.india_state_SubsectorShrwtFllt_agg_elect_td_trn <- A23.india_state_subsector_shrwt_agg_elect_td_trn %>%
      mutate(subsector = paste(subsector,region), year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(-region) %>%
      mutate(region = "India") %>%
      select(region, supplysector, subsector, year.fillout, share.weight)



    # Shareweights of agg_elect_td_trn technologies # OUTPUT
    L223.india_state_TechShrwt_agg_elect_td_trn <- A23.india_state_tech_shrwt_agg_elect_td_trn %>%
     # Expand table to include all model base and future years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, subsector, technology,region)) %>%
      # Extrapolate to fill out values for all years
      # Rule 2 is used so years that may be outside of min-max range are assigned values from closest data, as opposed to NAs
      group_by(supplysector, subsector, technology, region) %>%
      mutate(share.weight = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      filter(year %in% MODEL_YEARS) %>% # This will drop 1971
      mutate(subsector = paste(subsector,region), technology = subsector) %>%
      select(supplysector, subsector, technology, year, share.weight) %>%
      mutate(region = "India") %>%
      select(region, supplysector, subsector, technology, year, share.weight)



    L223.india_state_TechCoeff_agg_elect_td_trn <- A23.india_state_tech_shrwt_agg_elect_td_trn %>%
      # Expand table to include all model base and future years
      complete(year = c(year, MODEL_YEARS), nesting(supplysector, subsector, technology,region)) %>%
      rename(market.name = region) %>%
    mutate (region = "India", minicam.energy.input = technology) %>%
    filter(year %in% MODEL_YEARS) %>% # This will drop 1971
      mutate(subsector = paste(subsector,market.name), technology = subsector, coefficient = 1) %>%
      select(region, supplysector, subsector, technology, minicam.energy.input, market.name, year, coefficient)




    # ===================================================


    L223.india_state_Supplysector_agg_elect_td_trn %>%
      add_title("Supply sector information for agg_elec sector") %>%
      add_units("Unitless") %>%
      add_comments("Supply sector information for agg_elec was written for all regions") %>%
      add_legacy_name("L223.india_state_Supplysector_agg_elect_td_trn") %>%
      add_precursors("gcam-india/A23.india_state_sector_agg_elect_td_trn") ->
      L223.india_state_Supplysector_agg_elect_td_trn

    L223.india_state_SubsectorLogit_agg_elect_td_trn %>%
      add_title("Supply subsector information for agg_elec sector") %>%
      add_units("Unitless") %>%
      add_comments("Supply subsector information for agg_elec was written for all regions") %>%
      add_legacy_name("L223.india_state_SubsectorLogit_agg_elect_td_trn") %>%
      add_precursors("gcam-india/A23.india_state_subsector_logit_agg_elect_td_trn") ->
      L223.india_state_SubsectorLogit_agg_elect_td_trn

    L223.india_state_SubsectorShrwtFllt_agg_elect_td_trn %>%
      add_title("Supply subsector information for agg_elec sector") %>%
      add_units("Unitless") %>%
      add_comments("Supply subsector information for agg_elec was written for all regions") %>%
      add_legacy_name("L223.india_state_SubsectorShrwtFllt_agg_elect_td_trn") %>%
      add_precursors("gcam-india/A23.india_state_subsector_shrwt_agg_elect_td_trn") ->
      L223.india_state_SubsectorShrwtFllt_agg_elect_td_trn

    L223.india_state_TechShrwt_agg_elect_td_trn %>%
      add_title("Supply subsector information for agg_elec sector") %>%
      add_units("Unitless") %>%
      add_comments("Supply subsector information for agg_elec was written for all regions") %>%
      add_legacy_name("L223.india_state_TechShrwt_agg_elect_td_trn") %>%
      add_precursors("gcam-india/A23.india_state_tech_shrwt_agg_elect_td_trn") ->
      L223.india_state_TechShrwt_agg_elect_td_trn

    L223.india_state_TechCoeff_agg_elect_td_trn %>%
      add_title("Supply subsector information for agg_elec sector") %>%
      add_units("Unitless") %>%
      add_comments("Supply subsector information for agg_elec was written for all regions") %>%
      add_legacy_name("L223.india_state_TechCoeff_agg_elect_td_trn") %>%
      add_precursors("gcam-india/A23.india_state_tech_shrwt_agg_elect_td_trn") ->
      L223.india_state_TechCoeff_agg_elect_td_trn


    return_data(L223.india_state_Supplysector_agg_elect_td_trn,
                L223.india_state_SubsectorLogit_agg_elect_td_trn,
                L223.india_state_SubsectorShrwtFllt_agg_elect_td_trn,
                L223.india_state_TechShrwt_agg_elect_td_trn,
                L223.india_state_TechCoeff_agg_elect_td_trn)
  } else {
    stop("Unknown command")
  }
}
