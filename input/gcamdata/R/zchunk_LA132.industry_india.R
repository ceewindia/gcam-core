#' module_gcamindia_LA132.industry
#'
#' Provides industrial energy consumption and industrial feedstock consumption by region/fuel/historical year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L132.india_state_in_EJ_indnochp_F}, \code{L132.india_state_in_EJ_indchp_F}, \code{L132.india_state_out_EJ_indchp_F}, \code{L132.india_state_in_EJ_indfeed_F}. The corresponding file in the
#' original data system was \code{LA132.Industry.R} (gcam-india level1).
#' @details Provides for each US state industrial energy consumption and industrial feedstock consumption by region/fuel/historical year.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author AM Nov20

module_gcamindia_LA132.industry <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L101.india_state_EB_EJ_state_S_F",
             "L122.india_state_in_EJ_refining_F",
             "L123.in_EJ_R_indchp_F_Yh",
             "L123.out_EJ_R_indchp_F_Yh",
             "L132.in_EJ_R_indenergy_F_Yh",
             "L232.StubTechCalInput_indenergy",
             "L2321.StubTechCalInput_cement_heat",
             "L1322.in_EJ_R_indfeed_F_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L132.india_state_in_EJ_indnochp_F",
             "L132.india_state_in_EJ_indchp_F",
             "L132.india_state_out_EJ_indchp_F",
             "L132.india_state_in_EJ_indfeed_F"))
  } else if(command == driver.MAKE) {

    # Silence package checks
    fuel <- year <- state <- value <- sector <- refinery_comsumption <- GCAM_region_ID <-
      multiplier <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    L101.india_state_EB_EJ_state_S_F <- get_data(all_data, "L101.india_state_EB_EJ_state_S_F")
    L122.india_state_in_EJ_refining_F <- get_data(all_data, "L122.india_state_in_EJ_refining_F")
    L123.in_EJ_R_indchp_F_Yh <- get_data(all_data, "L123.in_EJ_R_indchp_F_Yh")
    L123.out_EJ_R_indchp_F_Yh <- get_data(all_data, "L123.out_EJ_R_indchp_F_Yh")
    L132.in_EJ_R_indenergy_F_Yh <- get_data(all_data, "L132.in_EJ_R_indenergy_F_Yh")
    L232.StubTechCalInput_indenergy <- get_data(all_data, "L232.StubTechCalInput_indenergy")
    L2321.StubTechCalInput_cement_heat <- get_data(all_data, "L2321.StubTechCalInput_cement_heat")
    L1322.in_EJ_R_indfeed_F_Yh <- get_data(all_data, "L1322.in_EJ_R_indfeed_F_Yh")

    # ===================================================

    # PART 1. Compute industrial energy use, removing energy used in refining

    # Aggregate all refining fuel consumption for electricity and gas by state
    L122.india_state_in_EJ_refining_F %>%
      filter(fuel %in% c("electricity", "gas")) %>%
      group_by(state, fuel, year) %>% summarise(refinery_comsumption = sum(value)) %>% ungroup ->
      L132.india_state_in_EJ_refining_elecgas

    # Adjust industrial fuel consumption by removing refinery consumption computed above
    L132.india_state_in_EJ_ind_elecgas_adj <-  L101.india_state_EB_EJ_state_S_F %>%
      filter(sector %in% gcamindia.energyindustries, fuel %in% c("electricity", "gas")) %>%
      left_join_error_no_match(L132.india_state_in_EJ_refining_elecgas, by = c("state", "fuel", "year")) %>%
      mutate(value = value - refinery_comsumption,
             value = if_else(value < 0, 0, value)) %>%
      select(-refinery_comsumption)


    # Bind above with other fuels considered in GCAM's "industrial energy use" sector
    L101.india_state_EB_EJ_state_S_F %>%
      filter(sector %in% gcamindia.energyindustries,
             fuel %in% gcam.IND_ENERGY_USE,
             fuel != "gas") %>%
      bind_rows(L132.india_state_in_EJ_ind_elecgas_adj) ->
      L132.india_state_in_EJ_indenergy_F_unscaled

    # Compute fuel consumption by state and sector as proportion of india total
    L132.india_state_in_EJ_indenergy_F_unscaled %>%
      group_by(fuel, year) %>%
      mutate(value = value / sum(value)) %>% ungroup ->
      L132.india_state_in_pct_ind_F


    # PART 2. Apportion india consumption and output to state level for non-cogeneration and cogeneration

    # Apportion national-level industrial energy consumption to states - NON-COGEN

    L132.in_EJ_R_indenergy_F_Yh_new <- L132.in_EJ_R_indenergy_F_Yh %>% mutate(sectorx = sector)

    L132.india_state_in_EJ_indnochp_elec <- L132.india_state_in_pct_ind_F %>%
      mutate(sectorx = "industry_energy") %>% rename(multiplier = value) %>%
      # ^^ prepare for smooth join
      left_join_error_no_match(filter(L132.in_EJ_R_indenergy_F_Yh_new, GCAM_region_ID == gcam.INDIA_CODE),
                               by = c("fuel", "year", "sectorx")) %>%
      mutate(value = value *  multiplier) %>% mutate(sector = sector.x) %>%
      select(-multiplier, -GCAM_region_ID, -sectorx, -sector.y, -sector.x) %>%
      filter(fuel == "electricity") %>% select(state, sector, fuel, year, value)

    L132.india_state_ind_cement_nochp <- L2321.StubTechCalInput_cement_heat %>%
      filter (region == gcam.india_REGION) %>%
    mutate(supplysector = "industrial energy use") %>%
      left_join_error_no_match(filter(L232.StubTechCalInput_indenergy,region == gcam.india_REGION),
                               by = c("stub.technology", "year", "supplysector")) %>%
      mutate (value = calibrated.value.x + calibrated.value.y) %>% select(-supplysector) %>%
      mutate (sector = "industry_energy") %>% rename (fuel = stub.technology, region = region.x) %>%
      select (region, sector, fuel, year, value)

    # The industry_india_processing function is used in place of a for loop in the old data sytem.
    # This function checks to see if the input data needs to be expanded to all states or used as
    # is.

    # industry_india_processing: is a function that
    industry_india_processing <- function(data) {

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

    L132.india_state_ind_cement_nochp_1 <- industry_india_processing(L132.india_state_ind_cement_nochp) %>%
      rename (state = region) %>%  mutate(sectorx = sector)


    L132.india_state_in_EJ_indnochp_noelec <- L132.india_state_in_pct_ind_F %>%
      mutate(sectorx = "industry_energy") %>% rename(multiplier = value) %>%
      filter(year %in% c(MODEL_BASE_YEARS)) %>%
      # ^^ prepare for smooth join
      left_join (L132.india_state_ind_cement_nochp_1, by = c("state","fuel", "year", "sectorx")) %>%
      mutate(value = value *  multiplier) %>%
      select(-multiplier) %>%
      mutate(sector = sector.x) %>%
      select(-sectorx, -sector.y) %>%
      select (state, sector, fuel, year, value)

    L132.india_state_in_EJ_indnochp_bio <- L132.india_state_in_EJ_indnochp_noelec %>% filter(fuel == "biomass")
    L132.india_state_in_EJ_indnochp_RL <- L132.india_state_in_EJ_indnochp_noelec %>% filter (fuel == "refined liquids")
    L132.india_state_in_EJ_indnochp_coal <- L132.india_state_in_EJ_indnochp_noelec %>% filter (fuel == "coal")
    L132.india_state_in_EJ_indnochp_gas <- L132.india_state_in_EJ_indnochp_noelec %>% filter (fuel == "gas")

    L132.india_state_in_EJ_indnochp_F <- bind_rows(L132.india_state_in_EJ_indnochp_elec,L132.india_state_in_EJ_indnochp_bio,
                                                   L132.india_state_in_EJ_indnochp_RL,L132.india_state_in_EJ_indnochp_coal,
                                                   L132.india_state_in_EJ_indnochp_gas)

    # Apportion national-level industrial energy consumption to states - COGEN
    L132.india_state_in_pct_ind_F %>%
      filter(fuel %in% gcam.IND_ENERGY_USE) %>%
      # ^^ remove fuels that are inputs to cogen systems, i.e., not electricity
      mutate(sector = "chp_elec") %>% rename(multiplier = value) %>%
      # ^^ prepare for smooth join
      left_join_error_no_match(filter(L123.in_EJ_R_indchp_F_Yh, GCAM_region_ID == gcam.INDIA_CODE),
                               by = c("fuel", "year", "sector")) %>%
      mutate(value = value * multiplier) %>%
      select(-multiplier, -GCAM_region_ID) %>% unique ()->
      L132.india_state_in_EJ_indchp_F

    # Apportion nation-level industrial cogen output to states
    L132.india_state_in_pct_ind_F %>%
      filter(fuel %in% gcam.IND_ENERGY_USE) %>%
      mutate(sector = "chp_elec") %>% rename(multiplier = value) %>%
      # ^^ prepare for smooth join
      left_join_error_no_match(filter(L123.out_EJ_R_indchp_F_Yh, GCAM_region_ID == gcam.INDIA_CODE),
                               by = c("fuel", "year", "sector")) %>%
      mutate(value = value * multiplier) %>%
      select(-multiplier, -GCAM_region_ID) %>% unique() ->
      L132.india_state_out_EJ_indchp_F


    # PART 3: Industrial feedstocks
    # Note: Gas and liquid fuels only; each is treated separately...
    # ... Liquid fuels are apportioned to states by petchem feed and asphalt;
    # ... gas fuels are apportioned by petchem feed only.

    # Compute petroleum feedstocks by state (as proportion of india total)
    L101.india_state_EB_EJ_state_S_F %>%
      filter(sector %in% gcamindia.feedstocksindustries,
             fuel %in% c("refined liquids (const feed)", "refined liquids (petchem feed)")) %>%
      group_by(state, sector, year) %>% summarise(value = sum(value)) %>% ungroup %>%
      mutate(fuel = "refined liquids") %>%
      group_by(fuel, year) %>%
      mutate(value = value / sum(value)) %>% ungroup ->
      # ^^ computes values as proportion of india totals
      L132.india_state_in_EJ_indfeed_liq_unscaled

    # Compute natural gas feedstocks by state (as proportion of india total)
    # Note: assumes petrochemical feedstocks as basis for disaggregating natural gas feedstocks to states
    L101.india_state_EB_EJ_state_S_F %>%
      filter(sector %in% gcamindia.feedstocksindustries,
             fuel == "refined liquids (petchem feed)") %>%
      mutate(fuel = "gas") %>%
      group_by(fuel, year) %>%
      mutate(value = value / sum(value)) %>% ungroup ->
      # ^^ computes values as proportion of india totals
      L132.india_state_pct_indfeed_gas

    # Replicate natural gas feedstock proportions to create coal table
    # Note: these proportions don't actually matter, because coal feedstocks are zero for all years in india
    L132.india_state_pct_indfeed_gas %>%
      mutate(fuel = "coal") %>%
      bind_rows(L132.india_state_pct_indfeed_gas) %>%
      bind_rows(L132.india_state_in_EJ_indfeed_liq_unscaled) ->
      L132.india_state_pct_indfeed_F
    # ^^ proportions for petroleum, gas, and coal

    # Apportion feedstocks among states
    L1322.in_EJ_R_indfeed_F_Yh_new <- L1322.in_EJ_R_indfeed_F_Yh %>% mutate(sectorx = sector)


    L132.india_state_in_EJ_indfeed_F <- L132.india_state_pct_indfeed_F %>%
      mutate (sectorx = "industry_feedstocks") %>%
      rename(multiplier = value) %>%
      # ^^ prepare for smooth join
      left_join_error_no_match(filter(L1322.in_EJ_R_indfeed_F_Yh_new, GCAM_region_ID == gcam.INDIA_CODE),
                               by = c("sectorx", "fuel", "year")) %>%
      mutate(value = value * multiplier) %>%
      # ^^ get state portions
      select(-multiplier, -GCAM_region_ID) %>%
      mutate(sector = sector.x) %>%
      select(state, sector, fuel, year, value)



    ## OUTPUTS
    L132.india_state_in_EJ_indnochp_F %>%
      add_title("Industrial sector non-cogen input energy by state and fuel") %>%
      add_units("EJ") %>%
      add_comments("Computed by apportioning india-level consumption among states") %>%
      add_legacy_name("L132.india_state_in_EJ_indnochp_F") %>%
      add_precursors("L132.in_EJ_R_indenergy_F_Yh",
                     "L232.StubTechCalInput_indenergy",
                      "L2321.StubTechCalInput_cement_heat",
                     "L101.india_state_EB_EJ_state_S_F",
                     "L122.india_state_in_EJ_refining_F") ->
      L132.india_state_in_EJ_indnochp_F

    L132.india_state_in_EJ_indchp_F %>%
      add_title("Industrial sector cogeneration input energy by state and fuel") %>%
      add_units("EJ") %>%
      add_comments("Computed by apportioning india-level consumption among states") %>%
      add_legacy_name("L132.india_state_in_EJ_indchp_F") %>%
      add_precursors("L123.out_EJ_R_indchp_F_Yh",
                     "L101.india_state_EB_EJ_state_S_F",
                     "L122.india_state_in_EJ_refining_F") ->
      L132.india_state_in_EJ_indchp_F

    L132.india_state_out_EJ_indchp_F %>%
      add_title("Industrial sector electricity cogeneration by state") %>%
      add_units("EJ") %>%
      add_comments("Computed by apportioning india-level CHP generation among states") %>%
      add_legacy_name("L132.india_state_out_EJ_indchp_F") %>%
      add_precursors("L123.in_EJ_R_indchp_F_Yh",
                     "L101.india_state_EB_EJ_state_S_F",
                     "L122.india_state_in_EJ_refining_F") ->
      L132.india_state_out_EJ_indchp_F

    L132.india_state_in_EJ_indfeed_F %>%
      add_title("Industrial feedstocks by state and fuel") %>%
      add_units("EJ") %>%
      add_comments("Computed by apportioning india-level feedstocks among states") %>%
      add_legacy_name("L132.india_state_in_EJ_indfeed_F") %>%
      add_precursors("L101.india_state_EB_EJ_state_S_F", "L1322.in_EJ_R_indfeed_F_Yh") ->
      L132.india_state_in_EJ_indfeed_F

    return_data(L132.india_state_in_EJ_indnochp_F, L132.india_state_in_EJ_indchp_F, L132.india_state_out_EJ_indchp_F, L132.india_state_in_EJ_indfeed_F)
  } else {
    stop("Unknown command")
  }
}
