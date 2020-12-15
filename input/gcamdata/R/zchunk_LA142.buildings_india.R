#' module_gcamindia_LA142.buildings
#'
#' Provides residential and commercial building energy consumption by region/fuel/historical year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L142.india_state_in_EJ_comm_F}, \code{L142.india_state_in_EJ_resid_F}. The corresponding file in the
#' original data system was \code{LA142.Buildings.R} (gcam-india level1).
#' @details Provides for each US state industrial energy consumption and industrial feedstock consumption by region/fuel/historical year.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author PNK Nov20


module_gcamindia_LA142.buildings <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L101.india_state_EB_EJ_state_S_F",
             "L142.in_EJ_R_bld_F_Yh"))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L142.india_state_in_EJ_comm_F",
             "L142.india_state_in_EJ_resid_F"))

  } else if(command == driver.MAKE) {

    # Silence package checks
    fuel <- year <- state <- value <- sector <- GCAM_region_ID <- multiplier <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    L101.india_state_EB_EJ_state_S_F <- get_data(all_data, "L101.india_state_EB_EJ_state_S_F")
    L142.in_EJ_R_bld_F_Yh <- get_data(all_data, "L142.in_EJ_R_bld_F_Yh")

    # ===================================================

    #Re-allocation of refined liquids from resid sector to comm for 2010 as in the core there is zero refined liquids for comm sector, which shouldn't be the case

    L142.in_EJ_R_bld_F_Yh_refliq_comm <- L142.in_EJ_R_bld_F_Yh %>%
      filter (GCAM_region_ID == gcam.INDIA_CODE, fuel == "refined liquids", year == "2010", sector == "bld_comm") %>%
      select (-value) %>% mutate(value = 0.251545)

    L142.in_EJ_R_bld_F_Yh_refliq_resid <- L142.in_EJ_R_bld_F_Yh %>%
      filter (GCAM_region_ID == gcam.INDIA_CODE, fuel == "refined liquids", year == "2010", sector == "bld_resid") %>%
      select (-value) %>% mutate(value = 0.70838797)


    L142.in_EJ_R_bld_F_Yh_gas_comm <- L142.in_EJ_R_bld_F_Yh %>%
      filter (GCAM_region_ID == gcam.INDIA_CODE, fuel == "gas", year == "2010", sector == "bld_comm") %>%
      select (-value) %>% mutate(value = 0.000929399)

    L142.in_EJ_R_bld_F_Yh_gas_resid <- L142.in_EJ_R_bld_F_Yh %>%
      filter (GCAM_region_ID == gcam.INDIA_CODE, fuel == "gas", year == "2010", sector == "bld_resid") %>%
      select (-value) %>% mutate(value = 0.000075)


    L142.in_EJ_R_bld_F_Yh <- L142.in_EJ_R_bld_F_Yh[-c(7200),]
    L142.in_EJ_R_bld_F_Yh <- L142.in_EJ_R_bld_F_Yh[-c(7399),]
    L142.in_EJ_R_bld_F_Yh <- L142.in_EJ_R_bld_F_Yh[-c(7160),]
    L142.in_EJ_R_bld_F_Yh <- L142.in_EJ_R_bld_F_Yh[-c(7357),]


    L142.in_EJ_R_bld_F_Yh <- L142.in_EJ_R_bld_F_Yh %>%
      bind_rows(L142.in_EJ_R_bld_F_Yh_refliq_comm, L142.in_EJ_R_bld_F_Yh_refliq_resid,
                L142.in_EJ_R_bld_F_Yh_gas_comm,L142.in_EJ_R_bld_F_Yh_gas_resid)

    # Computing comm energy consumption share from Energy Balance
    L142.india_state_in_EJ_comm_F <- L101.india_state_EB_EJ_state_S_F %>%
      filter(sector == "comm") %>%
      group_by(sector,fuel,year) %>%
      mutate(value = value /sum(value)) %>%
      ungroup %>%
      mutate (sector = "bld_comm") %>%
      rename (multiplier = value) %>%
      left_join(filter(L142.in_EJ_R_bld_F_Yh, GCAM_region_ID == gcam.INDIA_CODE),
                               by = c("fuel", "year", "sector")) %>%
      mutate (value = value * multiplier) %>%
      select (-multiplier, -GCAM_region_ID) %>%
      mutate (sector = "comm")



    # Computing resid energy consumption share from Energy Balance

    #Dividing core into urban and rural

    #share of urban and rural energy
    L101.india_state_EB_EJ_state_Urban <- L101.india_state_EB_EJ_state_S_F %>%
      filter(sector == "resid urban") %>%
      group_by(fuel, year) %>%
      mutate(value = sum(value)) %>%
      select(-state) %>%
      distinct()

    L101.india_state_EB_EJ_state_Rural <- L101.india_state_EB_EJ_state_S_F %>%
      filter(sector == "resid rural") %>%
      group_by(fuel, year) %>%
      mutate(value = sum(value)) %>%
      select(-state) %>%
      distinct()


    L101.india_state_EB_EJ_state_TotBld <-  L101.india_state_EB_EJ_state_Urban %>%
      bind_rows(L101.india_state_EB_EJ_state_Rural) %>%
      group_by(fuel, year) %>%
      mutate(value = sum(value)) %>%
      mutate(sector = "Tot Bld") %>%
      distinct()


    Shares_Urban <- L101.india_state_EB_EJ_state_Urban %>%
      left_join_error_no_match(L101.india_state_EB_EJ_state_TotBld, by = c("fuel","year")) %>%
      mutate(share = value.x/value.y)

    Shares_Rural <- L101.india_state_EB_EJ_state_Rural %>%
      left_join_error_no_match(L101.india_state_EB_EJ_state_TotBld, by = c("fuel","year")) %>%
      mutate(share = value.x/value.y)



    L142.in_EJ_India_Urban_Yh <- L142.in_EJ_R_bld_F_Yh %>%
      filter(GCAM_region_ID == gcam.INDIA_CODE, sector == "bld_resid") %>%
      group_by(fuel, year) %>%
      mutate(value = sum (value)) %>%
      select(-GCAM_region_ID) %>%
      distinct() %>%
      left_join_error_no_match(Shares_Urban, by = c("fuel","year")) %>%
      mutate(value = value * share) %>%
      mutate(sector = sector.x) %>%
      select(sector, fuel, year, value)


    L142.in_EJ_India_Rural_Yh <- L142.in_EJ_R_bld_F_Yh %>%
      filter(GCAM_region_ID == gcam.INDIA_CODE, sector == "bld_resid") %>%
      group_by(fuel, year) %>%
      mutate(value = sum (value)) %>%
      select(-GCAM_region_ID) %>%
      distinct() %>%
      left_join_error_no_match(Shares_Rural, by = c("fuel","year")) %>%
      mutate(value = value * share) %>%
      mutate(sector = sector.x) %>%
      select(sector, fuel, year, value)

    #Dividing into states
    L142.india_state_in_EJ_resid_rural_F <- L101.india_state_EB_EJ_state_S_F %>%
      filter(sector == "resid rural") %>%
      group_by(fuel,year) %>%
      mutate(value = value /sum(value)) %>%
      ungroup %>%
      rename (multiplier = value) %>%
      left_join_keep_first_only(L142.in_EJ_India_Rural_Yh, by = c("fuel", "year", "sector")) %>%
      mutate (value = value * multiplier) %>%
      select (-multiplier)

    L142.india_state_in_EJ_resid_urban_F <- L101.india_state_EB_EJ_state_S_F %>%
      filter(sector == "resid urban") %>%
      group_by(fuel,year) %>%
      mutate(value = value /sum(value)) %>%
      ungroup %>%
      rename (multiplier = value) %>%
      left_join_keep_first_only(L142.in_EJ_India_Urban_Yh, by = c("fuel", "year", "sector")) %>%
      mutate (value = value * multiplier) %>%
      select (-multiplier)

    # binding both rural and urban energy files

    L142.india_state_in_EJ_resid_F <- bind_rows(L142.india_state_in_EJ_resid_rural_F, L142.india_state_in_EJ_resid_urban_F)

    ## OUTPUTS
    L142.india_state_in_EJ_comm_F %>%
      add_title("Comm sector input energy by state and fuel") %>%
      add_units("EJ") %>%
      add_comments("Computed by apportioning india-level consumption among states") %>%
      add_legacy_name("L142.india_state_in_EJ_comm_F") %>%
      add_precursors("L101.india_state_EB_EJ_state_S_F",
                     "L142.in_EJ_R_bld_F_Yh") ->
      L142.india_state_in_EJ_comm_F

    L142.india_state_in_EJ_resid_F %>%
      add_title("Resid sector input energy by state and fuel") %>%
      add_units("EJ") %>%
      add_comments("Computed by apportioning india-level consumption among states") %>%
      add_legacy_name("L142.india_state_in_EJ_resid_F") %>%
      add_precursors("L101.india_state_EB_EJ_state_S_F",
                     "L142.in_EJ_R_bld_F_Yh") ->
      L142.india_state_in_EJ_resid_F


    return_data(L142.india_state_in_EJ_comm_F, L142.india_state_in_EJ_resid_F)
  } else {
    stop("Unknown command")
  }
}
