#' module_gcamindia_LA154.Transport
#'
#' Downscale transportation energy consumption and nonmotor data to the state level, generating three ouput tables.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L154.india_state_in_EJ_trn_India_F}, \code{L154.out_mpkm_state_india_trn_nonmotor_Yh}, \code{L154.in_EJ_state_trn_India_F}. The corresponding file in the
#' original data system was \code{LA154.Transport.R} (gcam-usa level1).
#' @details Transportation energy data was downscaled in proportion to GHG Platform state-level transportation energy data
#' @details Transportation nonmotor data was downscaled in proportion to state population
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author AM Nov20

module_gcamindia_LA154.Transport <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-india/trnIndia_GHG_mapping",
             FILE = "gcam-india/A10.EB_trn_SecFuel_mtoe",
             FILE = "gcam-india/A54.india_core_changed",
             "L154.out_mpkm_R_trn_nonmotor_Yh",
             "L100.Pop_thous_state_india",
             FILE = "gcam-india/A54.India_transport_structure_complete"))

  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L154.india_state_in_EJ_trn_India_F",
             "L154.in_EJ_state_trn_India_F",
             "L154.out_mpkm_state_india_trn_nonmotor_Yh"))

  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    trnIndia_GHG_mapping <- get_data(all_data, "gcam-india/trnIndia_GHG_mapping")
    A10.EB_trn_SecFuel_mtoe <- get_data(all_data, "gcam-india/A10.EB_trn_SecFuel_mtoe")
    A54.india_core_changed <- get_data(all_data, "gcam-india/A54.india_core_changed")
    L154.out_mpkm_R_trn_nonmotor_Yh <- get_data(all_data, "L154.out_mpkm_R_trn_nonmotor_Yh")
    L100.Pop_thous_state_india <- get_data(all_data, "L100.Pop_thous_state_india")
    A54.India_transport_structure_complete <- get_data(all_data, "gcam-india/A54.India_transport_structure_complete")

    # ===================================================

      # Silence package notes
      GCAM_region_ID <- UCD_sector <- mode <- size.class <- UCD_technology <- UCD_fuel <- fuel <- GHG_Platform_fuels <-
        year <- value <- GHG_Platform_sectors <- . <- fuel_sector <- state <- sector <- value_state <- value_national <-
        value_share <- value_mode <- NULL


    #Transport sector fuels to filter
    Transportation_Fuels <- c("Coal", "Electric", "Liquids", "NG", "FCEV", "BEV")

    #Modifying India level structure with rows
    Transportation_energy_consumption <- A54.india_core_changed %>%
        filter(year %in% HISTORICAL_YEARS, GCAM_region_ID == gcam.INDIA_CODE) %>%
      bind_rows(A54.India_transport_structure_complete) %>%
      mutate(size.class = "All") %>%
      filter(UCD_technology %in% Transportation_Fuels) %>%
      group_by(GCAM_region_ID, UCD_sector, mode, UCD_technology, UCD_fuel, fuel, year) %>%
      mutate(value = sum(value)) %>%
      arrange(mode) %>%
      ungroup() %>%
      unique() %>%
      left_join_error_no_match(trnIndia_GHG_mapping, by = c("fuel", "mode", "UCD_sector"))

    #Estimating State shares across sectors and fuels
    India_state_shares <- A10.EB_trn_SecFuel_mtoe %>%
      gather_years %>%
      filter(year %in% HISTORICAL_YEARS) %>%
      group_by(GHG_Platform_fuels, GHG_Platform_sectors, year) %>%
      mutate(share = value/sum(value)) %>%
      replace_na(list(value_share = 0))%>%
      select(State, GHG_Platform_fuels, GHG_Platform_sectors, year, share) %>%
      ungroup()

    India_state_shares[is.na(India_state_shares)] <- 0


      # Creating state level file for transport setcor and fuels
    L154.india_state_in_EJ_trn_India_F <- India_state_shares %>%
        left_join_keep_first_only(Transportation_energy_consumption, by = c("GHG_Platform_fuels", "GHG_Platform_sectors", "year")) %>%
        mutate(value = value * share) %>%
      select(State, UCD_sector, mode, size.class, UCD_technology, UCD_fuel, fuel, year, value)


      # Creating aggregate by fuel and name the sector output
      L154.in_EJ_state_trn_India_F <- L154.india_state_in_EJ_trn_India_F %>%
        group_by(State, fuel, year) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        mutate(sector = "transportation") %>% # Adding a column named "sector" with "transportation" as the entries
        select(State, sector, fuel, year, value)


      # Apportion non-motorized energy consumption to states on the basis of population
      L100.Pop_thous_state_india %>%
        group_by(year) %>%
        summarise(value_national = sum(value)) ->
        Pop_national

      L100.Pop_thous_state_india %>%
        left_join_error_no_match(Pop_national, by = "year") %>%
        mutate(value_share = value / value_national) %>% # Creating state share based on population
        select(state, year, value_share) ->
        Pop_state_share

      # Now we can use these shares to allocate the national data across the states
      L154.out_mpkm_R_trn_nonmotor_Yh %>%
        rename(value_mode = value) %>%
        filter(GCAM_region_ID == gcam.INDIA_CODE) %>%
        # Number of rows will change by adding states, so left_join_error_no_match cannot be used
        left_join(Pop_state_share, by = "year") %>%
        mutate(value = value_mode * value_share) %>% # Apportioning across the modes using the share data
        filter(year %in% HISTORICAL_YEARS) %>% # Ensuring within historical period
        select(state, mode, year, value) %>%
        mutate(year = as.integer(year)) ->
        L154.out_mpkm_state_india_trn_nonmotor_Yh


    # ===================================================

      L154.india_state_in_EJ_trn_India_F %>%
      add_title("Transportation energy consumption by state") %>%
      add_units("EJ") %>%
      add_comments("Transportation energy consumption data was downscaled to the state level using GHG Platform state energy data") %>%
      add_legacy_name("L154.india_state_in_EJ_trn_India_F") %>%
      add_precursors("gcam-india/A54.india_core_changed", "gcam-india/trnIndia_GHG_mapping", "gcam-india/A10.EB_trn_SecFuel_mtoe",
                     "gcam-india/A54.India_transport_structure_complete") ->
        L154.india_state_in_EJ_trn_India_F


      L154.out_mpkm_state_india_trn_nonmotor_Yh %>%
      add_title("Transportation non-motorized travel by mode and state") %>%
      add_units("million person-km") %>%
      add_comments("National data was allocated across the states in proportion to population") %>%
      add_legacy_name("L154.out_mpkm_state_india_trn_nonmotor_Yh") %>%
      add_precursors("L154.out_mpkm_R_trn_nonmotor_Yh", "L100.Pop_thous_state_india")  ->
        L154.out_mpkm_state_india_trn_nonmotor_Yh

      L154.in_EJ_state_trn_India_F %>%
      add_title("Transportation energy consumption by state and fuel") %>%
      add_units("EJ") %>%
      add_comments("Transportation energy consumption was aggregated by fuel, and the sector was named transportation") %>%
      add_legacy_name("L154.in_EJ_state_trn_India_F") %>%
      add_precursors("gcam-india/A54.india_core_changed", "gcam-india/trnIndia_GHG_mapping", "gcam-india/A10.EB_trn_SecFuel_mtoe") ->
        L154.in_EJ_state_trn_India_F

    return_data(L154.india_state_in_EJ_trn_India_F, L154.out_mpkm_state_india_trn_nonmotor_Yh, L154.in_EJ_state_trn_India_F)
  } else {
    stop("Unknown command")
  }
}
