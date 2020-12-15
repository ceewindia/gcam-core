#' module_gcamindia_LA122.refining
#'
#' Downscales crude oil refining inputs and outputs to state-level.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L122.india_state_in_EJ_refining_F}, \code{L122.india_state_out_EJ_refining_F}. The corresponding file in the
#' original data system was \code{LA122.Refining.R} (gcam-usa level1).
#' @details Downscales crude oil refining inputs and outputs to state-level.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author AM Nov20
module_gcamindia_LA122.refining <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L122.in_EJ_R_refining_F_Yh",
             "L122.out_EJ_R_refining_F_Yh",
             "L101.india_state_EB_EJ_state_S_F",
             FILE = "gcam-india/A22.india_state_biofuel"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L122.india_state_in_EJ_refining_F",
             "L122.india_state_out_EJ_refining_F"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Silence package checks
    GCAM_region_ID <- sector <- fuel <- year <- value <- value.x <- value.y <- state <-
      fuel.x <- Mgal.yr <- pct <- NULL

    # Load required inputs
    L122.in_EJ_R_refining_F_Yh        <- get_data(all_data, "L122.in_EJ_R_refining_F_Yh") %>%
      filter(GCAM_region_ID == gcam.INDIA_CODE)

    L122.out_EJ_R_refining_F_Yh       <- get_data(all_data, "L122.out_EJ_R_refining_F_Yh") %>%
      filter(GCAM_region_ID == gcam.INDIA_CODE)

    L101.india_state_EB_EJ_state_S_F        <- get_data(all_data, "L101.india_state_EB_EJ_state_S_F")

    A22.india_state_biofuel <- get_data(all_data, "gcam-india/A22.india_state_biofuel")

        # ===================================================
    # CRUDE OIL REFINING
    # NOTE: using crude oil input to industry as basis for allocation of crude oil refining to states
    # Crude oil consumption by industry is the energy used at refineries (input - output)

    # Step 1: Calculate the percentages of oil consumption in each state
    L122.india_state_pct_cor <- L101.india_state_EB_EJ_state_S_F %>%
      filter(sector == "Petrochem Refinery",
             fuel == "crude oil") %>%
      mutate(sector = "oil refining") %>%
      group_by(year) %>%
      # Percentage share of each state in a year for crude oil consumption by oil refining sector
      mutate(value = value / sum(value)) %>%
      ungroup()

    # Crude oil refining output by state
    # Apportion the national total to the states
    L122.india_state_out_EJ_cor <- L122.india_state_pct_cor %>%
      left_join_error_no_match(L122.out_EJ_R_refining_F_Yh, by = c("sector", "year")) %>%
      # State output value = state proportion * national output value
      mutate(value = value.x * value.y) %>%
      select(state, sector, fuel = fuel.x, year, value)

    # Inputs to crude oil refining - same method of portional allocations, but with multiple fuels
    # Oil refining input fuels
    oil_input_fuels <- L122.in_EJ_R_refining_F_Yh %>%
      filter(sector == "oil refining") %>%
      select(fuel) %>%
      distinct()

    # Repeat state proportions for all fuels in oil refining sector
    L122.india_state_pct_cor_repF <- L122.india_state_pct_cor %>%
      select(-fuel) %>%
      repeat_add_columns(oil_input_fuels)

    # Calculate state oil input values
    L122.india_state_in_EJ_cor_F <- L122.india_state_pct_cor_repF %>%
      left_join_error_no_match(L122.in_EJ_R_refining_F_Yh, by = c("sector", "fuel", "year")) %>%
      # State input value = state proportion * national input value
      mutate(value = value.x * value.y) %>%
      select(state, sector, fuel, year, value)

    # BIOMASS LIQUIDS
    # NOTE: using SEDS biofuel transformation-related losses to disaggregate ethanol production to states

    # Calculate the percentages of corn ethanol consumption in each state
    L122.india_pct_state_btle <- L101.india_state_EB_EJ_state_S_F %>%
      filter(sector == "corn ethanol") %>%
      group_by(year) %>%
      mutate(value = value / sum(value)) %>%
      ungroup() %>%
      replace_na(list(value = 0))

    # Corn ethanol output by state
    L122.india_out_EJ_state_btle <- L122.india_pct_state_btle %>%
      left_join_error_no_match(L122.out_EJ_R_refining_F_Yh, by = c("sector", "year")) %>%
      # State output value = state proportion * national output value
      mutate(value = value.x * value.y) %>%
      select(state, sector, fuel = fuel.x, year, value)

    # Corn ethanol inputs by state and fuel: Repeat percentage-wise table by number of fuel inputs
    # Corn ethanol input fuels
    india_corneth_input_fuels <- L122.in_EJ_R_refining_F_Yh %>%
      filter(sector == "corn ethanol") %>%
      select(fuel) %>%
      distinct()

    # Repeat state proportions for all fuels used in corn ethanol sector
    L122.india_pct_state_btle_repF <- L122.india_pct_state_btle %>%
      select(-fuel) %>%
      repeat_add_columns(india_corneth_input_fuels)

    # Corn ethanol inputs by state
    L122.india_in_EJ_state_btle_F <- L122.india_pct_state_btle_repF %>%
      left_join_error_no_match(L122.in_EJ_R_refining_F_Yh, by = c("sector", "fuel", "year")) %>%
      # State input value = state proportion * national input value
      mutate(value = value.x * value.y) %>%
      select(state, sector, fuel, year, value)

    # Biodiesel output by state
    # NOTE: SEDS does not cover biodiesel; using a separate EIA database for disaggregating this to states

    # Build table of percentages by historical year
    A22.india_state_biofuel <- A22.india_state_biofuel %>%
      transmute(pct = Mgal.yr / sum(Mgal.yr), state)

    # Joining EIA_biodiesel_Mgal.yr to all states and years
    L122.india_pct_state_btlbd <- tidyr::crossing(state = gcamindia.STATES,
                                            year = HISTORICAL_YEARS) %>%
      mutate(sector = "biodiesel",
             fuel = "biomass oil") %>%
      # Using left_join because not all states in EIA_biodiesel_Mgal.yr
      left_join(A22.india_state_biofuel, by = "state") %>%
      replace_na(list(pct = 0))

    # Apportion to the states
    L122.india_out_EJ_state_btlbd <- L122.india_pct_state_btlbd %>%
      left_join_error_no_match(L122.out_EJ_R_refining_F_Yh, by = c("sector", "year")) %>%
      # State output value = state proportion * national output value
      mutate(value = pct * value) %>%
      select(state, sector, fuel = fuel.x, year, value)

    # Biodiesel inputs by state and fuel
    # Biodiesel input fuels
    india_biodiesel_input_fuels <- L122.in_EJ_R_refining_F_Yh %>%
      filter(sector == "biodiesel") %>%
      select(fuel) %>%
      distinct()

    # Repeat state proportions for all fuels used in biodiesel sector
    L122.india_pct_state_btlbd_repF <- L122.india_pct_state_btlbd %>%
      select(-fuel) %>%
      repeat_add_columns(india_biodiesel_input_fuels)

    # Biodiesel inputs by state
    L122.india_in_EJ_state_btlbd_F <- L122.india_pct_state_btlbd_repF %>%
      left_join_error_no_match(L122.in_EJ_R_refining_F_Yh, by = c("sector", "fuel", "year")) %>%
      # State input value = state proportion * national input value
      mutate(value = pct * value) %>%
      select(state, sector, fuel, year, value)


    # Bind the tables of inputs and outputs of all refineries by state in the base years
    L122.india_state_in_EJ_refining_F <- bind_rows(L122.india_state_in_EJ_cor_F, L122.india_in_EJ_state_btle_F, L122.india_in_EJ_state_btlbd_F)

    L122.india_state_out_EJ_refining_F <- bind_rows(L122.india_state_out_EJ_cor, L122.india_out_EJ_state_btle, L122.india_out_EJ_state_btlbd)


    # ===================================================
    # Produce outputs
    L122.india_state_in_EJ_refining_F %>%
      add_title("Refinery energy inputs by state, sector, and fuel") %>%
      add_units("EJ") %>%
      add_comments("Crude oil input values apportioned to states") %>%
      add_legacy_name("L122.india_state_in_EJ_refining_F") %>%
      add_precursors("L101.india_state_EB_EJ_state_S_F",
                     "L122.in_EJ_R_refining_F_Yh",
                     "gcam-india/A22.india_state_biofuel") ->
      L122.india_state_in_EJ_refining_F

    L122.india_state_out_EJ_refining_F %>%
      add_title("Refinery output by state and sector") %>%
      add_units("EJ") %>%
      add_comments("Crude oil output values apportioned to states") %>%
      add_legacy_name("L122.india_state_out_EJ_refining_F") %>%
      add_precursors("L101.india_state_EB_EJ_state_S_F",
                     "L122.out_EJ_R_refining_F_Yh",
                     "gcam-india/A22.india_state_biofuel") ->
      L122.india_state_out_EJ_refining_F

    return_data(L122.india_state_in_EJ_refining_F, L122.india_state_out_EJ_refining_F)
  } else {
    stop("Unknown command")
  }
}
