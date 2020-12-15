#' module_gcamindia_L222.en_transformation
#'
#' Prepare the assumptions and calibrated outputs for energy transformation supplysectors, subsectors, and technologies specific to india sectors and/or states.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L222.india_state_DeleteStubTech_en}, \code{L222.india_state_PassThroughSector_en}, \code{L222.india_state_Tech_en}, \code{L222.india_state_TechShrwt_en}, \code{L222.india_state_TechInterp_en}, \code{L222.india_state_TechShrwt_en}, \code{L222.india_state_TechCoef_en}, \code{L222.india_state_Production_refining}, \code{L222.SectorLogitTables_india[[ curr_table ]]$data}, \code{L222.india_state_Supplysector_en}, \code{L222.india_state_SubsectorShrwtFllt_en}, \code{L222.india_state_StubTechProd_refining}, \code{L222.india_state_StubTechMarket_en}, \code{L222.india_state_CarbonCoef_en}. The corresponding file in the
#' original data system was \code{L222.en_transformation_india.R} (gcam-india level2).
#' @details This chunk sets up the india energy transformation technology databases as well as writing out assumptions to all states/sectors/markets for shareweights and logits.
#' Calibrated outputs and I:O coefficients are updated from global values produced by \code{\link{module_energy_L222.en_transformation}}.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author VC Nov20
module_gcamindia_L222.en_transformation <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "gcam-india/india_states_subregions",
             FILE = "gcam-india/A22.india_state_calibrated_techs_transformation",
             FILE = "gcam-india/A23.india_state_refining_feedstock_prod",
             "L222.Supplysector_en",
             "L222.SubsectorLogit_en",
             "L222.StubTech_en",
             "L222.StubTechCoef_refining",
             "L222.GlobalTechInterp_en",
             "L222.GlobalTechCoef_en",
             "L222.GlobalTechCost_en",
             "L222.GlobalTechShrwt_en",
             "L222.GlobalTechCapture_en",
             "L222.GlobalTechSCurve_en",
             "L122.india_state_out_EJ_refining_F",
             "L202.CarbonCoef"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L222.india_state_DeleteStubTech_en",
             "L222.india_state_PassThroughSector_en",
             "L222.india_state_Tech_en",
             "L222.india_state_TechShrwt_en",
             "L222.india_state_TechInterp_en",
             "L222.india_state_TechCoef_en",
             "L222.india_state_Production_refining",
             "L222.india_state_Supplysector_en",
             "L222.india_state_SubsectorShrwtFllt_en",
             "L222.india_state_StubTechProd_refining",
             "L222.india_state_StubTechMarket_en",
             "L222.india_state_CarbonCoef_en",
             "L222.india_state_GlobalTechSCurve_en",
             "L222.india_state_GlobalTechCost_en",
             "L222.india_state_SubsectorLogit_en",
             "L222.india_state_StubTech_en",
             "L222.india_state_StubTechCoef_refining",
             "L222.india_state_GlobalTechInterp_en",
             "L222.india_state_GlobalTechCoef_en",
             "L222.india_state_GlobalTechShrwt_en",
             "L222.india_state_GlobalTechCapture_en"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    india_states_subregions           <- get_data(all_data, "gcam-india/india_states_subregions")
    A22.india_state_calibrated_techs_transformation   <- get_data(all_data, "gcam-india/A22.india_state_calibrated_techs_transformation")
    A23.india_state_refining_feedstock_prod <- get_data(all_data, "gcam-india/A23.india_state_refining_feedstock_prod")
    L222.Supplysector_en              <- get_data(all_data, "L222.Supplysector_en")
    L222.SubsectorLogit_en            <- get_data(all_data, "L222.SubsectorLogit_en")
    L222.StubTech_en                  <- get_data(all_data, "L222.StubTech_en")
    L222.StubTechCoef_refining        <- get_data(all_data, "L222.StubTechCoef_refining")
    L222.GlobalTechInterp_en          <- get_data(all_data, "L222.GlobalTechInterp_en")
    L222.GlobalTechCoef_en            <- get_data(all_data, "L222.GlobalTechCoef_en")
    L222.GlobalTechCost_en            <- get_data(all_data, "L222.GlobalTechCost_en")
    L222.GlobalTechShrwt_en           <- get_data(all_data, "L222.GlobalTechShrwt_en")
    L222.GlobalTechCapture_en         <- get_data(all_data, "L222.GlobalTechCapture_en")
    L222.GlobalTechSCurve_en          <- get_data(all_data, "L222.GlobalTechSCurve_en")
    L122.india_state_out_EJ_refining_F<- get_data(all_data, "L122.india_state_out_EJ_refining_F")
    L202.CarbonCoef                   <- get_data(all_data, "L202.CarbonCoef")

    # silence check package notes
    logit.year.fillout <- year <- from.year <- to.year <- region <- supplysector <- subsector <-
      technology <- sector.name <- subsector.name <- sector <- state <- fuel <- value <- market.name <-
      trash <- calOutputValue <- minicam.energy.input <- supplysector.x <- supplysector.y <-
      calibration <- grid_region <- stub.technology <- key <- coal <- natural_gas <- coal_fract <-
      share.weight <- gas_fract <- NULL

    # Correct some of the inputs
    L222.Supplysector_en %>%
      mutate(logit.year.fillout = as.integer(logit.year.fillout)) -> # was character
      L222.Supplysector_en

    L222.SubsectorLogit_en  %>%
      mutate(logit.year.fillout = as.integer(logit.year.fillout)) -> # was character
      L222.SubsectorLogit_en

    L222.StubTechCoef_refining %>%
      mutate(year = as.integer(year)) -> # was double
      L222.StubTechCoef_refining

    L222.GlobalTechInterp_en %>%
      mutate(from.year = as.integer(from.year), # was character
             to.year = as.integer(to.year)) ->
      L222.GlobalTechInterp_en

    L222.GlobalTechCoef_en %>%
      mutate(year  = as.integer(year)) -> # was character
      L222.GlobalTechCoef_en

    L222.GlobalTechCost_en %>%
      mutate(year = as.integer(year)) -> # was double
      L222.GlobalTechCost_en

    L222.GlobalTechCapture_en %>%
      mutate(year = as.integer(year)) -> # was character
      L222.GlobalTechCapture_en

    L222.GlobalTechSCurve_en %>%
      mutate(year = as.integer(year)) -> # was character
      L222.GlobalTechSCurve_en


    # Some helpful functions:
    #
    # global_energy_to_india_nonGlobalTech - takes global energy inputs for non global tech
    # from L222.en_transformation.R and processes for use in india
    global_energy_to_india_nonGlobalTech <- function(data) {
      data %>%
        filter(region == gcam.india_REGION,
               supplysector %in% gcamindia.SECTOR_EN_NAMES) %>%
        write_to_all_india_states(names = c(names(data), "region")) %>%
        filter((subsector == "oil refining" & region %in% oil_refining_states) |
                 subsector != "oil refining") %>%
        mutate(supplysector = subsector)
    } # global_energy_to_india_nonGlobalTech

    # global_energy_to_india_GlobalTech - takes global energy inputs for global tech
    # from L222.en_transformation.R and processes for use in india
    global_energy_to_india_GlobalTech <- function(data) {
      data %>%
        filter(sector.name %in% gcamindia.SECTOR_EN_NAMES) %>%
        mutate(sector.name = subsector.name)
    } # global_energy_to_india_GlobalTech

    # Oil refining sectors are only created in states where the production is > 0 in the historical period.
    # Collect these states. Other techs are available everywhere
    L122.india_state_out_EJ_refining_F %>%
      filter(sector == "oil refining",
             year %in% HISTORICAL_YEARS) %>%
      group_by(state, sector, fuel) %>%
      summarise(value = sum(value)) %>%
      ungroup %>%
      filter(value > 0) %>%
      pull(state) ->
      oil_refining_states

    # L222.india_state_DeleteStubTech_en: remove existing stub technologies in the india region.
    # The supplysector and subsector structure in the sectors defined in gcamindia.SECTOR_EN_NAMES are retained
    L222.StubTech_en %>%
      filter(region == gcam.india_REGION,
             supplysector %in% gcamindia.SECTOR_EN_NAMES) ->
      L222.india_state_DeleteStubTech_en

    # L222.india_state_Tech_en: Just the technology pass-throughs used to set the proper node name, india region
    L222.SubsectorLogit_en %>%
      select(region, supplysector, subsector) %>%
      filter(region == gcam.india_REGION,
             supplysector %in% gcamindia.SECTOR_EN_NAMES) %>%
      repeat_add_columns(tibble(state = gcamindia.STATES)) %>%
      filter((subsector == "oil refining" & state %in% oil_refining_states) |
               subsector != "oil refining") %>%
      mutate(technology = paste(state, subsector, sep = gcamindia.STATE_SUBSECTOR_DELIMITER)) ->
      L222.india_state_Tech_en

    # save some of this information for the PassThroughSector information
    # L222.india_state_PassThroughSector_en: PassThroughSector information to send vintaging info from states to india.
    L222.india_state_Tech_en %>%
      select(state, subsector, supplysector, region) %>%
      rename(marginal.revenue.market = region,
             region = state,
             pass.through.sector = subsector,
             marginal.revenue.sector = supplysector) ->
      L222.india_state_PassThroughSector_en

    # select only relevant columns for L222.india_state_Tech_en, particularly dropping state
    L222.india_state_Tech_en %>%
      select(one_of(LEVEL2_DATA_NAMES[["Tech"]])) ->
      L222.india_state_Tech_en

    # L222.india_state_TechInterp_en: technology shareweights, india region
    # Technology interpolation only applies to calibrated technologies.
    # For biomass liquids, allow state shares to shift over time
    # (future techs are different than present techs).
    # Oil refining and biomass liquids shareweights are fixed at calibration values through max model year
    L222.india_state_Tech_en %>%
      filter(subsector %in% c("oil refining", "biomass liquids")) %>%
      mutate(apply.to = "share-weight",
             from.year = max(MODEL_BASE_YEARS),
             to.year = max(MODEL_YEARS),
             interpolation.function = "fixed") ->
      L222.india_state_TechInterp_en

    # L222.india_state_TechShrwt_en: technology shareweights in each year, india region
    L222.india_state_Tech_en %>%
      repeat_add_columns(tibble::tibble(year = MODEL_YEARS)) %>%
      # Split the state names out and because the refining tech names have spaces drop the extra
      separate(technology, c("state"), sep = " ", remove = F, extra = "drop") %>%
      left_join_error_no_match(A23.india_state_refining_feedstock_prod, by = c("state")) %>%
      mutate(coal_fract = coal / max(coal), gas_fract = natural_gas / max(natural_gas),
             share.weight = gcamindia.DEFAULT_SHAREWEIGHT,
             # Scaling coal to liquids and gas to liquids shareweights to 2015 resource production levels
             share.weight = if_else(grepl("coal", subsector), coal_fract, share.weight),
             share.weight = if_else(grepl("gas", subsector), gas_fract, share.weight),
             # Default the base year shareweights to 0. This will be over-ridden in calibration,
             share.weight = if_else(year %in% MODEL_BASE_YEARS, 0, round(share.weight, energy.DIGITS_SHRWT))) %>%
      select(region, supplysector, subsector, technology, year, share.weight) -> L222.india_state_TechShrwt_en


    # L222.india_state_TechCoef_en: technology coefficients and market names, india region
    L222.india_state_TechShrwt_en %>%
      select(one_of(LEVEL2_DATA_NAMES[["TechYr"]])) %>%
      mutate(minicam.energy.input = subsector,
             coefficient = gcamindia.DEFAULT_COEFFICIENT,
             market.name = technology) %>%
      separate(market.name, c("market.name", "trash"), extra = "merge", sep = gcamindia.STATE_SUBSECTOR_DELIMITER) %>%
      select(-trash) ->
      L222.india_state_TechCoef_en

    # L222.india_state_Production_refining: calibrated refinery production in india (consuming output of states)
    # Aggregated to the supplysector/subsector/technology level
      L122.india_state_out_EJ_refining_F %>%
        filter(year %in% MODEL_BASE_YEARS) %>%
        rename(calOutputValue = value) %>%
        mutate(calOutputvalue = round(calOutputValue, gcamindia.DIGITS_CALOUTPUT),
               region = gcam.india_REGION) %>%
        left_join_error_no_match(distinct(select(A22.india_state_calibrated_techs_transformation, sector, supplysector, subsector)), by = "sector") %>%
        mutate(technology = paste(state, subsector, sep = gcamindia.STATE_SUBSECTOR_DELIMITER),
               minicam.energy.input = subsector) %>%
        filter((subsector == "oil refining" & state %in% oil_refining_states) |
                 subsector != "oil refining") %>%
        # Aggregate
        group_by(region, supplysector, subsector, technology, minicam.energy.input, year) %>%
        summarise(calOutputValue = sum(calOutputValue)) %>%
        ungroup %>%
        mutate(share.weight.year = year) %>%
        set_subsector_shrwt %>%
        # The following line is equivalent to (but slightly faster than): mutate(tech.share.weight = if_else(calOutputValue == 0, 0, 1)) %>%
        mutate(tech.share.weight = abs(sign(calOutputValue))) %>%
        select(one_of(LEVEL2_DATA_NAMES[["Production"]])) ->
        L222.india_state_Production_refining

      # Process energy files from L222.en_transformation.R for use in the india,
      # slightly differently processing for global tech vs not inputs
      L222.india_state_SubsectorLogit_en      <- global_energy_to_india_nonGlobalTech(L222.SubsectorLogit_en)
      L222.india_state_StubTech_en            <- global_energy_to_india_nonGlobalTech(L222.StubTech_en)
      L222.india_state_StubTechCoef_refining  <- global_energy_to_india_nonGlobalTech(L222.StubTechCoef_refining)
      L222.india_state_GlobalTechInterp_en    <- global_energy_to_india_GlobalTech(L222.GlobalTechInterp_en)
      L222.india_state_GlobalTechCoef_en      <- global_energy_to_india_GlobalTech(L222.GlobalTechCoef_en)
      L222.india_state_GlobalTechCost_en      <- global_energy_to_india_GlobalTech(L222.GlobalTechCost_en)
      L222.india_state_GlobalTechShrwt_en     <- global_energy_to_india_GlobalTech(L222.GlobalTechShrwt_en)
      L222.india_state_GlobalTechCapture_en   <- global_energy_to_india_GlobalTech(L222.GlobalTechCapture_en)
      L222.india_state_GlobalTechSCurve_en    <- global_energy_to_india_GlobalTech(L222.GlobalTechSCurve_en)

      ### The same processing for optional/currently NULL inputs
      # L222.GlobalTechShutdownProfit_en_india  <- global_energy_to_india_GlobalTech(L222.GlobalTechShutdownProfit_en)
      # L222.GlobalTechShutdown_en_india        <- global_energy_to_india_GlobalTech(L222.GlobalTechShutdown_en)
      # L222.GlobalTechSCurveProfit_en_india    <- global_energy_to_india_GlobalTech(L222.GlobalTechSCurveProfit_en)
      # L222.GlobalTechLifetimeProfit_en_india  <- global_energy_to_india_GlobalTech(L222.GlobalTechLifetimeProfit_en)
      # L222.GlobalTechLifetime_en_india        <- global_energy_to_india_GlobalTech(L222.GlobalTechLifetime_en)

      # TODO: figure out a better strategy.  We need to have at least one technology be available in the final
      # calibration year so we can get a base cost for the absolute cost logit.  Having a share weight of zero
      # at the subsector is sufficient then to ensure we get no production in the calibration years
      L222.india_state_GlobalTechShrwt_en %>%
        mutate(share.weight = if_else(technology == "coal to liquids" & year == max(MODEL_BASE_YEARS), 1.0, share.weight),
               share.weight = if_else(technology == "gas to liquids" & year == max(MODEL_BASE_YEARS), 1.0, share.weight)) ->
        L222.india_state_GlobalTechShrwt_en

      # L222.india_state_Supplysector_en: Supplysector information, replace name of supplysector with the subsector names
      L222.india_state_SubsectorLogit_en %>%
        select(one_of(LEVEL2_DATA_NAMES[["Subsector"]])) %>%
        left_join_error_no_match(distinct(select(L222.SubsectorLogit_en, supplysector, subsector)),
                                 by = "subsector") %>%
        rename(supplysector = supplysector.x,
               old_supplysector = supplysector.y) %>%
        left_join_error_no_match(distinct(select(L222.Supplysector_en, -region)),
                                 by = c("old_supplysector" = "supplysector")) %>%
        select(one_of(LEVEL2_DATA_NAMES[["Supplysector"]])) ->
        L222.india_state_Supplysector_en

      # L222.india_state_Supplysector_en_logit.type - Note there is no competition here so just use the default logit type
      L222.india_state_Supplysector_en %>%
        mutate(logit.type = gcamindia.DEFAULT_LOGIT_TYPE) ->
        L222.india_state_Supplysector_en_logit.type

      # L222.india_state_SubsectorShrwtFllt_en: Subsector shareweights, there is no competition here, so just fill out with 1s
      # (will be over-ridden by base year calibration where necessary)
      L222.india_state_SubsectorLogit_en %>%
        select(one_of(LEVEL2_DATA_NAMES[["Subsector"]])) %>%
        mutate(year = min(MODEL_YEARS),
               share.weight = gcamindia.DEFAULT_SHAREWEIGHT) ->
        L222.india_state_SubsectorShrwtFllt_en

      # L222.india_state_StubTechProd_refining: calibrated fuel production by state.
      # Only take the tech IDs where the calibration is identified as output.
      #
      # Step 1, process the table of calibrated_techs to only include calibration=output and relevant columns
      A22.india_state_calibrated_techs_transformation %>%
        filter(calibration == "output") %>%
        select(sector, supplysector, subsector, technology) %>%
        distinct ->
        L222.india_state_calibrated_techs_tmp

      # Step 2, process L122.india_state_out_EJ_refining_F, joining the processed table of calibrated_techs from step 1,
      # to create L222.india_state_StubTechProd_refining. Note the supplysector is the same as the subsector within the states.
      L122.india_state_out_EJ_refining_F %>%
        filter(year %in% MODEL_BASE_YEARS) %>%
        rename(region = state,
               calOutputValue = value) %>%
        mutate(calOutputValue = round(calOutputValue, gcamindia.DIGITS_CALOUTPUT)) %>%
        left_join_error_no_match(L222.india_state_calibrated_techs_tmp, by = "sector") %>%
        mutate(supplysector = subsector,
               stub.technology = technology,
               share.weight.year = year) %>%
        set_subsector_shrwt() %>%
        mutate(tech.share.weight = if_else(calOutputValue > 0, 1, 0)) %>%
        select(one_of(LEVEL2_DATA_NAMES[["StubTechProd"]])) %>%
        filter((subsector == "oil refining" & region %in% oil_refining_states) |
                 subsector != "oil refining") ->
        L222.india_state_StubTechProd_refining

      # L222.india_state_StubTechMarket_en: market names of inputs to state refining sectors
      L222.india_state_GlobalTechCoef_en %>%
        select(one_of(LEVEL2_DATA_NAMES[["GlobalTechInput"]])) %>%
        write_to_all_india_states(names = c(LEVEL2_DATA_NAMES[["GlobalTechInput"]], "region")) %>%
        rename(supplysector = sector.name,
               subsector = subsector.name,
               stub.technology = technology) %>%
        mutate(market.name = gcam.india_REGION) %>%
        # switch designated fuel market names to the regional markets
        left_join_error_no_match(india_states_subregions %>%
                                   select(state, grid_region),
                                 by = c("region" = "state")) %>%
        mutate(market.name = if_else(minicam.energy.input %in% gcamindia.REGIONAL_FUEL_MARKETS,
                                     grid_region, market.name)) %>%
        select(-grid_region) -> L222.india_state_StubTechMarket_en

      # Finish L222.india_state_StubTechMarket_en by Setting electricity to the state markets
      L222.india_state_StubTechMarket_en %>%
        filter(minicam.energy.input %in% gcamindia.ELECT_TD_SECTORS) %>%
        mutate(market.name = region) ->
        tmp

      # create a key for filtering
      L222.india_state_StubTech_en %>%
        select(supplysector, subsector, stub.technology) %>%
        unite(key, supplysector, subsector, stub.technology, sep = "~") %>%
        distinct ->
        L222.india_state_StubTech_en_key

      L222.india_state_StubTechMarket_en %>%
        filter(!(minicam.energy.input %in% gcamindia.ELECT_TD_SECTORS)) %>%
        bind_rows(tmp) %>%
        select(one_of(LEVEL2_DATA_NAMES[["StubTechMarket"]])) %>%
        unite(key, supplysector, subsector, stub.technology, sep = "~") %>%
        filter(key %in% L222.india_state_StubTech_en_key$key) %>%
        separate(key, c("supplysector", "subsector", "stub.technology"), sep = "~") %>%
        filter((subsector == "oil refining" & region %in% oil_refining_states) |
                 subsector != "oil refining") ->
        L222.india_state_StubTechMarket_en

      # L222.india_state_CarbonCoef_en: energy carbon coefficients in india
      #
      # Step 1, process L202.CarbonCoef for joining
      L202.CarbonCoef %>%
        filter(region == gcam.india_REGION) %>%
        select(-region) %>%
        distinct ->
        L202.CarbonCoef_tmp

      # Step 2, create L222.india_state_CarbonCoef_en by joining the table from step 1.
      L222.india_state_Supplysector_en %>%
        select(region, supplysector) %>%
        distinct %>%
        left_join_error_no_match(distinct(select(L222.india_state_TechShrwt_en, subsector, supplysector)),
                                 by = c("supplysector" = "subsector")) %>%
        left_join_error_no_match(L202.CarbonCoef_tmp, by =  c("supplysector.y" = "PrimaryFuelCO2Coef.name")) %>%
        select(-supplysector.y) %>%
        rename(PrimaryFuelCO2Coef.name = supplysector) ->
        L222.india_state_CarbonCoef_en

    # Produce outputs
    L222.india_state_DeleteStubTech_en %>%
      mutate(region = region) %>%  # strip off attributes so we can re-write title, etc.
      add_title("Removes existing stub technologies in the india region") %>%
      add_units("NA") %>%
      add_comments("Removes existing stub technologies in the india region from L222.StubTech_en.") %>%
      add_comments("The supplysector and subsector structure in the sectors defined in gcamindia.SECTOR_EN_NAMES are retained")  %>%
      add_legacy_name("L222.india_state_DeleteStubTech_en") %>%
      add_precursors("L222.StubTech_en") ->
      L222.india_state_DeleteStubTech_en

    L222.india_state_PassThroughSector_en %>%
      add_title("PassThroughSector information to send vintaging info from states to india") %>%
      add_units("NA") %>%
      add_comments("state, subsector, supplysector, and region fromj L222.india_state_Tech_en is renamed.") %>%
      add_legacy_name("L222.india_state_PassThroughSector_en") %>%
      same_precursors_as(L222.india_state_Tech_en) ->
      L222.india_state_PassThroughSector_en

    L222.india_state_Tech_en %>%
      add_title("The technology pass-throughs used to set the proper node name, india region.") %>%
      add_units("units") %>%
      add_comments("india supplysector and subsector information from L222.SubsectorLogit_en is") %>%
      add_comments("repeated for all US states and updated.") %>%
      add_legacy_name("L222.india_state_Tech_en") %>%
      add_precursors("L222.SubsectorLogit_en",
                     "gcam-india/A23.india_state_refining_feedstock_prod") ->
      L222.india_state_Tech_en

    L222.india_state_TechShrwt_en %>%
      add_title("Technology shareweights in each year, india region") %>%
      add_units("NA") %>%
      add_comments("L222.india_state_Tech_en is repeated for model base year and future years and shareweights of 0 and") %>%
      add_comments("1 are added for each, respectively. Overwritten in calibration.") %>%
      add_legacy_name("L222.india_state_TechShrwt_en") %>%
      same_precursors_as(L222.india_state_Tech_en) ->
      L222.india_state_TechShrwt_en

    L222.india_state_TechInterp_en %>%
      add_title("Technology shareweights, india region") %>%
      add_units("NA") %>%
      add_comments("Technology interpolation only applies to calibrated technologies.For biomass liquids, ") %>%
      add_comments("allow state shares to shift over time since future techs are different than present techs.") %>%
      add_legacy_name("L222.india_state_TechInterp_en")  %>%
      same_precursors_as(L222.india_state_Tech_en) ->
      L222.india_state_TechInterp_en

    L222.india_state_TechCoef_en %>%
      add_title("Technology coefficients and market names, india region") %>%
      add_units("units") %>%
      add_comments("Data from L222.india_state_TechShrwt_en is renamed and filled out.") %>%
      add_legacy_name("L222.india_state_TechCoef_en") %>%
      same_precursors_as(L222.india_state_TechShrwt_en) ->
      L222.india_state_TechCoef_en

    L222.india_state_Production_refining %>%
      add_title("Calibrated refinery production in india (consuming output of states)") %>%
      add_units("NA") %>%
      add_comments("L122.india_state_out_EJ_refining_F is aggregated to the supplysector/subsector/technology level.") %>%
      add_legacy_name("L222.india_state_Production_refining") %>%
      add_precursors("gcam-india/A22.india_state_calibrated_techs_transformation",
                      "L122.india_state_out_EJ_refining_F") ->
      L222.india_state_Production_refining

    L222.india_state_Supplysector_en_logit.type %>%
      add_title("Supplysector information, replace name of supplysector with the subsector names") %>%
      add_units("Varies") %>%
      add_comments("L222.Supplysector_en and L222.SubsectorLogit_en is repeated and filtered for use in india states.") %>%
      add_comments("can be multiple lines") %>%
      add_legacy_name("L222.india_state_Supplysector_en") %>%
      add_precursors("L222.Supplysector_en",
                     "L222.SubsectorLogit_en") ->
      L222.india_state_Supplysector_en

    L222.india_state_SubsectorShrwtFllt_en %>%
      add_title("Subsector shareweights for energy in india") %>%
      add_units("NA") %>%
      add_comments("india energy subsector shareweights. There is no competition here, so shareweights are defaulted to 1.") %>%
      add_comments("Shareweights will be over-ridden by base year calibration.") %>%
      add_legacy_name("L222.india_state_SubsectorShrwtFllt_en") %>%
      same_precursors_as(L222.india_state_SubsectorLogit_en) ->
      L222.india_state_SubsectorShrwtFllt_en

    L222.india_state_StubTechProd_refining %>%
      add_title("india Calibrated fuel production by state.") %>%
      add_units("varies") %>%
      add_comments("Tech IDs where the calibration is identified as output in the calibrated_techs file are") %>%
      add_comments("are used to adjust data from L122.india_state_out_EJ_refining_F.") %>%
      add_legacy_name("L222.india_state_StubTechProd_refining") %>%
      add_precursors("gcam-india/A22.india_state_calibrated_techs_transformation",
                     "L122.india_state_out_EJ_refining_F") ->
      L222.india_state_StubTechProd_refining

    L222.india_state_StubTechMarket_en %>%
      add_title("Market names of inputs to state refining sectors") %>%
      add_units("varies") %>%
      add_comments("Data from L222.GlobalTechCoef_en is adjusted for use in US states, depending") %>%
      add_comments("on whether regional markets are used.") %>%
      add_legacy_name("L222.india_state_StubTechMarket_en") %>%
      add_precursors("gcam-india/india_states_subregions",
                     "L222.GlobalTechCoef_en") ->
      L222.india_state_StubTechMarket_en

    L222.india_state_CarbonCoef_en %>%
      add_title("Energy carbon coefficients in india") %>%
      add_units("varies") %>%
      add_comments("Carbon coefficients from L202.CarbonCoef are updated with india energy tech shareweights to") %>%
      add_comments("produce energy carbon coefficients in india.") %>%
      add_legacy_name("L222.india_state_CarbonCoef_en") %>%
      add_precursors("L222.SubsectorLogit_en",
                     "L202.CarbonCoef")  ->
      L222.india_state_CarbonCoef_en

    L222.india_state_SubsectorLogit_en %>%
      add_title("Subsector logit competition info for india energy states and sectors") %>%
      add_units("NA") %>%
      add_comments("Subsector logit data from L222.SubsectorLogit_en are filtered and repeated") %>%
      add_comments("for india sectors in each state.") %>%
      add_legacy_name("L222.india_state_SubsectorLogit_en") %>%
      add_precursors("L222.SubsectorLogit_en") ->
      L222.india_state_SubsectorLogit_en

    L222.india_state_StubTech_en %>%
      add_title("Stub technology map for india energy states and sectors.") %>%
      add_units("NA") %>%
      add_comments("The stub technology table from L222.StubTech_en is filtered and repeated") %>%
      add_comments("for india energy sectors in each state.") %>%
      add_legacy_name("L222.india_state_StubTech_en") %>%
      add_precursors("L222.StubTech_en") ->
      L222.india_state_StubTech_en

    L222.india_state_StubTechCoef_refining %>%
      add_title("Refining stub tech coefficients for india energy states and sectors") %>%
      add_units("NA") %>%
      add_comments("Coefficients for refining stub technologies in L222.StubTechCoef_refining are filtered and repeated") %>%
      add_comments("for india energy sectors in each state.") %>%
      add_legacy_name("L222.india_state_StubTechCoef_refining") %>%
      add_precursors("L222.StubTechCoef_refining") ->
      L222.india_state_StubTechCoef_refining

    L222.india_state_GlobalTechSCurve_en %>%
      add_title("Tech S curve parameters for india energy sectors.") %>%
      add_units("varies") %>%
      add_comments("S curve parameters from L222.GlobalTechScurve_en are filtered for india sectors.") %>%
      add_legacy_name("L222.india_state_GlobalTechSCurve_en") %>%
      add_precursors("L222.GlobalTechSCurve_en") ->
      L222.india_state_GlobalTechSCurve_en

    L222.india_state_GlobalTechCost_en %>%
      add_title("Tech costs for india energy sectors.") %>%
      add_units("varies") %>%
      add_comments("Tech cost data from L222.GlobalTechCost_en are filtered for india sectors.") %>%
      add_legacy_name("L222.india_state_GlobalTechCost_en") %>%
      add_precursors("L222.GlobalTechCost_en") ->
      L222.india_state_GlobalTechCost_en

    L222.india_state_GlobalTechInterp_en %>%
      add_title("Interpolation function key for india energy sectors.") %>%
      add_units("units") %>%
      add_comments("Interpolation function key from L222.GlobalTechInterp_en are filtered for india sectors.") %>%
      add_legacy_name("L222.india_state_GlobalTechInterp_en") %>%
      add_precursors("L222.GlobalTechInterp_en") ->
      L222.india_state_GlobalTechInterp_en

    L222.india_state_GlobalTechCoef_en %>%
      add_title("Technology coefficients for india energy sectors.") %>%
      add_units("NA") %>%
      add_comments("Global technology coefficients from L222.GlobalTechCoef_en are filtered for india sectors.") %>%
      add_legacy_name("L222.india_state_GlobalTechCoef_en") %>%
      add_precursors("L222.GlobalTechCoef_en") ->
      L222.india_state_GlobalTechCoef_en

    L222.india_state_GlobalTechShrwt_en %>%
      add_title("Technology shareweights for india energy sectors") %>%
      add_units("NA") %>%
      add_comments("Shareweights from L222.GlobalTechShrwt_en are filtered for india energy sectors.") %>%
      add_legacy_name("L222.india_state_GlobalTechShrwt_en") %>%
      add_precursors("L222.GlobalTechShrwt_en") ->
      L222.india_state_GlobalTechShrwt_en

    L222.india_state_GlobalTechCapture_en %>%
      add_title("Carbon capture data for india energy sectors") %>%
      add_units("NA") %>%
      add_comments("Carbon capture data  from L222.GlobalTechCapture_en are filtered for india energy sectors.") %>%
      add_legacy_name("L222.india_state_GlobalTechCapture_en") %>%
      add_precursors("L222.GlobalTechCapture_en") ->
      L222.india_state_GlobalTechCapture_en

    return_data(L222.india_state_DeleteStubTech_en, L222.india_state_PassThroughSector_en, L222.india_state_Tech_en,
                L222.india_state_TechShrwt_en, L222.india_state_TechInterp_en, L222.india_state_TechCoef_en, L222.india_state_Production_refining,
                L222.india_state_Supplysector_en, L222.india_state_SubsectorShrwtFllt_en, L222.india_state_StubTechProd_refining, L222.india_state_StubTechMarket_en,
                L222.india_state_CarbonCoef_en, L222.india_state_GlobalTechSCurve_en,
                L222.india_state_GlobalTechCost_en,
                L222.india_state_SubsectorLogit_en,
                L222.india_state_StubTech_en,
                L222.india_state_StubTechCoef_refining,
                L222.india_state_GlobalTechInterp_en,
                L222.india_state_GlobalTechCoef_en,
                L222.india_state_GlobalTechShrwt_en,
                L222.india_state_GlobalTechCapture_en)
  } else {
    stop("Unknown command")
  }
}
