#' module_aglu_LA100.0_LDS_preprocessing
#'
#' Read in and process LDS (Land Data System) files.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{object}. The corresponding file in the
#' original data system was \code{LA100.0_LDS_preprocessing.R} (aglu level1).
#' @details Read in the various LDS datasets; regularize their column names and
#' GLU (Geographic Land Unit) data; change Taiwan ISO to that of mainland China; make LDS_ag_HA_ha and
#' LDS_ag_prod_t tables consistent. See Di Vittorio, A., P. Kyle, and W. Collins. 2016.
#' What are the effects of Agro-Ecological Zones and land use region boundaries on
#' land resource projection using the Global Change Assessment Model? Environmental
#' Modelling & Software 85, 246-265. http://dx.doi.org/10.1016/j.envsoft.2016.08.016.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr gather spread
#' @author BBL March 2017
module_aglu_LA100.0_LDS_preprocessing <- function(command, ...) {

  namelist <- c("Land_type_area_ha",
                "LDS_ag_HA_ha",
                "LDS_ag_prod_t",
                "LDS_value_milUSD",
                "MIRCA_irrHA_ha",
                "MIRCA_rfdHA_ha",
                "Mueller_yield_levels",
                "Ref_veg_carbon_Mg_per_ha",
                "Water_footprint_m3")
  dirname <- "aglu/LDS/"

  if(command == driver.DECLARE_INPUTS) {
    x <- paste0(dirname, namelist)
    names(x) <- rep("FILE", length(x))
    return(x)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(paste0("L100.", namelist))
  } else if(command == driver.MAKE) {

    . <- value <- iso <- NULL             # silence package check.
    L100.Land_type_area_ha <- L100.LDS_value_milUSD <- L100.MIRCA_irrHA_ha <-
        L100.MIRCA_rfdHA_ha <- L100.Mueller_yield_levels <-
        L100.Ref_veg_carbon_Mg_per_ha <- L100.Water_footprint_m3 <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    LDSfiles <- list()
    for(nm in namelist) {
      LDSfiles[[nm]] <- get_data(all_data, paste0(dirname, nm))
    }

    # Go through all data frames and...
    for(nm in namelist) {

      # Regularize data frame names
      names(LDSfiles[[nm]]) %>%
        sub("ctry_iso", "iso", .) %>%
        sub("reglr_iso", "GTAP_region", .) %>%
        sub("glu_code", aglu.GLU, .) %>%
        sub("land_type", "land_code", .) %>%
        sub("SAGE_crop", "GTAP_crop", .) %>%
        sub("mirca_crop", "MIRCA_crop", .) %>%
        sub("use_sector", "GTAP_use", .) ->
        names(LDSfiles[[nm]])

      # Replace numerical GLU code with a concatenation of "GLU" and the
      # three-digit code (padded with zeroes as necessary)
      if(aglu.GLU %in% names(LDSfiles[[nm]])) {
        LDSfiles[[nm]][[aglu.GLU]] <- paste(aglu.GLU, sprintf("%03d", LDSfiles[[nm]][[aglu.GLU]]), sep = aglu.GLU_NAME_DELIMITER)
      }

      # From GPK 3/31/17: We don't have Taiwan as an aglu region, because (a) Taiwan was excluded
      # from SAGE/HYDE in the aggregation that Yuyu did, and (b) it was excluded from FAOSTAT
      # when I queried the data four years ago. Alan Di Vittorio has addressed (a); it's now
      # included in the land cover data that's the input to the data system. And, FAOSTAT now
      # includes Taiwan in their various databases. However, the FAOSTAT data currently in the
      # data system was queried in about 2012, and Taiwan was added in 2014, so it's not actually
      # in our level0 data files. As such, we're not yet in a position to add Taiwan as an aglu
      # region. We would need to update our FAOSTAT queries, which will come along with more fun
      # because they've certainly changed country names and quantities (if not the available data
      # altogether). The steps performed here basically re-map the ISO code of Taiwan back to China,
      # and aggregate anything that needs aggregation (e.g., quantity variables like land cover
      # totals, but not characteristic variables like vegetative carbon densities).

      # Re-set Taiwan to mainland China, as the current version of AgLU
      # (and pre-2015 versions of FAOSTAT) doesn't disaggregate Taiwan
      if("iso" %in% names(LDSfiles[[nm]])) {
        d <- LDSfiles[[nm]]
        if(nm != "Ref_veg_carbon_Mg_per_ha") {
          at <- attributes(d)
          d$iso[d$iso == "twn"] <- "chn"
          d %>%
            # group by everything EXCEPT for value and sum up
            group_by_(.dots = names(d)[-grep("value", names(d))]) %>%
            summarise(value = sum(value)) %>%
            ungroup() %>%
            # summarise() produces a new tibble, but we don't want to lose file info
            same_attributes_as(d) %>%
            add_comments("Taiwan ISO collapsed into China") ->
            LDSfiles[[nm]]
        }
        # Drop Taiwan from the carbon contents
        if(nm == "Ref_veg_carbon_Mg_per_ha") {
          d %>%
            filter(iso != "twn") %>%
            add_comments("Removed data with Taiwan ISO") ->
            LDSfiles[[nm]]
        }
      }
    }

    # Add necessary legacy and precursor information and assign to environment
    for(nm in namelist) {
      legacy_name <- paste0("L100.", nm)
      LDSfiles[[nm]] %>%
        add_legacy_name(legacy_name) %>%
        add_precursors(paste0(dirname, nm)) ->
        df
      assign(legacy_name, df)
    }

    # The production and harvested area tables have values <1 clipped, resulting
    # in some country/glu/crops present in one but not the other. For now these will
    # simply be dropped; in the future, we may want to add a digit of rounding in the lds
    L100.LDS_ag_HA_ha %>%
      semi_join(L100.LDS_ag_prod_t, by = c("iso", aglu.GLU, "GTAP_crop")) ->
      L100.LDS_ag_HA_ha
    L100.LDS_ag_prod_t %>%
      semi_join(L100.LDS_ag_HA_ha, by = c("iso", aglu.GLU, "GTAP_crop")) ->
      L100.LDS_ag_prod_t

    # And we're done
    return_data(L100.Land_type_area_ha,
                L100.LDS_ag_HA_ha,
                L100.LDS_ag_prod_t,
                L100.LDS_value_milUSD,
                L100.MIRCA_irrHA_ha,
                L100.MIRCA_rfdHA_ha,
                L100.Mueller_yield_levels,
                L100.Ref_veg_carbon_Mg_per_ha,
                L100.Water_footprint_m3)
  } else {
    stop("Unknown command")
  }
}
