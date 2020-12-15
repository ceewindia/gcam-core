#' module_gcamindia_batch_en_transformation_xml
#'
#' Construct XML data structure for \code{en_transformation_india.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{en_transformation_india.xml}. The corresponding file in the
#' original data system was \code{batch_en_transformation_india_xml.R} (gcamindia XML).
#' @author VC Nov20
module_gcamindia_batch_en_transformation_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L222.india_state_DeleteStubTech_en",
             "L222.india_state_PassThroughSector_en",
             "L222.india_state_SubsectorLogit_en",
             "L222.india_state_StubTech_en",
             "L222.india_state_StubTechCoef_refining",
             "L222.india_state_GlobalTechInterp_en",
             "L222.india_state_GlobalTechCoef_en",
             "L222.india_state_GlobalTechCost_en",
             "L222.india_state_GlobalTechShrwt_en",
             "L222.india_state_GlobalTechCapture_en",
             "L222.india_state_GlobalTechSCurve_en",
             "L222.india_state_Tech_en",
             "L222.india_state_TechShrwt_en",
             "L222.india_state_TechInterp_en",
             "L222.india_state_TechShrwt_en",
             "L222.india_state_TechCoef_en",
             "L222.india_state_Production_refining",
             "L222.india_state_Supplysector_en",
             "L222.india_state_SubsectorShrwtFllt_en",
             "L222.india_state_StubTechProd_refining",
             "L222.india_state_StubTechMarket_en",
             "L222.india_state_CarbonCoef_en"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "en_transformation_india.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L222.india_state_DeleteStubTech_en <- get_data(all_data, "L222.india_state_DeleteStubTech_en")
    L222.india_state_PassThroughSector_en <- get_data(all_data, "L222.india_state_PassThroughSector_en")
    L222.india_state_SubsectorLogit_en <- get_data(all_data, "L222.india_state_SubsectorLogit_en")
    L222.india_state_StubTech_en <- get_data(all_data, "L222.india_state_StubTech_en")
    L222.india_state_StubTechCoef_refining <- get_data(all_data, "L222.india_state_StubTechCoef_refining")
    L222.india_state_GlobalTechInterp_en <- get_data(all_data, "L222.india_state_GlobalTechInterp_en")
    L222.india_state_GlobalTechCoef_en <- get_data(all_data, "L222.india_state_GlobalTechCoef_en")
    L222.india_state_GlobalTechCost_en <- get_data(all_data, "L222.india_state_GlobalTechCost_en")
    L222.india_state_GlobalTechShrwt_en <- get_data(all_data, "L222.india_state_GlobalTechShrwt_en")
    L222.india_state_GlobalTechCapture_en <- get_data(all_data, "L222.india_state_GlobalTechCapture_en")
    L222.india_state_GlobalTechSCurve_en <- get_data(all_data, "L222.india_state_GlobalTechSCurve_en")
    L222.india_state_Tech_en <- get_data(all_data, "L222.india_state_Tech_en")
    L222.india_state_TechShrwt_en <- get_data(all_data, "L222.india_state_TechShrwt_en")
    L222.india_state_TechInterp_en <- get_data(all_data, "L222.india_state_TechInterp_en")
    L222.india_state_TechShrwt_en <- get_data(all_data, "L222.india_state_TechShrwt_en")
    L222.india_state_TechCoef_en <- get_data(all_data, "L222.india_state_TechCoef_en")
    L222.india_state_Production_refining <- get_data(all_data, "L222.india_state_Production_refining")
    L222.india_state_Supplysector_en <- get_data(all_data, "L222.india_state_Supplysector_en")
    L222.india_state_SubsectorShrwtFllt_en <- get_data(all_data, "L222.india_state_SubsectorShrwtFllt_en")
    L222.india_state_StubTechProd_refining <- get_data(all_data, "L222.india_state_StubTechProd_refining")
    L222.india_state_StubTechMarket_en <- get_data(all_data, "L222.india_state_StubTechMarket_en")
    L222.india_state_CarbonCoef_en <- get_data(all_data, "L222.india_state_CarbonCoef_en")

    technology <- year <- NULL # Silence package checks
    # ===================================================
    # Rename tibble columns to match the header information.
    L222.india_state_Tech_en <- rename(L222.india_state_Tech_en, pass.through.technology = technology)
    L222.india_state_SubsectorShrwtFllt_en <- rename(L222.india_state_SubsectorShrwtFllt_en, year.fillout = year)

    # Produce outputs
    create_xml("en_transformation_india.xml") %>%
      add_node_equiv_xml("sector") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L222.india_state_DeleteStubTech_en, "DeleteStubTech") %>%
      add_xml_data(L222.india_state_PassThroughSector_en, "PassThroughSector") %>%
      add_logit_tables_xml(L222.india_state_SubsectorLogit_en, "SubsectorLogit") %>%
      add_xml_data(L222.india_state_StubTech_en, "StubTech") %>%
      add_xml_data(L222.india_state_StubTechCoef_refining, "StubTechCoef") %>%
      add_xml_data(L222.india_state_GlobalTechInterp_en, "GlobalTechInterp") %>%
      add_xml_data(L222.india_state_GlobalTechCoef_en, "GlobalTechCoef") %>%
      add_xml_data(L222.india_state_GlobalTechCost_en, "GlobalTechCost") %>%
      add_xml_data(L222.india_state_GlobalTechShrwt_en, "GlobalTechShrwt") %>%
      add_xml_data(L222.india_state_GlobalTechCapture_en, "GlobalTechCapture") %>%
      add_xml_data(L222.india_state_GlobalTechSCurve_en, "GlobalTechSCurve") %>%
      add_xml_data(L222.india_state_Tech_en, "PassThroughTech") %>%
      add_xml_data(L222.india_state_TechInterp_en, "TechInterp") %>%
      add_xml_data(L222.india_state_TechShrwt_en, "TechShrwt") %>%
      add_xml_data(L222.india_state_TechShrwt_en, "TechShrwt") %>%
      add_xml_data(L222.india_state_TechCoef_en, "TechCoef") %>%
      add_xml_data(L222.india_state_Production_refining, "Production") %>%
      add_logit_tables_xml(L222.india_state_Supplysector_en, "Supplysector") %>%
      add_xml_data(L222.india_state_SubsectorShrwtFllt_en, "SubsectorShrwtFllt") %>%
      add_xml_data(L222.india_state_StubTechProd_refining, "StubTechProd") %>%
      add_xml_data(L222.india_state_StubTechMarket_en, "StubTechMarket") %>%
      add_xml_data(L222.india_state_CarbonCoef_en, "CarbonCoef") %>%
      add_precursors("L222.india_state_DeleteStubTech_en",
                     "L222.india_state_PassThroughSector_en",
                     "L222.india_state_SubsectorLogit_en",
                     "L222.india_state_StubTech_en",
                     "L222.india_state_StubTechCoef_refining",
                     "L222.india_state_GlobalTechInterp_en",
                     "L222.india_state_GlobalTechCoef_en",
                     "L222.india_state_GlobalTechCost_en",
                     "L222.india_state_GlobalTechShrwt_en",
                     "L222.india_state_GlobalTechCapture_en",
                     "L222.india_state_GlobalTechSCurve_en",
                     "L222.india_state_Tech_en",
                     "L222.india_state_TechShrwt_en",
                     "L222.india_state_TechInterp_en",
                     "L222.india_state_TechShrwt_en",
                     "L222.india_state_TechCoef_en",
                     "L222.india_state_Production_refining",
                     "L222.india_state_Supplysector_en",
                     "L222.india_state_SubsectorShrwtFllt_en",
                     "L222.india_state_StubTechProd_refining",
                     "L222.india_state_StubTechMarket_en",
                     "L222.india_state_CarbonCoef_en") ->
      en_transformation_india.xml

    return_data(en_transformation_india.xml)
  } else {
    stop("Unknown command")
  }
}
