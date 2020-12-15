#' module_gcamindia_batch_HDDCDD_constdds_xml
#'
#' Construct XML data structure for \code{HDDCDD_constdds_india.xml}.
#' @author  PNK Nov20
module_gcamindia_batch_HDDCDD_constdds_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L244.india_state_HDDCDD_constdds"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "HDDCDD_constdds_india.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L244.india_state_HDDCDD_constdds <- get_data(all_data, "L244.india_state_HDDCDD_constdds")

    # Produce outputs
    create_xml("HDDCDD_constdds_india.xml") %>%
      add_xml_data(L244.india_state_HDDCDD_constdds, "HDDCDD") %>%
      add_precursors("L244.india_state_HDDCDD_constdds") ->
      HDDCDD_constdds_india.xml

    return_data(HDDCDD_constdds_india.xml)
  } else {
    stop("Unknown command")
  }
}
