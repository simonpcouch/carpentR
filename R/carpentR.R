#' Package: carpentR
#'
#' This R Package implements a model from Stephen R. Carpenter's 1992 
#' paper on predicting lake algal blooms using plankton dynamics and other 
#' variables, and was developed specifically for use in Sam Fey's Ecology 
#' class at Reed College.
#'
#' @docType package
#' @name carpentR
#' @importFrom dplyr %>%
"_PACKAGE"

utils::globalVariables(c(".", "perttype", "day", "phosphorus",
                         "algal_chlorophyll", "blue_green_chlorophyll",
                         "type", "value", "zooplankton_biomass"))