#' Run the simulation for Carpenter (1992)
#' 
#' @description This function implements the model from Carpenter's 1992
#' paper "Destabilization of Planktonic Ecosystems and Blooms of Blue-Green
#' Algae."
#' 
#' @param nanoplankter_diameter diameter of nanoplankter (micrometers):
#' acceptable range is [2, 5]
#' @param alga_diameter diameter of blue green alga (micrometers): acceptable
#' range is [50, 2000]
#' @param length_herbivore herbivore length (millimeters): acceptable range
#' is [.3, 2.5]
#' @param death_rate_herbivore herbivore death rate (fraction per day):
#' acceptable range is [.05, .3]
#' @param p_influx_rate P influx rate (ug P per L per day): acceptable range
#' is [.1, .4]
#' @param p_outflow_rate P outflow rate (fraction per day): acceptable range
#' is [0, 1]
#' @param mixed_layer_depth mixed layer depth (meters): acceptable range is
#' [3.7, 10.2]
#' 
#' @return This function returns a tibble, with columns:
#' \describe{
#' \item{day}{the simulation day}
#' \item{phosphorus}{phosphorus concentration (micrograms per liter)}
#' \item{algal_chlorophyll}{algal chlorophyll concentration (micrograms 
#' per liter)}
#' \item{blue_green_chlorophyll}{blue green chlorophyll concentration 
#' (micrograms per liter)}
#' \item{zooplankton_biomass}{zooplankton biomass (micrograms per liter)}
#' }
#' 
#' @examples 
#' # run the model using the mendota defaults
#' carpenter_model(nanoplankter_diameter = 5.0,
#'                 alga_diameter = 40,
#'                 length_herbivore = 1,
#'                 death_rate_herbivore = .1,
#'                 p_influx_rate = 0.7,
#'                 p_outflow_rate = 0.0003,
#'                 mixed_layer_depth = 2.5)
#' @export

carpenter_model <- function(nanoplankter_diameter, alga_diameter,
                            length_herbivore, death_rate_herbivore,
                            p_influx_rate, p_outflow_rate, mixed_layer_depth) {
  
  # extract all of the arguments into a named list
  arguments <- as.list(environment())
  
  # check to make sure that the supplied arguments fall within the range
  # that are considered "valid"
  check_arguments(arguments)
  
  # some calculations for coefficients to be used backend
  x <- calculate_coefficients(arguments)
  
  # run the model
  results <- run_carpenter(x)
  
  data.frame(
    day = results$day,
    phosphorus = results$p_results,
    algal_chlorophyll = results$achl,
    blue_green_chlorophyll = results$bchl,
    zooplankton_biomass = results$zb
    )

}
