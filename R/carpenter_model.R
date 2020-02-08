#' Run the simulation for Carpenter (1992)
#' 
#' @description This function implements the model from Carpenter's 1992
#' paper "Destabilization of Planktonic Ecosystems and Blooms of Blue-Green
#' Algae."
#' 
#' @param nanoplankter_diameter diameter of nanoplankter (um) (previously diam1)
#' @param alga_diameter diameter of blue green alga (um) (previously diam2)
#' @param length_herbivore herbivore length (mm) (previously zlen)
#' @param death_rate_herbivore herbivore death rate (fraction per day) (previously c[3])
#' @param p_influx_rate P influx rate (ug P per L per day) (previously c[11])
#' @param p_outflow_rate P outflow rate (fraction per day) (previously outflow)
#' @param mixed_layer_depth mixed layer depth (m) (previously zmix)
#' 
#' @return This function returns a named list, where the first entry is a
#' dataframe containing the results of the simulations, and the remaining
#' entries give the supplied arguments and intermediate results from the model.
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
  
  output <- tibble::tibble(
    day = results$day,
    phosphorus = results$p_results,
    algal_chlorphyll = results$achl,
    blue_green_chlorphyll = results$bchl,
    zooplankton_biomass = results$zb
    )
  
  output
}
