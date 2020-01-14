#' @param nanoplankter_diameter diameter of nanoplankter (um) (previously diam1)
#' @param alga_diameter diameter of blue green alga (um) (previously diam2)
#' @param length_herbivore herbivore length (mm) (previously zlen)
#' @param death_rate_herbivore herbivore death rate (fraction per day) (previously c[3])
#' @param p_influx_rate P influx rate (ug P per L per day) (previously c[11])
#' @param p_outflow_rate P outflow rate (fraction per day) (previously outflow)
#' @param mixed_layer_depth mixed layer depth (m) (previously zmix)
#' @examples 
#' # These are known as the "Mendota defaults"
#' carpenter_model(nanoplankter_diameter = 5.0,
#'                 alga_diameter = 40,
#'                 herbivore_length = 1,
#'                 herbivore_death_rate = .1,
#'                 p_influx_rate = 0.7,
#'                 p_outflow_rate = 0.0003,
#'                 mixed_layer_depth = 2.5)       
carpenter_model <- function(nanoplankter_diameter, alga_diameter,
                            length_herbivore, death_rate_herbivore,
                            p_influx_rate, p_outflow_rate, mixed_layer_depth) {
  
  # extract all of the arguments into a named list
  arguments <- as.list(environment())
  
  # check to make sure that the supplied arguments fall within the range
  # that are considered "valid"
  check_arguments(arguments)
  
  # some calculations for coefficients to be used backend
  coefficients <- calculate_coefficients(arguments)
  
  # run the model
  results <- run_carpenter()
}
