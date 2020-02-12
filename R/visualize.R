#' Visualize Carpenter model data
#' 
#' @description These functions allow the user to visualize the outputs of
#' the \code{carpenter_model()} function. \code{visualize_carpenter()} calls
#' each of the other visualization functions in succession!
#' 
#' @param model_data A dataframe outputted by \code{carpenter_model()}
#' 
#' @return A ggplot object displaying the concentration of the relevant
#' variables over time.
#' 
#' @examples 
#' # run the model using the mendota defaults
#' model <- carpenter_model(nanoplankter_diameter = 5.0,
#'                          alga_diameter = 40,
#'                          length_herbivore = 1,
#'                          death_rate_herbivore = .1,
#'                          p_influx_rate = 0.7,
#'                          p_outflow_rate = 0.0003,
#'                          mixed_layer_depth = 2.5)
#'                         
#' # plot the data using the different visualize_* functions!
#' visualize_phosphorus(model)
#' visualize_chlorophyll(model)
#' visualize_zooplankton(model)
#' visualize_carpenter(model)
#' 
#' # you can add on layers to the output of this function
#' # as would any ggplot output
#' visualize_phosphorus(model) + ggplot2::theme_minimal()
#' 
#' @seealso \code{carpenter_model()} for the function to generate appropriate 
#' data.
#' @export
carpenter_visualize <- function(model_data) {
  
  list(
    visualize_phosphorus(model_data),
    visualize_chlorophyll(model_data),
    visualize_zooplankton(model_data)
  )
  
}

#' @rdname carpenter_visualize
#' @export
carpenter_viz_phosphorus <- function(model_data) {
  
  check_model_data(model_data)
  
  model_data %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = day, y = phosphorus) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Day", 
                  y = "Phosphorus (ug/L)", 
                  title = "Simulation: Phosphorus Levels Over Time")
  
}

#' @rdname carpenter_visualize
#' @export
carpenter_viz_chlorophyll <- function(model_data) {
  
  check_model_data(model_data)
  
  model_data %>%
    dplyr::mutate(
      total_chlorophyll = algal_chlorophyll + blue_green_chlorophyll
      ) %>%
    tidyr::pivot_longer(cols = 2:6,
                        names_to = "type",
                        values_to = "value") %>%
    dplyr::filter(!type %in% c("phosphorus", "zooplankton_biomass")) %>%
    dplyr::mutate(type = factor(type, labels = c("Algal", 
                                                 "Blue-Green", 
                                                 "Total"))) %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = day, y = value, color = type) +
    ggplot2::geom_line() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(y = "Chlorophyll Concentration (ug/L)",
                  x = "Day",
                  color = "Chlorophyll Type",
                  title = "Simulation: Chlorophyll Levels Over Time")
}

#' @rdname carpenter_visualize
#' @export
carpenter_viz_zooplankton <- function(model_data) {
  
  check_model_data(model_data)
  
  model_data %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = day, y = zooplankton_biomass) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Day", 
                  y = "Zooplankton Biomass (ug/L)", 
                  title = "Simulation: Zooplankton Biomass Over Time")
}