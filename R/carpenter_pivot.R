#' Pivot Carpenter model data
#' 
#' @description This function allows you to convert data outputted by
#'  \code{carpenter_model()} from "wide" to "long" format, or the other way 
#'  around. Depending on the desired use case for data outputted by the modeling
#'  function, the data may or may not already be "tidy" (and thus easier to
#'  use with functions from the tidyverse.) This function is just a wrapper
#'  around \code{\link[tidyr:pivot_wider]{pivot_wider}} and 
#'  \code{\link[tidyr:pivot_longer]{pivot_longer}}--see the documentation for
#'  these functions for more complicated use cases.
#' 
#' @param model_data A dataframe outputted by \code{carpenter_model()} or
#' \code{carpenter_pivot()} itself.
#' 
#' @return A tibble, either with 480 rows and 3 columns or 120 rows and 5 
#' columns.
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
#' # convert the data to long format!
#' carpenter_pivot(model)
#' 
#' # running the function twice returns the original data
#' carpenter_pivot(carpenter_pivot(model))
#' 
#' @seealso \code{carpenter_model()} for the function to generate appropriate 
#' data and \code{carpenter_visualize()} to visualize the data.
#' @export
carpenter_pivot <- function(model_data) {
  
  if (!nrow(model_data) %in% c(120, 480)) {
    stop(sprintf(c("It looks like the dataset you provided has ",
                 nrow(model_data), " rows, but it should have 120. ",
                 "Please provide a dataset outputted by `carpenter_model()`.")))
  }
  
  if (ncol(model_data) == 5) {
    return(
    model_data %>%
      tidyr::pivot_longer(2:5, 
                          names_to = "type", 
                          values_to = "concentration")
    )
  }
  
  if (ncol(model_data) == 3) {
    return(
    model_data %>%
      tidyr::pivot_wider(id_cols = "day",
                         names_from = "type", 
                         values_from = "concentration")
    )
  }
  
  # otherwise, the number of columns in the data is neither
  stop(sprintf(c("It looks like the dataset you provided has ",
                 ncol(model_data), " columns rather than 3 or 5. ",
                 "Please provide a dataset outputted by `carpenter_model()`.")))
  
}