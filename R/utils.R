# an example argument list to make checking functions quicker
example_arguments <- list(nanoplankter_diameter = 3, 
                          alga_diameter = 100,
                          length_herbivore = 1, 
                          death_rate_herbivore = .1,
                          p_influx_rate = 1, 
                          # this is "E" in the paper, no formal bounds given
                          p_outflow_rate = .00003, 
                          # this bound pulled from "Numerical analyses"
                          mixed_layer_depth = 5)


# a named list giving the valid ranges for the different arguments,
# as given by the original paper
                     # the below 5 given exactly in middle of p. 464
valid_ranges <- list(nanoplankter_diameter = c(2, 5), 
                     alga_diameter = c(50, 2000),
                     length_herbivore = c(.3, 2.5), 
                     death_rate_herbivore = c(.05, .3),
                     p_influx_rate = c(.1, 4), 
                     # this is "E" in the paper, no formal bounds given
                     p_outflow_rate = c(0, 1), 
                     # this bound pulled from "Numerical analyses"
                     mixed_layer_depth = c(3.7, 10.2))


# takes in an index, vector of values, and vector of names for the values--
# this is a workaround to lapply not taking in named list elements
argument_within_range <- function(index, values, names) {
  
  x <- values[index]
  name <- names[index]
  
  # pull out the index of the named argument
  name_index <- which(names(valid_ranges) == name,
                      arr.ind = TRUE)

  # check whether it's in the range
  x > valid_ranges[[name_index]][1] & x < valid_ranges[[name_index]][2]
}

