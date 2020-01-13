# Constants -------------------------------------------------------------
# this section stores constants and defaults used throughout the package

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

# a list of default arguments set throughout the original source code
default_arguments <- list(c1 = 0.245,
                          c2 = 0.000907,
                          d = .2, # also called c3 in source
                          e = 0.4, # also called c4 in source
                          f1 = 0.1, # also called c5 in source
                          f2 = 0.4, # also called c6 in source
                          g1 = 1.0, # also called c7 in source
                          g2 = 0.7, # also called c8 in source
                          h1 = 9.06, # also called c9 in source
                          h2 = 24.3, # also called c10 in source
                          i = 0.7, # also called c11 in source
                          k = 97.0, # also called c12 in source
                          s1 = 0.01, # also called c13 in source
                          s2 = 0.05, # also called c14 in source
                          t1 = 0.201, # also called c15 in source
                          t2 = 10.8, # also called c16 in source
                          v1 = 1.37, # also called c17 in source
                          v2 = 0.43, # also called c18 in source
                          # the following four values were prompted for
                          # pre-April-1997, but set automatically thereafter
                          initial_p = .2, # also called c19 in source
                          initial_sa = 5, # also called c20 in source
                          initial_la = 5, # also called c21 in source
                          initial_z = 2, # also called c22 in source
                          interval_between_saves = 1, #also called c23 in source
                          n_intervals_saved = 120, # also called c24 in source
                          n_steps_per_day = 2) # also called c25 in source


# Functions --------------------------------------------------------------
# this section implements utility functions used inside of 
# top-level functions in the package

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