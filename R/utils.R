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
default_arguments <- list(c2 = 0.000907,
                          d = .2, # also called c3 in source
                          e = 0.4, # also called c4 in source
                          f1 = 0.1, # also called c5 in source
                          f2 = 0.4, # also called c6 in source
                          g1 = 1.0, # also called c7 in source
                          g2 = 0.7, # also called c8 in source
                          h2 = 24.3, # also called c10 in source
                          i = 0.7, # also called c11 in source
                          s2 = 0.05, # also called c14 in source
                          t1 = 0.201, # also called c15 in source
                          t2 = 10.8, # also called c16 in source
                          v2 = 0.43, # also called c18 in source
                          # the following four values were prompted for
                          # pre-April-1997, but set automatically thereafter
                          initial_p = .2, # also called c19, p1 in source
                          initial_sa = 5, # also called c20, a1 in source
                          initial_la = 5, # also called c21, b1 in source
                          initial_z = 2, # also called c22, z1 in source
                          interval_length = 1, #also called c23 in source
                          n_intervals = 120, # also called c24 in source
                          n_steps_per_day = 2, # also called c25 in source
                          day = 1,
                          nanop_form_resist = 1.0, # previously phi1
                          bg_alga_form_resist = 1.0, # previously phi2
                          altype2 = 1,
                          pt = 1, 
                          ps = 1, 
                          pertsize = 0) 


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

# computes the flushing rate of phosphorus, algae, and zooplankton
compute_rates <- function(x) {
  
  x$q <- 1.0 + (x$c1 * x$t1 * x$initial_sa) + (x$c2 * x$t2 * x$initial_la)
  x$r1 <- x$v1 * x$initial_p / (x$h1 + x$initial_p)
  x$r2 <- x$v2 * x$initial_p / (x$h2 + x$initial_p)
  x$u <- ((x$g1 * x$initial_sa) + (x$g2 * x$initial_la = 5)) / x$k
  x$ddep <- 1 - x$u
  if (x$ddep < 0) {x$ddep <- 0}
  x$znum <- {((1 - x$e - x$f1) * x$c1 * x$initial_sa) + 
    ((1 - x$e - x$f2) * x$c2 * x$initial_la)}
  x$excr <- {x$e * x$initial_z * ((x$c1 * x$initial_sa) + 
                                    (x$c2 * x$initial_la)) / x$q}
  
  # rate for phosphorus (previously der1)
  x$der_p <- {x$i - (x$r1 * x$initial_sa * x$ddep) - 
    (x$r2 * x$initial_la * x$ddep) + x$excr - (x$p_outflow_rate * x$initial_p)}
  # rate for small algae (previously der2)
  x$der_sa <- {(x$r1 * x$initial_sa * x$ddep) - 
      (x$c1 * x$initial_sa * x$initial_z / x$q) - (x$s1 * x$initial_sa)}
  # rate for large algae (previously der3)
  x$der_la <- (x$r2 * x$initial_la * x$ddep) - 
    (x$c2 * x$initial_la * x$initial_z / x$q) - (x$s2 * x$initial_la)
  # rate for zooplankton (previously der4)
  x$der_z <- (x$initial_z * x$znum/x$q) - (x$d*x$initial_z)
  
  # return the whole list
  x
}

# a function to check the bounds of p, sa, la, and z
check_bounds <- function(x) {
  if (x <= 0) {
    x <- 1e-6
  } else if (x >= 200) {
    stop(sprintf("The given arguments resulted in an unstable model."))
  }
  x
}

# estimates integrals using runge-kutta order 4 method
estimate_integrals <- function(x) {
  
}


