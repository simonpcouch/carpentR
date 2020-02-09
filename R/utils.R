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
                          # c1 is calculated directly in calculate_coeff...
default_arguments <- list(c2 = 0.000907,
                          d = .2, # also called c3 in source
                          e = 0.4, # also called c4 in source
                          f1 = 0.1, # also called c5 in source
                          f2 = 0.4, # also called c6 in source
                          g1 = 1.0, # also called c7 in source
                          g2 = 0.7, # also called c8 in source
                          # c9 is referred to as h1 elsewhere in source
                          h2 = 24.3, # also called c10 in source
                          i = 0.7, # also called c11 in source
                          # c12 is referred to as k elsewhere in source
                          # c13 is referred to as s1 elsewhere in source
                          s2 = 0.05, # also called c14 in source
                          t1 = 0.201, # also called c15 in source
                          t2 = 10.8, # also called c16 in source
                          # c17 is referred to as v1 elsewhere in source
                          v2 = 0.43, # also called c18 in source
                          interval_length = 1, #also called c23 in source
                          n_intervals = 120, # also called c24 in source
                          n_steps_per_day = 2, # also called c25 in source
                          nanop_form_resist = 1.0, # previously phi1
                          bg_alga_form_resist = 1.0, # previously phi2
                          altype2 = 1,
                          pt = 1, 
                          ps = 1, 
                          pertsize = 0,
                          pulseday = 60) %>%
  c(.,  # the following four values were prompted for
        # pre-April-1997, but set automatically thereafter
    list(p_results = c(.2, rep(NA, .$n_intervals - 1)), 
        # also called c19, p1 in source
        sa_results = c(5, rep(NA, .$n_intervals - 1)),
        # also called c20, a1 in source
        la_results = c(5, rep(NA, .$n_intervals - 1)),
        # also called c21, b1 in source
        z_results = c(2, rep(NA, .$n_intervals - 1)),
        achl = rep(NA, .$n_intervals),
        bchl = rep(NA, .$n_intervals),
        tchl = rep(NA, .$n_intervals),
        zb = rep(NA, .$n_intervals),
        day = 1:.$n_intervals,
        h = 1/.$n_steps_per_day))


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
compute_rates <- function(y, x) {
  
  # actually resets the constants every time
  x$q <- 1 + (x$c1 * x$t1 * y[[2]]) + (x$c2 * x$t2 * y[[3]])
  x$r1 <- x$v1 * y[[1]] / (x$h1 + y[[1]])
  x$r2 <- x$v2 * y[[1]] / (x$h2 + y[[1]])
  x$u <- ((x$g1 * y[[2]]) + (x$g2 * y[[3]])) / x$k
  x$ddep <- 1 - x$u
  if (x$ddep < 0) {x$ddep <- 0}
  x$znum <- {((1 - x$e - x$f1) * x$c1 * y[[2]]) + 
    ((1 - x$e - x$f2) * x$c2 * y[[3]])}
  x$excr <- {x$e * y[[4]] * ((x$c1 * y[[2]]) + 
                                    (x$c2 * y[[3]])) / x$q}

  
  # initialize a der variable this a vector containing flushing rate of
  # phosphorus, small algae, large algae, and zooplankton (in that order)
  der <- c()
  
  der[1] <- {x$i - (x$r1 * y[[2]] * x$ddep) - 
    (x$r2 * y[[3]] * x$ddep) + x$excr - (x$p_outflow_rate * y[[1]])}
  # rate for small algae (sa) (previously der2)
  der[2] <- {(x$r1 * y[[2]] * x$ddep) - 
      (x$c1 * y[[2]] * y[[4]] / x$q) - (x$s1 * y[[2]])}
  # rate for large algae (la) (previously der3)
  der[3] <- (x$r2 * y[[3]] * x$ddep) - 
    (x$c2 * y[[3]] * y[[4]] / x$q) - (x$s2 * y[[3]])
  # rate for zooplankton (z) (previously der4)
  der[4] <- (y[[4]] * x$znum/x$q) - (x$d*y[[4]])
  
  
  # return der
  der
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
estimate_integrals <- function(y, x) {
  
  x$hh <- x$h * .05
  x$h6 <- x$h / 6

  x$dydx <- compute_rates(y, x)
  
  # initialize yt -- the flushing rates of the p, sa, la, and z
  yt <- rep(NA, 4)
  
  for (i in 1:4) {
    yt[i] <- y[[i]] + x$hh * x$dydx[i]
  }
  
  x$dyt <- compute_rates(yt, x)
  
  for (i in 1:4) {
    yt[i] <-  y[[i]] + x$hh * x$dyt[i]
  }
  
  x$dym <- compute_rates(yt, x)
  
  for (i in 1:4) {
    yt[i] <-  y[[i]] + x$h * x$dym[i]
    x$dym[i] <- x$dyt[i] + x$dym[i]
  }
  
  x$dyt <- compute_rates(yt, x)
  
  for (i in 1:4) {
    x$yout[i] <-  y[[i]] + x$h6*(x$dydx[i] + x$dyt[i] + (2 * x$dym[i]))
  }
  
  # return the whole list
  x
  
}


