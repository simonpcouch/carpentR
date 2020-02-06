# a function that actually runs the model making use of all
# of the derived variables -- it takes in a named list
# outputted by calculate_coefficients
run_carpenter <- function(x) {
  
  # set some constants
  x$h <- 1 / x$n_steps_per_day
  x$pulseday <- 60
  
  # preallocate some vectors
  x$p_results <- x$sa_results <- x$la_results <- x$z_results <- 
    day <- achl <- bchl <- tchl <- zb <- rep(NA, x$n_intervals - 1)
  x$x <- 1
  
  # triply nested loops.. sheesh (use this instead of the apply/map family
  # so that we don't have to pass values as function-level variables 
  # and since all vectors can be pre-allocated)
  for (i in 2:x$n_intervals) {

    # reset the values at the beginning of each iteration
    # initial concentrations of p, sa, la, and z
    y <- list(x$initial_p, x$initial_sa, x$initial_la, x$initial_z)
    
    for (j in 1:x$interval_length) {
      for (k in 1:x$n_steps_per_day) {
        
        # estimate integrals using runge-kutta order 4 method
        x <- estimate_integrals(y, x)
        
        # check the bounds of the different variables
        y <- lapply(list(y[[1]], y[[2]], y[[3]], y[[4]]), check_bounds)
        
        x$x <- round(x$x + x$h)
        x$tday <- x$x
        
        if (x$tday == x$pulseday) {
          if (x$perttype == 1) {
            y[[1]] <- y[[1]] + x$pertsize
          } else if (perttype == 2) {
            y[[2]] <- y[[2]] * x$pertsize
            y[[3]] <- y[[3]] * x$pertsize
          }
        }
      } # end of k-indexed loop
    } # end of j-indexed loop
    
    day[i] <- x
    x$p_results[i] <- y[[1]]
    x$sa_results[i] <- y[[2]]
    x$la_results[i] <- y[[3]]
    x$z_results[i] <- y[[4]]
    
  } # end of i-indexed loop
  
  # another i indexed loop, beginning at 1?
  for (i in 1:x$n_intervals) {
    
    # some calculations for algal chlorophyll
    achl[i] <- x$g1 * y[[2]][i]
    bchl[i] <- x$g2 * y[[3]][i]
    tchl[i] <- achl[i] + bchl[i]
    zb[i] <- x$z_results[i] / 0.018

  }
  
  # store the results as list elements
  x[c("achl", "bchl", "tchl", "zb", "day")] <- list(achl, bchl, tchl, zb, day)

  # return the list
  x
  
} # end of run_carpenter
