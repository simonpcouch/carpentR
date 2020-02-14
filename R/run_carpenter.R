# a function that actually runs the model making use of all
# of the derived variables -- it takes in a named list
# outputted by calculate_coefficients
run_carpenter <- function(x) {
  
  # set initial values
  y <- c(x$p_results[1], x$sa_results[1], 
         x$la_results[1], x$z_results[1])
  
  # triply nested loops.. sheesh (use this instead of the apply/map family
  # so that we don't have to pass values as function-level variables 
  # and since all vectors have been pre-allocated)
  for (i in 2:x$n_intervals) {
    
    for (j in 1:x$interval_length) {
      for (k in 1:x$n_steps_per_day) {
        
        # estimate integrals using runge-kutta order 4 method
        x <- estimate_integrals(y, x)
        
        # check the bounds of the different variables, and overwrite
        # y with x$yout
        y <- lapply(x$yout, check_bounds) %>% unlist()
        
        x$tday <- round(1 + x$h)
        
        if (x$tday == x$pulseday) {
          if (x$perttype == 1) {
            y[1] <- y[1] + x$pertsize
          } else if (perttype == 2) {
            y[2] <- y[2] * x$pertsize
            y[3] <- y[3] * x$pertsize
          }
        }
      } # end of k-indexed loop
    } # end of j-indexed loop
    
    x$p_results[i] <- y[1]
    x$sa_results[i] <- y[2]
    x$la_results[i] <- y[3]
    x$z_results[i] <- y[4]
    
  } # end of i-indexed loop
  
  # another i indexed loop, beginning at 1
  for (i in 1:x$n_intervals) {
    
    # some calculations for algal chlorophyll
    x$achl[i] <- x$g1 * x$sa_results[i]
    x$bchl[i] <- x$g2 * x$la_results[i]
    x$tchl[i] <- x$achl[i] + x$bchl[i]
    x$zb[i] <- x$z_results[i] / 0.018

  }

  # return the list
  x
  
} # end of run_carpenter
