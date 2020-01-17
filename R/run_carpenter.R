# a function that actually runs the model making use of all
# of the derived variables -- it takes in a named list
# outputted by calculate_coefficients
run_carpenter <- function(x) {
  
  # set some constants
  x$h <- 1 / x$n_steps_per_day
  x$pulseday <- 60
  
  x$p_results <- rep(NA, x$n_intervals - 1)
  x$sa_results <- rep(NA, x$n_intervals - 1)
  x$la_results <- rep(NA, x$n_intervals - 1)
  x$z_results <- rep(NA, x$n_intervals - 1)
  
  x <- 1
  day <- rep(NA, x$n_intervals - 1)
  
  # triply nested loops.. sheesh (use this instead of the apply family
  # so that we don't have to pass values as function-level variables 
  # and since all vectors can be pre-allocated)
  for (i in 2:x$n_intervals) {
    
    p <- x$initial_p
    sa <- x$initial_sa
    la <- x$initial_la
    z <- x$initial_z
    
    for (j in 1:x$interval_length) {
      for (k in 1:x$n_steps_per_day) {
        
        # estimate integrals using runge-kutta order 4 method
        estimate_integrals(x)
        
        # check the bounds of the different variables
        c(p, sa, la, z) <- lapply(list(p, sa, la, z),
                                  check_bounds) %>%
                           unlist()
        
        x <- round(x + x$h)
        x$tday <- x
        
        if (x$tday == x$pulseday) {
          if (x$perttype == 1) {
            p <- p + x$pertsize
          } else if (perttype == 2) {
            sa <- sa * x$pertsize
            la <- la * x$pertsize
          }
        }
      } # end of k-indexed loop
    } # end of j-indexed loop
    
    day[i] <- x
    x$p_results[i] <- p
    x$sa_results[i] <- sa
    x$la_results[i] <- la
    x$z_results[i] <- z
    
  } # end of i-indexed loop
  

  
} # end of run_carpenter