# this function computes a bunch of derived variables from the given ones.
calculate_coefficients <- function(user_arguments) {

  # bind the user arguments to the default arguments for accessing
  # within the function
  args <- c(user_arguments, default_arguments)
  
  # all sorts of basic calculations and derivations... the "args" list is
  # modified or appended to by each of these calls
  args$nanoplankter_radius <- args$nanoplankter_diameter / 2
  args$nanoplankter_volume <- (4/3) * pi * (args$nanoplankter_radius^3)
  args$log_nanoplankter_volume <- log(args$nanoplankter_volume)
  args$v1 <- 1.855 - (0.266 * args$log_nanoplankter_volume)
  args$h1 <- 6.48 * 2 * log(args$nanoplankter_diameter)
  args$s1 <- {0.01822 * args$nanoplankter_radius^2 / 
    (args$nanop_form_resist * args$mixed_layer_depth)}
  
  # changed 3/25/97, turned back 6/27/06, c[13] := 0.01;
  
  args$alga_radius <- args$alga_diameter / 2
  args$alga_volume <- (4/3) * pi * (args$alga_radius^3)
  args$log_alga_volume <- log(args$alga_volume)
  args$v2 <- 1.855 - (0.266 * args$log_alga_volume)
  args$h2 <- 6.48 * 2 * log(args$alga_diameter)
  args$s2 <- {0.01822 * args$alga_radius^2 / 
      (args$bg_alga_form_resist * args$mixed_layer_depth)}  
  if (args$altype2 == 1) {args$s2 <- 0.05}
  if (args$altype2 == 2) {args$s2 <- 0.01}
  args$k <- (973/args$mixed_layer_depth) - 0.6
  args$zwt <- 9.86 * exp(2.01 * log(args$length_herbivore))
  args$f_nano <- {-0.719 + (0.587*log(args$zwt)) - 
    (0.165*args$log_nanoplankter_volume^2) + (0.887*args$log_nanoplankter_volume)}
  args$fco1_nano <- args$f_nano - 0.486
  args$fco2_nano <- args$f_nano + 1.317
  args$c1 <- exp(args$fco1_nano*log(10)) * 0.556 / args$zwt
  args$fmax_nano <- exp(args$fco2_nano*log(10)) * 0.178 / args$zwt
  args$t1 <- 1 / args$fmax_nano

  # if blue green diameter is twice the burns value, c is decreased 10x
  args$dmax <- 2.0 * ((22.0 * args$length_herbivore) + 4.9)
  args$f_alga <- {-0.719 + (0.587*log(args$zwt)) - 
      (0.165*args$log_alga_volume^2) + (0.887*args$log_alga_volume)}
  args$fco1_alga <- args$f_alga - 0.486
  args$fco2_alga <- args$f_alga + 1.317
  args$c2 <- exp(args$fco1_alga*log(10)) * 0.556 / args$zwt
  if (args$alga_diameter > args$dmax) {args$c2 <- args$c2 / 10}
  
  args$fmax_alga <- exp(args$fco2*log(10))*0.178 / args$zwt
  args$t2 <- 1.0 / args$fmax_alga
  
  args
}
