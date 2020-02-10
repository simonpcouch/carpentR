check_arguments <- function(argument_list) {
  
  # first, check that each of the supplied arguments is numeric
  argument_is_numeric <- lapply(argument_list, is.numeric)
  
  if (FALSE %in% unlist(argument_is_numeric)) {
    bad_arguments <- which(!unlist(argument_is_numeric))
    
    error_message <- paste0("The following arguments given are not numeric: ",
                            names(argument_list)[bad_arguments], ". ",
                            "Please ensure that, if each of the arguments ",
                            "supplied look like numbers, they are not ",
                            "enclosed in \"quotes.\"")
    
    stop(sprintf(error_message))
  }
  
  # next, check if each of the arguments are within the "valid" range
  # ...an irritating solution since lapply doesn't take in named list elements
  range_checks <- lapply(seq(1, length(argument_list)),
                         argument_within_range,
                         argument_list,
                         names(argument_list)) %>%
    unlist()
  
  for (i in 1:length(range_checks)) {
    if (!range_checks[i]) {
      stop(sprintf(paste0("The supplied ", names(argument_list)[i],
                          " argument is outside of the acceptable range.",
                          " Please see the function documentation for",
                          " acceptable argument ranges.")))
    }
  }

}

