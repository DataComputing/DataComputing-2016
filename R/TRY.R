
check_names_in_data <- function(command, .data){
  v_names <- all.vars(command)
  env_names <- ls(.GlobalEnv)
  missing <- v_names[! v_names %in% c(names(.data), env_names)]
  if (length(missing) == 0) {
    res <- NULL # no problems
  } else {
    res <- paste("No such variable: ", missing, collapse=", ")
  }
  
  res
}
#======================
check_filter <- function(command, command_str, .data){
  problems <- check_names_in_data(command, .data)
  # Check that there are no assignment = in place of ==
  if( grepl("[^=]=[^=]", command_str))
    problems[length(problems) + 1] <- 
      "Don't use the single = .  Use == for equality comparison."

  # Check that each argument evaluates to a boolean
  if ( ! is.null(problems)) return(problems)
  # otherwise
  for (k in 2:length(command)) {
    this_argument <- deparse(command[[k]])
    # now evaluate this_arg in the context of .data
    value <- eval(command[[k]], envir = .data)
    if (is.character(value)) 
      problems[length(problems)+1] <- 
        paste("Argument", k, "is categorical, not 'logical'.")
    if (! is.numeric(value) && ! is.logical(value)) 
      problems[length(problems)+1] <- 
        paste("Argument", k, "cannot be coerced to a 'logical'.")

  }
 
  # Return the accumulated problems
  problems
}
#============================
check_select <- function(command, command_str, .data){
  # not yet set up for the sequence-naming functions in dplyr
  check_names_in_data(command, .data)
}
#============================

check_arguments <- 
  function(verb_name=NULL, command, command_str, .data) {
  res <- switch(verb_name,
           "filter" = check_filter(command, command_str, .data),
           "select" = check_select(command, command_str, .data))
  
  res
}
#=============================

TRY <- function(.data, command) {
  View(.data, title="TRY INPUT")
  command <- substitute(command)
  verb_name <- as.character(command[[1]])
  problems <- 
    check_arguments(verb_name, command,  # The call
                    deparse(command),  # as a string
                    .data)

  # Were there problems?
  if (length(problems) > 0) {
    cat(paste0("Problems in ", deparse(command), ":", "\n * "))
    cat(paste(problems, collapse="\n * "), "\n")
    # print them out and ask if user wants to go ahead anyways
    choice <- menu(c("Continue", "STOP"), title = "\nGo ahead anyways?")
    if ( choice != 1 ) {
         stop("Terminated by user.", call. = FALSE)
    }
  }
  
  # If they go ahead, calculate the value, display, and return
  yy <- paste0(".data %>% ", deparse(command))
  res <- eval(parse(text = yy))
  View(res, title=paste("TRY", "OUTPUT"))
  
  res
}