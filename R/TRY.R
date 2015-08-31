#' Look for mistakes in dplyr commands
#'
#' When wrapped around a dplyr command, TRY()
#' displays the input data frame and scans the command for common
#' errors such as variable name mis-spellings, failure to name
#' arguments as appropriate, etc.  If the command is successful,
#' the command output is returned and it can be assigned, piped to
#' further operations, etc.
#'
#' A WAYPOINT() can be inserted into a chain to suspend the processing
#' to give you a chance to look at the input and output from the previous
#' TRY() step. These are displayed in the `TRY INPUT` and `TRY OUTPUT` tabs
#' in the editor.  Then, you are prompted to continue or quit at this point.
#'
#' ALL commands must have the data table input PIPED into them.  Don't
#' use the data table as the first argument (unless it is the right table
#' in a join operation).
#'
#' @rdname TRY
#' @param command a dplyr command (for a piped input)
#' @param interactive if TRUE (the default) ask user whether to continue
#' @param name character string naming a waypoint
#' when the command has problems
#' @return The output of the dplyr command
#'
# @examples \dontrun{
#   require(mosaicData)
#   # height is not a variable in KidsFeet
#   # arguments to summarise should be named
#   # watch out for na.rm in man
#   KidsFeet %>% group_by(sex) %>% TRY(summarise(mean(height)))
# }


#' @aliases TRY WAY_POINT
#' @export
TRY <- function(.data, command,
                interactive=getOption("TRY_interactive",default=TRUE)) {
  # don't evaluate <command> yet
  command <- substitute(command)

  if (interactive) {
    if (inherits(.data, "data.frame")) View(.data, title="TRY INPUT")
  }
  # return value
  TRY_helper(.data, command, interactive)
}
# ====================
# TRY_helper() doesn't have to apply substitute to the command
# That's already been done in TRY()
TRY_helper <- function(.data, command, interact) {
  verb_name <- as.character(command[[1]])
  if (verb_name == "%>%" || verb_name == "+") {
    # it's a compound statement, divide it into parts
    mid_data <- TRY_helper(.data, command[[2]], interact)
    # replace .data and continue on
    .data <- mid_data
    verb_name <- as.character(command[[3]][[1]])
    command <- command[[3]] # just the rest of the command
  }
  problems <-
    check_arguments(verb_name, command,  # The call
                    deparse(command),  # as a string
                    .data,
                    interact)

  # Were there problems?
  show_problems(problems, command, interact)

  # If no problems, evaluate the command, display errors if any, and return
  res <- eval_command(command, interact, .data) # produces value to return
  return(res)

}
# =====================
eval_command <- function(command, interact, .data) {
  in_between <- "%>%"
  # deal with ggplot's use of "+"
  if (grepl("geom_", as.character(command[[1]]))) in_between <- "+"

  this_task <- paste(".data", in_between, deparse(command))
  res <- try( eval(parse(text = this_task)), silent=TRUE )

  if (inherits(res, "try-error")) {
    cat("Error running command:\n    ", deparse(command), "\n")
    cat("Why? ", gsub("Error :", "", geterrmessage()), "\n")
    stop(paste0("Terminated by ", command[[1]]), call. = FALSE)
  } else {
    # something was calculated!
    if (interact) {
      if (inherits(res, "data.frame")) {
        View(res, title=paste("TRY", "OUTPUT"))
      }

      if (is.ggplot(res)) {
        # Show the plot up to now?
        if (length(res$layers) == 0) {
          p <- res + geom_blank()
        } else {
          p <- res
        }

        make_plot <- try(print(p), silent = TRUE)
        if (inherits(make_plot, "try-error")) {
          cat("Error running command:\n    ", deparse(command), "\n")
          cat("Why? ", gsub("Error :", "", geterrmessage()), "\n")
          stop(paste0("Terminated by ", command[[1]]), call. = FALSE)
        }
      }

    }
    return(res)
  }
}

#======================
show_problems <- function(problems, command, interact) {
  if (length(problems) > 0) {
    cat(paste0("Problems in ", deparse(command), ":", "\n * "))
    cat(paste(problems, collapse="\n * "), "\n")
    if (interact) {
      # print them out and ask if user wants to go ahead anyways
      choice <- menu(c("Continue", "STOP"), title = "\nGo ahead anyways?")
      if ( choice != 1 ) {
        stop("Terminated by user.", call. = FALSE)
      }
    }
  }
}
# ====================

#' @rdname TRY
#' @export
WAYPOINT <-
  function(.data, name = NULL,
           interact=
             getOption("TRY_interactive",default=TRUE)) {
    name <- substitute(name) # in case quotes were forgotten.
    if (is.null(name)) {
      warning("Give each waypoint a unique name")
      name <- "Unnamed waypoint"
    }
    if (interact) {
      if (is.ggplot(.data)) {
        print(.data)
        message <-
          paste0("Stopped at waypoint ", name, ".\n  Plot being displayed\n",
                 "Press 'Enter' to continue, Q to quit. ===>\n\n\n")
      }
      else {
        message <-
          paste0("Stopped at waypoint ", name,
                 "\n You can see the input to the last step\n",
                 "  and the output from it in the editor tabs:\n",
                 "  TRY INPUT and TRY OUTPUT.\n\n",
                 "Press 'Enter' to continue, Q to quit. ===>\n\n\n")
      }
      response <- readline(message)

      if( grepl("Q|q", response))
        stop(paste0("Quiting from waypoint: ", name, .call = FALSE))
    }
    # Ready to go on ...
    .data # return the input data
}

# ========================
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
check_filter <- function(command, command_str, .data, interact){
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
# get the argument names from a call
argument_names <- function(command) {
  # grab_names returns its arguments as an unevaluated list
  grab_names <- function(...) {
    as.list(match.call())
  }
  # modify the command so that it uses grab_names
  command[[1]] <- grab_names
  L <- eval(command)
  res <- names(L[-1]) # all but the function itself

  res
}
#============================
check_select <- function(command, command_str, .data, interact){
  # not yet set up for the sequence-naming functions in dplyr
  check_names_in_data(command, .data)
}
#=============================
check_arrange <- function(command, command_str, .data, interact){
  check_names_in_data(command, .data)
}
#============================
check_group_by <- function(command, command_str, .data, interact){
  check_names_in_data(command, .data)
}
#============================
check_tally <- function(command, command_str, .data, interact) {
  if (length(command) > 1 ) return("tally() takes no arguments.")
  return(NULL) # no problems

  # In fact, it can take a "sort" argument and a "wt" argument.
}

#============================
check_mutate <- function(command, command_str, .data, interact){
  res <- NULL
  # do the variables mentioned
  P <- check_names_in_data(command, .data)
  if ( ! is.null(P)) res[length(res) + 1] <- P
  P <- args_must_be_named(command, command_str, .data)
  if ( ! is.null(P)) res[length(res) + 1] <- P

  res
}
#============================
check_rename <- function(command, command_str, .data, interact){
  vars <- all.vars(command)

  missing <- vars[! vars %in% names(.data)]
  if (length(missing) == 0) {
    res <- NULL # no problems
  } else {
    res <- "Old variable names go to right of =."
    res[2] <- paste("No such variable in data table: ",
                    missing, collapse=", ")
  }

  P <- args_must_be_named(command, command_str, .data)
  if( ! is.null(P)) res[length(res) + 1] <- P

  res
}
# ========================
args_must_be_named <- function(command, command_str, .data){
  # Watch out for a failure to use named arguments
  res <- NULL
  if (length(command) <= 1 ) return(res)

  arg_names <- argument_names(command)
  if (is.null(arg_names))
    res <- "Arguments must be in named-argument style."
  no_names <- which(arg_names == "")
  if (length(no_names) == 1)
    res <- paste("Argument", no_names, "isn't being given a new name.", collapse=",")
  if (length(no_names) > 1)
    res <- paste("Arguments", no_names, "aren't being given a new name.", collapse=",")

  res
}
#=============================
check_join <- function(command, command_str, .data, interact){
  # first argument must be a data frame

  if (length(command) > 1) {
    # get that data frame
    right_table <- try(eval(command[[2]]))
    if (inherits(right_table, "try-error"))
      return(
        paste0(
          "Can't evaluate first argument: ",
          deparse(command[[2]])))
  } else {
    if ( ! inherits(right_table, "data.table"))
      return("First argument must be a data table.")
  }

  if (length(command) > 1 &&
      argument_is_data_frame(command[[2]]) ){
    res <- NULL
  } else {
    return(
      "First argument must be the data table to join to the input."
    )
  }

  # first argument was a data table, so we can examine
  # its variable names and check that a join is possible
  res <- NULL
  # is there a by= argument?
  if (length(command) == 3) {
    right_by <- try(eval(command[[3]]))
    # did something go wrong?
    if (inherits(right_by, "try-error")) {
      # just a guess.  There might be other problems
      return(
        paste0(
          "Missing quotes around variable names in ",
          deparse(command[[3]]), "?"))
    }

    left_by <- names(right_by)
    if (is.null(left_by)) left_by <- right_by

    # are the names in their respective data tables?
    missing_left <- left_by[ ! left_by %in% names(.data)]
    missing_right <- right_by[ ! right_by %in% names(right_table)]
    if (length(missing_left) != 0) {
      res[length(res) + 1] <-
        paste0("Variables ", missing_left,
               " not in left table.", collapse=", ")
    }
    if (length(missing_right) != 0) {
      res[length(res) + 1] <-
        paste0("Variables ", missing_right,
               " not in right table.", collapse=", ")
    }

  } else {
    # make sure there are some variables in common
    overlap <- names(.data) %in% names(right_table)
    if (sum(overlap) == 0)
      res[length(res) + 1] <- "No variables to join by."
  }

  res
}
#=============================
check_summarise <- function(command, command_str, .data, interact) {
  res <- args_must_be_named(command, command_str, .data)
  P <-   check_names_in_data(command, .data)
  if ( ! is.null(P)) res[length(res) + 1 ] <- P

  # check on use of rm.na
  requires_rm.na <- "mean|median|sd|max|min|var|sum|prod"
  for (k in 2:length(command)) {
    this_arg <- deparse(command[[k]])
    if (grepl(requires_rm.na, this_arg) &&
        ! grepl("na.rm *= *TRUE", this_arg ))
      res[length(res)+1] <-
        paste0("na.rm=TRUE not used in argument ", k, ": ",
               deparse(command[[k]]))
  }

  res
}
# =======================
argument_is_data_frame <- function(command) {
  var_names <- all.vars(command)

  for (this_name in var_names) {
    test_string <-
      paste0("inherits(", this_name, ", 'data.frame')")
    res <- eval(parse(text=test_string))
    if (res) return(TRUE)
  }

  FALSE

}
#============================
check_ggplot <- function(command, command_str, .data, interact) {


  # Replace this with the logic from check_layer, which does more checking.
  P <- check_names_in_data(command, .data)
  P <- c(P, check_aes_form(command, command_str, .data))

  return(P)
}
#============================
# list of commonly used aesthetics
plausible_aesthetics <-
  c("x", "y", "color", "colour", "shape", "size", "group",
    "alpha", "fill", "linetype", "lower", "middle", "upper",
    "ymax", "ymin", "linetype", "weight")
#============================
check_aes_form <- function(command, command_str, .data) {
  P <- NULL
  if (grepl("aes=", command_str)) {
    P <- "Use aes() as function, *not* 'aes='."
  } else {
    # look for the aes() call
    aes_call <- find_subcommand(command, "aes")
    if ( ! is.null(aes_call)) { # there is an aes() call
      # check the names of the arguments.  Make sure they are reasonable.
      arg_names <- argument_names(aes_call)
      # every argument must have a name
      if (any(arg_names == ""))
        P <- c(P, "All arguments to aes() must be named.")

      not_in_list <- arg_names[ ! arg_names %in% plausible_aesthetics]
      if (length(not_in_list) != 0) {
        message <- paste0("Are you sure these are aesthetics? ",
                          not_in_list,
                          collapse = ",")
        P <- c(P, message)
      }

      # all aethetics in aes() must be mapped to variable names
      var_names <- names(.data)
      bad_names <- NULL
      for (k in 2:length(aes_call)) {
        in_this <- all.vars(aes_call[[k]])
        bad_names <- c(bad_names, in_this[ ! in_this %in% var_names])
      }
      if (length(bad_names) != 0) {
        message <- paste0(
          "aes() takes only variable names as arguments.\n",
          "    These are not variable names: ", bad_names, collapse = ",")
        P <- c(P, message)
      }
    }

  }

  P # return value
}
# ============================
find_subcommand <- function(command, command_name) {
  # recurse over command tree and return any branch headed
  # by <command_name>

  # check this command
  if (is.name(command)) # just an unadorned name
    return(NULL)

  if(as.character(command[[1]]) == command_name) # A match!
    return(command)

  if (length(command) <= 1) {
    # we're at a leaf
    return(NULL)
  } else {
    for (k in 2:length(command)) {
      res <- find_subcommand(command[[k]], command_name)
      if ( ! is.null(res)) return(res)
    }
  }

  return(NULL)

}
#============================
check_layer <- function(command, command_str, .data, interact) {
  # See if .data is a data frame or a ggplot object
  # If the latter, check whether <command> has a
  # data= component and set that
  # to be the data to be checked.
  # Otherwise, extract the data from the ggplot object
  arg_names <- argument_names(command)
  data_subcommand <- which("data" == arg_names) # the index - 1
  if (length(data_subcommand) == 0 ) {
    if (inherits(.data, "ggplot") || # two ways to identify ggplot obj.
        all(c("data", "panel", "plot") %in% names(.data)) ) {
      whatever_the_data_is <- .data$data # get from ggplot
    }
  } else {
    data_subcommand <- command[[data_subcommand + 1]]

    whatever_the_data_is <- try(eval(data_subcommand))
    if (inherits(whatever_the_data_is, "try-error")) {
      return(paste("Data table", deparse(data_subcommand), "not found."))
    }
  }

  P <- check_names_in_data(command, whatever_the_data_is)
  P <- c(P, check_aes_form(command, command_str,
                           whatever_the_data_is))

  # Now check that all the named arguments
  # other than data= are *not* in the data
  other_named <- which( ! arg_names %in% c("data", ""))

  bad <- NULL
  for (k in other_named) {
    vname <- as.character(command[[k+1]])
    dnames <- names(whatever_the_data_is)
    if (vname %in% dnames)
      bad <- c(bad, paste("Aesthetic", arg_names[k],
                          "should be constant,  not variable:", vname))
  }
  P <- c(P, bad)

  # Are the aesthetic names right?
  the_aesthetics <- arg_names[other_named]

  bad <- the_aesthetics[ ! the_aesthetics %in% plausible_aesthetics]
  if (length(bad) > 0) {
    message <- paste0("Are you sure these are aesthetics? ",
                      bad,
                      collapse = ",")
    P <- c(P, message)
  }


  return(P)
}
#============================
check_arguments <-
  function(verb_name=NULL, command, command_str, .data, interact) {
    fun <- switch(verb_name,
           "filter"  = check_filter,
           "select"  = check_select,
           "arrange" = check_arrange,
           "group_by"= check_group_by,
           "rename"  = check_rename,
           "tally"   = check_tally,
           "summarise" = check_summarise,
           "summarize" = check_summarise,
           "inner_join"= check_join,
           "left_join" = check_join,
           "anti_join" = check_join,
           "semi_join" = check_join,
           "full_join" = check_join,
           "mutate"    = check_mutate,
           "transform" = check_mutate,
           "ggplot"    = check_ggplot, # should it be check_layer???
           "geom_point"= check_layer,
           "geom_line" = check_layer,
           "geom_path" = check_layer,
           "geom_density" = check_layer,
           "geom_bar"  = check_layer,
           "geom_boxplot" = check_layer,
           "geom_histogram" = check_layer,

           NULL
           )
    if(is.null(fun)) {
      return(paste0("No TRY capability for checking ",
                    verb_name, "() function"))
    }

    fun(command, command_str, .data, interact)
}
