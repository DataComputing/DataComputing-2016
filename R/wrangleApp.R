#' Help with dplyr command construction
#'
#' \code{wrangleApp()} opens an interactive (shiny) app that lets you edit
#' interactively a
#' potentially complex \code{dplyr} chain of commands. It provides documentation
#' and argument vetting.
#'
#' On return, \code{wrangleApp()} opens an editor to display and run the command.  Often,
#' the user will cut and paste this into a chunk in an Rmd document.
#'
#'
#' @rdname wrangleApp
#' @param Input  A data frame or piped input from a chain of \code{dplyr} commands.
#'
#' @examples
#' \dontrun{require(mosaicData)
#' wrangleApp( KidsFeet %>% select( sex, length, width, biggerfoot) )
#' }


#' @export

wrangleApp <- function( Input ) {
  # Grab the name of the input.
  nameOfInputDF <- as.character(substitute( Input ))
  # Format the input object as the first part of the assembled command
  startingCommand <- gsub("%>%", "%>%\n", nameOfInputDF )
  # The data frame that the constructed command will work on
  Din <- eval( parse(text=startingCommand) )  # just for initialization of UI
  possibleDisplayVars <- NULL

  # Clean up formatted quotes in strings turned to arguments
  fix_quotes <- function( S ) {
    # clean up fancy quotes, etc. in the command
    return( chartr("“‘’”", "\"''\"", S ) )
  }
  # Describe the verb:
  # D is description, A is whether assignments are allowed for non-formals
  # mustbe describes any required arguments
  verbDescription <- list(
    arrange=list(D="Changes the order of cases in the data table.",
                 A=FALSE,mustbe=character(0)),
    filter=list(D="Removes cases from a data table.",
                A=FALSE,mustbe=character(0)),
    group_by=list(D="Modifier for following verb to do the operation by groups.",
                A=FALSE,mustbe=character(0)),
    ungroup=list(D="Removes any grouping.",A=FALSE,mustbe="data.frame"),
    left_join=list(D="Combines two tables keeping all in LEFT.  If there is no match to a case in RIGHT, the case is retained in the output with NA for the new fields added by RIGHT.",
                   A=FALSE,mustbe="data.frame"),
    inner_join=list(D="Combines two tables, keeping all in LEFT that have a match in RIGHT",
                A=FALSE,mustbe="data.frame"),
    mutate=list(D="Create or modify variables from existing variables.",
                A=TRUE,mustbe=character(0)),
    transmute=list(D="Create or modify variables (like <code>mutate()</code> but drops other variables.",
                   A=TRUE, mustbe=character(0)),
    rename=list(D="Rename one or more variables",
                A=TRUE,mustbe=character(0)),
    select=list(D="Choose which columns to keep",
                A=FALSE,mustbe=character(0)),
    summarise=list(D="Reduce the table into the specified groups.",
                A=TRUE,mustbe=character(0)),
    tally=list(D="Quick count of the number of cases in each group.",
                A=FALSE,mustbe=character(0)),
    NONE=list(D="Editor is now clear.  Select your verb.",
                A=FALSE,mustbe=character(0))
  )
  verbList <- c(names(verbDescription))
  joinDescription <- c(" ", "Joining combines two tables (conventionally called LEFT and RIGHT).",
                  "The LEFT table will be the input from the previous commands and the RIGHT table **must** be given in the first argument.",
                  "The combining is done by matched cases, where matching is determined by any columns the LEFT and RIGHT tables have in common.",
                  " ",
                  "To specify specific variables by which to match, use the <code>by=</code> argument. This should be a character vector of variable names.",
                  " ",
                  "To join by different variables in LEFT and RIGHT,  use a named vector. For example, <code>by = c(\"a\" = \"b\")</code> will look for matches between LEFT's a to RIGHT's b.",
                  "Common join functions:",
                  "    inner_join()",
                  "    left_join()",
                  "    semi_join()",
                  "    anti_join()",
                  "    merge()",
                  "ADD THE EXPLANATIONS OF THESE.",
                  "ALSO, arrange to check when factor and character variables",
                  "are being matched, or two factors with different levels.")

  commonArgsText = list(
    arrange = c("The variables by which to arrange, or a function of those variables",
                "<code>desc(var)</code> --- put in descending order",
                "<code>var1, var2</code> --- arrange by var1, break ties with var2"),
    filter = c("One or more conditions involving the variables",
               "<code>sex=='F'</code> --- pass through cases where sex is 'F'",
               "<code>age > 18</code> --- pass through cases where age is greater than 18",
               "Other comparisons: <code>!=</code>, <code>&#60;=</code>",
               "<code>sex=='F', age > 18</code> --- Multiple arguments: all must be true.",
               "<code>sex=='F' | age > 18</code> --- Within one arg.: either is true",
               "<b>String matching</b>",
               "<code>grepl( </code> regex, var <code>)</code>",
               "<b>First few, biggest, smallest</b>",
               "<code>row_number( ) < 3</code> --- the first two",
               "<code>rank( age ) == 1</code> --- the biggest one "
    ),
    left_join = c( "Join two tables.",
                   "Retains all cases from LEFT, inserting NAs in variables from RIGHT",
                   "when there is not a match in RIGHT."),
    inner_join = c( "Join two tables.",
                    "Returns all rows from LEFT where there are matching values in RIGHT, and all columns from LEFT and RIGHT. ",
                    "If there are multiple matches between LEFT and RIGHT, all combination of the matches are returned.", joinDescription ),
    group_by = c("Set grouping variables for the output data frame.",
                 "Arguments: names of variables to group by.",
                 "These variables will be retained in the output, others will be eliminated."),
    ungroup = c("Removes grouping from a data frame. Doesn't otherwise change anything.",
                "No arguments are required."),
    mutate = c("Construct new variables from old.",
               "Arguments: always have this form <newVarName>=<some calculation>",
               "Example: <code>area=length*width, heightCM=2.54*heightInches</code>",
               "Using an existing variable on the LHS of <code>=</code> will modify that variable.",
               "All other variables are retained."),
    transmute = c("Construct new variables from old like <code>mutate()</code>.",
                  "Arguments: always have this form <newVarName>=<some calculation>",
                  "Example: <code>area=length*width, heightCM=2.54*heightInches</code>",
                  "Using an existing variable on the LHS of <code>=</code> will modify that variable.",
                  "Unlike <code>mutate()</code>, all other variables are deleted."),
    rename = c("For each variable to be renamed, give an argument of the form",
               "<em>new_name<code>=</code>original_name</em>.",
               "Example:",
               "<code>age=Survey_Participant_Age, politics=Survey_Political_Affiliation</code>"
    ),
    summarise = c("Reduce the cases in a table to one case per specified group.",
                  "Typically, you want to use <code>group_by</code> before ",
                  "<code>summarise()</code>",
                  "Example: with input <code>KidsFeet %>% group_by( sex)",
                  "<code>summarise( mean(length, na.rm=TRUE)</code>"),
    tally = c("Do a quick count by existing groups.",
              "Typically used after <code>group_by()</code>",
              "No arguments needed, just make sure you've already grouped",
              "or you'll get the equivalent of <code>nrow()</code>"),
    select = c("Keep the specified variables, eliminated unspecified variables.",
               "Arguments: names of variables to keep.",
               "A minus sign before the name (e.g. <code>-sex</code>) means to keep all but those specified."),



    lastOne = c("Just a placeholder.  Enter documentation for new commands before this.")
  )

  # The app itself
  # Running App --- see the bottom of this function

  #shinyApp(
    ui = fluidPage(
      tags$head(tags$style(type="text/css",
                           "#arguments {width: 300px;}")),
      tags$head(tags$style(type="text/css",
                           "#description {color: blue;}")),
      tags$head(tags$style(type="text/css",
          "#assembled {font-family: Courier; font-size: 11pt; width: 400pt; height: 50pt;}")),
      tags$head(tags$style(type="text/css",".error {color: red;}")),
      tags$head(tags$style(type="text/css", "#outTable {font-size: 8pt;}")),
      tags$head(tags$style(type="text/css", "#inTable {font-size: 8pt;}")),
      tags$head(tags$style(type="text/css", "#runIt {margin-top: 22px;}")),
      tags$head(tags$style(type="text/css", "#acceptCurrentCommand {margin-top: 22px;}")),
      tags$head(tags$style(type="text/css", "#allDone {margin-top: 22px;}")),
      # Results section
      fluidRow(
        column(4,
               p("Process starts with ... "),
               HTML(paste0(
                 '<textarea id="assembled" rows="5" cols="80">',
                 startingCommand,
                 '</textarea>')
                 )
        )
      ),
      tabsetPanel(
        id="inputTabs", position="left",
        ## First panel
        tabPanel( "Output from above commands",
                  tableOutput('inTable')
        ),
        ## Second panel
        tabPanel( "Display controls",
                  fluidRow(
        column(3,
               p(" "),
               wellPanel(
                 selectInput("inVars", "Show these variables:",
                             multiple=TRUE,
                             choices=(possibleDisplayVars <<- names(Din)),
                             selected=names(Din)[1:pmin(8,ncol(Din))]
                 ),
                 sliderInput("showN","Lines to show",
                             value=5, min=1, max=100, step=1
                 )
               )
        )))


      ),
      htmlOutput("inputGroups"),
      # The command editor ==============================
      wellPanel(
        fluidRow(
          column(2,
                 selectInput(
                   "verb",
                   "Data Verb",choices=verbList)
          ),
          column(4,
                 textInput(
                   "arguments",
                   "Arguments",value="")
          ),
          column(1,
                 actionButton(
                   "runIt", "Test")
 #                  HTML("<b>Try it!</b>"))
          ),
          column(1, actionButton(
            "acceptCurrentCommand",
            "Accept"
          )),
          column(1, actionButton("allDone","Quit"))
        ),
        fluidRow( textOutput( "description"),htmlOutput("warnings") )

      ),
      tabsetPanel(id="helpTabs", position="left",
        # You can change the <title> of the tabs, but not the <value>
        # which is the handle by which the server references the tab.
        tabPanel( title="Result",
                  value="Result", # Don't change this
                  htmlOutput("outputGroups"),
                  HTML("<small>"),
                  tableOutput('outTable'),
                  HTML("</small>")),
        tabPanel( title="Hints",
                  value="Hints", # Don't change this
                  htmlOutput("commonArgs")),
        tabPanel( title="Development", p("New features can be tested here."))


        )
    ) # end of ui
    server = function( input, output, session ) {
      outTable <- NULL # just a placeholder
      # The input table
      #       output$inTable = renderDataTable(
      #         Din[input$inVars],
      #         options = list(
      #           lengthMenu = c(1,2,5,10,25,50,100,500,1000),
      #           pageLength = 2
      #         )
      #       )
      output$inTable <- renderTable({
        # Need to convert to data.frame to avoid problems with renderTable()
        # on grouped_df
        Tmp <- head( getDin()[input$inVars], input$showN )
        gps <- groups( Tmp )
        message <-
          if (is.null( gps)  ) "No grouping variables in input table."
          else paste("Input table grouped by:", paste(gps, collapse=" & ") )
        output$inputGroups <- renderText( HTML( message ))
        data.frame( Tmp  )
      })
      # Hints  ===============
      output$description <- renderText({
        descript <- verbDescription[[input$verb]]$D
        if( length(descript) == 0 )
          return( "No description available.  Talk to the developer to fix this.")
        else return( paste0(input$verb, ": ",descript) )
      })
      # add currently edited command to accumulated commands
      observe({
        input$acceptCurrentCommand # the dependency
        isolate({
          if (input$acceptCurrentCommand > 0 ) {
            # Get the current contents
            current <- fix_quotes( isolate( input$assembled ) )
            newText <- paste( current, "%>%\n",
                              theNewCommand(),
                              collapse="" )
            # add them to the "assembled" statement
            updateTextInput( session, "assembled", value=newText )
            # clean the editing buffer in preparation for the next step.
            # updateSelectInput( session, "verb", selected="NONE")
            # above is commented out so that the verb remains the same
            # no point changing something that the user might not notice
            updateTextInput( session, "arguments", value="" )
          }
        })
      })
      # Show the Hints tab when a new verb is selected
      observe({
        # When the verb is changed, show the appropriate hints on use
        input$verb # for the dependency
        updateTabsetPanel( session, inputId="helpTabs", selected="Hints")
      })


      output$commonArgs <- renderText({
        descript <- commonArgsText[[input$verb]]
        if( length(descript) == 0 ) {
          return( "No examples available.  Talk to the developer to fix this.")
        }
        else {
          return( HTML(c(paste0("<h5>Help with ",input$verb,"()</h5>"),
                         paste(descript,collapse="<br/>") ) ) )
        }
      })

      # Get the data that comes from the end of the collected statement:
      getDin <- reactive({
        TmpDF <- try(
          eval.parent( parse(text=fix_quotes( input$assembled ) ), n=2 )
        )
        if (inherits(TmpDF, "try-error")) {
          warning("Error in the assembled commands.  Did you edit them?")
          return( data.frame() ) # an empty data frame.
        }
        # if the variable names have changed, fix the input variables
        currentDisplayVars <- isolate(input$inVars)
        notDisplayedNow <-
          possibleDisplayVars[ ! possibleDisplayVars %in% currentDisplayVars ]
        varsInInput <- names(TmpDF)
        # Keep selected any all from Din that were not on the exclude list
        keepTheseSelected <- varsInInput[ ! varsInInput %in% notDisplayedNow ]
        # Update the widget
        updateSelectInput( session, "inVars",
                           choices=possibleDisplayVars <<- varsInInput,
                           selected=keepTheseSelected)
        return( TmpDF)
      })

      # Format the currently edited statement as a complete command
      theNewCommand <- reactive({
        verb <- input$verb
        # Customization for special verbs
        if (verb=="NONE")
          command <- paste0( fix_quotes(input$arguments) )
        else
          command <- paste0( verb, "( ",fix_quotes(input$arguments), " )")
        return( command )
      })
      # On stop, return the assembled command
      observe({
        if( input$allDone > 0 )
          stopApp(fix_quotes(input$assembled) )
      })

      # Test the newly formed command and display any errors
      observe({
        input$runIt # for the dependency
        currentArgs <- fix_quotes(isolate( input$arguments )) # for the dependency
        thisVerb <- isolate( input$verb )
        # First, check the arguments


        commandForTesting <- theNewCommand( )

        if (currentArgs == "") return(NULL)

        check <-
          checkDplyrArgs( currentArgs,
                          thisVerb,
                          getDin(),
                          mustBe=verbDescription[[thisVerb]]$mustbe,
                          assignOK=verbDescription[[thisVerb]]$A )


        statusMessage <- formatDplyrCheck( check )
        if (nchar(statusMessage) == 0 ) statusMessage <- "OK."
        output$warnings <- renderText({
          HTML( statusMessage )
        })
        # Second, test the whole command: does each argument parse?
        # NOT YET IMPLEMENTED
      })

      # Run the newly formed command and display the results
      observe({
        input$runIt
        isolate({ # Observer should run only when input$runIt changes.
          Tmp <- getDin( ) # runs accumulated commands to produce input to new command


          # Run the new command, using the input
          if (input$runIt == 0 || nrow( Tmp ) == 0 ) { #nothing to show
            Result <- data.frame()
          } else {
            fullCommand <- paste0("Tmp %>% ", theNewCommand() )
            command <- try( parse( text=fullCommand ) )
            if (inherits( command, "try-error ")) {
              output$warnings <- renderText(
                HTML( paste("<div class='error'>Error in the statement<br />",
                            gsub("^.+ : ","",errMessage), "</div>") )
              )
            } else {
              Result <- try( eval( command, envir=Tmp ) )
              if (inherits(Result, "try-error")) {
                # The command was not executable
                errMessage <- geterrmessage()
                output$warnings <- renderText(
                  HTML( paste("<div class='error'>Error in the statement<br />",
                              gsub("^.+ : ","",errMessage), "</div>") )
                )
                Result <- data.frame() # make output empty
              }
            }
          }

          output$outTable <- renderTable({
            # Display how the table is grouped.
            gps <- groups( Result )
            message <-
              if (is.null( gps)  ) "No grouping variables."
              else paste("Grouped by:", paste(gps, collapse=" & ") )


            output$outputGroups <- renderText( HTML(message) )
            head(
              data.frame( Result ),
              input$showN
            )
          })
          if ( ! identical(Result, data.frame()) ) {
            updateTabsetPanel( session, inputId="helpTabs", selected="Result")
          }
        })
      })
      # Translating input to output, parsing command, etc.
      getOutTable <- reactive({
        if (input$runIt == 0) return(data.frame()) # initialization

        # Check to see if the command has errors.
        isCommandOK <- testCommand( )
        # immediateInput should be the result of that.
        Tmp <- getDin()

        if( nrow(Tmp) == 0 ||
              commandForTesting=="" ||
              grepl( "Error:", statusMessage )) {
          # there's nothing in Tmp or verb was NONE or error in arguments
          return( Tmp)
        } else {
          fullCommand <- paste0("Tmp %>% ", commandForTesting )
          command <- parse( text=fullCommand )
          Result <- eval( command, envir=immediateInput )
          return( Result )
        }
      })

      # The output table =====================
      #       output$outDataTable <- renderDataTable(
      #         getOutTable(),
      #         options = list(
      #           lengthMenu = c(1,2,5,10,25,50,100,500,1000),
      #           pageLength = 2
      #         )
      #       )
#       output$outTable <- renderTable({
#         # Convert to data.frame to avoid problems with renderTable()
#         # on grouped_df
#         Tmp <- head( getOutTable(), input$showN )
#         data.frame( Tmp )
#      })
    } # end of server
#  ) # end of shinyApp()
#  rstudio::viewer("127.0.0.1")
  resultString <- runApp( list(ui=ui,server=server))
  tmpName <- tempfile( pattern="commandEdit", fileext=".R")
  fileCon <- file( tmpName )
  writeLines( resultString, fileCon)
  close( fileCon )
  file.edit( tmpName )
} # end of function



