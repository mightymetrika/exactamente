#' exactamente_app: Interactive Shiny App for Exploring Bootstrap Methods
#'
#' Launches a Shiny app to interactively explore and compare Exact and Regular
#' bootstrap methods and All. The app provides a user-friendly interface, allowing
#' users to modify various parameters associated with each bootstrap method,
#' visualize the results, and understand the differences between the methods.
#'
#' @export
#'
#' @return A shinyApp object representing the running Shiny application. This function
#' is primarily called for its side effects, which include the launching and running
#' of the Shiny app.
#'
#' @examples
#' if(interactive()){
#'   exactamente_app()
#' }
exactamente_app <- function() {

  # User Interface
  ui <- shiny::fluidPage(
    theme = shinythemes::shinytheme("cerulean"),
    shiny::titlePanel("Explore Bootstrap Methods"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::numericInput("n_bootstraps", "Number of bootstraps:", value = 10000, min = 1),
        shiny::textAreaInput("data", "Enter data (numeric vector):", "rnorm(5)"),
        shiny::checkboxInput("check_size", "Check size:", value = TRUE),
        shiny::textInput("anon", "Statistic function:", "mean"),
        shiny::textInput("density_args", "Arguments for density function:", "bw = 'nrd0', kernel = 'gaussian'"),
        shiny::numericInput("lb", "Lower bound for CI:", value = 0.025),
        shiny::numericInput("ub", "Upper bound for CI:", value = 0.975),
        shiny::actionButton("run", "Run"),
        shiny::verbatimTextOutput("data_warning"),
        shiny::verbatimTextOutput("bootstrap_warning"),
        shiny::verbatimTextOutput("function_warning"),
        shiny::verbatimTextOutput("CI_warning")
      ),
      shiny::mainPanel(
        shiny::uiOutput("table_title"),  # output for table title
        shiny::tableOutput("summary_table"),
        shiny::uiOutput("plot_title"),  # output for plot title
        shiny::plotOutput("plot")
      )
    )
  )

  # Server logic
  server <- function(input, output) {

    data_check <- shiny::reactive({
      # Evaluate the input string
      data <- try(eval(parse(text = input$data)), silent = TRUE)

      # Check if data is a valid numeric vector, and show a warning message if not
      if(inherits(data, "try-error") || !is.numeric(data)) {
        return("Please enter a valid R numeric vector.")
      } else if(any(is.na(data))) {
        return("Data cannot contain missing values.")
      } else {
        return(NULL)
      }
    })

    bootstrap_check <- shiny::reactive({
      if(!is.wholenumber(input$n_bootstraps) || input$n_bootstraps < 2) {
        return("Please enter a positive integer greater than 1 for number of bootstraps.")
      } else {
        return(NULL)
      }
    })

    function_check <- shiny::reactive({
      # Try to evaluate the user's input as a function, if it fails, try to append "function(x)" before the input
      fun <- try(eval(parse(text = input$anon)), silent = TRUE)
      if(inherits(fun, "try-error")) {
        fun <- try(eval(parse(text = paste0("function(x)(", input$anon,"(x))"))), silent = TRUE)
      }
      # Check if the evaluated input is a function
      if(!inherits(fun, "try-error") && is.function(fun)) {
        # Test if the function returns a valid numeric result
        data <- try(eval(parse(text = input$data)), silent = TRUE)
        if(inherits(data, "try-error") || !is.numeric(data)) {
          return(NULL)
        }
        result <- try(fun(data), silent = TRUE)
        if(inherits(result, "try-error") || any(is.nan(result)) || any(is.infinite(result))) {
          return("The provided statistic function creates NaN or Inf values. Please modify the function.")
        }
        return(NULL)
      } else {
        return("Please enter a valid R function.")
      }
    })

    CI_check <- shiny::reactive({
      if(!is.numeric(input$lb) || !is.numeric(input$ub) || input$lb < 0 || input$ub > 1 || input$lb >= input$ub) {
        return("Please enter valid values for CI bounds (lb < ub, both between 0 and 1).")
      } else {
        return(NULL)
      }
    })

    density_args_check <- shiny::reactive({
      args <- try(eval(parse(text = paste0("list(", input$density_args, ")"))), silent = TRUE)
      if(inherits(args, "try-error")) {
        return("Please enter valid arguments for density function.")
      } else {
        return(NULL)
      }
    })

    output$data_warning <- shiny::renderText(data_check())
    output$bootstrap_warning <- shiny::renderText(bootstrap_check())
    output$function_warning <- shiny::renderText(function_check())
    output$CI_warning <- shiny::renderText(CI_check())
    output$density_args_warning <- shiny::renderText(density_args_check())

    shiny::observeEvent(input$run, {

      # Check if inputs are valid
      if(!is.null(data_check()) || !is.null(bootstrap_check()) || !is.null(function_check()) || !is.null(CI_check()) || !is.null(density_args_check())) {
        return()  # Skip rest of the code if any input is not valid
      }

      # Parse user inputs
      data <- eval(parse(text = input$data))
      anon <- try(eval(parse(text = input$anon)), silent = TRUE)
      if(inherits(anon, "try-error")) {
        anon <- eval(parse(text = paste0("function(x)(", input$anon,"(x))")))
      }
      density_args <- eval(parse(text = paste0("list(", input$density_args, ")")))

      # Run both "Exact" and "Regular" methods
      shiny::validate(
        shiny::need(tryCatch({
          result <- e_vs_r(data, input$n_bootstraps, input$check_size, anon,
                           input$lb, input$ub, density_args, title = "")
        }, error = function(e) e$message), "An error occurred.")
      )

      output$summary_table <- shiny::renderTable(result$summary_table)
      output$plot <- shiny::renderPlot(print(result$comp_plot))

      output$table_title <- shiny::renderUI({
        shiny::h3("Bootstrap Method Summary", align = "left")
      })

      output$plot_title <- shiny::renderUI({
        shiny::h3("Comparison of Bootstrap Distributions", align = "left")
      })

    })
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}

is.wholenumber <- function(x) {
  # Check if input is numeric (or can be coerced to numeric)
  if (!is.numeric(x)) {
    stop("Input must be numeric.")
  }

  # Check if input is missing (NA)
  if (any(is.na(x))) {
    return(FALSE)
  }

  # Check if input is a whole number
  return(all(x == floor(x)))
}
