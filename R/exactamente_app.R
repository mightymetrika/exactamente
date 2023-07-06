#' Run the Exactamente Shiny App
#'
#' This function starts a Shiny app that lets the user interactively run
#' bootstrap analysis using either exact or regular bootstrapping, or both.
#'
#' @return Nothing; this function is called for its side effect of starting the
#'   Shiny app.
#' @export
#' @examples
#' \dontrun{
#' exactamento_app()
#' }
exactamente_app <- function() {

  # User Interface
  ui <- shiny::fluidPage(
    shiny::titlePanel("Explore Bootstrap Methods"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::radioButtons("method", "Choose method:",
                            c("Exact Case", "Exact", "Regular", "All")),
        shiny::numericInput("n_bootstraps", "Number of bootstraps:", value = 10000, min = 1),
        shiny::textAreaInput("data", "Enter data (numeric vector):", "rnorm(5)"),
        shiny::checkboxInput("check_size", "Check size:", value = TRUE),
        shiny::textInput("anon", "Statistic function:", "mean"),
        shiny::textInput("density_args", "Arguments for density function:", "bw = 0.5, kernel = 'gaussian'"),
        shiny::numericInput("lb", "Lower bound for CI:", value = 0.025),
        shiny::numericInput("ub", "Upper bound for CI:", value = 0.975),
        shiny::actionButton("run", "Run"),
        shiny::verbatimTextOutput("data_warning"),
        shiny::verbatimTextOutput("bootstrap_warning"),
        shiny::verbatimTextOutput("function_warning"),
        shiny::verbatimTextOutput("CI_warning")
      ),
      shiny::mainPanel(
        shiny::tableOutput("summary_table"),
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
      } else {
        return(NULL)
      }
    })

    bootstrap_check <- shiny::reactive({
      if(!is.wholenumber(input$n_bootstraps) || input$n_bootstraps < 1) {
        return("Please enter a positive integer for number of bootstraps.")
      } else {
        return(NULL)
      }
    })

    function_check <- shiny::reactive({
      fun <- try(eval(parse(text = paste0("function(x)(", input$anon,"(x))"))), silent = TRUE)
      if(inherits(fun, "try-error")) {
        return("Please enter a valid R function.")
      } else {
        return(NULL)
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
      anon <- eval(parse(text = paste0("function(x)(", input$anon,"(x))")))
      density_args <- eval(parse(text = paste0("list(", input$density_args, ")")))

      # Run chosen method and generate outputs
      if(input$method == "Exact Case") {
        result <- ecase_bootstrap(data, input$check_size, anon,
                                  input$lb, input$ub, density_args)
        output$summary_table <- shiny::renderTable(summary(result))
        output$plot <- shiny::renderPlot(plot(result, "Exact Case Bootstrap Distribution"))
      } else if (input$method == "Exact") {
        result <- exact_bootstrap(data, input$check_size, anon,
                                  input$lb, input$ub, density_args)
        output$summary_table <- shiny::renderTable(summary(result))
        output$plot <- shiny::renderPlot(plot(result, "Exact Bootstrap Distribution"))
      } else if(input$method == "Regular") {
        result <- reg_bootstrap(data, input$n_bootstraps, anon, input$lb, input$ub,
                                density_args)
        output$summary_table <- shiny::renderTable(summary(result))
        output$plot <- shiny::renderPlot(plot(result, "Regular Bootstrap Distribution"))
      } else if(input$method == "All") {
        result <- e_vs_r(data, input$n_bootstraps, input$check_size, anon,
                         input$lb, input$ub, density_args)
        output$summary_table <- shiny::renderTable(result$summary_table)
        output$plot <- shiny::renderPlot(print(result$comp_plot))
      }

    })
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}

is.wholenumber <- function(x) {
  x == as.integer(x)
}
