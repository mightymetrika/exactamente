#' Run the Exactamento Shiny App
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
exactamento_app <- function() {

  # User Interface
  ui <- shiny::fluidPage(
    shiny::titlePanel("Exactamento - Bootstrap Methods"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::radioButtons("method", "Choose method:",
                            c("Exact", "Regular", "Exact & Regular")),
        shiny::numericInput("n_bootstraps", "Number of bootstraps:", value = 10000, min = 1),
        shiny::textAreaInput("data", "Enter data (numeric vector):", "rnorm(5)"),
        shiny::checkboxInput("check_size", "Check size:", value = TRUE),
        shiny::textInput("anon", "Statistic function:", "mean"),
        shiny::numericInput("lb", "Lower bound for HDI:", value = 0.025),
        shiny::numericInput("ub", "Upper bound for HDI:", value = 0.975),
        shiny::actionButton("run", "Run"),
        shiny::verbatimTextOutput("data_warning"),
        shiny::verbatimTextOutput("bootstrap_warning"),
        shiny::verbatimTextOutput("function_warning"),
        shiny::verbatimTextOutput("HDI_warning")
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

    HDI_check <- shiny::reactive({
      if(!is.numeric(input$lb) || !is.numeric(input$ub) || input$lb < 0 || input$ub > 1 || input$lb >= input$ub) {
        return("Please enter valid values for HDI bounds (lb < ub, both between 0 and 1).")
      } else {
        return(NULL)
      }
    })

    output$data_warning <- shiny::renderText(data_check())
    output$bootstrap_warning <- shiny::renderText(bootstrap_check())
    output$function_warning <- shiny::renderText(function_check())
    output$HDI_warning <- shiny::renderText(HDI_check())

    shiny::observeEvent(input$run, {

      # Check if inputs are valid
      if(!is.null(data_check()) || !is.null(bootstrap_check()) || !is.null(function_check()) || !is.null(HDI_check())) {
        return()  # Skip rest of the code if any input is not valid
      }

      # Parse user inputs
      data <- eval(parse(text = input$data))
      anon <- eval(parse(text = paste0("function(x)(", input$anon,"(x))")))

      # Run chosen method and generate outputs
      if(input$method == "Exact") {
        result <- exact_bootstrap(data, input$n_bootstraps, input$check_size, anon)
        output$summary_table <- shiny::renderTable(bootsummer(result, input$lb, input$ub))
        output$plot <- shiny::renderPlot(boot_plot(result, "Exact Bootstrap Distribution"))
      } else if(input$method == "Regular") {
        result <- reg_bootstrap(data, input$n_bootstraps, anon)
        output$summary_table <- shiny::renderTable(bootsummer(result, input$lb, input$ub))
        output$plot <- shiny::renderPlot(boot_plot(result, "Regular Bootstrap Distribution"))
      } else if(input$method == "Exact & Regular") {
        result <- e_vs_r(data, input$n_bootstraps, input$check_size, anon, input$lb, input$ub)
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

# exactamento_app <- function() {
#
#   # User Interface
#   ui <- shiny::fluidPage(
#     shiny::titlePanel("Exactamento - Bootstrap Methods"),
#     shiny::sidebarLayout(
#       shiny::sidebarPanel(
#         shiny::radioButtons("method", "Choose method:",
#                             c("Exact", "Regular", "Exact & Regular")),
#         shiny::numericInput("n_bootstraps", "Number of bootstraps:", value = 10000, min = 1),
#         shiny::textAreaInput("data", "Enter data (numeric vector):", "rnorm(5)"),
#         shiny::checkboxInput("check_size", "Check size:", value = TRUE),
#         shiny::textInput("anon", "Statistic function:", "mean"),
#         shiny::numericInput("lb", "Lower bound for HDI:", value = 0.025),
#         shiny::numericInput("ub", "Upper bound for HDI:", value = 0.975),
#         shiny::actionButton("run", "Run"),
#         shiny::verbatimTextOutput("data_warning")
#       ),
#       shiny::mainPanel(
#         shiny::tableOutput("summary_table"),
#         shiny::plotOutput("plot")
#       )
#     )
#   )
#
#   # Server logic
#   server <- function(input, output) {
#
#     data_check <- shiny::reactive({
#       # Evaluate the input string
#       data <- try(eval(parse(text = input$data)), silent = TRUE)
#
#       # Check if data is a valid numeric vector, and show a warning message if not
#       if(inherits(data, "try-error") || !is.numeric(data)) {
#         return("Please enter a valid R numeric vector.")
#       } else {
#         return(NULL)
#       }
#     })
#
#     output$data_warning <- shiny::renderText(data_check())
#
#     shiny::observeEvent(input$run, {
#
#       # Check if data is valid
#       if(!is.null(data_check())) {
#         return()  # Skip rest of the code if data is not valid
#       }
#
#       # Parse user inputs
#       data <- eval(parse(text = input$data))
#       anon <- eval(parse(text = paste0("function(x)(", input$anon,"(x))")))
#
#       # Run chosen method and generate outputs
#       if(input$method == "Exact") {
#         result <- exact_bootstrap(data, input$n_bootstraps, input$check_size, anon)
#         output$summary_table <- shiny::renderTable(bootsummer(result, input$lb, input$ub))
#         output$plot <- shiny::renderPlot(boot_plot(result, "Exact Bootstrap Distribution"))
#       } else if(input$method == "Regular") {
#         result <- reg_bootstrap(data, input$n_bootstraps, anon)
#         output$summary_table <- shiny::renderTable(bootsummer(result, input$lb, input$ub))
#         output$plot <- shiny::renderPlot(boot_plot(result, "Regular Bootstrap Distribution"))
#       } else if(input$method == "Exact & Regular") {
#         result <- e_vs_r(data, input$n_bootstraps, input$check_size, anon, input$lb, input$ub)
#         output$summary_table <- shiny::renderTable(result$summary_table)
#         output$plot <- shiny::renderPlot(print(result$comp_plot))
#       }
#
#     })
#   }
#
#   # Run the application
#   shiny::shinyApp(ui = ui, server = server)
# }
