#' Launch the Rcade App
#'
#' Launches the full Shiny app for the Rcade package, including game navigation and modules.
#'
#' @return No return value. This function launches the Shiny app.
#' @export
#' @import shiny
launch_app <- function() {
 
  ui <- navbarPage(
    "Game Hub", id = "tabs",
    tabPanel("Home",
             fluidPage(
               h2("Choose a game"),
               actionButton("go_tic", "Tic Tac Toe"),
               actionButton("go_nonogram", "Nonogram")
             )
    ),
    tabPanel("Tic Tac Toe", ticTacToeUI("tic1")),
    tabPanel("Nonogram",     nonogramUI("non1"))
  )
  
  server <- function(input, output, session) {
    observeEvent(input$go_tic,      updateTabsetPanel(session, "tabs", selected = "Tic Tac Toe"))
    observeEvent(input$go_nonogram, updateTabsetPanel(session, "tabs", selected = "Nonogram"))
    
    ticTacToeServer("tic1")
    nonogramServer("non1")
  }
  
  shinyApp(ui, server)
}
