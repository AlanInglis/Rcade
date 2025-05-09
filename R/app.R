#' Launch the Rcade App
#'
#' Launches the full Shiny app for the Rcade package, including game navigation and modules.
#'
#' @return No return value. This function launches the Shiny app.
#' @export
#' @import shiny
launch_app <- function() {
  addResourcePath("myimg", "inst/www")
  
  ui <- navbarPage(
    "Game Hub", id = "tabs",
    tabPanel("Home",
             fluidPage(
               tags$head(
                 tags$style(HTML("
            body { background-color: #111; color: #eee; font-family: 'Press Start 2P', cursive; }
            .retro-btn {
              background-color: #ff0066;
              color: white;
              font-size: 16px;
              font-family: 'Press Start 2P', cursive;
              border: 3px solid #00ffcc;
              padding: 15px;
              margin: 10px;
              box-shadow: 4px 4px #000;
              cursor: pointer;
            }
            .retro-btn:hover {
              background-color: #ff3399;
            }
            .game-image {
              width: 100%;
              max-width: 600px;
              margin: 20px auto;
              display: block;
            }
          "))
               ),
               tags$link(
                 href = "https://fonts.googleapis.com/css2?family=Press+Start+2P&display=swap",
                 rel = "stylesheet"
               ),
               img(src = "myimg/Rcade.png", class = "game-image"),
               tags$div(
                 style = "text-align: center; margin-top: -360px;",
                 actionButton("go_tic",      "Tic Tac Toe",     class = "retro-btn"),
                 actionButton("go_nonogram", "Nonogram",        class = "retro-btn"),
                 actionButton("go_cave",     "Cave Adventure",  class = "retro-btn"),
                 actionButton("go_invaders", "Space Invaders",  class = "retro-btn"),
                 actionButton("go_quest",    "R Quest",         class = "retro-btn"),
                 actionButton("go_pong",     "Pong",            class = "retro-btn"),
                 actionButton("go_donkey",   "Donkey Kong",     class = "retro-btn")
               )
             )
    ),
    tabPanel("Tic Tac Toe",    ticTacToeUI("tic1")),
    tabPanel("Nonogram",       nonogramUI("non1")),
    tabPanel("Cave Adventure", caveUI("cave1")),
    tabPanel("Space Invaders", spaceInvadersUI("invaders1")),
    tabPanel("R Quest",        shinyquestUI("quest1")),
    tabPanel("Pong",           play_pong("pong")),
    tabPanel("Donkey Kong",    launch_donkey_kong())
  )
  
  server <- function(input, output, session) {
    observeEvent(input$go_tic,      updateTabsetPanel(session, "tabs", selected = "Tic Tac Toe"))
    observeEvent(input$go_nonogram, updateTabsetPanel(session, "tabs", selected = "Nonogram"))
    observeEvent(input$go_cave,     updateTabsetPanel(session, "tabs", selected = "Cave Adventure"))
    observeEvent(input$go_invaders, updateTabsetPanel(session, "tabs", selected = "Space Invaders"))
    observeEvent(input$go_quest,    updateTabsetPanel(session, "tabs", selected = "R Quest"))
    observeEvent(input$go_pong,     updateTabsetPanel(session, "tabs", selected = "Pong"))
    observeEvent(input$go_donkey,   updateTabsetPanel(session, "tabs", selected = "Donkey Kong"))
    
    observe({
      observeEvent(input$tabs, {
        session$sendCustomMessage("activeTabChanged", input$tabs)
      })
    })
    
    observeEvent(input$tabs, {
      session$sendCustomMessage("pong-state", list(
        playing = (input$tabs == "Pong")
      ))
      session$sendCustomMessage("spaceinvaders-state", list(
        playing = (input$tabs == "Space Invaders")
      ))
    })
    
    ticTacToeServer("tic1")
    nonogramServer("non1")
    caveServer("cave1")
    spaceInvadersServer("invaders1")
    shinyquestServer("quest1")
  }
  
  shinyApp(ui, server)
}

