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
               tags$head(
                 tags$style(HTML("
        body { background-color: #111; color: #eee; font-family: 'Press Start 2P', cursive; }
        h2, h3 { color: #00ffcc; text-shadow: 1px 1px #000; }
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
        .game-icon {
          width: 100px;
          margin: 10px;
        }
      "))
               ),
               tags$link(
                 href = "https://fonts.googleapis.com/css2?family=Press+Start+2P&display=swap",
                 rel = "stylesheet"
               ),
               h2("🎮 Welcome to the Rcade 🎮"),
               tags$div(
                 style = "text-align: center;",
                 img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/3/32/Tetris.svg/1200px-Tetris.svg.png", class = "game-icon"),
                 img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/4/43/Pacman.svg/1200px-Pacman.svg.png", class = "game-icon"),
                 img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/8/88/Space_Invaders.svg/1200px-Space_Invaders.svg.png", class = "game-icon")
               ),
               h3("Choose a game to play:"),
               tags$div(
                 style = "text-align: center;",
                 actionButton("go_tic", "Tic Tac Toe", class = "retro-btn"),
                 actionButton("go_nonogram", "Nonogram", class = "retro-btn"),
                 actionButton("go_cave", "Cave Adventure", class = "retro-btn")
               )
             )
    ),
    tabPanel("Tic Tac Toe", ticTacToeUI("tic1")),
    tabPanel("Nonogram",     nonogramUI("non1")),
    tabPanel("Cave Adventure", caveUI("cave1"))
  )
  
  server <- function(input, output, session) {
    observeEvent(input$go_tic,      updateTabsetPanel(session, "tabs", selected = "Tic Tac Toe"))
    observeEvent(input$go_nonogram, updateTabsetPanel(session, "tabs", selected = "Nonogram"))
    observeEvent(input$go_cave,     updateTabsetPanel(session, "tabs", selected = "Cave Adventure"))
    
    ticTacToeServer("tic1")
    nonogramServer("non1")
    caveServer("cave1")
  }
  
  shinyApp(ui, server)
}
