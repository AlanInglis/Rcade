#' Space Invaders UI
#'
#' @param id Shiny module ID
#' @return UI for Space Invaders game
#' @export
spaceInvadersUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
        body { background-color: black; }
        .grid-cell {
          width: 30px;
          height: 30px;
          border: 1px solid #333;
          display: inline-block;
          margin: 0;
          padding: 0;
        }
        .player  { background-color: #00ffcc; }
        .enemy   { background-color: #ff3333; }
        .bullet  { background-color: #ffff00; }
        .empty   { background-color: black; }
        .grid-row { line-height: 0; }
      "))
    ),
    h3("Space Invaders", style = "text-align: center; color: #00ffcc;"),
    h4(textOutput(ns("level_text")), style = "text-align: center; color: #ccc;"),
    h4(textOutput(ns("score_text")), style = "text-align: center; color: white;"),
    h4(textOutput(ns("gameover_text")), style = "text-align: center; color: red; font-weight: bold;"),
    h4(textOutput(ns("win_text")), style = "text-align: center; color: lime; font-weight: bold;"),
    uiOutput(ns("game_grid")),
    br(),
    div(style = "text-align: center;",
        actionButton(ns("left"), "◀"),
        actionButton(ns("fire"), "🔺"),
        actionButton(ns("right"), "▶"),
        actionButton(ns("next_level"), "➡️ Next Level"),
        actionButton(ns("reset"), "🔁 Reset")
    )
  )
}

#' Space Invaders Server
#'
#' @param id Shiny module ID
#' @export
spaceInvadersServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    grid_rows <- 10
    grid_cols <- 15
    
    state <- reactiveValues(
      player_col = 8,
      enemies = matrix(FALSE, nrow = grid_rows, ncol = grid_cols),
      bullet = NULL,
      enemy_direction = 1,
      game_over = FALSE,
      win = FALSE,
      score = 0,
      level = 1
    )
    
    initialise_game <- function() {
      state$player_col <- 8
      state$bullet <- NULL
      state$enemy_direction <- 1
      state$game_over <- FALSE
      state$win <- FALSE
      
      state$enemies <- matrix(FALSE, nrow = grid_rows, ncol = grid_cols)
      
      if (state$level == 1) {
        state$enemies[1, ] <- sample(c(TRUE, FALSE), grid_cols, replace = TRUE, prob = c(0.25, 0.75))
      } else if (state$level == 2) {
        state$enemies[1:2, ] <- sample(c(TRUE, FALSE), 2 * grid_cols, replace = TRUE, prob = c(0.25, 0.75))
      } else if (state$level >= 3) {
        state$enemies[1:3, ] <- sample(c(TRUE, FALSE), 3 * grid_cols, replace = TRUE, prob = c(0.25, 0.75))
      }
    }
    
    observeEvent(input$reset, {
      state$score <- 0
      state$level <- 1
      initialise_game()
    })
    
    observeEvent(input$next_level, {
      if (state$level < 3) state$level <- state$level + 1
      initialise_game()
    })
    
    output$level_text <- renderText({
      paste("Level:", state$level)
    })
    
    render_grid <- function() {
      lapply(1:grid_rows, function(row) {
        div(class = "grid-row",
            lapply(1:grid_cols, function(col) {
              cell_class <- "empty"
              if (!is.null(state$bullet) && state$bullet$row == row && state$bullet$col == col) {
                cell_class <- "bullet"
              } else if (state$enemies[row, col]) {
                cell_class <- "enemy"
              } else if (!state$game_over && !state$win && row == grid_rows && col == state$player_col) {
                cell_class <- "player"
              }
              div(class = paste("grid-cell", cell_class))
            })
        )
      })
    }
    
    output$game_grid <- renderUI({
      tagList(render_grid())
    })
    
    output$score_text <- renderText({
      paste("Score:", state$score)
    })
    
    output$gameover_text <- renderText({
      if (state$game_over) "GAME OVER" else ""
    })
    
    output$win_text <- renderText({
      if (state$win) "YOU WIN!" else ""
    })
    
    observeEvent(input$left, {
      if (!state$game_over && !state$win && state$player_col > 1) state$player_col <- state$player_col - 1
    })
    
    observeEvent(input$right, {
      if (!state$game_over && !state$win && state$player_col < grid_cols) state$player_col <- state$player_col + 1
    })
    
    observeEvent(input$fire, {
      if (!state$game_over && !state$win && is.null(state$bullet)) {
        state$bullet <- list(row = grid_rows - 1, col = state$player_col)
      }
    })
    
    observe({
      invalidateLater(100)
      isolate({
        if (state$game_over || state$win) return()
        if (!is.null(state$bullet)) {
          br <- state$bullet$row
          bc <- state$bullet$col
          if (br <= 1) {
            state$bullet <- NULL
          } else if (state$enemies[br - 1, bc]) {
            state$enemies[br - 1, bc] <- FALSE
            state$bullet <- NULL
            state$score <- state$score + 100
          } else {
            state$bullet$row <- br - 1
          }
        }
        
        if (all(!state$enemies)) {
          if (state$level < 3) {
            state$level <- state$level + 1
            initialise_game()
          } else {
            state$win <- TRUE
          }
        }
      })
    })
    
    observe({
      invalidateLater(1000)
      isolate({
        if (state$game_over || state$win) return()
        
        enemies <- state$enemies
        dir <- state$enemy_direction
        
        cols_with_enemies <- which(colSums(enemies) > 0)
        if (length(cols_with_enemies) == 0) return()
        min_col <- min(cols_with_enemies)
        max_col <- max(cols_with_enemies)
        
        if ((dir == 1 && max_col >= grid_cols) || (dir == -1 && min_col <= 1)) {
          enemies <- rbind(matrix(FALSE, 1, grid_cols), enemies[-grid_rows, ])
          state$enemy_direction <- -dir
        } else {
          shifted <- matrix(FALSE, nrow = grid_rows, ncol = grid_cols)
          if (dir == 1) {
            shifted[, 2:grid_cols] <- enemies[, 1:(grid_cols - 1)]
          } else {
            shifted[, 1:(grid_cols - 1)] <- enemies[, 2:grid_cols]
          }
          enemies <- shifted
        }
        
        state$enemies <- enemies
        
        if (any(enemies[grid_rows, ])) {
          state$game_over <- TRUE
        }
      })
    })
    
    observeEvent(TRUE, {
      initialise_game()
    }, once = TRUE)
  })
}