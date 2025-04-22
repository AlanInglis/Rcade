#' R Quest UI
#'
#' @param id Module ID
#' @export
shinyquestUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
        body { background-color: #111; color: #eee; }
        .grid { display: inline-block; line-height: 0; vertical-align: top; }
        .row { display: block; }
        .cell {
          width: 30px; height: 30px;
          display: inline-block;
          border: 1px solid #333;
          margin: 0; padding: 0;
        }
        .player { background-color: lime; }
        .wall   { background-color: #444; }
        .goal   { background-color: gold; }
        .floor  { background-color: #222; }
        .puzzle { background-color: dodgerblue; }

        .modal-content {
          background-color: white !important;
          color: black !important;
        }
        .modal-title {
          color: black !important;
        }
        .modal-body p, .modal-body pre, .modal-body label {
          color: black !important;
        }
      ")),
      # tags$script(HTML(sprintf("
      # document.addEventListener('keydown', function(e) {
      # if (['ArrowUp', 'ArrowDown', 'ArrowLeft', 'ArrowRight'].includes(e.key)) {
      # Shiny.setInputValue('%s', e.key, {priority: 'event'});
      # }
      # });
      #                          ", 
      #                          paste0(id, '-key_press'))))
      tags$script(HTML(sprintf("
  (function() {
    var inputId = '%s';
    var handler = function(e) {
      if (['ArrowUp', 'ArrowDown', 'ArrowLeft', 'ArrowRight'].includes(e.key)) {
        Shiny.setInputValue(inputId, e.key, {priority: 'event'});
      }
    };

    function bindKeys() {
      document.addEventListener('keydown', handler);
    }

    function unbindKeys() {
      document.removeEventListener('keydown', handler);
    }

    function updateKeyBinding(currentTab) {
      if (currentTab === 'R Quest') {
        bindKeys();
      } else {
        unbindKeys();
      }
    }

    // Watch for tab change via input$tabs
    Shiny.addCustomMessageHandler('activeTabChanged', function(tabLabel) {
      updateKeyBinding(tabLabel);
    });

    // Initial check once DOM is ready
    document.addEventListener('DOMContentLoaded', function() {
      var current = document.querySelector('#tabs li.active a');
      if (current) updateKeyBinding(current.textContent.trim());
    });
  })();
", paste0(id, "-key_press"))))
    ),
    h2("R Quest: The Debugger's Dungeon", align = "center"),
    div(style = "text-align: center; margin-bottom: 10px;",
        actionButton(ns("reset_game"), "\u21bb Reset Game")
    ),
    uiOutput(ns("game_ui"))
  )
}


#' R Quest Server
#'
#' @param id Module ID
#' @export
shinyquestServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    main_rows <- 10
    main_cols <- 10
    bridge_cols <- 5
    bridge_row <- 6
    goal_rows <- 3
    goal_cols <- 3
    
    total_cols <- main_cols + bridge_cols + goal_cols
    total_rows <- max(main_rows, goal_rows + 3)
    
    map_matrix <- matrix(0, nrow = total_rows, ncol = total_cols)
    
    bridge_start_col <- main_cols + 1
    bridge_cols_seq <- bridge_start_col:(bridge_start_col + bridge_cols - 1)
    goal_row <- bridge_row
    goal_col <- total_cols - 1
    
    # Puzzle bank
    puzzle_bank <- list(
      list(q = "What does `mean(c(1, 2, NA), na.rm = TRUE)` return?", a = "1.5",
           explanation = "`na.rm = TRUE` removes the NA, so the mean is (1 + 2)/2 = 1.5."),
      list(q = "Complete: `c(1, 2, 3) |> ___(2)` to get 2 3", a = "tail",
           explanation = "`tail(x, 2)` returns the last 2 elements of a vector."),
      list(q = "`sum(c(TRUE, TRUE, FALSE))` returns?", a = "2",
           explanation = "TRUE is 1, FALSE is 0, so 1 + 1 + 0 = 2."),
      list(q = "What does `paste('R', 'Studio', sep = '')` return?", a = "RStudio",
           explanation = "`sep = ''` joins strings without a space."),
      list(q = "`which(c(FALSE, TRUE, TRUE))` returns?", a = "2 3",
           explanation = "`which()` returns indices of TRUE values."),
      list(q = "Complete: `mtcars |> ___(mpg)` to get mpg vector", a = "pull",
           explanation = "`pull()` extracts a vector. `select()` returns a data frame."),
      list(q = "What does `nchar('shiny')` return?", a = "5",
           explanation = "`nchar()` counts characters."),
      list(q = "Which function counts rows in a data frame?", a = "nrow",
           explanation = "`nrow()` counts rows."),
      list(q = "`round(3.1415, 2)` returns?", a = "3.14",
           explanation = "`round()` to 2 decimal places."),
      list(q = "`seq(2, 6, by = 2)` returns?", a = "2 4 6",
           explanation = "Sequence from 2 to 6 by 2."),
      list(q = "Complete: `c(1, 2, 3) |> ___()` to reverse it", a = "rev",
           explanation = "`rev()` reverses a vector."),
      list(q = "What does `length(unique(c(1, 2, 2, 3)))` return?", a = "3",
           explanation = "`unique()` gives 1, 2, 3."),
      list(q = "`tolower('ABC')` returns?", a = "abc",
           explanation = "`tolower()` makes all lowercase."),
      list(q = "What does `!is.na(NA)` return?", a = "FALSE",
           explanation = "`is.na(NA)` is TRUE; `!TRUE` is FALSE."),
      list(q = "What is the result of `TRUE & FALSE`?", a = "FALSE",
           explanation = "Only TRUE & TRUE returns TRUE."),
      list(q = "What does `sum(c(2, 4, 6)) / length(c(2, 4, 6))` return?", a = "4",
           explanation = "Mean = (2+4+6)/3 = 4."),
      list(q = "Which function returns structure of a data frame?", a = "str",
           explanation = "`str()` shows internal structure."),
      list(q = "Complete: `iris |> ___(Species)` to see unique species", a = "distinct",
           explanation = "`distinct()` returns unique rows."),
      list(q = "What does `rep(1, 3)` return?", a = "1 1 1",
           explanation = "`rep(x, 3)` repeats x three times."),
      list(q = "`identical(1, 1.0)` returns?", a = "FALSE",
           explanation = "`1` is integer, `1.0` is numeric.")
    )
    
    state <- reactiveValues(
      player_row = 2,
      player_col = 2,
      puzzle_active = FALSE,
      current_puzzle = NULL,
      puzzles_solved = 0,
      solved_tiles = character(),
      game_won = FALSE,
      puzzle_assignments = list()
    )
    
    new_game <- function() {
      map_matrix[,] <<- 0
      map_matrix[1, ] <<- 1
      map_matrix[total_rows, ] <<- 1
      map_matrix[, 1] <<- 1
      map_matrix[, total_cols] <<- 1
      map_matrix[-bridge_row, (main_cols + 1):(main_cols + bridge_cols)] <<- 1
      map_matrix[-bridge_row, (main_cols + bridge_cols + 1):(total_cols - 1)] <<- 1
      map_matrix[bridge_row, bridge_cols_seq] <<- 1
      map_matrix[goal_row, goal_col] <<- 2
      
      for (i in (goal_row - 1):(goal_row + 1)) {
        for (j in (goal_col - 1):(goal_col + 1)) {
          if (map_matrix[i, j] == 0) map_matrix[i, j] <<- 0
        }
      }
      
      set.seed(sample(1:9999, 1))
      empty_tiles <- which(map_matrix[2:(main_rows - 1), 2:(main_cols - 1)] == 0, arr.ind = TRUE)
      empty_tiles[, 1] <- empty_tiles[, 1] + 1
      empty_tiles[, 2] <- empty_tiles[, 2] + 1
      puzzle_tiles <- empty_tiles[sample(nrow(empty_tiles), length(puzzle_bank)), ]
      tile_keys <- apply(puzzle_tiles, 1, function(x) paste(x[1], x[2], sep = "-"))
      assignments <- setNames(puzzle_bank, tile_keys)
      for (i in 1:nrow(puzzle_tiles)) {
        map_matrix[puzzle_tiles[i, "row"], puzzle_tiles[i, "col"]] <<- 3
      }
      
      state$player_row <- 2
      state$player_col <- 2
      state$puzzle_active <- FALSE
      state$current_puzzle <- NULL
      state$puzzles_solved <- 0
      state$solved_tiles <- character()
      state$game_won <- FALSE
      state$puzzle_assignments <- assignments
    }
    
    new_game()
    
    observeEvent(input$reset_game, {
      new_game()
    })
    
    observeEvent(input$key_press, {
      if (state$puzzle_active || state$game_won) return()
      
      row <- state$player_row
      col <- state$player_col
      
      new_row <- row
      new_col <- col
      
      if (input$key_press == "ArrowUp")    new_row <- max(1, row - 1)
      if (input$key_press == "ArrowDown")  new_row <- min(total_rows, row + 1)
      if (input$key_press == "ArrowLeft")  new_col <- max(1, col - 1)
      if (input$key_press == "ArrowRight") new_col <- min(total_cols, col + 1)
      
      if (map_matrix[new_row, new_col] == 1) return()
      
      state$player_row <- new_row
      state$player_col <- new_col
      
      if (map_matrix[new_row, new_col] == 2) {
        state$game_won <- TRUE
        showModal(modalDialog(
          title = "YOU WIN!",
          p("You've reached the goal and completed the quest."),
          easyClose = TRUE
        ))
        return()
      }
      
      key <- paste(new_row, new_col, sep = "-")
      if (map_matrix[new_row, new_col] == 3 && !is.null(state$puzzle_assignments[[key]])) {
        state$puzzle_active <- TRUE
        state$current_puzzle <- state$puzzle_assignments[[key]]
        showModal(modalDialog(
          title = "R Puzzle!",
          p("Solve this:"),
          pre(state$current_puzzle$q),
          textInput(ns("puzzle_input"), "Answer:", ""),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("submit_puzzle"), "Submit"),
            actionButton(ns("skip_puzzle"), "Skip")
          )
        ))
      }
    })
    
    observeEvent(input$submit_puzzle, {
      correct <- tolower(trimws(input[["puzzle_input"]])) ==
        tolower(state$current_puzzle$a)
      
      if (correct) {
        key <- paste(state$player_row, state$player_col, sep = "-")
        state$puzzles_solved <- state$puzzles_solved + 1
        state$solved_tiles <- c(state$solved_tiles, key)
        
        coords <- strsplit(key, "-")[[1]]
        row <- as.integer(coords[1])
        col <- as.integer(coords[2])
        map_matrix[row, col] <<- 0
        
        if (state$puzzles_solved <= length(bridge_cols_seq)) {
          map_matrix[bridge_row, bridge_cols_seq[state$puzzles_solved]] <<- 0
        }
        
        removeModal()
        showModal(modalDialog(
          title = "\u2705 Correct!",
          p(state$current_puzzle$explanation),
          footer = actionButton(ns("continue_game"), "Continue"),
          easyClose = FALSE
        ))
      } else {
        showNotification("Incorrect. Try again!", type = "error")
      }
    })
    
    observeEvent(input$skip_puzzle, {
      state$puzzle_active <- FALSE
      state$current_puzzle <- NULL
      removeModal()
    })
    
    observeEvent(input$continue_game, {
      state$puzzle_active <- FALSE
      state$current_puzzle <- NULL
      removeModal()
    })
    
    output$game_ui <- renderUI({
      div(style = "text-align: center;",
          div(class = "grid",
              lapply(1:total_rows, function(i) {
                div(class = "row",
                    lapply(1:total_cols, function(j) {
                      tile_type <- if (state$player_row == i && state$player_col == j) {
                        "player"
                      } else if (map_matrix[i, j] == 1) {
                        "wall"
                      } else if (map_matrix[i, j] == 2) {
                        "goal"
                      } else if (map_matrix[i, j] == 3) {
                        "puzzle"
                      } else {
                        "floor"
                      }
                      div(class = paste("cell", tile_type))
                    })
                )
              })
          )
      )
    })
  })
}