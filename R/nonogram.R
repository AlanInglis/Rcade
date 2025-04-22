
#' Predefined Nonogram Puzzles
#'
#' A list of predefined nonogram puzzles. Each puzzle includes row clues, column clues, and the solution matrix.
#'
#' @format A list of lists, where each inner list contains:
#' \describe{
#'   \item{row_clues}{A list of character vectors specifying the clues for each row}
#'   \item{col_clues}{A list of character vectors specifying the clues for each column}
#'   \item{solution}{A logical matrix representing the correct filled-in solution}
#' }
#'
#' @details These puzzles are used by the \code{nonogramUI} and \code{nonogramServer} modules to generate interactive nonogram logic games.
#' @keywords datasets
#' @name puzzles
#' @docType data
#' @usage puzzles

# --- Puzzle definitions ---
puzzles <- list(
  # Level 1
  list(
    row_clues = list('4', '2', c('2','2'), '2', '1'),
    col_clues = list('1', c('1','1'), c('1','1'), '4', '4'),
    solution = matrix(c(
      FALSE, TRUE,  TRUE,  TRUE,  TRUE,
      FALSE, FALSE, FALSE, TRUE,  TRUE,
      TRUE,  TRUE,  FALSE, TRUE,  TRUE,
      FALSE, FALSE, FALSE, TRUE,  TRUE,
      FALSE, FALSE, TRUE,  FALSE, FALSE
    ), 5, 5, byrow = TRUE)
  ),
  
  # Level 2
  list(
    row_clues = list(c('2','1'), c('1','3'), c('1','2'), '3', '4', '1'),
    col_clues = list('1', '5', '2', '5', c('2','1'), '2'),
    solution = matrix(c(
      TRUE,  TRUE,  FALSE, FALSE, FALSE,  TRUE,
      FALSE, TRUE,  FALSE, TRUE,  TRUE,   TRUE,
      FALSE, TRUE,  FALSE, TRUE,  TRUE,   FALSE,
      FALSE, TRUE,  TRUE,  TRUE,  FALSE,  FALSE,
      FALSE, TRUE,  TRUE,  TRUE,  TRUE,   FALSE,
      FALSE, FALSE, FALSE, TRUE,  FALSE,  FALSE
    ), 6, 6, byrow = TRUE)
  ),
  
  # Level 3
  list(
    row_clues = list(
      '10', c('3','3'), c('2','1','1','2'), c('1','1','1','1'),
      c('1','1'), c('1','1','1','1'), c('1','4','1'),
      c('2','2','2'), c('3','3'), '10'
    ),
    col_clues = list(
      '10', c('3','3'), c('2','1','2'), c('1','2','1','1'),
      c('1','2','1'), c('1','2','1'), c('1','2','1','1'),
      c('2','1','2'), c('3','3'), '10'
    ),
    solution = matrix(c(
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE,
      TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE,
      TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE,
      TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
      TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE,
      TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE,
      TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE,
      TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE,
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE
    ), 10, 10, byrow = TRUE)
  ),
  
  # Level 4: Cat face
  list(
    row_clues = list(
      c('6','11','6'), c('6','11','6'), c('5','9','5'),
      c('5','1','7','1','5'), c('4','1','5','1','4'), c('4','3','3','4'),
      c('4','15','4'), c('3','15','3'), c('3','17','3'), c('2','17','2'),
      c('2','3','7','3','2'), c('3','7','3'), c('2','19','2'), '17',
      c('2','8','8','2'), c('7','7'), c('2','19','2'), c('3','13','3'),
      c('5','11','5'), c('6','6')
    ),
    col_clues = list(
      c('11','1','1','4'), c('11','1','1','4'), c('9','3'),
      c('7','1','1','1','2'), c('4','9','2'), c('2','11','1'), '15',
      c('2','5','7'), c('3','5','7'), c('4','13'), c('5','13'),
      c('5','8','3'), c('5','8','3'), c('5','8','3'), c('5','13'),
      c('4','13'), c('3','5','7'), c('2','5','7'), '15', c('2','11','1'),
      c('4','9','2'), c('7','1','1','1','2'), c('9','3'),
      c('11','1','1','4'), c('11','1','1','4')
    ),
    solution = matrix(c(
      # row 1
      T, T, T, T, T, T, F, T, T, T, T, T, T, T, T, T, T, T, F, T, T, T, T, T, T,
      # row 2
      T, T, T, T, T, T, F, T, T, T, T, T, T, T, T, T, T, T, F, T, T, T, T, T, T,
      # row 3
      T, T, T, T, T, F, F, F, T, T, T, T, T, T, T, T, T, F, F, F, T, T, T, T, T,
      # row 4
      T, T, T, T, T, F, T, F, F, T, T, T, T, T, T, T, F, F, T, F, T, T, T, T, T,
      # row 5
      T, T, T, T, F, F, T, F, F, F, T, T, T, T, T, F, F, F, T, F, F, T, T, T, T,
      # row 6
      T, T, T, T, F, F, T, T, T, F, F, F, F, F, F, F, T, T, T, F, F, T, T, T, T,
      # row 7
      T, T, T, T, F, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, F, T, T, T, T,
      # row 8
      T, T, T, F, F, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, F, F, T, T, T,
      # row 9
      T, T, T, F, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, F, T, T, T,
      # row 10
      T, T, F, F, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, F, F, T, T,
      # row 11
      T, T, F, F, T, T, T, F, F, T, T, T, T, T, T, T, F, F, T, T, T, F, F, T, T,
      # row 12
      F, F, F, F, T, T, T, F, F, T, T, T, T, T, T, T, F, F, T, T, T, F, F, F, F,
      # row 13
      T, T, F, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, F, T, T,
      # row 14
      F, F, F, F, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, F, F, F, F,
      # row 15
      T, T, F, T, T, T, T, T, T, T, T, F, F, F, T, T, T, T, T, T, T, T, F, T, T,
      # row 16
      F, F, F, F, T, T, T, T, T, T, T, F, F, F, T, T, T, T, T, T, T, F, F, F, F,
      # row 17
      T, T, F, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, F, T, T,
      # row 18
      T, T, T, F, F, F, T, T, T, T, T, T, T, T, T, T, T, T, T, F, F, F, T, T, T,
      # row 19
      T, T, T, T, T, F, F, T, T, T, T, T, T, T, T, T, T, T, F, F, T, T, T, T, T,
      # row 20
      T, T, T, T, T, T, F, F, F, F, F, F, F, F, F, F, F, F, F, T, T, T, T, T, T
    ), 20, 25, byrow = T)
  )
)

#' Nonogram UI Module
#'
#' Constructs the UI for a nonogram puzzle game using Shiny modules. Displays the puzzle grid, clue layout, and control buttons.
#'
#' @param id A character string specifying the module namespace ID.
#'
#' @return A Shiny UI definition (`fluidPage`) for the nonogram game interface.
#' @export

# Nonogram UI module
nonogramUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Nonogram Puzzle"),
    div(style = "margin-bottom:10px; font-size:20px; font-weight:bold;",
        textOutput(ns("level_info"))
    ),
    div(style = "display: flex; justify-content: center; align-items: center;",
        uiOutput(ns("puzzle"))
    ),
    fluidRow(
      column(4, actionButton(ns("reset"),       "Reset")),
      column(4, actionButton(ns("show_sol"),    "Show solution")),
      column(4, actionButton(ns("next_puzzle"), "Next Puzzle"))
    ),
    div(style = "margin-top:10px; font-weight:bold;",
        textOutput(ns("status"))
    )
  )
}

#' Nonogram Server Module
#'
#' Implements the server logic for a nonogram puzzle game using Shiny modules. Manages puzzle state, cell toggling, solution checking, and puzzle progression.
#'
#' @param id A character string specifying the module namespace ID.
#'
#' @return No return value. This function is called for its side effects.
#' @export
#
# Nonogram server logic module
nonogramServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    puzzle_index <- reactiveVal(1)
    cur          <- reactive(puzzles[[puzzle_index()]])
    
    output$level_info <- renderText({
      paste("Level", puzzle_index())
    })
    
    size_params <- reactive({
      nc <- length(cur()$col_clues)
      nr <- length(cur()$row_clues)
      
      if (nc > 20 || nr > 20) {
        list(cell_size = 16, clue_width = 36, font_size = 10)
      } else if (nc > 9 || nr > 9) {  # target Level 3 (10x10)
        list(cell_size = 25, clue_width = 45, font_size = 14)
      } else {
        list(cell_size = 50, clue_width = 60, font_size = 18)
      }
    })
    
    state <- reactiveVal()
    observeEvent(puzzle_index(), {
      p <- cur()
      state(matrix(FALSE, length(p$row_clues), length(p$col_clues)))
    }, ignoreInit = FALSE)
    
    row_solved <- reactive({
      p <- cur(); m <- state()
      vapply(seq_along(p$row_clues), function(i) all(m[i, ] == p$solution[i, ]), logical(1))
    })
    col_solved <- reactive({
      p <- cur(); m <- state()
      vapply(seq_along(p$col_clues), function(j) all(m[, j] == p$solution[, j]), logical(1))
    })
    
    output$puzzle <- renderUI({
      p      <- cur()
      sp     <- size_params()
      cell   <- sp$cell_size
      clue_w <- sp$clue_width
      fnt    <- sp$font_size
      nr     <- length(p$row_clues)
      nc     <- length(p$col_clues)
      max_col <- max(sapply(p$col_clues, length))
      max_row <- max(sapply(p$row_clues, length))
      
      tags$table(style = "border-collapse:collapse;",
                 # column clues
                 do.call(tagList, lapply(seq_len(max_col), function(r) {
                   tags$tr(
                     lapply(seq_len(max_row), function(j) {
                       tags$th(style = sprintf("width:%spx; height:%spx; border:1px solid #000;", clue_w, cell))
                     }),
                     lapply(seq_len(nc), function(j) {
                       pos <- r - (max_col - length(p$col_clues[[j]]))
                       val <- if (pos >= 1) p$col_clues[[j]][pos] else ""
                       tags$th(val, style = sprintf("border:1px solid #000; width:%spx; height:%spx; font-size:%spx; text-align:center; vertical-align:bottom;	color:%s;", cell, cell, fnt, if (col_solved()[j]) "lightgreen" else "white"))
                     })
                   )
                 })),
                 # row clues and cells
                 do.call(tagList, lapply(seq_len(nr), function(i) {
                   tags$tr(
                     lapply(seq_len(max_row), function(k) {
                       pos <- k - (max_row - length(p$row_clues[[i]]))
                       val <- if (pos >= 1) p$row_clues[[i]][pos] else ""
                       tags$th(val, style = sprintf("border:1px solid #000; width:%spx; height:%spx; font-size:%spx; text-align:center; vertical-align:middle; color:%s;", clue_w, cell, fnt, if (row_solved()[i]) "lightgreen" else "white"))
                     }),
                     lapply(seq_len(nc), function(j) {
                       cid    <- session$ns(paste0("cell_", i, "_", j))
                       filled <- state()[i, j]
                       tags$td(id = cid, style = sprintf("position:relative; border:1px solid #000; width:%spx; height:%spx; background:%s;", cell, cell, if (filled) "steelblue" else "white"),
                               tags$div(onclick = sprintf("Shiny.setInputValue('%s',{i:%d,j:%d,nonce:Math.random()})", session$ns("cell_click"), i, j),
                                        style="position:absolute; top:0; left:0; width:100%; height:100%; cursor:pointer;"))
                     })
                   )
                 }))
      )
    })
    
    observeEvent(input$cell_click, {
      c <- input$cell_click; m <- state()
      m[c$i, c$j] <- !m[c$i, c$j]
      state(m)
    }, ignoreInit = TRUE)
    
    observeEvent(input$reset, {
      p <- cur()
      state(matrix(FALSE, length(p$row_clues), length(p$col_clues)))
    })
    observeEvent(input$show_sol, {
      state(cur()$solution)
    })
    observeEvent(input$next_puzzle, {
      if (puzzle_index() < length(puzzles)) puzzle_index(puzzle_index() + 1)
    })
    
    output$status <- renderText({
      if (identical(state(), cur()$solution)) "Well done! You solved the puzzle." else ""
    })
  })
}

