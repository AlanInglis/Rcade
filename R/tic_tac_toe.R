#' Tic Tac Toe UI Module
#'
#' Generates the UI layout for a Tic Tac Toe game using Shiny modules.
#'
#' @param id A character string specifying the module namespace ID.
#'
#' @return A Shiny UI definition (a `fluidPage` object) for the Tic Tac Toe game.
#' @export


ticTacToeUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Tic Tac Toe"),
    sidebarLayout(
      sidebarPanel(
        div(
          tags$label("Choose difficulty:", style = "color: black;"),
          selectInput(ns("difficulty"), label = NULL,
                      choices = c("Easy", "Hard"),
                      selected = "Easy")
        ),
        actionButton(ns("reset"), "Reset")
      ),
      mainPanel(
        splitLayout(
          cellWidths = rep("100px", 3), gap = "0px", cellArgs = list(style = "padding:0px;"),
          actionButton(ns("cell11"), "", style = "width:100px; height:100px; font-size:24px;"),
          actionButton(ns("cell12"), "", style = "width:100px; height:100px; font-size:24px;"),
          actionButton(ns("cell13"), "", style = "width:100px; height:100px; font-size:24px;")
        ),
        splitLayout(
          cellWidths = rep("100px", 3), gap = "0px", cellArgs = list(style = "padding:0px;"),
          actionButton(ns("cell21"), "", style = "width:100px; height:100px; font-size:24px;"),
          actionButton(ns("cell22"), "", style = "width:100px; height:100px; font-size:24px;"),
          actionButton(ns("cell23"), "", style = "width:100px; height:100px; font-size:24px;")
        ),
        splitLayout(
          cellWidths = rep("100px", 3), gap = "0px", cellArgs = list(style = "padding:0px;"),
          actionButton(ns("cell31"), "", style = "width:100px; height:100px; font-size:24px;"),
          actionButton(ns("cell32"), "", style = "width:100px; height:100px; font-size:24px;"),
          actionButton(ns("cell33"), "", style = "width:100px; height:100px; font-size:24px;")
        ),
        br(),
        verbatimTextOutput(ns("status"))
      )
    )
  )
}

ticTacToeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    board   <- reactiveVal(matrix("", 3, 3))
    current <- reactiveVal("X")
    status  <- reactiveVal("Your turn")
    
    checkWin <- function(b, p) {
      rows  <- apply(b, 1, function(r) all(r == p))
      cols  <- apply(b, 2, function(c) all(c == p))
      diags <- c(
        all(diag(b) == p),
        all(diag(b[, 3:1]) == p)
      )
      any(c(rows, cols, diags))
    }
    
    available <- function(b) {
      inds <- which(b == "", arr.ind = TRUE)
      if (nrow(inds) == 0) return(matrix(numeric(0), 0, 2))
      inds
    }
    
    hardMove <- function(b) {
      for (pos in split(as.data.frame(available(b)), seq(nrow(available(b))))) {
        b2 <- b; b2[pos$row, pos$col] <- "O"
        if (checkWin(b2, "O")) return(c(pos$row, pos$col))
      }
      for (pos in split(as.data.frame(available(b)), seq(nrow(available(b))))) {
        b2 <- b; b2[pos$row, pos$col] <- "X"
        if (checkWin(b2, "X")) return(c(pos$row, pos$col))
      }
      avail <- available(b)
      avail[sample(nrow(avail), 1), ]
    }
    
    observeEvent(input$reset, {
      board(matrix("", 3, 3))
      current("X")
      status("Your turn")
    })
    
    for (i in 1:3) for (j in 1:3) local({
      ii <- i; jj <- j; btn <- paste0("cell", ii, jj)
      observeEvent(input[[btn]], {
        req(current() == "X", status() == "Your turn")
        b <- board()
        if (b[ii, jj] == "") {
          b[ii, jj] <- "X"
          board(b)
          if (checkWin(b, "X")) {
            status("You win!")
          } else if (nrow(available(b)) == 0) {
            status("Draw!")
          } else {
            current("O"); status("Computer turn")
          }
        }
      }, ignoreInit = TRUE)
    })
    
    observeEvent(current(), {
      if (current() == "O" && status() == "Computer turn") {
        b    <- board()
        diff <- input$difficulty
        if (diff == "Easy") {
          move <- available(b)[sample(nrow(available(b)), 1), ]
        } else {
          move <- hardMove(b)
        }
        b[move[1], move[2]] <- "O"
        board(b)
        if (checkWin(b, "O")) {
          status("Computer wins!")
        } else if (nrow(available(b)) == 0) {
          status("Draw!")
        } else {
          current("X"); status("Your turn")
        }
      }
    })
    
    observe({
      b <- board()
      for (i in 1:3) for (j in 1:3) {
        updateActionButton(session, paste0("cell", i, j), label = b[i, j])
      }
    })
    
    output$status <- renderText(status())
  })
}