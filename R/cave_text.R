#' Cave Adventure UI
#'
#' Constructs the UI for the Cave Adventure text-based game. This module creates
#' a basic text interface with input for commands and displays for location descriptions
#' and inventory.
#'
#' @param id Character string used to namespace the UI elements.
#'
#' @return A `shiny::fluidPage` UI element.
#' @export
#' @import shiny


# UI Module
caveUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Mini Cave Adventure"),
    verbatimTextOutput(ns("game_text"), placeholder = TRUE),
    textInput(ns("command"), "Enter command:", placeholder = "e.g. go north, take torch, use torch"),
    actionButton(ns("submit"), "Submit"),
    tags$hr(),
    verbatimTextOutput(ns("inventory"), placeholder = TRUE)
  )
}

#' Cave Adventure Server
#'
#' Implements the game logic for the Cave Adventure text-based game. Handles
#' user input commands, updates game state including movement, inventory,
#' item usage, and puzzle solving. Should be used with \code{\link{caveUI}}.
#'
#' @param id Character string used to namespace the server-side module.
#'
#' @return No return value. This function is called for its side effects.
#' @export
#' @import shiny

# Server Module
caveServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rooms <- reactiveVal({
      list(
        cave_entrance = list(description = "You stand at the entrance of a dark cave. Paths lead north and east.", exits = list(north = "narrow_passage", east = "forest"), items = c("torch")),
        narrow_passage = list(description = "A narrow passage winds deeper into the cave. It is damp and echoes with unseen movement.", exits = list(south = "cave_entrance", north = "underground_lake"), items = character()),
        forest = list(description = "You are in a dense forest. The cave lies to the west.", exits = list(west = "cave_entrance"), items = c("stick")),
        underground_lake = list(description = "You reach a vast underground lake. The air is still. There is a locked door to the east.", exits = list(south = "narrow_passage", east = "locked_door"), items = c("key")),
        locked_door = list(description = "You face a heavy iron door with an ancient lock shaped like a crescent moon.", exits = list(west = "underground_lake"), items = character(), locked = TRUE),
        treasure_chamber = list(description = "Gold and jewels glint in the torchlight. You have found the lost hoard!", exits = list(south = "locked_door"), items = c("ancient_scroll")),
        secret_passage = list(description = "A narrow tunnel slopes upward toward daylight.", exits = list(up = "cave_exit", west = "ancient_hall"), items = character()),
        cave_exit = list(description = "You emerge back at the cave entrance, victorious and laden with treasure.\nBut is there more to be seen?", exits = list(down = "secret_passage"), items = character()),
        ancient_hall = list(description = "Grand murals line the walls, depicting heroes of old. A chamber lies to the north.", exits = list(east = "secret_passage", north = "riddle_chamber"), items = c("mysterious_gem")),
        riddle_chamber = list(description = "A stone statue asks: “What flies forever but never moves?”", exits = list(north = 'cave_exit2', south = "ancient_hall"), items = character(), answer = "time", solved = FALSE),
        cave_exit2 = list(description = "You emerge back at the cave entrance, victorious and laden with even more treasure.", exits = list(), items = character())
      )
    })
    
    state <- reactiveValues(
      location   = "cave_entrance",
      inventory  = character(),
      message    = "",
      torch_lit  = FALSE
    )
    
    observeEvent(input$submit, {
      req(input$command)
      cmd <- tolower(trimws(input$command))
      tokens <- strsplit(cmd, "\\s+")[[1]]
      rm <- rooms()
      
      # Movement logic
      if (length(tokens) >= 2 && tokens[1] == "go") {
        dir <- tokens[2]
        if (dir == "north" && state$location %in% c("cave_entrance", "narrow_passage") && !state$torch_lit) {
          state$message <- "It's too dark to see. You need to use the torch first."
          updateTextInput(session, "command", value = "")
          return()
        }
        exits <- rm[[state$location]]$exits
        if (!is.null(exits[[dir]])) {
          state$location <- exits[[dir]]
          state$message  <- paste("You go", dir)
          if (state$location == "cave_exit2" && "stick" %in% state$inventory) {
            state$message <- paste(state$message, "...huh, I guess that stick was useless! THE END!!!!")
          }
        } else {
          state$message <- paste("You can't go", dir, "from here.")
        }
        
        # Take items
      } else if (length(tokens) >= 2 && tokens[1] == "take") {
        item <- paste(tokens[-1], collapse = " ")
        here_items <- rm[[state$location]]$items
        if (item %in% here_items) {
          state$inventory <- c(state$inventory, item)
          rm[[state$location]]$items <- setdiff(here_items, item)
          rooms(rm)
          state$message <- paste("You took the", item)
        } else {
          state$message <- paste("There is no", item, "here.")
        }
        
        # Use torch
      } else if (cmd == "use torch") {
        if ("torch" %in% state$inventory) {
          state$torch_lit <- TRUE
          state$message <- "You light the torch. The cave is illuminated."
        } else {
          state$message <- "You have no torch to use."
        }
        
        # Use key on locked door
      } else if (cmd == "use key" && state$location == "locked_door") {
        if ("key" %in% state$inventory && rm$locked_door$locked) {
          rm$locked_door$exits <- list(north = "treasure_chamber", west = "underground_lake")
          rm$locked_door$locked <- FALSE
          rooms(rm)
          state$message <- "You insert the key. The door creaks open to the north."
        } else {
          state$message <- "You have nothing that fits the lock."
        }
        
        # Read scroll
      } else if (cmd == "read scroll" && state$location == "treasure_chamber") {
        if ("ancient_scroll" %in% state$inventory) {
          rm$treasure_chamber$exits <- list(south = "locked_door", east = "secret_passage")
          rooms(rm)
          state$message <- "The scroll glows. A hidden door opens to the east."
        } else {
          state$message <- "You have nothing to read here."
        }
        
        # Riddle answer
      } else if (state$location == "riddle_chamber" && grepl("^answer ", cmd)) {
        guess <- sub("^answer\\s+", "", cmd)
        if (!rm$riddle_chamber$solved && guess == rm$riddle_chamber$answer) {
          rm$riddle_chamber$exits <- list(north = "cave_exit2")
          rm$riddle_chamber$solved <- TRUE
          rooms(rm)
          state$message <- "The statue slides aside, revealing a passage to the north."
        } else {
          state$message <- "The statue remains unmoved. That is not the answer."
        }
        
        # Look
      } else if (cmd == "look") {
        state$message <- rm[[state$location]]$description
        
        # Inventory
      } else if (cmd == "inventory") {
        state$message <- paste("You are carrying:", if (length(state$inventory) > 0) paste(state$inventory, collapse = ", ") else "nothing")
        
        # Unknown
      } else {
        state$message <- "Unknown command."
      }
      
      updateTextInput(session, "command", value = "")
    })
    
    output$game_text <- renderText({
      rm <- rooms()
      loc <- state$location
      desc <- rm[[loc]]$description
      items <- rm[[loc]]$items
      extra <- if (length(items) > 0) paste("You see:", paste(items, collapse = ", "))
      paste(desc, extra, state$message, sep = "\n\n")
    })
    
    output$inventory <- renderText({
      inv <- state$inventory
      paste("Inventory:", if (length(inv) > 0) paste(inv, collapse = ", ") else "Empty")
    })
  })
}