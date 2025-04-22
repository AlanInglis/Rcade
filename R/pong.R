#' Play Pong in a Shiny module
#'
#' This function renders a self-contained Pong game UI and client-side JavaScript logic
#' as a Shiny module. It includes a canvas-based game interface with keyboard control,
#' scoreboard, and visual styling consistent with the Rcade package theme.
#' The game is initialised via a custom message (`<id>-init`) and supports tab-aware
#' event listener management to prevent arrow key conflicts when the game is not active.
#'
#' @param id A unique module ID used for namespacing the UI elements and event handlers.
#'
#' @return A `tagList` containing the HTML, CSS, and JavaScript needed to run the Pong game
#' within a Shiny tab panel or UI section.
#'
#'
#' @export

play_pong <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    tags$style(HTML(sprintf("
  #%s-container {
    background-color: #cceeff;
    font-family: monospace;
    text-align: center;
    padding-bottom: 20px;
  }
  #%s-title {
    font-size: 36px;
    margin-top: 20px;
    font-weight: bold;
    color: #003366;
  }
  #%s-message {
    font-size: 20px;
    margin: 10px 0;
    height: 30px;
    color: #cc3300;
  }
  #%s-scoreboard {
    font-size: 22px;
    margin-bottom: 20px;
    color: #222;
  }
  #%s {
    background: black;
    display: block;
    margin: 0 auto;
    border: 4px solid white;
  }
", ns("container"), ns("title"), ns("message"), ns("scoreboard"), ns("canvas")))),
    div(
      id = ns("container"),
      style = "display: flex; flex-direction: column; align-items: center; justify-content: center;",
      div(id = ns("title"), "PONG"),
      div(id = ns("message"), ""),
      tags$canvas(id = ns("canvas")),
      div(id = ns("scoreboard"), "Player: 0 | AI: 0")
    ),
    
    tags$script(HTML(sprintf("
  window.onload = function() {
    const ns = '%s-';
    const canvas = document.getElementById(ns + 'canvas');
    const ctx = canvas.getContext('2d');
    
    Shiny.addCustomMessageHandler('pong-state', function(message) {
  if (message.playing) {
    if (!gamePaused) animFrame = requestAnimationFrame(draw);
  } else {
    cancelAnimationFrame(animFrame);
  }
});

    let paddleHeight = 75, paddleWidth = 10;
    let playerY, aiY;
    let ballX, ballY, ballSpeedX, ballSpeedY;
    let playerScore = 0, aiScore = 0;
    const paddleSpeed = 5;
    const WIN_SCORE = 10;
    let upPressed = false, downPressed = false;
    let gamePaused = false;
    let animFrame;

    canvas.width = 600;
    canvas.height = 400;

    function resetBall() {
      ballX = canvas.width / 2;
      ballY = canvas.height / 2;
      ballSpeedX = 4 * (Math.random() > 0.5 ? 1 : -1);
      ballSpeedY = 2 * (Math.random() * 2 - 1);
    }

    function updateScoreboard() {
      document.getElementById(ns + 'scoreboard').textContent =
        `Player: ${playerScore} | AI: ${aiScore}`;
    }

    function showMessage(text) {
      document.getElementById(ns + 'message').textContent = text;
    }

    function keyDown(e) {
      if (e.key === 'ArrowUp') upPressed = true;
      if (e.key === 'ArrowDown') downPressed = true;
    }

    function keyUp(e) {
      if (e.key === 'ArrowUp') upPressed = false;
      if (e.key === 'ArrowDown') downPressed = false;
    }

    function draw() {
      if (gamePaused) return;

      if (upPressed && playerY > 0) playerY -= paddleSpeed;
      if (downPressed && playerY < canvas.height - paddleHeight) playerY += paddleSpeed;
      aiY += (ballY - (aiY + paddleHeight / 2)) * 0.05;

      ballX += ballSpeedX;
      ballY += ballSpeedY;

      if (ballY <= 0 || ballY >= canvas.height) ballSpeedY *= -1;

      if (ballX <= paddleWidth && ballY > playerY && ballY < playerY + paddleHeight) {
        let deltaY = ballY - (playerY + paddleHeight / 2);
        ballSpeedY = deltaY * 0.25;
        ballSpeedX *= -1;
      }

      if (ballX >= canvas.width - paddleWidth &&
          ballY > aiY && ballY < aiY + paddleHeight) {
        let deltaY = ballY - (aiY + paddleHeight / 2);
        ballSpeedY = deltaY * 0.25;
        ballSpeedX *= -1;
      }

      if (ballX < 0) {
        aiScore++;
        updateScoreboard();
        if (aiScore >= WIN_SCORE) {
          showMessage('AI wins! Restarting...');
          gamePaused = true;
          cancelAnimationFrame(animFrame);
          setTimeout(startGame, 2000);
          return;
        }
        resetBall();
      }

      if (ballX > canvas.width) {
        playerScore++;
        updateScoreboard();
        if (playerScore >= WIN_SCORE) {
          showMessage('Player wins! Restarting...');
          gamePaused = true;
          cancelAnimationFrame(animFrame);
          setTimeout(startGame, 2000);
          return;
        }
        resetBall();
      }

      ctx.clearRect(0, 0, canvas.width, canvas.height);
     // draw dashed middle line
      ctx.beginPath();
      ctx.setLineDash([10, 10]);
      ctx.moveTo(canvas.width / 2, 0);
      ctx.lineTo(canvas.width / 2, canvas.height);
      ctx.strokeStyle = 'white';
      ctx.lineWidth = 2;
      ctx.stroke();
      ctx.setLineDash([]);

       // draw paddles
       ctx.fillStyle = 'white';
       ctx.fillRect(0, playerY, paddleWidth, paddleHeight);
       ctx.fillRect(canvas.width - paddleWidth, aiY, paddleWidth, paddleHeight);
       
       // draw ball
       ctx.beginPath();
       ctx.arc(ballX, ballY, 8, 0, Math.PI * 2);
       ctx.fill();

        // next frame
        animFrame = requestAnimationFrame(draw);
    }

    function startGame() {
      playerY = aiY = (canvas.height - paddleHeight) / 2;
      resetBall();
      updateScoreboard();
      showMessage('');
      document.addEventListener('keydown', keyDown);
      document.addEventListener('keyup', keyUp);
      gamePaused = false;
      animFrame = requestAnimationFrame(draw);
    }

    startGame();
  };
", id)))
  )
}


