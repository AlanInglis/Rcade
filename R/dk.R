
#' Launch Donkey Kong
#'
#' Launches the Donkey Kong game as a standalone Shiny app within the Rcade package.
#' This game uses Phaser 3 via JavaScript and includes pixel art, platforming, collision,
#' and keyboard controls (arrow keys). The game automatically pauses when the user switches tabs.
#'
#' @return No return value. This function launches the game as a Shiny app.
#' @export
#'
#' @import shiny
launch_donkey_kong <- function() {
  shiny::shinyApp(
    ui = fluidPage(
      div(class = "text-center",
          h1("Donkey Kong", style = "margin-top: 20px; margin-bottom: 10px;")
      ),
      tags$head(
        tags$script(src = "https://cdn.jsdelivr.net/npm/phaser@3/dist/phaser.min.js"),
        tags$style(type = "text/css", HTML("
          /* ── choose one zoom factor here ─────────────────────────── */
          :root { --zoom: 0.5; }           /* 0.8 = 80 % • 1 = full size • 1.2 = 120 % */
          
          /* enlarge the rendered canvas */
          #game-container canvas {
            transform: scale(var(--zoom));
            transform-origin: top left;
            image-rendering: pixelated;    /* crisp pixel art */
          }
          
          /* wrapper grows with the zoom so nothing is clipped */
          #game-container {
            width : calc(512px * var(--zoom));
            height: calc(720px * var(--zoom));
            margin: auto;
            border: 2px solid #555;
            /* NB: no overflow rule here – let it expand */
          }
          
          /* ---- let tab panes grow vertically & show their children ---- */
          .tab-content, .tab-content > .tab-pane {
            overflow: visible !important;   /* override Bootstrap's hidden overflow   */
            height  : auto      !important; /* drop any fixed/flex height constraints */
          }
          
          /* keep the dark theme */
         body { background: #111; color: #fff; }
        "))
      ),
      div(id = "game-container"),
      div(style = "width: 512px; margin: 20px auto 0 auto; text-align: left;",
          actionButton("resetBtn", "Reset Game",
                       class = "btn btn-danger mb-2", width = "120px")
      ),
      tags$script(HTML(donkey_kong_js()))
    ),
    
    server = function(input, output, session) {
      observeEvent(input$resetBtn, {
        session$sendCustomMessage("resetGame", list())
      })
    }
  )
}


donkey_kong_js <- function() {
  js <- '
    
    const BARREL_SPEED = 140;
    const JUMP_VELOCITY = -400;
    const LEFT_BOUND = 10;
    const RIGHT_BOUND = 502;
    const NUM_LEVELS = 6;
    const GAP = 100;
    const GAME_W = 512;
    const GAME_H = 650;   // 720

    const config = {
    type   : Phaser.AUTO,
    parent : "game-container",
    width  : 512,
    height : 650,
    pixelArt: true,
    physics: { default: "arcade", arcade: { gravity: { y: 600 } } },
    backgroundColor: "#222222",
    scene  : { preload, create, update }
    };

    let game;
    let player, platforms, cursors, barrels, scoreText,
        gameOver = false, score = 0;

    // Pause handling
    document.addEventListener("visibilitychange", () => {
      if (!game || !game.scene || !game.scene.isActive("default")) return;
      if (document.hidden) game.scene.pause("default");
      else game.scene.resume("default");
    });
    window.addEventListener("blur", () => {
      if (game && game.scene) game.scene.pause("default");
    });
    window.addEventListener("focus", () => {
      if (game && game.scene) game.scene.resume("default");
    });

    function startGame() {
      score = 0;
      gameOver = false;
      game = new Phaser.Game(config);
    }

    function preload () {}

    function create () {
      const scene = this;

      // Platforms
      platforms = this.physics.add.staticGroup();
      const levelsY = Array.from({ length: 6 }, (_, i) => 640 - i * GAP);
      levelsY.forEach((y, i) => {
        const width = (i % 2 === 0) ? 460 : 400;
        const x = (i % 2 === 0) ? 256 + 60 : 256 - 60;
        const p = scene.add.rectangle(x, y, width, 20, 0x654321);
        scene.physics.add.existing(p, true);
        platforms.add(p);
      });

      // Mario sprite
      const gfx = this.add.graphics();
      const block = (x, y, w, h, c) => gfx.fillStyle(c, 1).fillRect(x, y, w, h);
      block(2, 0, 12, 3, 0xff0000);
      block(4, 3, 8, 3, 0xff0000);
      block(4, 6, 8, 6, 0xffcc99);
      block(4, 8, 8, 2, 0x4b2e16);
      block(2, 12, 12, 4, 0xff0000);
      block(4, 16, 8, 4, 0x0066ff);
      block(4, 20, 3, 4, 0x0066ff);
      block(9, 20, 3, 4, 0x0066ff);
      block(4, 24, 3, 2, 0x4b2e16);
      block(9, 24, 3, 2, 0x4b2e16);
      gfx.generateTexture("marioTex", 16, 26);
      gfx.destroy();

      player = this.physics.add.sprite(32, 600, "marioTex");
      player.setScale(1.3);
      player.body.setSize(16, 26);
      player.setBounce(0.1);
      player.setCollideWorldBounds(true);

      cursors = this.input.keyboard.createCursorKeys();
      this.events.on("update", () => {
        if (cursors.left.isDown) player.setFlipX(true);
        if (cursors.right.isDown) player.setFlipX(false);
      });

      // Barrels
      barrels = this.physics.add.group();
      this.time.addEvent({
        delay: 2000,
        callback: () => spawnBarrel(scene),
        loop: true
      });

      // Princess goal
      if (!this.textures.exists("princessTex")) {
        const gfxP = this.add.graphics();
        const b = (x, y, w, h, c) => gfxP.fillStyle(c, 1).fillRect(x, y, w, h);
        b(4, 0, 8, 3, 0xFFFF00);
        b(5, 3, 6, 4, 0xFFCC99);
        b(3, 3, 2, 2, 0xFFA500);
        b(11, 3, 2, 2, 0xFFA500);
        b(3, 7, 10, 8, 0xFF69B4);
        gfxP.generateTexture("princessTex", 16, 16);
        gfxP.destroy();
      }

      const topPlat = platforms.getChildren().slice(-1)[0];
      const rawX = topPlat.x + topPlat.width / 2 - 12;
      const goalX = Math.min(LEFT_BOUND, rawX);
      const goalY = topPlat.y - 20;
      const goalSprite = this.add.sprite(goalX, goalY, "princessTex");
      goalSprite.setScale(1.5);
      this.physics.add.existing(goalSprite, true);
      this.physics.add.overlap(player, goalSprite, reachGoal, null, this);

      this.physics.add.collider(player, platforms);
      this.physics.add.collider(barrels, platforms);
      this.physics.add.overlap(player, barrels, hitBarrel, null, this);

      scoreText = this.add.text(16, 16, "Score: 0", { fontSize: "16px", fill: "#ffffff" });
    }

    function spawnBarrel(scene) {
      const fromLeft = Phaser.Math.Between(0, 1) === 0;
      const xPos = fromLeft ? 80 : 432;
      const dir = fromLeft ? 1 : -1;
      const barrel = scene.add.circle(xPos, 70, 9, 0xffa500);
      scene.physics.add.existing(barrel);
      barrel.body.setCircle(9);
      barrel.body.setAllowGravity(true);
      barrel.body.setBounce(0, 0);
      barrel.body.setVelocityX(dir * BARREL_SPEED);
      barrel.body.setAngularVelocity(dir * 360);
      barrel.body.setFriction(0, 0);
      barrels.add(barrel);
    }

    function hitBarrel(playerObj, barrel) {
      this.physics.pause();
      playerObj.fillColor = 0xff0000;
      gameOver = true;
      scoreText.setText("GAME OVER – Reset");
    }

    function reachGoal(playerObj, goalObj) {
      this.physics.pause();
      gameOver = true;
      scoreText.setText("YOU WIN! – Reset");
    }

    function update () {
      if (gameOver) return;
      if (cursors.left.isDown) {
        player.body.setVelocityX(-160);
      } else if (cursors.right.isDown) {
        player.body.setVelocityX(160);
      } else {
        player.body.setVelocityX(0);
      }
      if (cursors.up.isDown && player.body.blocked.down) {
        player.body.setVelocityY(JUMP_VELOCITY);
      }
      barrels.children.iterate(b => {
        if (!b) return;
        const body = b.body;
        if (b.x <= LEFT_BOUND && body.velocity.x < 0) {
          body.setVelocityX(BARREL_SPEED);
          body.setAngularVelocity(Math.abs(body.angularVelocity));
          b.x = LEFT_BOUND;
        } else if (b.x >= RIGHT_BOUND && body.velocity.x > 0) {
          body.setVelocityX(-BARREL_SPEED);
          body.setAngularVelocity(-Math.abs(body.angularVelocity));
          b.x = RIGHT_BOUND;
        }
        if (body.blocked.down && Math.abs(body.velocity.x) < 10) {
          const dir = body.angularVelocity >= 0 ? 1 : -1;
          body.setVelocityX(dir * BARREL_SPEED);
          body.setAngularVelocity(dir * 360);
        }
      });
      score += 0.01;
      scoreText.setText("Score: " + Math.floor(score));
    }

    startGame();

    if (typeof Shiny !== "undefined") {
      Shiny.addCustomMessageHandler("resetGame", function () {
        if (game) game.destroy(true);
        startGame();
      });
    }
  '
  return(js)
}