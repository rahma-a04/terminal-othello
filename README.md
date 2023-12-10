# cs3110-final-project

A terminal-based version of the board game Othello, created for our CS 3110 final project.

## Authors

Arthi Vijayakumar (av439)
Rahma Abdullah (ra567)
Mary Yuan (cy452)
Om Deshmukh (od66)

## Setup

Refer to INSTALL.txt for installation instructions

## Gameplay

1.  Type command "make othello" to start the othello game

2.  Next, type 's difficulty', with difficulty being easy, medium, hard, or godmode,
    for singleplayer. For multiplayer, type 'm'. You can also type 'q' to quit, and
    'h' for help.

         Example 's medium'

3.  If playing against a computer, choose a color. You may respond 'b' for black
    and 'w' for white. Otherwise, the game automatically starts with black, and one
    player can choose to start the game.

4.  Congrats! You started the game! You can enter moves as (letter, number).
    Example 'f 5'. The board is printed as a grid. The empty spaces are empty. The
    black tiles are the circles with black in the center. The white tiles are circles
    with white in the center. The range of values for row is 1 <= row <= 8 and column
    is A <= column <= H.

5.  If you are playing in mutiplayer mode, you can enter moves for each color one
    after the other.
    If you are playing in singleplayer, after you enter the first move, the computer
    will play its own move and you can enter your next move. Type 'history' at any
    point in the gameplay to enter history mode, where you can choose a previous
    point in the game to return to.

6.  Type 'q' at any point to quit the game. Type 'h' at any point to list all
    available commands.

7.  When finished, run dune clean in terminal.
