# cs3110-final-project

A terminal-based version of the board game Othello, created for our CS 3110 final project.

Authors:

- Rahma Abdullah (ra567)
- Om Deshmukh (od66)
- Arthi Vijayakumar (av439)
- Mary Yuan (cy452)

## Setup

Refer to INSTALL.txt for installation instructions

## Gameplay

Refer to this page for the rules of Othello: https://www.worldothello.org/about/about-othello/othello-rules/official-rules/english

1.  Type command "make othello" to start the othello game

2.  Next, for a singleplayer game, type 's [easy | medium | hard | godmode]' to
    select an easy, medium, hard, or extreme opponent. For multiplayer, type
    'm'. You can also type 'q' to quit, and 'h' for help.

             Example: 's medium'

3.  If playing against a computer, choose a color. You may respond 'b' for black
    and 'w' for white. Otherwise, the game automatically starts with black, and one
    player can choose to start the game.

4.  Congrats! You started the game! You can enter moves as (letter, number).
    Example 'f 5'. The board is printed as a grid. The empty spaces are empty. The
    black tiles are the circles with black in the center. The white tiles are circles
    with white in the center. The range of values for row is 1 <= row <= 8 and column
    is A <= column <= H.

    <img width="384" alt="image" src="https://media.github.coecis.cornell.edu/user/14426/files/4fcc40b2-54c6-4ad4-9d46-7987f5193444">

5.  If you are playing in mutiplayer mode, you can enter moves for each color one
    after the other.
    If you are playing in singleplayer, after you enter the first move, the computer
    will play its own move and you can enter your next move.

    <img width="459" alt="image" src="https://media.github.coecis.cornell.edu/user/14426/files/e36f9ce3-f3b6-491a-ad19-0e2ac99f4f58">

6.  Type 'history' at any point in a current game to enter history mode, where
    you can choose a previous point in the game to return to.

7.  Type 'q' at any point to quit the game. Type 'h' at any point to list all
    available commands.

8.  When finished, run dune clean in terminal.
