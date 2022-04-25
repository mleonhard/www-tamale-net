------------------------------------------------------------------------------
Michael Leonhard (CS account mleonhar)
12 Oct 2005
Assignment: MP3
Class: CS 340, Fall 2005
TA: Nitin Jindal
System: jdk-1.5.0.4 and Eclipse 3.1 on Windows XP
------------------------------------------------------------------------------

Dots and Boxes is a game written in Java by Michael Leonhard.  It uses the
Swing library.  A menu is provided with the following options:
 * Exit - quit the program
 * New Game - display a dialog box allowing the user to choose a board size
              and start a new game
 * How to Play - show gameplay instructions
 * About - view information about the program and its author
 
## How to Play ###############################################################

The game consists of a field of dots.  Move the mouse over the field and click
to draw a line.  Take turns with the computer.  Complete a box to get another
turn.  Your boxes will show O.  The computer's boxes get X.  Complete the most
boxes to win!

## Program Structure #########################################################

The program consists of nine Java classes:

 * MP3 class

This class makes the main DotsAndBoxes object and starts the default game.

 * DotsAndBoxes class

This class extends JFrame and shows the menu, status bar, and game board.  It
handles menu actions to show the About box, How to Play box, and New Game
dialog box.  In response to an Exit menu action, it disposes of itself.

The New Game dialog object is created but not initially shown.  The dialog 
disposes of itself after accepting user input.  After being disposed, the
dialog is essentially invisible.  The difference between disposed and
invisible is that an invisible window still has an active event queue.  The 
Java VM will halt only when the last event queue is destroyed (because the
last window is disposed).

The New Game dialog callback is also handled in the DotsAndBoxes object.  To
start a new game, any existing Field object is removed from the content pane
and a new field object is created and added to the content pane.  The score
counters in the status bar are also reset to zero.

 * CounterLabel class

This class extends JLabel to show a numerical counter and description.  It is
used to display player scores.

 * Field class

This class extends JComponent to draw the dots and boxes and receive mouse
input.  It keeps track of the boxes and lines in the game.  The computer 
player line selection algorithm is also implemented here.

Drawing is accomplished by first iterating through the array of lines.  Each
line object is queried to see if it is visible.  If visible, it is queried for
its color.  The rectangle representing the line is then drawn on the screen.

After drawing the lines, the program iterates through the array of boxes.
Each box is queried to see if it is "complete".  A complete box is one that
has all four sides drawn in.  If complete, the box is queried to find out who
completed it: the user or the computer player.  An X symbol is drawn if the
computer player completed the box.  An O symbol is drawn if the user completed
the box.

Finally, the dots are drawn.

When a mouse event occurs, the field finds out which box is under the mouse
cursor.  It then queries the corresponding Box object for the Line object that
is nearest to the mouse cursor.  The mouse event and Line object are then
passed to the game state object for processing.

 * Box class

The box class represents a box area in the game field.  Every box object
has references to four Line objects which make up its sides.  The box keeps
track of its status as "complete" or not, and remembers who completed it.

 * Line class

Every Line object represents a line in the game field.  The line can be the
side of one box, or between two boxes.  The object has four states:

 Unshown - initial state where the line is invisible
    [drawn=false, visible=false, color=don't care]

 Hovered - transient state entered when the mouse hovers near the line.
    [drawn=false, visible=true, color=Line.hoveredColor]
 
 Chosen - transient state used to show that the line has been chosen by the
    computer player.  A different color helps the user to find the line and
    know the computer's choice.
    [drawn=false, visible=true, color=Line.chosenColor]

 Drawn - permanent state where the line is drawn into the game board.  The
    line enters this state after it is chosen by the player or computer.  It
    can make a box complete.
    [drawn=true, visible=true, color=Line.drawnColor]

 * State class

This class handles gameplay by coordinating the user's input and computer
player's actions.  There are six states for the game:

 Wander: mouse wanders over board, hovering over lines
 Pressed: mouse button was pressed on hovered line and has not moved off it
 WaitMouseUp: mouse button was pressed but a line is not selected
 Computer: it is the computer's turn
 ComputerWait: computer made a turn and is waiting to make its next turn
    so user can see which turn it just made. (Line is Chosen but not Drawn)
 EndGame: the game has ended

The state object handles events by either ignoring the event or performing
an action and state transition.

 * NewGameDialog class

This class extends JDialog to allow user to select dimensions of the board and
start a new game.  Two spinner widgets allow the user to select the number of
columns and rows.  Each spinner holds numbers between 1 and 50.

When created, the dialog receives a callback object as a parameter to the
constructor.  The "New Game" JButton has and action handler that disposes the
dialog and passes the spinner rows and columns values to the callback object.
An input map entry binds the ENTER key to this action handler.  Another action
handler and input map entry allows the ESCAPE key to dispose of the dialog.
Both of these input map entries are defined on the New Game button, but with
the WHEN_IN_FOCUSED_WINDOW flag.  Thus they catch ENTER and ESCAPE key presses
that occur anywhere in the dialog.

 * NewGameParametersCallback class

This is an interface for passing new game parameters from dialog object to
main game object.

## Status ####################################################################

All portions of the assignment description are implemented.

This program allows the user to select the number of rows and columns for
5 points of EXTRA CREDIT.

## Compiling #################################################################

Compile the program with the following command line:

javac Box.java CounterLabel.java DotsAndBoxes.java Field.java Line.java \
  MP3.java NewGameDialog.java NewGameParametersCallback.java State.java

Alternatively, this command may be used: javac *.java

## Executing #################################################################

Execute the program with this command: java -cp . MP3
 	
### Notes ####################################################################

During the development and testing of this program, many errors were
discovered in the Java AWT and Swing libraries.  The worst are listed here.

JFrame ignores setMinimumSize().

Under Linux:
JComponent.getBounds always returns x=0 and y=0.
This means that setBounds(getBounds()) results in the window being 
repositioned with its client area origin at the screen origin.

Calls to setSize() result in the same repositioning.  Thus setSize() is
useless on Linux.

JComponent.getLocation(Point) and Component.getLocation() also return (0,0).
Component.getLocationOnScreen() appears to return the valid location of the
upper-left corner of the native window.  Component.setLocation() appears to
translate the provided values from the origin of the client area.  This means
that setLocation(getLocationOnScreen()) results in the window moving up and to
the left by a certain amount.

Under Linux, JComponent.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW) appears
to behave as JComponent.getInputMap(JComponent.WHEN_FOCUSED).  This means that
pressing Escape in the new game dialog only works if the New Game button is
focused.

JFrame under Linux will let the window manager choose the location.  Under
Windows the JFrame will appear at (0,0) unless setLocationByPlatform(true) is
called first.  The setLocationByPlatform() method is available in Java 1.5, so
it cannot be used on the UIC CS systems, which run Java 1.4.

Under Windows, new JOptionPane dialog boxes appear centered over their parent
JFrame.  Under Linux they do not.  Sometimes they are aligned to the left side
of the screen.  Other times they are positioned in a cascade.

############################
Thanks for reading this far!
############################