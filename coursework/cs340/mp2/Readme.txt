Michael Leonhard (CS account mleonhar)
Assignment 2 (MP2)
jEdit, jdk-1.5.0.4, Windows XP
22 Sep 2005
CS 340, Fall 2005
Instructor: Pat Troy
TA: Nitin Jindal

------------------------------------------------------------------------------
Table of Contents:
 I. Introduction
  1. Compiling
  2. Running
 II Program Usage
  1. Game play
  2. Objective
  3. Turns
  4. Menu
 III. Program Organization
  1. MP2 Class
  2. TurnCounterLabel Class
  3. Card Class
  4. TurnedCardManager Class
  5. MemoryGame Class
 IV Status
  1. Assignment Functionality
  2. Extra Credit
 V Acknowledgements
------------------------------------------------------------------------------

I. Introduction

This program implements a graphical Memory game.  It was developed on Windows XP
using JDK 1.5.0.4 and jEdit.

 1. Compiling

Compile the java source code files into class files with the following command:

	javac Card.java MemoryGame.java MP2.java TurnCounterLabel.java \
		TurnedCardManager.java

or more simply:

	javac *.java

If you receive the following warning then you are using a recent Java version:
	Note: TurnedCardManager.java uses unchecked or unsafe operations.
	Note: Recompile with -Xlint:unchecked for details.
The TurnedCardManager class uses a vector to store data.  Since Java 1.5,
Vectors and other Collection classes support a special type declaration syntax
called generics.  Programs that use generics may encounter fewer runtime errors.
The new Java compiler encourages the use of generics by issuing a warning about
unchecked operations.  The TurnedCardManager class was originally developed 
using generics.  They were removed to regain compatibility with the old Java
1.4.0 found on the UIC CS servers.

More information about Generics is available at:
http://java.sun.com/j2se/1.5.0/docs/guide/language/generics.html

 2. Running

The program may be run by loading the main class file in the java runtime.  The 
following command must be executed from the directory where the class files are 
found:

	java -cp . MP2

------------------------------------------------------------------------------

II Program Usage

MemoryGame uses Swing to display a window with 8 pairs of cards laid out in four
rows.  Loading MP2 from the command line causes the program window to appear
with a new game ready to be played.

 1. Game play

The game is played by clicking on the cards.  Initially the cards are all face
down.  Clicking on one card will turn it up, allowing the user to see what kind
of card it is.  Clicking on a second card will also turn it up.  If the two
cards are identical, the user has discovered a pair.  The pair will remain face
up.  If the cards are different, they will automatically flip face down after a
short delay.

 2. Objective

The game is won by turning up all of the cards (8 pairs).  There is no way to
lose the game.

 3. Turns

Every time the user flips up two cards, the turn counter will increment by one.
The user may attempt to win the game using the fewest number of turns. 

 4. Menu

Instructions are available by choosing How To Play from the Help menu.  The Help
menu also has an About option which will display information about the program 
and its author.  The user may begin a new game at any time by choosing New Game
from the Game menu.  The Game menu also has an Exit command which will close the
application.

------------------------------------------------------------------------------

III. Program Organization

The program is implemented with five classes.  MemoryGame is the main program
class.

 1. MP2 Class

The purpose of this class is to create an instance of the main MemoryGame class
and initiate a new game.  MP2 contains no data members and only one function.
This is the static main function which is invoked by the java runtime when the
class is loaded from the command line.

 2. TurnCounterLabel Class

This class inherits from JLabel to display the turn count.  It keeps track of 
the number of turns in an int data member.  MemoryGame creates one instance of
this class and keeps it for the duration of the program instance.  The counter
is reset at the beginning of each game.  It is incremented with each pair of
cards turned over by the user.  These reset and increment actions are initiated
by public methods.

 3. Card Class

This class inherits from JLabel to act as a simple container for card images.
A Card object is provided front and back images at construction time.  These
images are provided as ImageIcon objects.

A number is also associated with the card.  Every card that has the same face 
image will be given the same number.  An accessor makes this number available 
to methods that would compare two cards to see if they make a pair.

The Card object processes its own mouse events.  A click results in the card
requesting permission to flip.  If it receives permission, it changes itself to
display the icon of the front face.

The main program window is resizeable, as are the Card/JLabel objects.  The
icons displayed are not resizeable.  If the main window is made very small then
the icons will bunch together and overlap.  Similarly, if the main window is
made very large, considerable empty space will appear among the icons.  This
empty space is part of the Card/JLabel screen area so the Card object will
receive mouse events that occur inside it.  To maintain consistency with user's
expectations, it was necessary to test each mouse click to determine if it 
occurred on the card icon and not in the whitespace.

Similarly, the Java Swing library's discrimination of mouse clicks and mouse
drags is not ideal.  During a mouse click, it is common for the mouse to move
slightly between the moment the button is depressed and the instant it is
released.  Modern mouse hardware is more precise, making this more common than
it used to be.  Java's Swing will interpret _any_ mouse movement during a click
to indicate that the user is performing a drag operation.  During testing, it
became apparent that this behavior was unacceptible for the Memory game.  The
solution is to treat both clicks and drags, as reported by Swing, as click
commands.  The program was modified to register a click when a drag operation
occurs entirely within the bounds of the Card/JLabel and begins and ends inside
the area of the card icon.  This allows natural and high speed gameplay.

 4. TurnedCardManager Class

This class keeps a list of cards that have been turned up in the current turn.
One TurnedCardManager object is created for each game.  All of the Card objects
use the same manager to coordinate.

When a card receives a click, it requests permission from the manager to turn
over.  If fewer than 2 cards are currently in the list, the manager adds the
card to the list and gives permission.  If the list now has 2 cards in it, the
manager signals the TurnCounterLabel to increment.  It then checks if the 2
cards make a pair (by comparing their numbers).  Pairs are immediately forgotten
by the manager and left in their face-up state.  If the 2 cards are different,
the manager starts a Swing timer object to call it back after a short delay.
Upon callback, the manager signals the cards to flip face down again and then it
forgets the cards.

 5. MemoryGame Class

This is the main program class.  It is instantiated by MP2 on program startup.
The MemoryGame object loads the card images and creates the program window, 
menu, turn counter label, card manager, cards, and a container for the cards,
called a card field.

The class handles menu events.  It contains code to shut down the application,
display the HowToPlay and About information, and start a new game.

Starting a new game is accomplished by discarding any existing card field and
creating a new card manager, card field, and cards.  The cards must be
randomized.  This is accomplished by randomly rearranging values in an int
array.  The new cards are then created using the these int values to select 
their faces and numbers.  The cards all share a new TurnedCardManager instance
to coordinate gameplay.

Finally, the MemoryGame class configures the main program window to cause the
application to shutdown when the window is closed.

------------------------------------------------------------------------------

IV Status

 1. Assignment Functionality

All portions of the assignment are implemented.

 2. Extra Credit

The Extra Credit requirement has been met.  Images are used for game locations.

------------------------------------------------------------------------------

V Acknowledgements

This program uses a set of card images created by a person known as "Oxymoron."
The images are released under the GNU General Public License
(http://www.gnu.org/licenses/gpl.html).  The source distribution of these
images is included in the file cards20.zip.  A copy of the license may be found
in a file called Copying, inside cards20.zip.
