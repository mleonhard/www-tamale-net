/**
 * This program implements a graphical memory game.
 *
 * Please see Readme.txt for more information.
 *
 * Assignment: MP2
 * Class: CS 340, Fall 2005
 * TA: Nitin Jindal
 * System: jEdit, jdk-1.5.0.4, Windows XP
 * @author Michael Leonhard (CS account mleonhar)
 * @version 22 Sep 2005
*/

public class MP2
{
	/**
	* This method is called when the class is loaded from the command line.
	* It makes an instance of the main game class and starts a game on it.
	*
	* @param argv command line parameters (ignored)
	*/
	public static void main( String[] argv )
	{
		// make an instance of the main game class
		MemoryGame instance = new MemoryGame();
		// start a new game
		instance.newGame();
	}
}
