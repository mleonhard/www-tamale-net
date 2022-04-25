/**
 * This program implements a dots and boxes game.
 *
 * Please see Readme.txt for more information.
 * 
 * Assignment: MP3
 * Class: CS 340, Fall 2005
 * TA: Nitin Jindal
 * System: jdk-1.5.0.4 and Eclipse 3.1 on Windows XP
 * @author Michael Leonhard (CS account mleonhar)
 * @version 12 Oct 2005
 */

public class MP3 {
	/**
	 * This method is called when the class is loaded from the command line.
	 * It makes an instance of the main game class and starts a game on it.
	 *
	 * @param argv command line parameters (ignored)
	 */
	public static void main(String[] argv) {
		// make an instance of the main game class
		DotsAndBoxes instance = new DotsAndBoxes();
		// start a new game
		instance.newGame(6, 6);
	}
}
