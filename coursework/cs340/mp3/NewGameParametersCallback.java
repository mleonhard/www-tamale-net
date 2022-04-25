/**
 * This interface defines a callback that user interface code can use to deliver
 * the parameters for the new game. (dialog box method calls this callback method)
 * 
 * Assignment: MP3
 * Class: CS 340, Fall 2005
 * TA: Nitin Jindal
 * System: jdk-1.5.0.4 and Eclipse 3.1 on Windows XP
 * @author Michael Leonhard (CS account mleonhar)
 * @version 12 Oct 2005
 */

public interface NewGameParametersCallback {
	/**
	 * method to accept new game parameters
	 * 
	 * @param cols number of colums of boxes in new game
	 * @param rows number of rows of boxes in new game
	 */
	public void newGameParameters(int cols, int rows);
}