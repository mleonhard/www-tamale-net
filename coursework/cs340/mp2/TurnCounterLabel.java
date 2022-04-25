/**
 * This class inherits from JLabel and implements the turn counter widget.
 *
 * Assignment: MP2
 * Class: CS 340, Fall 2005
 * TA: Nitin Jindal
 * System: jEdit, jdk-1.5.0.4, Windows XP
 * @author Michael Leonhard (CS account mleonhar)
 * @version 22 Sep 2005
*/

import javax.swing.JLabel;

public class TurnCounterLabel extends JLabel
{
	// data fields
	private int numTurns = 0;
	private final String DESCRIPTION = "Turns Taken: ";
	
	/**
	 * Update the text label with the current counter value
	*/
	private void update()
	{
		setText(DESCRIPTION + Integer.toString(this.numTurns));
	}
	
	/**
	 * Default constructor, starts counter at 0
	*/
	public TurnCounterLabel()
	{
		super();
		reset();
	}
	
	/**
	 * Increments the counter and updates the text label
	*/
	public void increment()
	{
		this.numTurns++;
		update();
	}
	
	/**
	 * Resets the counter to zero and updates the text label
	*/
	public void reset()
	{
		this.numTurns = 0;
		update();
	}
}
