/**
 * This class extends JLabel to implement a graphical counter widget.
 * 
 * Assignment: MP3
 * Class: CS 340, Fall 2005
 * TA: Nitin Jindal
 * System: jdk-1.5.0.4 and Eclipse 3.1 on Windows XP
 * @author Michael Leonhard (CS account mleonhar)
 * @version 12 Oct 2005
 */

import javax.swing.JLabel;

public class CounterLabel extends JLabel {
	// version number of this class, used for serialization
	private static final long serialVersionUID = 1L;
	// the value of the counter
	private int count;
	// the description of the counter
	private String description;

	/**
	 * Update the text label with the current counter value
	 */
	private void update() {
		setText(description + Integer.toString(this.count));
	}

	/**
	 * Default constructor, starts counter at 0
	 * 
	 * @param desc a text description of the counter. The number will be
	 *        appended to this for display.
	 */
	public CounterLabel(String desc) {
		// allow the super class to initialize (JLabel)
		super();
		// save the description
		this.description = desc;
		// start the counter at zero
		reset();
	}

	/**
	 * Increments the counter and updates the text label
	 */
	public void increment() {
		this.count++;
		update();
	}

	/**
	 * Adds the value to the counter and updates the label
	 * 
	 * @param n the number to add to the counter
	 */
	public void add(int n) {
		this.count += n;
		update();
	}

	/**
	 * Resets the counter to zero and updates the text label
	 */
	public void reset() {
		this.count = 0;
		update();
	}
}