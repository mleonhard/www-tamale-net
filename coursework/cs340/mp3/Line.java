/**
 * Holds a information about a line.
 * 
 * Assignment: MP3
 * Class: CS 340, Fall 2005
 * TA: Nitin Jindal
 * System: jdk-1.5.0.4 and Eclipse 3.1 on Windows XP
 * @author Michael Leonhard (CS account mleonhar)
 * @version 12 Oct 2005
 */

import java.awt.Color;

public class Line {
	// orientation constants
	public static final int HORIZONTAL = 0;
	public static final int VERTICAL = 1;

	// field where the line is found
	private Field field;
	// column where the line is found
	private int col;
	// row where the line is found
	private int row;
	// line orientation, HORIZONTAL or VERTICAL
	private int orientation;
	// the line is visible on the field
	private boolean visible = false;
	// the line has been drawn into the field
	private boolean drawn = false;
	// the color of the line
	private Color color;
	// the two boxes that this line borders
	private Box box0;
	private Box box1;

	// color when we are hovered over
	private Color hoveredColor;
	// color when chosen by computer
	private Color chosenColor;
	// color when drawn into the field
	private Color drawnColor;

	/**
	 * Creates a new Line object with the specified characteristics
	 * 
	 * @param field the Field object where this line will be rendered
	 * @param orientation VERTICAL or HORIZONTAL
	 * @param col the column of lines where this line is to be found
	 * @param row the row of lines where this line is to be found
	 */
	public Line(Field field, int orientation, int col, int row) {
		// make colors
		this.hoveredColor = new Color(0x1fbd53);
		this.chosenColor = Color.RED;
		this.drawnColor = new Color(0x707070);
		
		// save the data
		this.field = field;
		this.orientation = orientation;
		this.col = col;
		this.row = row;
	}

	/**
	 * Save the specified box. We are one of its sides.
	 * 
	 * @param box we are a side of this box
	 */
	public void youAreMySide(Box box) {
		// save as first box
		if (this.box0 == null) this.box0 = box;
		// save as second box
		else if (this.box1 == null) this.box1 = box;
		// error
		//else System.out.println("Line.youAreMySide(" + box
		//		+ ") ALREADY 2 BOXES " + this);
	}

	/**
	 * Draws the line into the field
	 * 
	 * @param userDrawn true means user draws, false if computer does it
	 * @return number of boxes completed by the drawing of this line
	 */
	public int draw(boolean userDrawn) {
		// if (this.drawn) System.out.println("Line.draw() ALREADY DRAWN " +
		// this);
		// else System.out.println("Line.draw() " + this);

		// set our flags and color
		this.drawn = true;
		this.visible = true;
		this.color = this.drawnColor;

		// request a redraw of ourself
		this.repaint();

		int boxesMade = 0;
		// the first box is now completed (fourth side drawn in)
		if (this.box0 != null && this.box0.numDrawnSides() == 4) {
			this.box0.youAreComplete(userDrawn);
			boxesMade++;
		}
		// the other box is now completed (fourth side drawn in)
		if (this.box1 != null && this.box1.numDrawnSides() == 4) {
			this.box1.youAreComplete(userDrawn);
			boxesMade++;
		}
		// return the number of boxes made
		return boxesMade;
	}

	/**
	 * Shows that the line is chosen by the computer
	 */
	public void showAsChosen() {
		// if (this.drawn) System.out.println("Line.showAsChosen() ALREADY DRAWN
		// "
		// + this);
		// else System.out.println("Line.showAsChosen() " + this);

		// set our flags and color
		this.drawn = false;
		this.visible = true;
		this.color = this.chosenColor;

		// request a redraw of ourself
		this.repaint();
	}

	/**
	 * Shows that the mouse hovers over this line
	 */
	public void showAsHovered() {
		// if (this.drawn) System.out
		// .println("Line.showAsHovered() ALREADY DRAWN " + this);
		// else System.out.println("Line.showAsHovered() " + this);

		// set our flags and color
		this.drawn = false;
		this.visible = true;
		this.color = this.hoveredColor;

		// request a redraw of ourself
		this.repaint();
	}

	/**
	 * Stops showing the line
	 */
	public void stopShowing() {
		// if (this.drawn) System.out.println("Line.stopShowing() ALREADY DRAWN
		// "
		// + this);
		// else if (!this.visible) System.out
		// .println("Line.stopShowing() NOT VISIBLE " + this);
		// else System.out.println("Line.stopShowing() " + this);

		// set our flags
		this.drawn = false;
		this.visible = false;

		// request a redraw of ourself
		this.repaint();
	}

	/**
	 * Request a repaint of line
	 */
	private void repaint() {
		// Request the field to repaint the area occupied by the line
		this.field.repaintLine(this.orientation, this.col, this.row);
	}

	/**
	 * Makes a string representation of the line
	 * 
	 * @return a string representation of the line
	 */
	public String toString() {
		// string representing orientation
		String sOrien = "horizontal";
		if (this.orientation == Line.VERTICAL) sOrien = "vertical";
		// generate full string
		return "Line[" + sOrien + " col=" + this.col + " row=" + this.row + "]";
	}

	/**
	 * Accessor for column
	 * 
	 * @return the column of lines where this line is found
	 */
	public int getCol() {
		return col;
	}

	/**
	 * Accessor for row
	 * 
	 * @return the row of lines where this line is found
	 */
	public int getRow() {
		return row;
	}

	/**
	 * Accessor for orientation
	 * 
	 * @return the orientation of the line, HORIZONTAL or VERTICAL
	 */
	public int getOrientation() {
		return orientation;
	}

	/**
	 * Accessor for visible
	 * 
	 * @return true if the line is visible, otherwise false
	 */
	public boolean isVisible() {
		return visible;
	}

	/**
	 * Accessor for drawn
	 * 
	 * @return true if the line is drawn, otherwise false
	 */
	public boolean isDrawn() {
		return drawn;
	}

	/**
	 * Accessor for color
	 * 
	 * @return the color of the line
	 */
	public Color getColor() {
		return color;
	}
}
