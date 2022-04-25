/**
 * Represents a box. Sides are Line objects shared with neighboring boxes.
 * 
 * Assignment: MP3
 * Class: CS 340, Fall 2005
 * TA: Nitin Jindal
 * System: jdk-1.5.0.4 and Eclipse 3.1 on Windows XP
 * @author Michael Leonhard (CS account mleonhar)
 * @version 12 Oct 2005
 */

public class Box {
	// constants representing sides
	private static final int NORTH = 0;
	private static final int SOUTH = 1;
	private static final int EAST = 2;
	private static final int WEST = 3;
	// field where the box is found
	private Field field;
	// the box's position in the field
	private int col;
	private int row;
	// Line objects that make the four sides
	private Line line[];
	// stores whether the box has been completed or not
	private boolean completed = false;
	// the user completed the box (false if computer completed it)
	private boolean userCompleted = false;

	/**
	 * Makes a new box in the field
	 * 
	 * @param field the Field object where this box will be rendered
	 * @param col the column of boxes where this box is to be found
	 * @param row the row of boxes where this box is to be found
	 * @param northLine the line that makes up the north side of the box
	 * @param southLine the line that makes up the south side of the box
	 * @param eastLine the line that makes up the east side of the box
	 * @param westLine the line that makes up the west side of the box
	 */
	public Box(Field field, int col, int row, Line northLine, Line southLine,
			Line eastLine, Line westLine) {
		// save data
		this.field = field;
		this.col = col;
		this.row = row;

		// make the line holder
		this.line = new Line[4];
		// save the lines
		this.line[Box.NORTH] = northLine;
		this.line[Box.SOUTH] = southLine;
		this.line[Box.EAST] = eastLine;
		this.line[Box.WEST] = westLine;
		// tell each line that it is one of our sides
		northLine.youAreMySide(this);
		southLine.youAreMySide(this);
		eastLine.youAreMySide(this);
		westLine.youAreMySide(this);
	}

	/**
	 * Makes a string representation of the box
	 * 
	 * @return a string representation of the box
	 */
	public String toString() {
		String completionString = "";
		if (this.completed & this.userCompleted) completionString = " userCompleted";
		if (this.completed & !this.userCompleted) completionString = " computerCompleted";
		return "Box[" + this.col + "," + this.row + completionString + "]";
	}

	/**
	 * Returns the side of the box that is closes to the point
	 * 
	 * @param x the x position to test
	 * @param y the y position to test
	 * @param boxL the length of a box side
	 * @return the line that is closest to the point
	 */
	public Line getNearestLine(int x, int y, int boxL) {
		// System.out.println("Box.getNearestLine() x="+x+" y="+y+" "+this);
		int side;

		// TODO select between undrawn lines so clicking in three sided
		//      box returns the undrawn door
		
		// north east
		if (y < x) {
			// north west, so north
			if (y < boxL - x) side = NORTH;
			// south east, so east
			else side = EAST;
		}
		// south west
		else {
			// north west, so west
			if (y < boxL - x) side = WEST;
			// south east, so south
			else side = SOUTH;
		}
		// return the line
		return this.line[side];
	}

	/**
	 * Returns the opposite of the provided side
	 * 
	 * @param side the side
	 * @return the opposite of side
	 */
	public static int oppositeSide(int side) {
		if (side == NORTH) return SOUTH;
		if (side == SOUTH) return NORTH;
		if (side == EAST) return WEST;
		else return EAST;
	}

	/**
	 * Accessor for box sides
	 * 
	 * @param side the side to retreive
	 * @return the Line object on that side of the box
	 */
	public Line getLine(int side) {
		return this.line[side];
	}

	/**
	 * Accessor for box column position
	 * 
	 * @return the column where this box resides
	 */
	public int getCol() {
		return col;
	}

	/**
	 * Accessor for box row position
	 * 
	 * @return the row where this box resides
	 */
	public int getRow() {
		return row;
	}

	/**
	 * Determines how many sides are drawn
	 * 
	 * @return the number of drawn sides
	 */
	public int numDrawnSides() {
		int num = 0;
		// iterate through lines
		for (int i = 0; i < 4; i++) {
			// the line is drawn
			if (this.line[i].isDrawn()) num++;
		}
		// return the count
		return num;
	}

	/**
	 * Called by Line to inform the box that its fourth side was just drawn in
	 * 
	 * @param userDrawn true user drew the last line, flase if the computer did
	 */
	public void youAreComplete(boolean userDrawn) {
		// set the flags
		this.completed = true;
		this.userCompleted = userDrawn;
		// System.out.println("Box.youAreComplete() " + this);
		// request that the box area be repainted so symbol can show
		this.field.repaintBox(this.col, this.row);
	}

	/**
	 * Returns the completion status of the box
	 * 
	 * @return true if the box has four sides drawn, otherwise false
	 */
	public boolean isCompleted() {
		return completed;
	}

	/**
	 * Returns who completed the box
	 * 
	 * @return true if the user drew the last side, false if the computer did
	 */
	public boolean isUserCompleted() {
		return userCompleted;
	}
}
