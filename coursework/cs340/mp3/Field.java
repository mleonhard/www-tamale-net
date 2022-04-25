/**
 * Displays the game field consisting of a grid of dots and lines.  Forwards
 * mouse events to state machine.
 * 
 * Assignment: MP3
 * Class: CS 340, Fall 2005
 * TA: Nitin Jindal
 * System: jdk-1.5.0.4 and Eclipse 3.1 on Windows XP
 * @author Michael Leonhard (CS account mleonhar)
 * @version 12 Oct 2005
 */

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.util.Random;
import java.util.Vector;
import javax.swing.JComponent;
import javax.swing.event.MouseInputListener;

public class Field extends JComponent implements MouseInputListener {
	// version number of this class, used for serialization
	private static final long serialVersionUID = 1L;
	// game state object
	private State state;
	// number of columns of boxes
	int cols;
	// number of rows of boxes
	int rows;
	// origin of the box field
	private Point origin;
	// the length of each box side
	private int boxL;
	// size of a dots
	private int dotSize;
	// thickness of lines;
	private int lineThickness;
	// arrays of vertical and horizontal lines
	// format is line[VERTICAL][col][row]
	private Line[][][] line;
	// array of Box objects, box[col][row]
	private Box[][] box;
	// Pseudo-random number generator
	Random prng;

	// color of the field background
	private Color backgroundColor;
	// color of dots
	private Color dotColor;
	// color of X symbol (computer completed square)
	private Color xColor;
	// color of O symbol (user completed square)
	private Color oColor;

	/**
	 * Constructor, prepares object for rendering
	 * 
	 * @param cols number of columns in field
	 * @param rows number of rows in field
	 * @param computerCounter counter for computer's score
	 * @param userCounter counter for user's score
	 */
	public Field(int numCols, int numRows, CounterLabel userCount,
			CounterLabel computerCount) {
		// allow the super-class to initialize (JComponent)
		super();

		// save data
		this.cols = numCols;
		this.rows = numRows;

		// allocate origin data
		this.origin = new Point();

		// create colors
		this.backgroundColor = new Color(0xeeeeee);
		this.dotColor = Color.BLACK;
		this.xColor = new Color(0xa25f5f);
		this.oColor = new Color(0x72BD89);;

		// initialize the pseudo random number generator
		this.prng = new Random();

		// make state object
		this.state = new State(this, userCount, computerCount);

		// make array to hold lines
		this.line = new Line[2][this.cols + 1][this.rows + 1];

		// make Line objects
		for (int row = 0; row <= this.rows; row++) {
			for (int col = 0; col <= this.cols; col++) {
				// not lowest row, so make vertical line
				if (row != this.rows) this.line[Line.VERTICAL][col][row] = new Line(
						this, Line.VERTICAL, col, row);
				// not rightmost column, so make horizontal line
				if (col != this.cols) this.line[Line.HORIZONTAL][col][row] = new Line(
						this, Line.HORIZONTAL, col, row);
			}
		}

		// make array to hold Box objects
		this.box = new Box[this.cols][this.rows];

		// make Box objects
		for (int row = 0; row < this.rows; row++) {
			for (int col = 0; col < this.cols; col++) {
				// box sides
				Line northLine = this.line[Line.HORIZONTAL][col][row];
				Line southLine = this.line[Line.HORIZONTAL][col][row + 1];
				Line eastLine = this.line[Line.VERTICAL][col + 1][row];
				Line westLine = this.line[Line.VERTICAL][col][row];
				// make the box
				this.box[col][row] = new Box(this, col, row, northLine,
						southLine, eastLine, westLine);
			}
		}

		// listen to own mouse input
		this.addMouseListener(this);
		this.addMouseMotionListener(this);
	}

	/**
	 * Returns true if this component is completely opaque.
	 * 
	 * @return true because this widget draws its own background
	 */
	public boolean isOpaque() {
		return true;
	}

	/**
	 * Calculates scaling and position of field, requests repaint.
	 * 
	 * @param newWidth the new width of the component
	 * @param newHeight the new height of the component
	 */
	private void newSize(int newWidth, int newHeight) {
		// box side lengths
		double sideX = (newWidth - 2.0) / (this.cols + 0.4);
		double sideY = (newHeight - 2.0) / (this.rows + 0.4);
		// choose the smaller of the two
		double side = sideX;
		if (sideY < sideX) side = sideY;
		// center of the widget
		int centerX = newWidth / 2;
		int centerY = newHeight / 2;
		// the length of a box side (must be >0)
		this.boxL = (int) side;
		if (this.boxL < 1) this.boxL = 1;
		// calculate the dimensions of the field area
		int fieldWidth = this.boxL * this.cols;
		int fieldHeight = this.boxL * this.rows;
		// the origin of the field
		this.origin.x = (int)centerX - (fieldWidth / 2);
		this.origin.y = (int)centerY - (fieldHeight / 2);

		// dot size
		this.dotSize = (int) (side / 10);
		if (this.dotSize == 0) this.dotSize = 1;
		// line thickness
		this.lineThickness = (int) (side / 20);
		if (this.lineThickness == 0) this.lineThickness = 1;

		// request a repaint
		this.repaint();
	}

	/**
	 * Invoked by Swing to draw the component.
	 * 
	 * @param g the Graphics context in which to paint
	 */
	public void paint(Graphics g) {
		// TODO paint only inside clip rectangle

		// draw background
		Rectangle bounds = this.getBounds();
		g.setColor(this.backgroundColor);
		g.fillRect(bounds.x, bounds.y, bounds.width, bounds.height);

		// DRAW X and O in COMPLETED BOXES
		int off = this.lineThickness * 2;
		int off2 = off * 2;
		// iterate over rows and columns of boxes
		for (int row = 0; row < this.rows; row++) {
			for (int col = 0; col < this.cols; col++) {
				// box is completed
				if (this.box[col][row].isCompleted()) {
					int y = this.origin.y + row * boxL;
					int x = this.origin.x + col * boxL;
					// user made this box, so draw an O
					if (this.box[col][row].isUserCompleted()) {
						g.setColor(this.oColor);
						g.fillOval(x + off, y + off, this.boxL - off2,
								this.boxL - off2);
						g.setColor(this.backgroundColor);
						g.fillOval(x + off2, y + off2, this.boxL - off2 * 2,
								this.boxL - off2 * 2);
					}
					// computer made it, so drawn an X
					else {
						g.setColor(this.xColor);
						g.fillOval(x + this.lineThickness, y
								+ this.lineThickness, this.boxL
								- this.lineThickness * 2, this.boxL
								- this.lineThickness * 2);
						g.setColor(this.backgroundColor);
						// east
						g.fillArc(x + off2, y + off, this.boxL - off2,
								this.boxL - off2, -45, 90);
						// west
						g.fillArc(x, y + off, this.boxL - off2, this.boxL
								- off2, 135, 90);
						// north
						g.fillArc(x + off, y, this.boxL - off2, this.boxL
								- off2, 45, 90);
						// south
						g.fillArc(x + off, y + off2, this.boxL - off2,
								this.boxL - off2, -135, 90);
					}
				}
			}
		}

		// DRAW LINES
		// half the thickness of a line
		int halfThick = this.lineThickness / 2;
		// iterate over lines (two for every dot, except lower right dot)
		for (int row = 0; row <= this.rows; row++) {
			int y = this.origin.y + row * boxL;
			for (int col = 0; col <= this.cols; col++) {
				int x = this.origin.x + col * boxL;
				// not rightmost column, so draw horizontal line
				if (col != this.cols) {
					// line objects
					Line hor = this.line[Line.HORIZONTAL][col][row];
					// horizontal line is visible
					if (hor.isVisible()) {
						g.setColor(hor.getColor());
						g.fillRect(x, y - halfThick, boxL, this.lineThickness);
					}
				}
				// not lowest row, so draw vertical line
				if (row != this.rows) {
					Line ver = this.line[Line.VERTICAL][col][row];
					// vertical line is visible
					if (ver.isVisible()) {
						g.setColor(ver.getColor());
						g.fillRect(x - halfThick, y, this.lineThickness, boxL);
					}
				}
			}
		}

		// DRAW DOTS
		g.setColor(this.dotColor);
		// half the dot size
		int halfDot = this.dotSize / 2;
		// iterate over rows of dots (not boxes!)
		for (int row = 0; row <= this.rows; row++) {
			int y = this.origin.y + row * boxL;
			// iterate over columns of dots (not boxes!)
			for (int col = 0; col <= this.cols; col++) {
				int x = this.origin.x + col * boxL;
				// draw dot
				g.fillOval(x - halfDot, y - halfDot, dotSize, dotSize);
			}
		}
	}

	/**
	 * Resize this component to the supplied dimensions
	 * 
	 * @param d the new size of this component
	 */
	public void setSize(Dimension d) {
		// System.out.println("Field.setSize: " + d);
		// let the super-class handle this
		super.setSize(d);
		// get ready to render as the new size
		this.newSize(d.width, d.height);
	}

	/**
	 * Resize this component to the specified width and height
	 * 
	 * @param width the new width of this component in pixels
	 * @param height the new height of this component in pixels
	 */
	public void setSize(int width, int height) {
		// System.out.println("Field.setSize: width=" + width + " height="
		// + height);
		// let the super-class handle this
		super.setSize(width, height);
		// get ready to render as the new size
		this.newSize(width, height);
	}

	/**
	 * Moves and resizes this component. The new location of the top-left corner
	 * is specified by x and y, and the new size is specified by width and
	 * height.
	 * 
	 * @param x the new x-coordinate of this component
	 * @param y the new y-coordinate of this component
	 * @param width the new width of this component
	 * @param height the new height of this component
	 */
	public void setBounds(int x, int y, int width, int height) {
		// System.out.println("Field.setBounds: x=" + x + " y=" + y + " width="
		// + width + " height=" + height);
		// let the super-class handle this
		super.setBounds(x, y, width, height);
		// get ready to render as the new size
		this.newSize(width, height);
	}

	/**
	 * Gets the line that is nearest the given point
	 * 
	 * @param x the x-coordinate of the point
	 * @param y the y-coordinate of the point
	 * @return the line that is nearest to the point
	 */
	private Line getNearestLine(int x, int y) {
		// find the mouse position relative to the field origin
		x -= origin.x;
		y -= origin.y;
		// mouse is over the box at this row and column
		int col = x / boxL;
		int row = y / boxL;

		// clamp point into field
		if (col < 0) col = 0;
		if (col >= this.cols) col = this.cols - 1;
		if (row < 0) row = 0;
		if (row >= this.rows) row = this.rows - 1;

		// the nearest box
		Box nearestBox = this.box[col][row];

		// find mouse position relative to the box's origin
		x -= boxL * col;
		y -= boxL * row;

		// get the Line from the Box objectt
		return nearestBox.getNearestLine(x, y, boxL);
	}

	/**
	 * Invoked when the mouse button has been clicked (pressed and released) on
	 * a component. Does nothing.
	 * 
	 * @param e information about the mouse
	 */
	public void mouseClicked(MouseEvent e) {
		// this event is ignored
	}

	/**
	 * Invoked when a mouse button has been pressed on a component. Passes event
	 * to state machine.
	 * 
	 * @param e information about the mouse
	 */
	public void mousePressed(MouseEvent e) {
		// lookup line nearest to the mouse pointer
		Line nearestLine = getNearestLine(e.getX(), e.getY());
		// pass the event to state machine
		this.state.mousePressed(nearestLine);
	}

	/**
	 * Invoked when a mouse button has been released on a component. Passes
	 * event to state machine.
	 * 
	 * @param e information about the mouse
	 */
	public void mouseReleased(MouseEvent e) {
		// lookup line nearest to the mouse pointer
		Line nearestLine = getNearestLine(e.getX(), e.getY());
		// pass the event to state machine
		this.state.mouseReleased(nearestLine);
	}

	/**
	 * Invoked when the mouse enters a component. Passes event to state machine.
	 * 
	 * @param e information about the mouse
	 */
	public void mouseEntered(MouseEvent e) {
		// lookup line nearest to the mouse pointer
		Line nearestLine = getNearestLine(e.getX(), e.getY());
		// pass the event to state machine
		this.state.mouseMoved(nearestLine);
	}

	/**
	 * Invoked when the mouse exits a component. Informs state machine.
	 * 
	 * @param e information about the mouse
	 */
	public void mouseExited(MouseEvent e) {
		// inform state machine that mouse is not near any line
		this.state.mouseMoved(null);
	}

	/**
	 * Invoked when a mouse button is pressed on a component and then dragged.
	 * Forwards movement event to state machine.
	 * 
	 * @param e information about the mouse
	 */
	public void mouseDragged(MouseEvent e) {
		// lookup line nearest to the mouse pointer
		Line nearestLine = getNearestLine(e.getX(), e.getY());
		// pass the event to state machine
		this.state.mouseMoved(nearestLine);
	}

	/**
	 * Invoked when the mouse cursor has been moved onto a component but no
	 * buttons have been pushed. Forwards event to state machine.
	 * 
	 * @param e information about the mouse
	 */
	public void mouseMoved(MouseEvent e) {
		// lookup line nearest to the mouse pointer
		Line nearestLine = getNearestLine(e.getX(), e.getY());
		// pass the event to state machine
		this.state.mouseMoved(nearestLine);
	}

	/**
	 * Initiates a repaint for the area occupied by the specified line
	 * 
	 * @param orientation the line is Line.HORIZONTAL or Line.VERTICAL
	 * @param col the column of the line
	 * @param row the row of the line
	 */
	public void repaintLine(int orientation, int col, int row) {
		// half the thickness of a line
		int halfThick = this.lineThickness / 2;
		// coordinates of box origin
		int x = this.origin.x + col * boxL;
		int y = this.origin.y + row * boxL;
		// line is horizontal, request repainting of its rectangle
		if (orientation == Line.HORIZONTAL) this.repaint(x, y - halfThick,
				boxL, this.lineThickness);
		// line is vertical, request repainting of its rectangle
		else this.repaint(x - halfThick, y, this.lineThickness, boxL);
	}

	/**
	 * Initiates a repaint for the area occupied by the specified box
	 * 
	 * @param col the column of the box
	 * @param row the row of the box
	 */
	public void repaintBox(int col, int row) {
		// half the thickness of a line
		int halfThick = this.lineThickness / 2;
		// coordinates of box origin
		int x = this.origin.x + col * this.boxL;
		int y = this.origin.y + row * this.boxL;
		// request a repaint of the rectangular region
		this.repaint(x - halfThick, y - halfThick, this.boxL
				+ this.lineThickness, this.boxL + this.lineThickness);
	}

	/**
	 * Checks if any line is undrawn
	 * 
	 * @return true if there is an undrawn line, otherwise false
	 */
	public boolean hasUndrawnLine() {
		// iterate over lines
		for (int row = 0; row <= this.rows; row++) {
			for (int col = 0; col <= this.cols; col++) {
				// not rightmost column, so check horizontal line
				if (col != this.cols) {
					// horizontal line is not drawn
					if (!this.line[Line.HORIZONTAL][col][row].isDrawn()) {
						// System.out.println("Field.hasUndrawnLine() "
						// + this.line[Line.HORIZONTAL][col][row]
						// + " is undrawn");
						return true;
					}
				}
				// not lowest row, so check vertical line
				if (row != this.rows) {
					// vertical line is not drawn
					if (!this.line[Line.VERTICAL][col][row].isDrawn()) {
						// System.out.println("Field.hasUndrawnLine() "
						// + this.line[Line.VERTICAL][col][row]
						// + " is undrawn");
						return true;
					}
				}
			}
		}
		// all lines are drawn
		return false;
	}

	/**
	 * Searches for boxes whose number of drawn sides is n or m
	 * 
	 * @param n number of drawn sides
	 * @param m number of drawn sides
	 * @return the boxes that match the criterion
	 */
	private Box[] boxesWithNumDrawnSides(int n, int m) {
		// make vector to hold the boxes found
		Vector matchingBoxes = new Vector();
		// check every Box object
		for (int row = 0; row < this.rows; row++) {
			for (int col = 0; col < this.cols; col++) {
				Box candidateBox = this.box[col][row];
				int num = candidateBox.numDrawnSides();
				// box has the required number of sides, so add to the list
				if (num == n || num == m) matchingBoxes.add(candidateBox);
			}
		}
		// make array to hold result
		Box[] result = new Box[matchingBoxes.size()];
		// return result as array of Box objects
		return (Box[]) matchingBoxes.toArray(result);
	}

	/**
	 * Chooses a box at random and then one of its sides at random
	 * 
	 * @param boxes the boxes to choose from
	 * @return the randomly chosen side
	 */
	private Line randomUndrawnSide(Box[] boxes) {
		// choose a box at random
		int n = prng.nextInt(boxes.length);
		Box chosenBox = boxes[n];

		// loop until an undrawn side is chosen
		while (true) {
			// TODO move this into Box.java, pass prng
			// choose a side at random
			n = prng.nextInt(4);
			// get the line corresponding to that side
			Line chosenLine = chosenBox.getLine(n);
			// line is undrawn
			if (!chosenLine.isDrawn()) return chosenLine;
		}
	}

	/**
	 * Chooses an undrawn line according to following algorithm:
	 * 
	 * Search for all boxes with 3 drawn sides. Choose a side at random. If no
	 * boxes have 3 drawn sides then search for boxes with 0 or 1 drawn sides.
	 * Choose one at random. If no boxes have 0 or 1 drawn sides then search for
	 * boxes with 2 drawn sides. Choose a side at random. If no boxes are found
	 * then return null.
	 * 
	 * @return the chosen line, or null
	 */
	public Line computerChooseLine() {
		// get all boxes with 3 drawn sides
		Box[] matchingBoxes = boxesWithNumDrawnSides(3, 3);
		// some boxes were found, choose a random side
		if (matchingBoxes.length > 0) return randomUndrawnSide(matchingBoxes);
		// get all boxes that have 0 or 1 drawn sides
		matchingBoxes = boxesWithNumDrawnSides(0, 1);
		// some boxes were found, choose a random side
		if (matchingBoxes.length > 0) return randomUndrawnSide(matchingBoxes);
		// get all boxes that have 2 drawn sides (there must be some)
		matchingBoxes = boxesWithNumDrawnSides(2, 2);
		// some boxes were found, choose a random side
		if (matchingBoxes.length > 0) return randomUndrawnSide(matchingBoxes);
		// no boxes were found with 0, 1, 2, or 3 undrawn sides, so all lines
		// are drawn
		return null;
	}
}
