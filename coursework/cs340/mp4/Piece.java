import java.util.Collection;
import java.util.LinkedList;

/**
 * Piece represents a rectangular piece in the puzzle. Pieces can move
 * horizontally, vertically, or both, but movement occurrs in only one direction
 * at a time. Piece position and dimensions are immutable. Movement is acheived
 * by generating a list of new pieces in the various possible positions
 * resulting from the "movement" of this piece.
 * 
 * @author Michael Leonhard (mleonhar)
 * @version 2005-10-30
 * @version JDK 1.5.0.5, Eclipse 3.1.0, Windows XP
 * @version CS 340, Fall 2005, Instructor: Pat Troy, TA: Nitin Jindal
 */
public class Piece
{
	private String name; // the name of the piece
	private int x, y, width, height; // position in the puzzle and size
	private char movement;// bits showing horizontal or vertical movability
	public static final char H_MOVE = 0x01; // piece can move horizontally
	public static final char V_MOVE = 0x02; // piece can move vertically

	/**
	 * Constructor: initializes a piece that is not yet part of any puzzle
	 * 
	 * @param x the column where the piece lives
	 * @param y the row where the piece lives
	 * @param width the width of the piece (in columns)
	 * @param height the height of the piece (in rows)
	 * @param movement bit mask of piece's movement ability: V_MOVE, H_MOVE
	 * @throws PuzzleException if any of the parameters are invalid
	 */
	public Piece(int x, int y, int width, int height, char movement)
			throws PuzzleException
	{
		// check parameters
		if (x < 0 | y < 0) throw new PuzzleException(
				"The position of the piece is invalid.");
		if (width < 1 | height < 1) throw new PuzzleException(
				"The dimensions of the piece are invalid.");
		if (((movement & ~(H_MOVE | V_MOVE)) != 0) | movement == 0) throw new PuzzleException(
				"The piece's movement mask contains an unknown flag.");
		if ((movement & (H_MOVE | V_MOVE)) == 0) throw new PuzzleException(
				"The piece has no movement flag (immovable piece is not supported).");
		// save data
		this.x = x;
		this.y = y;
		this.width = width;
		this.height = height;
		this.movement = movement;
	}

	/**
	 * Private Constructor: used for cloning, does no error checking
	 * 
	 * @param x the column where the piece lives
	 * @param y the row where the piece lives
	 * @param width the width of the piece (in columns)
	 * @param height the height of the piece (in rows)
	 * @param movement bit mask of piece's movement ability: V_MOVE, H_MOVE
	 */
	public Piece(int x, int y, int width, int height, char movement, String name)
	{
		// save data
		this.x = x;
		this.y = y;
		this.width = width;
		this.height = height;
		this.movement = movement;
		this.name = name;
	}

	/**
	 * Checks if the piece can fit into the puzzle without overlapping any other
	 * piece or hanging off the edge of the board.
	 * 
	 * @param puzzle the puzzle to check if the piece will fit
	 * @throws PuzzleException if the piece cannot fit into the puzzle
	 */
	public void checkFit(Puzzle puzzle) throws PuzzleException
	{
		// check if we can fit in the puzzle
		for (int x = this.x; x < this.x + this.width; x++)
			for (int y = this.y; y < this.y + this.height; y++)
				if (!puzzle.isEmpty(x, y, null)) throw new PuzzleException(
						"Piece cannot fit into board.  It would overlap with"
								+ "another piece or hang off the edge of the board.");
	}

	/**
	 * Informs the piece that it has been added to the specified puzzle. This
	 * method registers the locations that the piece occupies in the puzzle
	 * board. No error checking is performed so one must call checkFit() before
	 * calling this method.
	 * 
	 * @param puzzle the puzzle to which the piece has been added
	 */
	public void addedToPuzzle(Puzzle puzzle)
	{
		// add ourself to the puzzle
		for (int x = this.x; x < this.x + this.width; x++)
			for (int y = this.y; y < this.y + this.height; y++)
				puzzle.occupyLocation(x, y, this);
	}

	/**
	 * Generates a string representation of the piece
	 * 
	 * @return human readable string with information about the piece
	 */
	public String toString()
	{
		String movementString = "illegal";
		if (((movement & H_MOVE) != 0) & ((movement & V_MOVE) != 0)) movementString = "b";
		else if ((movement & H_MOVE) != 0) movementString = "h";
		else if ((movement & V_MOVE) != 0) movementString = "v";
		return "Piece(" + name + ", " + x + "," + y + " " + width + "x"
				+ height + " " + movementString + ")";
	}

	/**
	 * Mutator for name
	 * 
	 * @param newName the new name of the piece
	 */
	public void setName(String newName)
	{
		this.name = newName;
	}

	/**
	 * Accessor for name
	 * 
	 * @return the name of the piece
	 */
	public String getName()
	{
		return name;
	}

	/**
	 * Accessor for x
	 * 
	 * @return Returns the column where the piece lives
	 */
	public int getX()
	{
		return x;
	}

	/**
	 * Accessor for y
	 * 
	 * @return Returns the row where the piece lives
	 */
	public int getY()
	{
		return y;
	}

	/**
	 * Accessor for width
	 * 
	 * @return Returns the width of the piece (number of columns spanned)
	 */
	public int getWidth()
	{
		return width;
	}

	/**
	 * Accessor for height
	 * 
	 * @return Returns the height of the piece (number of rows spanned)
	 */
	public int getHeight()
	{
		return height;
	}

	/**
	 * Accessor for axes of movement
	 * 
	 * @return axes in which this piece can move: V_MOVE, H_MOVE, or both
	 */
	public char getMovement()
	{
		return movement;
	}

	/**
	 * Finds a list of all valid moves that the piece can make.
	 * 
	 * @param puzzle the puzzle where the piece lives
	 * @return the list of valid moves
	 */
	public Collection getValidMoves(Puzzle puzzle)
	{
		// allocate the list of moves
		LinkedList moves = new LinkedList();

		// the directions in which the piece can travel
		boolean xPosTravel, xNegTravel, yPosTravel, yNegTravel;
		// the piece can travel horizontally
		xPosTravel = xNegTravel = (this.movement & H_MOVE) != 0;
		// the piece can travel vertically
		yPosTravel = yNegTravel = (this.movement & V_MOVE) != 0;

		// check distances from 1 until no more travel is allowed
		for (int dist = 1; xPosTravel | xNegTravel | yPosTravel | yNegTravel; dist++)
		{
			// MP4.dprint("checking distance " + dist+ (xPosTravel ? " xPos" :
			// "")+ (xNegTravel ? " xNeg" : "")+ (yPosTravel ? " yPos" : "")+
			// (yNegTravel ? " yNeg" : ""));

			// add the move of the specified distance
			if (xPosTravel) xPosTravel = addMove(puzzle, moves, H_MOVE, dist);
			if (xNegTravel) xNegTravel = addMove(puzzle, moves, H_MOVE, -dist);
			if (yPosTravel) yPosTravel = addMove(puzzle, moves, V_MOVE, dist);
			if (yNegTravel) yNegTravel = addMove(puzzle, moves, V_MOVE, -dist);
		}

		// return the list of moves
		return moves;
	}

	/**
	 * Checks if the piece fits after moving delta spaces along the specified
	 * axis. A copy of the piece in the destination position is then created. A
	 * PieceMove object is created and added to the provided list.
	 * 
	 * @param puzzle the puzzle where the piece lives
	 * @param moves the list of moves (new move gets added to this)
	 * @param axis the axis of movement: H_MOVE or V_MOVE
	 * @param delta the distance and direction of travel along the axis
	 * @return true if the move is valid, otherwise false
	 * @throws IllegalArgumentException if moves is null, delta is zero, or axis
	 *         is not equal to H_MOVE and not equal to V_MOVE
	 */
	private boolean addMove(Puzzle puzzle, Collection moves, char axis,
			int delta)
	{
		// check parameters
		if (moves == null) throw new IllegalArgumentException(
				"moves list is null");
		if (delta == 0) throw new IllegalArgumentException(
				"delta may not be zero");

		int newX = this.x;
		int newY = this.y;

		// horizontal movement
		if (axis == H_MOVE) newX += delta;
		// vertical movement
		else if (axis == V_MOVE) newY += delta;
		// error
		else throw new IllegalArgumentException("invalid axis");

		// MP4.dprint("checking " + this + " at (" + newX + "," + newY + ") on
		// Puzzle:\r\n"+this.puzzle.prettyString());

		// check if we fit in the new position (overlapping with self is ok)
		for (int x = newX; x < newX + this.width; x++)
		{
			for (int y = newY; y < newY + this.height; y++)
			{
				// the position is occupied by a piece other than us, so give up
				// on this move
				if (!puzzle.isEmpty(x, y, this))
				{
					// MP4.dprint("not empty at (" + x + "," + y + ")");
					return false;
				}
				// MP4.dprint("open at (" + x + "," + y + ")");
			}
		}

		// make a copy of ourself with the new position
		Piece newPiece = new Piece(
				newX, newY, this.width, this.height, this.movement, this.name);

		// make a move object to represent the move
		PieceMove move = new PieceMove(this, newPiece, axis, delta);
		// add it to the list of moves
		moves.add(move);
		// success!
		return true;
	}
}
