/**
 * PieceMove represents the movement of one piece in the puzzle. It holds
 * references to the Piece object that is on the old position and to the piece
 * object that is in the new position.
 * 
 * @author Michael Leonhard (mleonhar)
 * @version 2005-10-30
 * @version JDK 1.5.0.5, Eclipse 3.1.0, Windows XP
 * @version CS 340, Fall 2005, Instructor: Pat Troy, TA: Nitin Jindal
 */
public class PieceMove
{
	private Piece sourcePiece; // the piece before moving
	private Piece destPiece; // the piece after moving
	private char axis; // the axis of movement
	private int delta; // the distance moved (may be negative)

	/**
	 * Constructor: initializes the data members with the provided values
	 * 
	 * @param source the piece before movement
	 * @param dest the piece after movement
	 * @param axis the axis of travel (must be H_MOVE or V_MOVE)
	 * @param delta the distance and direction travelled on the axis (may not be
	 *        zero)
	 */
	public PieceMove(Piece source, Piece dest, char axis, int delta)
	{
		// check parameters
		if (source == null) throw new IllegalArgumentException(
				"source may not be null");
		if (dest == null) throw new IllegalArgumentException(
				"dest may not be null");
		if (axis != Piece.H_MOVE && axis != Piece.V_MOVE) throw new IllegalArgumentException(
				"invalid axis");
		if (delta == 0) throw new IllegalArgumentException(
				"delta may not be zero");

		// save parameters
		this.sourcePiece = source;
		this.destPiece = dest;
		this.axis = axis;
		this.delta = delta;
	}

	/**
	 * Makes a compact human readable string representation of the move
	 * 
	 * @return string version of move
	 */
	public String toString()
	{
		String direction = (axis == Piece.V_MOVE) ? "v" : "h";
		return "PieceMove(" + direction + " " + delta + " source="
				+ sourcePiece + " dest=" + destPiece;
	}

	/**
	 * Makes a nice human readable string representation of the move
	 * 
	 * @return pretty string version of move
	 */
	public String prettyString()
	{
		// direction of travel
		String direction = "??";
		if (axis == Piece.V_MOVE && delta < 0) direction = "up";
		else if (axis == Piece.V_MOVE && delta > 0) direction = "down";
		else if (axis == Piece.H_MOVE && delta < 0) direction = "left";
		else if (axis == Piece.H_MOVE && delta > 0) direction = "right";
		// distance travelled
		int absDelta = (this.delta > 0) ? this.delta : -this.delta;
		String units = "spaces";
		if (absDelta == 1) units = "space";
		// build the string
		return "Piece " + this.destPiece.getName() + " " + direction + " "
				+ absDelta + " " + units;
	}

	/**
	 * Accessor for distance and direction travelled
	 * 
	 * @return the distance travelled, sign indicates direction
	 */
	public int getDelta()
	{
		return delta;
	}

	/**
	 * Accessor for axis of movement
	 * 
	 * @return the axis of movement, V_MOVE or H_MOVE
	 */
	public char getAxis()
	{
		return axis;
	}

	/**
	 * Accessor for the pre-move piece
	 * 
	 * @return the source piece
	 */
	public Piece getSourcePiece()
	{
		return sourcePiece;
	}

	/**
	 * Accessor for the post-move piece
	 * 
	 * @return the destination piece
	 */
	public Piece getDestPiece()
	{
		return destPiece;
	}
}
