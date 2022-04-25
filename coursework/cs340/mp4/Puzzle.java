import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

/**
 * Puzzle is used to represent a single puzzle state. It keeps a list of Pieces
 * contained in the puzzle. It also uses an array of the same dimensions as the
 * puzzle to keep track of the occupancy status of each location on the board. A
 * list of moves indicates how one can transition from the initial puzzle state
 * to this one. The class is also able to generate a list of new puzzle objects
 * that each represent a state that is reachable by moving only one piece on
 * this puzzle.
 * 
 * A maximum of 127 pieces is supported.
 * 
 * @author Michael Leonhard (mleonhar)
 * @version 2005-10-30
 * @version JDK 1.5.0.5, Eclipse 3.1.0, Windows XP
 * @version CS 340, Fall 2005, Instructor: Pat Troy, TA: Nitin Jindal
 */
public class Puzzle
{
	private int width, height; // dimensions of the puzzle, in columns and rows
	private ArrayList pieces; // list of pieces in the puzzle
	// array representing squares of the puzzle
	// board[x][y] is a reference to the piece occupying column x and row y
	private Piece[][] board;
	private ArrayList moveList; // list of moves that led to this puzzle
	private boolean solved; // puzzle is solved (goal piece at rightmost edge)

	/**
	 * Constructor: initializes an empty puzzle
	 * 
	 * @param width the number of columns in the puzzle, must be >0
	 * @param height the number of rows in the puzzle, must be >0
	 * @throws PuzzleException if the dimensions are invalid
	 */
	public Puzzle(int width, int height) throws PuzzleException
	{
		MP4.dprint("Puzzle(width=" + width + " height=" + height + ")");
		// check parameters
		if (width < 1 | height < 1) throw new PuzzleException(
				"The dimensions of the puzzle are invalid.");
		// save data
		this.width = width;
		this.height = height;
		// allocate pieces list
		this.pieces = new ArrayList();
		// allocate board
		this.board = new Piece[width][height];
		// allocate moves list
		this.moveList = new ArrayList();
	}

	/**
	 * Private Constructor: used for cloning, no error checking is performed
	 * 
	 * @param width the number of columns in the puzzle
	 * @param height the number of rows in the puzzle
	 * @param pieces the ArrayList of pieces
	 * @param board the array of references representing the puzzle board
	 */
	private Puzzle(int width, int height, ArrayList pieces, Piece[][] board,
			ArrayList moveList)
	{
		// save data
		this.width = width;
		this.height = height;
		this.pieces = pieces;
		this.board = board;
		this.moveList = moveList;
	}

	/**
	 * Returns a name for the piece with the specified number
	 * 
	 * @param num the number of the piece, must be in the range -1<num<127
	 * @return a name for the piece
	 * @throws IllegalArgumentException if num is out of range
	 */
	private static String pieceName(int num) throws IllegalArgumentException
	{
		// 61 piece names, as defined in the problem description:
		// Z 1..9 a..z A..Y
		String[] name = { "Z", "1", "2", "3", "4", "5", "6", "7", "8", "9",
				"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l",
				"m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x",
				"y", "z", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J",
				"K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V",
				"W", "X", "Y" };

		// negative
		if (num < 0) throw new IllegalArgumentException("num out of range");
		// single digit
		else if (num < 61) return name[num];
		// double digit, use # as first digit
		else if (num < 122) return "#" + name[num % 61];
		// double digit, use $ as first digit
		else if (num < 127) return "$" + name[num % 61];
		// out of range
		else throw new IllegalArgumentException("num out of range");
	}

	/**
	 * Add the piece to the puzzle.
	 * 
	 * @param piece the piece to be added
	 * @throws PuzzleException if the piece cannot fit in the puzzle, or there
	 *         are too many pieces
	 */
	public void add(Piece piece) throws PuzzleException
	{
		// the number of this piece
		int pieceNum = this.pieces.size();
		// we already have the maximum number of pieces
		if (pieceNum > 126) throw new PuzzleException(
				"The puzzle is full, with 127 pieces.");
		// let the piece check if it can fit in the board (throws exception)
		piece.checkFit(this);
		// add the piece to the list
		this.pieces.add(piece);
		// let the piece register the locations it occupies on the board
		piece.addedToPuzzle(this);
		// give the piece its name
		piece.setName(pieceName(pieceNum));
	}

	/**
	 * Checks if the specified location on the puzzle board is empty or occupied
	 * by the specified piece
	 * 
	 * @param x the column of the location to check
	 * @param y the row of the location to check
	 * @param piece piece that may already occupy the location
	 * @return true if the location is on the board and is occupied by the
	 *         provided piece or no piece, otherwise false
	 */
	public boolean isEmpty(int x, int y, Piece piece)
	{
		// location not on the board
		if (x < 0 || x >= this.width || y < 0 || y >= this.height) return false;

		// MP4.dprint("isEmpty board[" + x + "," + y + "] is " +
		// this.board[x][y]+ " piece=" + piece);

		// location is occupied by specified piece
		if (this.board[x][y] == piece) return true;

		// location is not occupied
		else if (this.board[x][y] == null) return true;

		// location is occupied by another piece
		return false;
	}

	/**
	 * Sets the piece as the occupant of the specified location
	 * 
	 * @param x the column of the location
	 * @param y the row of the location
	 * @param piece the piece to occupy the location
	 * @throws IllegalArgumentException if the location is not on the board
	 * @throws RuntimeException if the location is already occupied
	 */
	public void occupyLocation(int x, int y, Piece piece)
	{
		// location not on the board
		if (x < 0 || x >= this.width || y < 0 || y >= this.height) throw new IllegalArgumentException(
				"Piece " + piece + " cannot occupy location (" + x + "," + y
						+ ") as it is not on the board.");

		// location is already occupied
		else if (this.board[x][y] != null) throw new RuntimeException("Piece "
				+ piece + " cannot occupy location (" + x + "," + y
				+ ") as it is already occupied by piece " + this.board[x][y]);

		// the location is unoccupied
		else
		{
			// set the piece in the location
			this.board[x][y] = piece;

			// is the right column and the goal piece
			if (x == this.width - 1 && piece == this.pieces.get(0))
			{
				// this puzzle is now solved
				this.solved = true;
				// MP4.dprint("Puzzle is solved: " + this.prettyString());
			}
		}
	}

	/**
	 * Accessor for pieces array length
	 * 
	 * @return number of pieces in the puzzle
	 */
	public int getNumPieces()
	{
		return this.pieces.size();
	}

	/**
	 * Accessor for move list
	 * 
	 * @return list of PieceMove objects represening the moves taken from the
	 *         initial puzzle state to reach this puzzle
	 */
	public List getMoveList()
	{
		return this.moveList;
	}

	/**
	 * Accessor for solved field
	 * 
	 * @return true if the puzzle is solved, otherwise false
	 */
	public boolean isSolved()
	{
		return this.solved;
	}

	/**
	 * Constructs a string consisting of n copies of toDup. Example:
	 * dupString("*", 3) returns "***".
	 * 
	 * @param toDup the string to duplicate
	 * @param n how many times to duplicate it
	 * @return the constructed string
	 */
	public static String dupString(String toDup, int n)
	{
		String result = "";
		// make copies of toDup
		while (n-- > 0)
			result = result + toDup;
		// done!
		return result;
	}

	/**
	 * Returns a string version of the puzzle suitable for display to the user
	 * 
	 * @return a human readable string version of the puzzle
	 */
	public String prettyString()
	{
		int locationWidth = 1;
		// we have pieces with names that have two characters
		if (this.pieces.size() > 60) locationWidth = 2;

		// horizontal border
		String border = " "
				+ dupString("*", 1 + this.width * locationWidth + 1);
		// initialize with top border
		String result = border + "\r\n";

		// each row
		for (int y = 0; y < this.height; y++)
		{
			// left border
			result = result + " *";
			// each location in the row
			for (int x = 0; x < this.width; x++)
			{
				String locationName;
				// the location has no piece
				if (this.board[x][y] == null) locationName = ".";
				// the location has a piece
				else locationName = this.board[x][y].getName();
				// make padding so every location is the same width
				String padding = dupString(" ", locationWidth
						- locationName.length());
				// add the name
				result = result + padding + locationName;
			}
			// done with this row, add right border and start a new line
			result = result + "*\r\n";
		}
		// return with bottom border
		return result + border;
	}

	/**
	 * Returns a string representation of the puzzle. Two puzzles with identical
	 * pieces in the same locations will always have the same representational
	 * string.
	 * 
	 * @return a string representation of the puzzle
	 */
	public String toString()
	{
		String result = "";
		// each row
		for (int y = 0; y < this.height; y++)
			// each location in the row
			for (int x = 0; x < this.width; x++)
			{
				// the location has no piece
				if (this.board[x][y] == null) result = result + " ";
				// the location has a piece
				else result = result + this.board[x][y].getName();
			}
		return result;
	}

	/**
	 * Returns an HTML string version of the puzzle. A table is used to lay out
	 * the puzzle and its pieces.
	 * 
	 * @param caption a text title for the table, must not be null
	 * @return HTML string representation of the puzzle
	 */
	public String toHtmlString(String caption)
	{
		// check parameter
		if (caption == null) throw new IllegalArgumentException(
				"caption may not be null");

		// TABLE start
		String result = "<table class='puzzletable' border='1'><caption>"
				+ caption + "</caption>";

		// allocate list to hold visited pieces
		ArrayList visitedPieces = new ArrayList();

		// each row
		for (int y = 0; y < this.height; y++)
		{
			// ROW start
			result = result + "<tr>";
			// each location in the row
			for (int x = 0; x < this.width; x++)
			{
				// the piece for this location
				Piece piece = this.board[x][y];
				// the location has no piece, so make an empty cell
				if (piece == null) result = result
						+ "<td width='50' height='50'></td>";
				// the location has a piece that we have not visited
				else if (!visitedPieces.contains(piece))
				{
					// add it to the list of visited pieces
					visitedPieces.add(piece);
					// the piece may span columns or rows
					String colSpan = "";
					if (piece.getWidth() > 1) colSpan = " colspan='"
							+ piece.getWidth() + "'";
					String rowSpan = "";
					if (piece.getHeight() > 1) rowSpan = " rowspan='"
							+ piece.getHeight() + "'";
					// output the piece
					result = result
							+ "<td width='50' height='50' align='center' valign='middle'"
							+ colSpan + rowSpan + ">" + piece.getName()
							+ "</td>";
				}
				// we have already visited this piece, so do nothing
				else
				;
			}
			// ROW end
			result = result + "</tr>";
		}
		return result + "</table>";
	}

	/**
	 * Searches for all valid puzzles that can be obtained by making a single
	 * move on this puzzle. If a solved puzzle is found, a partial result will
	 * be returned, with the solved puzzle at its head.
	 * 
	 * @return the list of puzzles (may be empty)
	 */
	public Collection getSingleMovePuzzles()
	{
		// allocate the list
		Collection list = new LinkedList();

		// check each piece
		for (Iterator iter = this.pieces.iterator(); iter.hasNext();)
		{
			// the piece to check
			Piece piece = (Piece) iter.next();
			// MP4.dprint("Searching moves of " + piece);

			// all valid moves for this piece
			Collection moves = piece.getValidMoves(this);

			// each move
			for (Iterator moveIter = moves.iterator(); moveIter.hasNext();)
			{
				// get the move object
				PieceMove move = (PieceMove) moveIter.next();
				// MP4.dprint(move.toString());

				// clone the puzzle, using the move
				Puzzle newPuzzle = this.clone(move);
				// add it to the list of puzzles
				list.add(newPuzzle);
				// the solution was found so stop searching
				if (newPuzzle.isSolved()) return list;
			}
		}
		// return the list
		return list;
	}

	/**
	 * Creates a clone of the puzzle where the piece has been moved, as
	 * specified in the move object.
	 * 
	 * @param move the move that resulted in this puzzle
	 * @return the new Puzzle object
	 */
	private Puzzle clone(PieceMove move)
	{
		// the pieces of the move
		Piece oldPiece = move.getSourcePiece();
		Piece newPiece = move.getDestPiece();

		// copy our pieces list
		ArrayList newPieces = (ArrayList) this.pieces.clone();
		// overwrite the source piece with the dest piece in the new list
		int i = newPieces.indexOf(oldPiece);
		// there should never be an error here
		if (i < 0) throw new RuntimeException(
				"oldPiece not found in newPieces list");
		// MP4.dprint("replacing newPieces["+i+"]="+newPieces.get(i)+" with
		// "+newPiece);
		newPieces.set(i, newPiece);

		// allocate a new board array
		Piece[][] newBoard = new Piece[this.width][this.height];
		// copy the entries in the board, except for references to the old piece
		for (int y = 0; y < this.height; y++)
			for (int x = 0; x < this.width; x++)
			{
				// the reference at this location
				Piece pieceRef = this.board[x][y];

				// MP4.dprint("board[" + x + "," + y + "] is " + pieceRef
				// + " old=" + oldPiece + " new=" + newPiece);

				// the reference is to the old piece, so use null
				if (pieceRef == oldPiece) pieceRef = null;
				// save the reference in the new board
				newBoard[x][y] = pieceRef;
			}

		// copy our move list
		ArrayList newMoveList = (ArrayList) this.moveList.clone();
		// add this move to it
		newMoveList.add(move);

		// instantiate the new puzzle
		Puzzle newPuzzle = new Puzzle(
				this.width, this.height, newPieces, newBoard, newMoveList);

		// let the new piece register the locations it occupies on the board
		newPiece.addedToPuzzle(newPuzzle);

		// the new puzzle is complete!
		return newPuzzle;
	}

	// ////////////////////////////////////////////////////////////////////////
	// TESTS
	// ////////////////////////////////////////////////////////////////////////
	/**
	 * Tests pieceName() with the supplied number
	 * 
	 * @param num the number to test
	 * @param expectedResult the expected result
	 * @throws IllegalArgumentException if the result is not as expected
	 */
	private static void testPieceName(int num, String expectedResult)
			throws IllegalArgumentException
	{
		// execute the method
		String result = pieceName(num);
		// the expect result was obtained
		if (result.equals(expectedResult)) return;
		// bad result
		throw new IllegalArgumentException("Test failed for PieceName(num="
				+ num + "): expected=\"" + expectedResult + "\" got=\""
				+ result + "\".");
	}

	/**
	 * Tests pieceName() with the supplied number
	 * 
	 * @param num the number to test
	 * @throws IllegalArgumentException if pieceName() does not generate an
	 *         IllegalArgumentException exception
	 * 
	 */
	private static void testPieceNameException(int num)
			throws IllegalArgumentException
	{
		// execute the method
		try
		{
			pieceName(num);
		}
		// exception occurred as expected
		catch (IllegalArgumentException e)
		{
			return;
		}
		// no exception occurred, so report this unexpected result
		throw new IllegalArgumentException(
				"Test failed for PieceName(num="
						+ num
						+ "): expected IllegalArgumentException but none was generated.");
	}

	/**
	 * Tests pieceName(int) with many different inputs, checking that the
	 * correct result is returned or an exception is properly thrown.
	 * 
	 * @throws IllegalArgumentException to indicate the results of the tests
	 */
	private static void testPieceName() throws IllegalArgumentException
	{
		// Test Valid Ranges
		// 0 -> Z (1)
		testPieceName(0, "Z");
		// 1..9 -> 1..9 (9)
		testPieceName(1, "1");
		testPieceName(9, "9");
		// 10..35 -> a..z (26)
		testPieceName(10, "a");
		testPieceName(35, "z");
		// 36..60 -> A..Y (25)
		testPieceName(36, "A");
		testPieceName(60, "Y");
		// 61 -> #Z (1)
		testPieceName(61, "#Z");
		// 62..70 -> #1..#9 (9)
		testPieceName(62, "#1");
		testPieceName(70, "#9");
		// 71..96 -> #a..#z (26)
		testPieceName(71, "#a");
		testPieceName(96, "#z");
		// 97..121 -> #A..#Y (25)
		testPieceName(97, "#A");
		testPieceName(121, "#Y");
		// 122 -> $Z (1)
		testPieceName(122, "$Z");
		// 123..126 -> $1..$4 (5)
		testPieceName(123, "$1");
		testPieceName(126, "$4");

		// Test Invalid Ranges
		testPieceNameException(123124124);
		testPieceNameException(127);
		testPieceNameException(-1);
		testPieceNameException(-12414124);

		// tests completed successfully
		throw new IllegalArgumentException("Tests completed successfully.");
	}
}
