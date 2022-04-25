import java.io.BufferedReader;
import java.io.EOFException;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * PuzzleLoader reads and processes a puzzle definition file. The resulting
 * puzzle object and error messages are available through accessors. For a
 * description of the file format, please see the <a href="MP4 Assignment
 * Description.html">assignment description</a>.
 * 
 * @author Michael Leonhard (mleonhar)
 * @version 2005-10-30
 * @version JDK 1.5.0.5, Eclipse 3.1.0, Windows XP
 * @version CS 340, Fall 2005, Instructor: Pat Troy, TA: Nitin Jindal
 */
public class PuzzleLoader
{
	private Puzzle puzzle; // the puzzle that was loaded from the file, or null
	private ArrayList errors; // errors encoungtered while loading the file
	private String fileName; // the name of the file

	/**
	 * Constructor: reads a puzzle definition from the file with the specified
	 * file name and creates a puzzle object from it, warnings and error are
	 * stored in the object.
	 * 
	 * @param fileName name of a file from which to read a puzzle definition
	 */
	public PuzzleLoader(String fileName)
	{
		// check parameter
		if (fileName == null) throw new IllegalArgumentException(
				"fileName may not be null");
		// save parameter
		this.fileName = fileName;

		// initialize the list of errors
		this.errors = new ArrayList();

		// read the initial puzzle state from the file
		FileReader file = null;
		try
		{
			// open input file
			file = new FileReader(fileName);
			BufferedReader reader = new BufferedReader(file);
			// read the file
			readPuzzle(reader);
		}
		// there was a problem with the format of the file
		catch (PuzzleException e)
		{
			// save the error message
			this.errors.add(e.toString());
			MP4.dprint(e.toString());
		}
		// the file couldn't be opened
		catch (FileNotFoundException e)
		{
			// save the error message
			this.errors.add("Unable to read file: " + e.toString());
			MP4.dprint("Unable to read file: " + e.toString());
		}
		// there was an error reading the file
		catch (IOException e)
		{
			// save the error message
			this.errors.add("Error reading file: " + e.toString());
			MP4.dprint("Error reading file: " + e.toString());
		}
		// close the file
		finally
		{
			try
			{
				// the file has been opened, so close it
				if (file != null) file.close();
			}
			// ignore any error closing the file
			catch (IOException e)
			{
			}
		}
	}

	/**
	 * Reads puzzle definition and creates a puzzle state object with pieces
	 * 
	 * @param reader the source of the line
	 * @throws PuzzleException if the file doesn't define a valid puzzle
	 * @throws IOException if the file cannot be read
	 */
	private void readPuzzle(BufferedReader reader) throws PuzzleException,
			IOException
	{
		// read the first line
		String line = reader.readLine();
		// unable to read
		if (line == null) throw new PuzzleException("File is empty");

		// split up the line
		String[] num = line.trim().split("\\s+");
		// not exactly 2 strings
		if (num.length != 2) throw new PuzzleException(
				"First line of file is malformed. It should contain two integers.");

		// try to convert the strings to numbers
		int height = 0, width = 0;
		try
		{
			height = Integer.parseInt(num[0]); // number of rows in puzzle
			width = Integer.parseInt(num[1]); // number of columns in
		}
		// the strings could not be converted
		catch (NumberFormatException e)
		{
			throw new PuzzleException(
					"First line of file is malformed. It should contain two integers.");
		}

		// check the numbers
		if (height < 1 || width < 1) throw new PuzzleException(
				"Error in first line of file: width and height must be greater than zero.");

		// allocate the initial puzzle state object
		this.puzzle = new Puzzle(width, height);

		// loop through lines of file
		for (int lineNumber = 2;; lineNumber++)
		{
			// catch errors
			try
			{
				// read the line and try to make a piece for it
				Piece piece = readPiece(reader);
				// try to add the piece to the puzzle
				this.puzzle.add(piece);
				// debugging printouts
				MP4.dprint("Added " + piece + ". New puzzle state:");
				MP4.dprint(this.puzzle.prettyString());
			}
			// an error occurred, treat it as a warning: save it and continue
			catch (PuzzleException e)
			{
				// save the error
				this.errors.add("Problem on line " + lineNumber + " of file: "
						+ e);
				MP4.dprint("Problem on line " + lineNumber + " of file: " + e);
				// go on to the next line
				continue;
			}
			// all lines in file were read
			catch (EOFException e)
			{
				// no valid pieces were found
				if (puzzle.getNumPieces() < 1)
				{
					// discard the empty puzzle
					this.puzzle = null;
					// send the error up to the constructor
					throw new PuzzleException(
							"The file contains no valid piece definitions.");
				}
				MP4.dprint("EOF");
				return;
			}
		}
	}

	/**
	 * Reads a line, processes it, and returns a new Piece object
	 * 
	 * @param reader the source of the line
	 * @return the piece represented by the data in the line
	 * @throws IOException if the line cannot be read
	 * @throws EOFException when the end of the file is reached
	 * @throws PuzzleException if the line contains an invalid piece definition
	 */
	private static Piece readPiece(BufferedReader reader) throws IOException,
			EOFException, PuzzleException
	{
		// read the line
		String line = reader.readLine();
		// end of file
		if (line == null) throw new EOFException();

		// split up the line
		String[] num = line.trim().split("\\s+");

		// for debugging
		String niceLine = "Line:";
		for (int i = 0; i < num.length; i++)
			niceLine = niceLine + " \"" + num[i] + "\"";
		MP4.dprint(niceLine);

		// not exactly 5 strings
		if (num.length != 5) throw new PuzzleException(
				"Piece definition should contain 5 items.");

		try
		{
			// convert the strings to numbers
			int y = Integer.parseInt(num[0]) - 1; // row position
			int x = Integer.parseInt(num[1]) - 1; // column position
			int width = Integer.parseInt(num[2]); // width
			int height = Integer.parseInt(num[3]); // height

			// interpret the movement flag
			char movement = 0;
			if (num[4].equals("h")) movement = Piece.H_MOVE;
			else if (num[4].equals("v")) movement = Piece.V_MOVE;
			else if (num[4].equals("b")) movement = Piece.H_MOVE | Piece.V_MOVE;
			else throw new PuzzleException(
					"Piece definition has unknown movement flag. Should be h, v, or b.");

			// make the piece object
			return new Piece(x, y, width, height, movement);
		}
		// a string could not be converted to integer
		catch (NumberFormatException e)
		{
			// convert the error
			throw new PuzzleException(
					"Piece definition contains malformed number.");
		}
	}

	/**
	 * Accessor for file name string
	 * 
	 * @return the fileName.
	 */
	public String getFileName()
	{
		return fileName;
	}

	/**
	 * Accessor for loaded puzzle
	 * 
	 * @return the puzzle, or null the puzzle could not be loaded
	 */
	public Puzzle getPuzzle()
	{
		return puzzle;
	}

	/**
	 * Accessor for the list of errors that were generated when reading the
	 * file. If the puzzle could not be loaded (getPuzzle() returns null), the
	 * cause of error will be the last item in the errors list.
	 * 
	 * @return the list of errors Strings
	 */
	public List getErrors()
	{
		return errors;
	}
}
