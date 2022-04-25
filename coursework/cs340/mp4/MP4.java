import java.io.FileWriter;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

/**
 * MP4 is a sliding block puzzle solver. It coordinates solving the puzzle by
 * instantiating PuzzleLoader and PuzzleSolution and handling the writing of
 * output to the console and HTML file.
 * 
 * TODO Learn how to use Java unit tests
 * 
 * @author Michael Leonhard (mleonhar)
 * @version 2005-10-30
 * @version JDK 1.5.0.5, Eclipse 3.1.0, Windows XP
 * @version CS 340, Fall 2005, Instructor: Pat Troy, TA: Nitin Jindal
 */
public class MP4
{
	// command line usage information
	private static final String USAGE_TEXT = "Usage: MP4 [-h] fileName\r\n"
			+ "  fileName is the name of a file that contains the initial puzzle state\r\n"
			+ "  -h write an HTML report to fileName.html\r\n";

	/**
	 * Prints the string to stdout (commented out in production version)
	 * 
	 * @param string the string to be printed
	 */
	public static void dprint(String string)
	{
		// print out the message
		// System.out.println(string);
		// flush the output buffer so the text appears right away
		// System.out.flush();
	}

	/**
	 * Prints the message to stdout
	 * 
	 * @param string the string to be printed
	 */
	public static void print(String string)
	{
		// print out the message
		System.out.println(string);
		// flush the output buffer so the text appears right away
		System.out.flush();
	}

	/**
	 * This method is called when the class is loaded from the command line. It
	 * processes command line parameters, instantiates PuzzleLoader and
	 * PuzzleSolver, and handles reporting on the console.
	 * 
	 * @param argv command line parameters
	 */
	public static void main(String[] argv)
	{
		// uncomment this line to enable tests
		// Puzzle.testPieceName();

		// print out program info
		print("MP4: Blocks Puzzle Solver by Michael Leonhard (mleonhar)");
		print("CS340, Fall 2005, Instructor: Pat Troy, TA: Nitin Jindal");
		print("2005-10-30 JDK 1.5.0.5, Eclipse 3.1.0, Windows XP\r\n");

		boolean writeHTML = false; // an HTML report will be written
		String fileName = null; // the name of the file to read

		try
		{
			int argc = argv.length; // number of parameters
			dprint("Found " + argc + " parameters");

			// loop through command line parameters
			for (int i = 0; i < argc; i++)
			{
				dprint("Checking parameter \"" + argv[i] + "\"");
				if (argv[i].equals("-h"))
				{
					// -h was specified before
					if (writeHTML) throw new PuzzleException(
							"Malformed Commandline: -h appears more than once");
					// the user wants an HTML report
					writeHTML = true;
				}
				// -g is not supported
				else if (argv[i].equals("-g")) throw new PuzzleException(
						"Malformed Commandline: -g is not supported");
				else
				{
					// file name was encountered already
					if (fileName != null) throw new PuzzleException(
							"Malformed Commandline: \"" + argv[i]
									+ "\" is not expected");
					// save the filename
					fileName = argv[i];
				}
			}
			// file name was not found
			if (fileName == null) throw new PuzzleException(USAGE_TEXT);

		}
		// an error occurred while processing the command line
		catch (PuzzleException e)
		{
			// print out the error and exit
			print(e.toString());
			return;
		}

		// load the initial puzzle
		PuzzleLoader loader = new PuzzleLoader(fileName);
		// show progress
		printInitialReport(loader);

		// the puzzle to solve
		Puzzle puzzle = loader.getPuzzle();

		PuzzleSolution solution;
		// the puzzle was loaded, so search for a solution to it
		if (puzzle != null) solution = new PuzzleSolution(puzzle);
		// no puzzle was loaded, so there is no solution
		else solution = null;

		// generate reports
		printFinalReport(solution);
		if (writeHTML) writeHtmlReport(loader, solution);
	}

	/**
	 * Writes a formatted HTML report with the results of the puzzle load and
	 * solution search operations. The report is written to a file named
	 * FNAME.html where FNAME is the String that was passed to the loader.
	 * 
	 * @param loader the loaded file information
	 * @param solution the solution to the puzzle, or null
	 */
	private static void writeHtmlReport(PuzzleLoader loader,
			PuzzleSolution solution)
	{
		FileWriter file = null;
		try
		{
			// check parameter
			if (loader == null) throw new IllegalArgumentException(
					"loader may not be null");

			// the initial puzzle
			Puzzle puzzle = loader.getPuzzle();

			// report file name
			String puzzleFileName = loader.getFileName();
			String reportFileName = puzzleFileName + ".html";
			MP4.print("Writing HTML report to file \"" + reportFileName + "\"");

			// open the report file
			file = new FileWriter(reportFileName);

			// html bits
			String htmlHeader = "<!DOCTYPE HTML PUBLIC '-//W3C//DTD HTML 4.01 Transitional//EN'>"
					+ "<html>"
					+ "<head><meta http-equiv='Content-Type' content='text/html; charset=iso-8859-1'>"
					+ "<title>"
					+ puzzleFileName
					+ " - MP4 Puzzle Solver Report</title></head>" + "<body>";
			String htmlFooter = "<p>End of report.</p></body></html>";

			// HEADER
			file.write(htmlHeader);

			// TITLE
			file.write("<h1>" + puzzleFileName
					+ " - MP4 Puzzle Solver Report</h1>");

			// if the puzzle was not loaded then we know that a fatal error
			// occurred
			boolean fatalError = puzzle == null;

			// the list of errors
			List errors = loader.getErrors();

			// INITIAL TITLE
			file.write("<h2>Initial Puzzle</h2>");

			// no errors occurred
			if (errors.isEmpty()) file
					.write("<p>Initial puzzle was loaded from &quot;"
							+ puzzleFileName
							+ "&quot; with no errors or warnings.</p>");
			// some errors occurred
			else
			{
				// ERRORS TITLE
				file
						.write("<p>Problems were encountered when reading the file &quot;"
								+ puzzleFileName + "&quot;:</p>" + "<ul>");

				// iterate through errors
				ListIterator iter = errors.listIterator();
				while (iter.hasNext())
				{
					String message = (String) iter.next();
					String type = "Warning";
					// this is a fatal error (the last error)
					if (fatalError && iter.hasNext() == false) type = "Error";
					// write the item
					file.write("<li>" + type + ": " + message + "</li>");
				}
				// ERRORS end
				file.write("</ul>");
			}

			// nothing else to print, so write the footer and finish
			if (fatalError)
			{
				file.write(htmlFooter);
				return;
			}

			// INITIAL PUZZLE
			file.write(puzzle.toHtmlString("Initial Puzzle (" + puzzleFileName
					+ ")"));

			// the puzzle was not loaded, so no solution was searched, so end
			// the report
			if (solution == null)
			{
				file.write(htmlFooter);
				return;
			}

			// SOLUTION TITLE
			file.write("<h2>Solution</h2>");

			// the list of moves that make up the solution
			List moves = solution.getSolutionMoves();

			// no solution was found
			if (moves == null)
			{
				// write the info and end the report
				file.write("<p>The puzzle has no solution.</p>");
				file.write(htmlFooter);
				return;
			}

			// SOLUTION INFO
			file.write("<p>The puzzle's solution consists of the following "
					+ moves.size() + " moves:</p><ol>");

			// each move in the solution
			for (Iterator iter = moves.iterator(); iter.hasNext();)
			{
				// the move
				PieceMove move = (PieceMove) iter.next();
				// the report item
				file.write("<li>" + move.prettyString() + "</li>");
			}

			// SOLUTION END
			file.write("</ol>");

			// SOLVED PUZZLE
			file
					.write(solution.getSolvedPuzzle().toHtmlString(
							"Solved Puzzle"));

			// FOOTER
			file.write(htmlFooter);
		}
		// an error occurred when opening or writing the file
		catch (IOException e)
		{
			print("The HTML report could not be written. " + e);
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
	 * Prints out information about the solution (or lack of one)
	 * 
	 * @param solution the solution to the puzzle, or null
	 */
	private static void printFinalReport(PuzzleSolution solution)
	{
		// the puzzle was not loaded, so no solution was searched, so do nothing
		if (solution == null) return;

		// the list of moves that make up the solution
		List moves = solution.getSolutionMoves();

		// no solution was found
		if (moves == null)
		{
			print("No solution was found.");
			return;
		}

		// print out the moves
		print("The solution consists of the following " + moves.size()
				+ " moves:");
		Iterator iter = moves.iterator();
		for (int i = 1; iter.hasNext(); i++)
		{
			PieceMove move = (PieceMove) iter.next();
			print(" " + i + ". " + move.prettyString());
		}

		// print out the solved puzzle
		print("Solved puzzle:\r\n" + solution.getSolvedPuzzle().prettyString());
	}

	/**
	 * Prints out the results of the file load operation.
	 * 
	 * @param loader the loaded file information
	 */
	private static void printInitialReport(PuzzleLoader loader)
	{
		Puzzle puzzle = loader.getPuzzle();
		// if the puzzle was not loaded then we know that a fatal error occurred
		boolean fatalError = puzzle == null;

		// iterate through warnings
		ListIterator iter = loader.getErrors().listIterator();
		while (iter.hasNext())
		{
			String message = (String) iter.next();
			// this is a fatal error (the last error)
			if (fatalError && iter.hasNext() == false) print("Error: "
					+ message);
			// this is just a warning
			else print("Warning: " + message);
		}

		// nothing else to print
		if (fatalError) return;

		// print out the initial puzzle
		print("Initial Puzzle:\r\n" + puzzle.prettyString());
		print("Finding shortest solution...");
	}
}
