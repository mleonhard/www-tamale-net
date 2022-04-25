import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

/**
 * PuzzleSolution searches the provided initial puzzle to find the shortest
 * solution. The solved puzzle and list of moves are available via accessors.
 * 
 * @author Michael Leonhard (mleonhar)
 * @version 2005-10-30
 * @version JDK 1.5.0.5, Eclipse 3.1.0, Windows XP
 * @version CS 340, Fall 2005, Instructor: Pat Troy, TA: Nitin Jindal
 */
public class PuzzleSolution
{
	private Set existingPuzzles; // list of puzzles that have been found
	private Puzzle solvedPuzzle; // the solved puzzle
	private List solutionMoves; // the moves that constitute the solution
	private List puzzles; // queue of puzzle to be searched

	/**
	 * Accessor for solution moves
	 * 
	 * @return list of PieceMove objects
	 */
	public List getSolutionMoves()
	{
		return solutionMoves;
	}

	/**
	 * Accessor for solved puzzle
	 * 
	 * @return solved puzzle object
	 */
	public Puzzle getSolvedPuzzle()
	{
		return solvedPuzzle;
	}

	/**
	 * Adds the puzzle to the search space, if it doesn't already exist there
	 * 
	 * @param puzzle the puzzle to be added
	 */
	private void addPuzzle(Puzzle puzzle)
	{
		// get a string representation of the puzzle (all puzzles with the same
		// board configuration will have the same string)
		String puzzleString = puzzle.toString();
		// try to add the string to the list of puzzles existing in the search
		boolean isNew = this.existingPuzzles.add(puzzleString);
		// the puzzle is not unique, so do nothing
		if (!isNew)
		{
			// MP4.dprint(this.puzzles.size() + " ALREADY EXISTS:
			// "+puzzleString);
			return;
		}
		// add the puzzle to the queue so it will be searched
		this.puzzles.add(puzzle);
		// MP4.dprint(this.puzzles.size() + " QUEUE <== " + puzzleString);
	}

	/**
	 * Constructor: searches for a solution to the initial puzzle.
	 * 
	 * @param initialPuzzle the starting puzzle
	 */
	public PuzzleSolution(Puzzle initialPuzzle)
	{
		// check parameter
		if (initialPuzzle == null) throw new IllegalArgumentException(
				"initialPuzzle may not be null)");

		// initial puzzle is solved
		if (initialPuzzle.isSolved())
		{
			// save it as the solution
			this.solvedPuzzle = initialPuzzle;
			// empty list of moves
			this.solutionMoves = new LinkedList();
			// done searching before we even started!
			return;
		}

		// allocate data structures
		this.existingPuzzles = new HashSet();
		this.puzzles = new LinkedList();

		// start out with initial puzzle
		this.addPuzzle(initialPuzzle);

		// search the puzzles in the queue
		while (!this.puzzles.isEmpty())
		{
			// get the puzzle from the front of the queue
			Puzzle puzzle = (Puzzle) this.puzzles.remove(0);
			// MP4.dprint("Searching puzzle:\r\n" + puzzle.prettyString());

			// get all puzzle states that can result from a single move on the
			// puzzle
			Collection reachablePuzzles = puzzle.getSingleMovePuzzles();

			// iterate over them
			for (Iterator iter = reachablePuzzles.iterator(); iter.hasNext();)
			{
				// get the new puzzle
				Puzzle newPuzzle = (Puzzle) iter.next();
				// the puzzle is a solution
				if (newPuzzle.isSolved())
				{
					// save the puzzle as the solution
					this.solvedPuzzle = newPuzzle;
					this.solutionMoves = newPuzzle.getMoveList();
					// done searching, cleanup
					this.existingPuzzles = null;
					this.puzzles = null;
					// done!
					return;
				}
				// try to add it to the queue
				this.addPuzzle(newPuzzle);
			}
		}
		// queue is empty, so there is no solution
		this.existingPuzzles = null;
		this.puzzles = null;
		return;
	}
}
