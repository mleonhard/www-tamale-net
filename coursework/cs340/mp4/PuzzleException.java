/**
 * Signals an error in loading or solving the puzzle. A message is provided.
 * 
 * @author Michael Leonhard (mleonhar)
 * @version 2005-10-30
 * @version JDK 1.5.0.5, Eclipse 3.1.0, Windows XP
 * @version CS 340, Fall 2005, Instructor: Pat Troy, TA: Nitin Jindal
 */
public class PuzzleException extends Exception
{
	private static final long serialVersionUID = 1L;

	/**
	 * Constructor: saves the provided message
	 * 
	 * @param message a string describing the exceptional state
	 */
	public PuzzleException(String message)
	{
		super(message);
	}

	/**
	 * Returns the message describing the exception
	 * 
	 * @return the message describing the exception
	 */
	public String toString()
	{
		return super.getMessage();
	}
}