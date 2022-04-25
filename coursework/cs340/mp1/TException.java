/**
 * TException class is used for generic program exceptions to notify the caller
 * that an exceptional condition occurred and provide carry a text description.
 * 
 * Class: CS 340, Fall 2005
 * System: jdk-1.5.0.4, Windows XP
 * @author Michael Leonhard
 * @version 8 Sep 2005
 */

public class TException extends Exception
{
	// fields
	private String M_text;
	
	/**
	 * Default constructor.  Default description is "Unspecified exception."
	*/
	public TException()
	{
		M_text = "Unspecified exception.";
	}
	
	/**
	 * Constructor.  Description may be specified.
	 */
	public TException( String description )
	{
		M_text = description;
	}
	
	// accessor
	public String toString() { return M_text; }
	
	/**
	 * Prints exception description on the console
	 */
	public void print()
	{
		System.out.println( M_text );
	}
}