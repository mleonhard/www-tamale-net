/**
 * RSA Key Generator.  Stored private and public keys in XML format.
 *
 * TODO: Disable debugging output
 * TODO: Write tests
 * TODO: Determine algorithm failures (in finding e, d, and div by zero)
 * 
 * Class: CS 340, Fall 2005
 * System: jdk-1.5.0.4, Windows XP
 * @author Michael Leonhard
 * @version 5 Sep 2005
 */

import java.util.*;
import java.io.*;

public class KeyGen
{
	// fields
	private String M_pubkeyFilename = null;
	private String M_privkeyFilename = null;
	private int M_p = -1, M_q = -1;
	private int M_n, M_phi, M_e, M_d;
	
	/**
	 * Constructor for objects of class KeyGen
	 */
	public KeyGen()
	{
	}
	
	/**
	 * Test the number for primeness, return true or false
	 */
	boolean isPrime( int n )
	{
		// prime numbers are all positive and greater than zero
		if( n <= 0 ) return false;
		// even numbers are not prime
		if( n % 2 == 0 ) return false;
		// non-even numbers below 8 are all prime: 1 3 5 7
		if( n < 8 ) return true;
		// search for possible factors
		for( int j = 3; j < n / 2; j += 2 )
		{
			// is j a factor?
			if( n % j == 0 )
			{
				printDebug( "isPrime/1 " + Integer.toString(n) + " is divisible by " + Integer.toString(j) );
				return false;
			}
		}
		printDebug( "isPrime/1 " + Integer.toString(n) + " is prime!" );
		return true;
	}
	
	/**
	 * Return the smallest prime number that is larger than n
	 */
	int findPrime( int n )
	{
		printDebug( "findPrime/1 checking for prime number larger than " + Integer.toString(n) );
		// if n is even then make it odd
		if( n % 2 == 0 ) n = n + 1;
		// loop to find a prime number
		while( !isPrime(n) )
		{
			printDebug( "findPrime/1 " + Integer.toString(n) + " is not prime" );
			n = n + 2;
		}
		printDebug( "findPrime/1 returning " + Integer.toString(n) );
		return n;
	}
	
	/**
	 * Prints debugging messages to the console
	 */
	static public void printDebug( String message )
	{
		//System.out.println( message );
	}
	
	/**
	 * Configures default values that were not specified by the user
	 */
	private void useNeededDefaults()
	{
		if( M_pubkeyFilename == null ) // use default filenames
		{
			M_pubkeyFilename = "pubkey.xml";
			M_privkeyFilename = "privkey.xml";
			// inform user
			System.out.println( "Filenames not specified.  Using defaults: pubkey.xml privkey.xml" );
		}
	}
	
	/**
	 * Try one time to get the second prime number from the user
	 */
	public int getSecondPrimeFromUser( BufferedReader consoleReader ) throws TException,IOException
	{
		int q = -1;

		// prompt the user and read the first number
		System.out.print( "Please enter prime number q:" );
		String qString = consoleReader.readLine().trim();
		// nothing was entered
		if( qString.length() == 0 ) return -1;
		
		try // convert parameter to integer
		{
			q = Integer.valueOf( qString ).intValue();
		} catch( NumberFormatException e ) {
			throw new TException( "You must type a positive integer that is prime." );
		}
		// integer must be prime
		if( !isPrime( q ) ) throw new TException( Integer.toString(q) + " is not prime.  You must type a prime number." );
		// must not be 1
		if( q == 1 ) throw new TException( "The number may not be 1." );
		return q;
	}
	
	/**
	 * Try one time to get the prime numbers from the user
	 */
	private void getPrimeNumbersFromUser( BufferedReader consoleReader ) throws TException,IOException
	{
		int p = -1, q = -1;
		
		// prompt the user and read the first number
		System.out.print( "Please enter prime number p:" );
		String pString = consoleReader.readLine().trim();
		
		// nothing was entered
		if( pString.length() == 0 ) return;
		
		try // convert parameter to integer
		{
			p = Integer.valueOf( pString ).intValue();
		} catch( NumberFormatException e ) {
			throw new TException( "You must type a positive integer that is prime." );
		}
		
		// integer must be prime
		if( !isPrime( p ) ) throw new TException( Integer.toString(p) + " is not prime.  You must type a prime number." );
		// must not be 1
		if( p == 1 ) throw new TException( "The number may not be 1." );
		
		// need second prime number, loop until user enters it correctly
		while( q == -1 )
		{
			try
			{
				q = getSecondPrimeFromUser(consoleReader);
			} catch( TException e ) {
				e.print();
			}
		}
		
		// the numbers must not be equal
		if( p == q ) throw new TException( "You may not enter the same number twice." );
		// the numbers must have a product larger than 127
		if(p * q < 128 ) throw new TException( "The product of the numbers must be greater than 127." );
		// save the values
		M_p = p;
		M_q = q;
	}
	
	/**
	 * Gets required data from the user
	 */
	private void getDataFromUser() throws IOException
	{
		// create a console reader object
		BufferedReader consoleReader = new BufferedReader( new InputStreamReader( System.in ) );
		// need prime numbers, loop until the user enters them correctly
		while( M_p == -1 )
		{
			try
			{
				getPrimeNumbersFromUser(consoleReader);
			} catch( TException e ) {
				e.print();
			}
		}
	}
	
	/**
	 * Processes the command line arguments
	 */
	private void processCommandLine( String[] argv ) throws TException
	{
		// loop through command line arguments
		int argc = argv.length;
		printDebug( "Number of parameters is " + Integer.toString(argc) );
		for( int i = 0; i < argc; i++ )
		{
			printDebug( "PARM: " + argv[i] );
			if( argv[i].equals("-o") )
			{
				// -o option may not be specified twice
				if( M_pubkeyFilename != null ) throw new TException( "Malformed Commandline: the -o option may be specified only once" );
				// there must be two more parameters
				if( i + 2 >= argc ) throw new TException( "Malformed Commandline: please specify two filenames after the -o option" );
				printDebug( "PARMS: " + argv[i+1] + " " + argv[i+2] );
				// the filenames must not be equal
				if( argv[i+1].equals(argv[i+2]) ) throw new TException( "Malformed Commandline: the pubkey and privkey filenames may not be the same." );
				// store filenames
				M_pubkeyFilename = argv[i+1];
				M_privkeyFilename = argv[i+2];
				// two extra parameters processed
				i = i + 2;
			}
			else if( argv[i].equals("-c") )
			{
				// initialize the pseudo random number generator
				Random randomizer = new Random();
				// choose p to be a random prime number in [16,116)
				M_p = findPrime(randomizer.nextInt(100) + 16);
				// choose q to be a random prime number in [16,116) that is not p
				M_q = M_p;
				while( M_q == M_p ) M_q = findPrime(randomizer.nextInt(100) + 16);
			}
			else // look for primes
			{
				// primes may not be specified twice
				if( M_p != -1 ) throw new TException( "Malformed Commandline: unexpected parameter: \"" + argv[i] + "\"" );
				// convert parameter to integer
				try {
					M_p = Integer.valueOf( argv[i] ).intValue();
				} catch( NumberFormatException e ) {
					throw new TException( "Malformed Commandline: expected integer but found \"" + argv[i] + "\"" );
				}
				// integer must be prime
				if( !isPrime( M_p ) ) throw new TException( "Malformed Commandline: expected prime number but found " + Integer.toString(M_p) );
				// must not be 1
				if( M_p == 1 ) throw new TException( "Malformed Commandline: prime number must not be 1" );
				// there must one more parameter
				if( i + 1 >= argc ) throw new TException( "Malformed Commandline: missing second prime number" );
				printDebug( "PARM: " + argv[i+1] );
				try // convert next parameter to integer 
				{ 
					M_q = Integer.valueOf( argv[i+1] ).intValue();
				} catch( NumberFormatException e ) {
					throw new TException( "Malformed Commandline: expected integer but found \"" + argv[i+1] + "\"" );
				}
				
				// integer must be prime
				if( !isPrime( M_q ) ) throw new TException( "Malformed Commandline: expected prime number but found " + Integer.toString(M_q) );
				// must not be 1
				if( M_q == 1 ) throw new TException( "Malformed Commandline: prime number must not be 1" );
				// the numbers must not be equal
				if( M_p == M_q ) throw new TException( "Malformed Commandline: the prime numbers may not be the same" );
				// the numbers must have a product larger than 127
				if( M_p * M_q < 128 ) throw new TException( "Malformed Commandline: the product of the primes must be >127" );
				// one extra parameter processed
				i = i + 1;
			}
			}
		}
	
	/**
	 * Test the a and b for a common denominator larger than 1
	 */
	boolean areRelativelyPrime( int a, int b )
	{
		String text = Integer.toString(a)+" and "+Integer.toString(b);
		printDebug( "areRelativelyPrime/2 testing " + text);
		while( b != 0 )
		{
			int t = b;
			b = a % b;
			a = t;
		}
		if( a == 1 ) printDebug( "areRelativelyPrime/2 " + text + " are relatively prime" );
		else printDebug( "areRelativelyPrime/2 " + text + " are not relatively prime" );
		return a == 1;
	}
	
	/**
	 * Generate n, phi, etc.
	 */
	private void generateKeys() throws TException
	{
		printDebug( "p is " + Integer.toString(M_p) );
		printDebug( "q is " + Integer.toString(M_q) );
		M_n = M_p * M_q;
		printDebug( "n is " + Integer.toString(M_n) );
		
		M_phi = (M_p - 1) * (M_q - 1);
		printDebug( "phi is " + Integer.toString(M_phi) );
		
		// find largest suitable e
		int e = M_n - 1;
		while( e == M_phi || !areRelativelyPrime(e, M_phi) )
		{
			// TODO: Try to find out if this can ever happen
			if( e == 1 ) throw new TException( "Unable to find valid value for e" );
			e--;
		}
		M_e = e;
		printDebug( "e is " + Integer.toString(M_e) );
		
		// search (inefficiently) for d
		int d = 3;
		while( (M_e*d) % M_phi != 1 )
		{
			// TODO: Try to find out if this can ever happen
			if( d == M_n || d == M_phi ) throw new TException( "Unable to find valid value for d" );
			d++;
		}
		M_d = d;
		printDebug( "d is " + Integer.toString(M_d) );
	}
	
	/**
	 * Write the private and public key files in XML format
	 */
	private void writeKeys() throws IOException
	{
		System.out.println( "Writing public key to file: " + M_pubkeyFilename );
		FileWriter pubKeyFile = new FileWriter( M_pubkeyFilename );
		pubKeyFile.write( 
			"<rsakey>\r\n" +
			"\t<evalue>"+Integer.toString(M_e)+"</evalue>\r\n" +
			"\t<nvalue>"+Integer.toString(M_n)+"</nvalue>\r\n" +
			"</rsakey>\r\n");
		pubKeyFile.close();
		
		System.out.println( "Writing private key to file: " + M_privkeyFilename );
		FileWriter privKeyFile = new FileWriter( M_privkeyFilename );
		privKeyFile.write( 
			"<rsakey>\r\n" +
			"\t<dvalue>"+Integer.toString(M_d)+"</dvalue>\r\n" +
			"\t<nvalue>"+Integer.toString(M_n)+"</nvalue>\r\n" +
			"</rsakey>\r\n");
		privKeyFile.close();
		printDebug( "done." );
	}
	
	/**
	 * Obtain (p,q) and generate the keys
	 */
	public void doJob( String[] argv )
	{
		// print program information
		System.out.println
		(
		"CS 340 Project 1 program 1 (KeyGen).\n" +
		"Michael Leonhard, 2005/09/05\n" +
		"Java 1.5.0.4 on Windows XP\n"
		);
		
		try
		{
			processCommandLine( argv );
			useNeededDefaults();
			getDataFromUser();
			generateKeys();
			writeKeys();
		} catch( TException e ) {
			printUsage();
			System.out.println();
			e.print();
			return;
		} catch (IOException e) {
			System.out.println("Problem writing files:");
			System.out.println(e.toString());
			return;
		}
	}
		
	/**
	 * Print the command line usage.
	 */
	public static void printUsage()
	{
		System.out.println( "Command Line Usage: keygen [-o <pub> <priv>] [<p> <q> | -c]" );
		System.out.println( " <pub> is the name of file to write public key" );
		System.out.println( " <priv> is the name of file to write private key" );
		System.out.println( " -c requests that prime numbers be generated automatically" );
		System.out.println( " <p> and <q> are unique prime numbers used to generate the keys" );
		System.out.println( " Note: p * q must be larger than 127" );
		System.out.println( "Example command line: keygen -o pubkey.xml privkey.xml -c" );
	}
	
	/**
	* This method is called when the class is loaded from the command line
	*/
	public static void main( String[] argv )
	{
		// make an instance of the class
		KeyGen instance = new KeyGen();
		// generate the keys
		instance.doJob(argv);
	}
}