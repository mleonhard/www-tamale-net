/**
 * RSA Cipher Program.  Encrypts or decrypts, reads XML key file.
 * 
 * TODO: Disable debugging output
 * 
 * Class: CS 340, Fall 2005
 * System: jdk-1.5.0.4, Windows XP
 * @author Michael Leonhard
 * @version 8 Sep 2005
 */

import java.util.*;
import java.io.*;
import java.math.BigInteger;

import javax.xml.parsers.*;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;

public class RsaCipher
{
	// fields
	private String M_inFilename = null;
	private String M_outFilename = null;
	private String M_keyFilename = null;
	private int M_n = -1;
	private int M_e = -1;
	private int M_d = -1;
	private int M_operation = -1;
	static int OPERATION_ENCRYPT = 0;
	static int OPERATION_DECRYPT = 1;
	
	/**
	 * Constructor for objects of class RsaCipher
	 */
	public RsaCipher()
	{
	}
	
	/**
	 * Prints debugging messages to the console
	 */
	static public void printDebug( String message )
	{
		//System.out.println( message );
	}
	
	/**
	 * Prompt the user and return a string, returning null instead of blank string
	 */
	private String getStringFromUser(String Prompt, BufferedReader consoleReader) throws IOException
	{
		// prompt the user and read the first number
		System.out.print(Prompt);
		String typedString = consoleReader.readLine().trim();
		// nothing was entered
		if( typedString.length() == 0 ) return null;
		return typedString;
	}
	
	/**
	 * Gets required data from the user
	 */
	private void getDataFromUser() throws IOException
	{
		// create a console reader object
		BufferedReader consoleReader = new BufferedReader( new InputStreamReader( System.in ) );
		// query user for required parameters, tailor prompts to desired operation
		if(M_operation == OPERATION_ENCRYPT)
		{
			while(M_keyFilename == null) M_keyFilename = getStringFromUser("Please enter name of public key file: ", consoleReader);
			while(M_inFilename == null) M_inFilename = getStringFromUser("Please enter name of plain text file to encrypt: ", consoleReader);
			while(M_outFilename == null) M_outFilename = getStringFromUser("Please enter name of ciphertext file to write: ", consoleReader);
		}
		else if(M_operation == OPERATION_DECRYPT)
		{
			while(M_keyFilename == null) M_keyFilename = getStringFromUser("Please enter name of private key file: ", consoleReader);
			while(M_inFilename == null) M_inFilename = getStringFromUser("Please enter name of ciphertext file to decrypt: ", consoleReader);
			while(M_outFilename == null) M_outFilename = getStringFromUser("Please enter name of plaintext file to write: ", consoleReader);
		}
		// program should never reach here
		else throw new Error("M_operation is invalid");
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
			if( argv[i].equals("-d") )
			{
				// -e or -d must not have been specified before
				if( M_operation != -1 ) throw new TException( "Malformed Commandline: only one -e or -d is allowed" );
				M_operation = OPERATION_DECRYPT;
			}
			else if( argv[i].equals("-e") )
			{
				// -e or -d must not have been specified before
				if( M_operation != -1 ) throw new TException( "Malformed Commandline: only one -e or -d is allowed" );
				M_operation = OPERATION_ENCRYPT;
			}
			else if( argv[i].equals("-f") )
			{
				// -f option may not be specified twice
				if( M_inFilename != null ) throw new TException( "Malformed Commandline: the -f option may be specified only once" );
				// there must be one more parameter
				if( i + 1 >= argc ) throw new TException( "Malformed Commandline: please specify a filename after the -f option" );
				printDebug( "PARM: " + argv[i+1] );
				// store filename
				M_inFilename = argv[i+1];
				// one extra parameter was processed
				i = i + 1;
			}
			else if( argv[i].equals("-o") )
			{
				// -o option may not be specified twice
				if( M_outFilename != null ) throw new TException( "Malformed Commandline: the -o option may be specified only once" );
				// there must be one more parameter
				if( i + 1 >= argc ) throw new TException( "Malformed Commandline: please specify a filename after the -o option" );
				printDebug( "PARM: " + argv[i+1] );
				// store filename
				M_outFilename = argv[i+1];
				// one extra parameter was processed
				i = i + 1;
			}
			else if( argv[i].equals("-k") )
			{
				// -k option may not be specified twice
				if( M_keyFilename != null ) throw new TException( "Malformed Commandline: the -k option may be specified only once" );
				// there must be one more parameter
				if( i + 1 >= argc ) throw new TException( "Malformed Commandline: please specify a filename after the -k option" );
				printDebug( "PARM: " + argv[i+1] );
				// store filename
				M_keyFilename = argv[i+1];
				// one extra parameter was processed
				i = i + 1;
			}
			// unknown parameter
			else throw new TException( "Malformed Commandline: unexpected parameter: \"" + argv[i] + "\"" );
		}
		
		// -e or -d must be specified
		if( M_operation == -1 ) throw new TException( "Malformed Commandline: -e or -d must be specified" );
	}
	
	/**
	 * Extract an integer from the supplied tag
	 */
	private int getIntTag(String tagName, Document document) throws TException
	{
		NodeList tagList = document.getElementsByTagName(tagName);
		if( tagList.getLength() != 1 ) throw new TException("Keyfile format error: "+tagName+" tag not found");
		// the <tagName> ... </tagName> tag
		Node tagNode = tagList.item(0);
		if( tagNode.getNodeType() != Node.ELEMENT_NODE )throw new TException("Keyfile format error: "+tagName+" tag is wrong type");
		// the items contained inside it
		NodeList childList = tagNode.getChildNodes();
		if( childList.getLength() != 1 ) throw new TException("Keyfile format error: "+tagName+" tag does not contain exactly one item");
		// the first and only item must be type text
		Node textNode = childList.item(0);
		if( textNode.getNodeType() != Node.TEXT_NODE ) throw new TException("Keyfile format error: expected text inside "+tagName+" tag");
		String valueString = textNode.getNodeValue().trim();
		printDebug(tagName + " is " + valueString);
		try { 
			// convert string to integer
			int value = Integer.valueOf(valueString).intValue();
			// must be positive and nonzero
			if( value < 1 ) throw new TException("Keyfile format error: "+tagName+" must be >0");
			// done!
			return value;
		} catch(NumberFormatException e) {
			throw new TException( "Keyfile format error: expected integer in "+tagName+" but found \"" + valueString + "\"" );
		}
	}
	
	/**
	 * Read the user-specified key file into a JDOM tree
	 */
	private void readKey()
		throws TException, javax.xml.parsers.ParserConfigurationException, org.xml.sax.SAXException
	{
		try 
		{
			DocumentBuilderFactory builderFactory = DocumentBuilderFactory.newInstance();
			DocumentBuilder docBuilder = builderFactory.newDocumentBuilder();
			Document document = docBuilder.parse(new File(M_keyFilename));
			Element rootElement = document.getDocumentElement();
			rootElement.normalize();
			printDebug("Keyfile root tag is " + rootElement.getTagName());
			// root tag must be rsakey
			if( !rootElement.getTagName().equals("rsakey") ) throw new TException("Keyfile format error: root tag must be rsakey");
			// get necessary values
			M_n = getIntTag("nvalue", document);
			if(M_operation == OPERATION_ENCRYPT ) M_e = getIntTag("evalue", document);
			if(M_operation == OPERATION_DECRYPT ) M_d = getIntTag("dvalue", document);
		} catch (java.io.IOException e) {
			throw new TException(
				"Unable to read key file \""+M_keyFilename+"\".\n"+
				"Technical error information: " + e.toString() );
		}
	}
	
	/**
	 * Reads inFile one byte at a time, writing one encrypted base10 integer to outFile per line
	 */
	private void doEncryption(FileReader inFile, FileWriter outFile) throws TException, IOException
	{
		try {
			BigInteger e = new BigInteger(Integer.toString(M_e));
			BigInteger n = new BigInteger(Integer.toString(M_n));
			// loop for each character
			for( int mChar = inFile.read(); mChar != -1; mChar = inFile.read() )
			{
				BigInteger m = new BigInteger(Integer.toString(mChar));
				String mPrimeString = m.modPow(e, n).toString();
				outFile.write(mPrimeString + "\n");
				printDebug(Integer.toString(mChar)+" -> " + mPrimeString);
			}
		} catch(ArithmeticException e){
			throw new TException("Error during encryption: " + e.toString());
		} catch(NumberFormatException e) {
			throw new TException("Error during encryption: " + e.toString());
		}
	}
	
	/**
	 * Reads inFile one line at a time, converts line contents to integer,
	 * decrypts integer and writes single character to outFile
	 */
	private void doDecryption(FileReader inFile, FileWriter outFile) throws TException, IOException
	{
		BufferedReader inBR = new BufferedReader( inFile );
		try {
			BigInteger d = new BigInteger(Integer.toString(M_d));
			BigInteger n = new BigInteger(Integer.toString(M_n));
			// loop for each line
			for( String line = inBR.readLine(); line != null; line = inBR.readLine() )
			{
				BigInteger m = new BigInteger(line);
				int mPrime = m.modPow(d, n).intValue();
				outFile.write(mPrime);
				printDebug(line+" -> " + Integer.toString(mPrime));
			}
		} catch(ArithmeticException e){
			throw new TException("Error during decryption: " + e.toString());
		} catch(NumberFormatException e) {
			throw new TException("Error during decryption: " + e.toString());
		}
	}
	
	/**
	 * Opens the in and out files, calls the proper method to perform the operation, closes files
	 */
	private void doOperation() throws IOException, TException
	{
		FileReader inFile;
		FileWriter outFile;
		
		// open input file
		try {
			inFile = new FileReader(M_inFilename);
		} catch (FileNotFoundException e) {
			throw new TException("Unable to read file: " + e.toString());
		}
		// open output file
		try {
			outFile = new FileWriter(M_outFilename);
		} catch (IOException e) {
			throw new TException("Unable to write to file: " + e.toString());
		}
		
		// do the operation
		if(M_operation == OPERATION_ENCRYPT) doEncryption(inFile, outFile);
		else doDecryption(inFile, outFile);
		
		// close the files
		inFile.close();
		outFile.close();
	}
	
	/**
	 * Call methods to the file and key, encrypt or decrypt, write the result
	 */
	public void doJob( String[] argv )
	{
		// print program information
		System.out.println
		(
		"CS 340 Project 1 program 2 (RsaCipher).\n" +
		"Michael Leonhard, 2005/09/08\n" +
		"Java 1.5.0.4 on Windows XP\n"
		);
		
		try
		{
			processCommandLine( argv );
			
			printDebug("M_inFilename = " + M_inFilename);
			printDebug("M_outFilename = " + M_outFilename);
			printDebug("M_keyFilename = " + M_keyFilename);
			if(M_operation == OPERATION_ENCRYPT) printDebug("M_operation = OPERATION_ENCRYPT");
			else if(M_operation == OPERATION_DECRYPT) printDebug("M_operation = OPERATION_ENCRYPT");
			else printDebug("M_operation = -1");
			
			getDataFromUser();
			
			printDebug("M_inFilename = " + M_inFilename);
			printDebug("M_outFilename = " + M_outFilename);
			printDebug("M_keyFilename = " + M_keyFilename);
			
			readKey();
			printDebug("M_n = " + Integer.toString(M_n));
			printDebug("M_e = " + Integer.toString(M_e));
			printDebug("M_d = " + Integer.toString(M_d));
			doOperation();
		} catch( TException e ) {
			e.print();
			return;
		} catch (IOException e) {
			System.out.println("Problem reading or writing files:");
			System.out.println(e.toString());
			return;
		} catch (javax.xml.parsers.ParserConfigurationException e) {
			System.out.println("Java XML Parser is not available.");
			System.out.println(e.toString());
			return;
		} catch (org.xml.sax.SAXException e) {
			System.out.println("Unable to load keyfile as XML.");
			System.out.println(e.toString());
			return;
		}
	}
	
	/**
	 * Print the command line usage.
	 */
	public static void printUsage()
	{
		System.out.println( "Command Line Usage: rsacipher -d|-e [-f <infile>] [-o outfile] [-k keyfile]" );
 		System.out.println( " -d decrypt the file, keyfile contains dvalue");
		System.out.println( " -e encrypt the file, keyfile contains evalue");
		System.out.println( " -f the file to en/decrypt, user prompted if -f not present");
		System.out.println( " -k keyfile, user prompted if -k not supplied");
		System.out.println( " -o file to write result, user prompted if not present");
		System.out.println( "Example: rsacipher -e -f hello.txt -o cipher.txt -k pubkey.xml" );
		System.out.println( "Example: rsacipher -d -f cipher.txt -o hello.txt -k privkey.xml" );
	}
	
	/**
	* This method is called when the class is loaded from the command line
	*/
	public static void main( String[] argv )
	{
		// show program usage information (help)
		if( argv.length == 0 )
		{
			printUsage();
			return;
		}
		// make an instance of the class
		RsaCipher instance = new RsaCipher();
		// generate the keys
		instance.doJob(argv);
	}
}