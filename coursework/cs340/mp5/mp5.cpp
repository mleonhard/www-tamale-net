/* King's Corner (MP5)
 * Michael Leonhard (mleonhar)
 * 2005-11-27
 * GCC 3.4.2, Dev-C++ 4.9.9.2, Windows XP
 * CS 340, Fall 2005, Instructor: Pat Troy, TA: Nitin Jindal
 *
 * mp5.cpp - Main function, utility functions
 * 
 * This file contains the program's entry point function, main.
 * It loops for each game, creating Game objects and prompting
 * the user to play again.  It also handles printing of exceptions.
 */

#include "mp5.h"

/* Prompts the user for a yes/no answer
 * returns: 0 or 1
 */
bool getYesNo(string prompt)
{
	string line;
	
	// loop until a proper answer is received
	while(true)
	{
		cout << prompt << " [yes/no]: ";
		getline(cin, line);
		//cout << "GOT \"" << line << "\"" << endl;
		if( line == "yes" ) return true;
		if( line == "no" ) return false;
		else cout
			<<"Your response was not understood.  "
			<<"Please type yes or no." << endl;
	}
};

/* Changes to the next player
 * returns: copy of player, for convenience
 */
Player operator++ (Player& player, int dummy)
{
	assert(player == COMPUTER || player == USER);
	if (player == COMPUTER) player = USER;
	else player = COMPUTER;
	return player;
};

/* Prints the player value to the output stream
 * returns: ostream (to allow expressions like: cout << player << endl)
 */
ostream& operator<< (ostream& out, Player player)
{
	assert(player == COMPUTER || player == USER);
	if (player == COMPUTER) out << "COMP";
	else out << "USER";
	return out;
};

/* Pauses to let the user read */
void waitForUser()
{
	cout << "[press enter to continue]" << endl;
	string dummy;
	getline(cin,dummy);
};

/* Entry point of program */
int main (int argc, char *argv[])
{
	try
	{
		// program information
		cout
			<<"King's Corner (MP5)" << endl
			<<"Michael Leonhard (mleonhar)" << endl
			<<"CS340, Fall 3005, Instructor: Pat Troy, TA: Nitin Jindal"<<endl;
		
		// seed the random number generator
		srand(time(NULL));
		
		// computer deals first
		Player nextDealer = COMPUTER;
		
		// loop for each game
		while(1)
		{
			Game game;
			nextDealer = game.play(nextDealer);
			// does the player want to play again?
			if (!getYesNo("Would you like to play again?")) throw user_quit();
			cout << "Beginning new game." << endl;
		}
	}
	catch (user_quit& e)
	{
		// exit
		cout << "Thanks for playing King's Corner.  Goodbye." << endl;
	}
	catch (exception& e)
	{
		cout << "EXCEPTION: " << e.what() << endl;
		return EXIT_FAILURE;
	}
    return EXIT_SUCCESS;
};
