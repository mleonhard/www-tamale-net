/* King's Corner (MP5)
 * Michael Leonhard (mleonhar)
 * 2005-11-27
 * GCC 3.4.2, Dev-C++ 4.9.9.2, Windows XP
 * CS 340, Fall 2005, Instructor: Pat Troy, TA: Nitin Jindal
 */

#include "mp5.h"

using namespace std;

/* Constructor, initializes data members
 */
Game::Game ()
 : computerPenaltyPoints_(0), userPenaltyPoints_(0)
{
	//cout << "Game::Game()" << endl;
};

/* Prints a string representation of the object to the specied outstream
 * out: the outstream on which to print
 */
void Game::print (ostream& out) const
{
	assert(computerPenaltyPoints_ >= 0 && userPenaltyPoints_ >= 0);
	out 
		<< "[Game computer=" << computerPenaltyPoints_
		<< "pts user=" << userPenaltyPoints_ << "pts]";
};

/* Plays a single game, creates Round objects for each round, keeps track of
 * player penalty points.
 * nextDealer: the player who will deal the next hand
 * returns: the player who will deal the next hand
 */
Player Game::play (Player nextDealer)
{
	assert(computerPenaltyPoints_ >= 0 && userPenaltyPoints_ >= 0);
	
	//cout << "Game::play(nextDealer=" << nextDealer << ") " << *this << endl;
	
	// loop for each round
	while (computerPenaltyPoints_ < 25 & userPenaltyPoints_ < 25)
	{
		// make a round object and play it
		Round round;
		round.deal(nextDealer);
		round.play();
		
		// process result of round
		//cout << "Result of round is " << round << endl;
		computerPenaltyPoints_ += round.getComputerPenaltyPoints();
		userPenaltyPoints_ += round.getUserPenaltyPoints();
		// print out result of round
		cout << endl;
		cout << "The computer has " << computerPenaltyPoints_ 
			<< " penalty points." << endl;
		cout << "You have " << userPenaltyPoints_ 
			<< " penalty points." << endl;
		// let the user read it before moving on
		waitForUser();
		// change dealers for next round
		nextDealer++;
	}
	announceWinner();
	return nextDealer;
};

/* Announces the winner and scores of the game.
 */
void Game::announceWinner()
{
	assert(computerPenaltyPoints_ >= 0 && userPenaltyPoints_ >= 0);
	
	if (computerPenaltyPoints_ >= 25 & userPenaltyPoints_ < 25)
		cout << "You Won the Game!  Congratulations!" << endl;
	else if (computerPenaltyPoints_ < 25 & userPenaltyPoints_ >= 25)
		cout << "The computer won the game.  Better luck next time." << endl;
	else throw runtime_error("Error determining winner.");
}


/* Facilitates printing game to outstream using << operator
 * returns: outstream (to allow expressions like: cout << game << endl)
 */
ostream& operator<< (ostream& out, const Game& g)
{
	g.print(out);
	return out;
};
