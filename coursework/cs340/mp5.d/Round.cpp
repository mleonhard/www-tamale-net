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
Round::Round ()
{
	//cout << "Round::Round()" << endl;
	cout << endl
		<< "Preparing for new round.  Shuffling deck." << endl;
	
	// initialize data members
	computerPenaltyPoints_ = 0;
	userPenaltyPoints_ = 0;
	// make laydown piles
	for (int i = 0; i < 8; i++)
		laydownPile_[i] = new LaydownPile(i>3, i+1);
	
	//cout << "draw pile is " << drawPile_ << endl;
};

/* Destructor, frees data members */
Round::~Round ()
{
	assert(computerPenaltyPoints_ >= 0 && userPenaltyPoints_ >= 0);
	//cout << "Round::~Round()" << endl;
	// delete laydown piles
	for (int i = 0; i < 8; i++)
	{
		assert(laydownPile_[i] != NULL);
		delete laydownPile_[i];
	}
};

/* Deals the cards
 * dealer: the player who deals the cards
 */
void Round::deal (Player dealer)
{
	assert(computerPenaltyPoints_ >= 0 && userPenaltyPoints_ >= 0);
	//cout << "Round::deal(dealer=" << dealer << ")" << endl;
	
	// inform the user
	if (dealer == COMPUTER) cout << "The computer deals." << endl;
	else if (dealer == USER) cout << "You deal." << endl;
	else throw runtime_error("unable to determine who deals");
	
	dealer++;					// the person opposite dealer
	turnTaker_ = dealer;		// is the first to take a turn
	
	// first turn taker also gets dealt first
	Player recipient = turnTaker_;
	//cout << "recipient=" << recipient << endl;
	
	// deal 14 cards
	for (int i=0; i < 14; i++)
	{
		// get the card to be dealt
		Card card = drawPile_.drawCard();
		assert(!card.isBlank());
		
		// deal it to the recipient
		//cout << "Dealing " << card << " to " << recipient << endl;
		if (recipient == COMPUTER) computerHand_.addCard(card);
		else if (recipient == USER) userHand_.addCard(card);
		else throw runtime_error("unable to deal");
		
		// change recipients
		recipient++;
	}
	
	// deal four cards into non-corner laydown piles
	for (int i=0; i < 4; i++)
	{
		// get the card to be dealt
		Card card = drawPile_.drawCard();
		assert(!card.isBlank());
		//cout << "Dealing " << card << " to pile " << i << endl;
		// add it to the laydown pile
		laydownPile_[i]->laydownCard(card);
	}
	
	//for (int i=0; i < 8; i++)
	//	cout << *laydownPile_[i] << endl;
	//cout << "Computer's hand: " << computerHand_ << endl;
	//cout << "User's hand: " << userHand_ << endl;
	//cout << drawPile_ << endl;	
}
 
/* Prints a string representation of the object to the specified ostream.
 * out: the ostream on which to print
 */
void Round::print (ostream& out) const
{
	assert(computerPenaltyPoints_ >= 0 && userPenaltyPoints_ >= 0);
	out << "[Round"
		<< " computer=" << computerPenaltyPoints_ << "pts"
		<< " user=" << userPenaltyPoints_ << "pts"
		<< " turn=" << turnTaker_
		<< "]";
};

/* Plays a single round by looping through sets of turns until one
 * player wins.
 */
void Round::play()
{
	//cout << "Round::play() " << *this << endl;
	
	// loop through turns, until a hand is empty
	while(!computerHand_.isEmpty() && !userHand_.isEmpty())
	{
		if (turnTaker_ == COMPUTER) computerTakesTurn();
		else if (turnTaker_ == USER) userTakesTurn();
		else throw runtime_error("invalid player taking turn");
		
		// next turn is taken by other player
		turnTaker_++;
	}
	// print out the final game configuration
	printGameInfo();
	// count each user's penalty points (one will be zero)
	computerPenaltyPoints_ = computerHand_.countPenaltyPoints();
	userPenaltyPoints_ = userHand_.countPenaltyPoints();
	// announce points
	if (computerPenaltyPoints_ > 0)
		cout << "Computer got " << computerPenaltyPoints_ 
			<< " penalty points from leftover cards." << endl;
	if (userPenaltyPoints_ > 0)
		cout << "You got " << userPenaltyPoints_ 
			<< " penalty points from leftover cards." << endl;
};

/* Attempts to play on pile from hand, or by moving laydown pile */
bool Round::playOnPile(LaydownPile* pile)
{
	//cout << "Round::playOnPile(pile=" << *pile << ") " << *this << endl;
	// try to play from non-corner piles
	for (int srcPile=0; srcPile<4; srcPile++)
		if (laydownPile_[srcPile]->playerMoveToPile(pile,COMPUTER))
			return true;
	// try to play from hand
	if (computerHand_.computerPlayOnPile(pile)) return true;
	// could not play on the specified pile
	return false;
}

/* Tries to lay down a card from the computer's hand, or move a laydown pile.
 * returns: true if a card was played or moved, otherwise false
 */
bool Round::computerPlayOnce()
{
	// Try to play into empty corner piles (Kings)
	//cout << "Trying to play into empty corner piles" << endl;
	for (int pile=4; pile<8; pile++)
		if (laydownPile_[pile]->isEmpty() && playOnPile(laydownPile_[pile]))
			return true;
	// Try to play into any non-empty pile
	//cout << "Trying to play into non-empty piles" << endl;
	for (int pile=0; pile<8; pile++)
		if (!laydownPile_[pile]->isEmpty() && playOnPile(laydownPile_[pile]))
			return true;
	// Try to play from hand on empty non-corner piles
	//cout << "Trying to play from hand on empty non-corner piles" << endl;
	for (int destPile=0; destPile<4; destPile++)
		if (laydownPile_[destPile]->isEmpty()
			&& computerHand_.computerPlayOnPile(laydownPile_[destPile]))
			return true;
	// no play could be made
	//cout << "No play could be made." << endl;
	return false;
}

/* Performs computer's turn, using algorithms */
void Round::computerTakesTurn()
{
	//cout << "Round::computerTakesTurn()" << endl;
	
	// make plays until no play is possible, or hand is empty
	while (!computerHand_.isEmpty() && computerPlayOnce())
	{
		//cout << "Computer made a play!" << endl;
		
		//for (int i=0; i < 8; i++) cout << *laydownPile_[i] << endl;
		//cout << "Computer's hand:";
		//computerHand_.printCards(cout);
		//cout << endl << endl;
	}

	// computer won the round!
	if (computerHand_.isEmpty())
	{
		cout << "The computer won the round!" << endl;
		return;
	}
	
	// try to draw a card
	Card card = drawPile_.drawCard();
	// a card was drawn
	if (!card.isBlank())
	{
		computerHand_.addCard(card);
		cout << "Computer drew a card." << endl;
		//cout << "Computer drew " << card << endl;
	}
	// a card was not drawn
	else cout
			<< "Computer could not draw a card because the draw pile is empty."
			<< endl;
}

/* Prints out the game information for the user */
void Round::printGameInfo() const
{
	// print out laydown piles
	for (int i=0; i < 8; i++)
	{
		cout << laydownPile_[i]->niceName() << ":";
		laydownPile_[i]->printCards(cout);
		cout << endl;
	}
	
	// print computer's hand info
	cout << "Computer player has " << computerHand_.numCards()
		<< " cards." << endl;
	//cout << "Computer's hand:";
	//computerHand_.printCards(cout);
	//cout << endl;
	
	// print user's hand
	cout << "Your hand: ";
	userHand_.printCards(cout);
	cout << endl;
	
	// print drawpile info
	int n = drawPile_.numCards();
	if (n == 0) cout << "Draw pile is empty." << endl;
	else if (n < 5) cout << "Draw pile has " << n << " cards." << endl;
	else cout << "Draw pile has many cards." << endl;
}

/* Performs user's turn, using player's choices */
void Round::userTakesTurn()
{
	//cout << "Round::userTakesTurn()" << endl;
	
	// loop for each user command, while hand is not empty
	string line;
	while (!userHand_.isEmpty())
	{
		// print game information
		cout << endl;
		printGameInfo();
		
		// prompt
		cout << "Move> ";
		// read the line
		getline(cin, line);
		cout << endl;
		
		// convert the line to lower case
		//
		//Note that this simpler form doesn't work on gcc:
		//  transform (line.begin(), line.end(), line.begin(),toupper);
		//The reason is that gcc's <iostream> contains an extra function
		//called toupper, in addition to the standard one in <cctype>.  The
		//compiler is not smart enough to say "ambiguous match for function..."
		//and instead it says "no matching function call to ...".  Stupid.
		//
		//The solution is to tell the compiler exactly which version to use.
		//(See http://gcc.gnu.org/ml/gcc-bugs/2003-06/msg01412.html)
		transform (line.begin(), line.end(), line.begin()
			, static_cast<int (*)(int)>(toupper));
		
		// Quit, require confirmation
		if (line == "Q")
		{
			if(getYesNo("Are you sure you want to quit?"))
				throw user_quit();
		}
		// Help
		else if (line == "H")
		{ 
			cout
				<< "King's Corner commands:" << endl
				<< " Q                 - Quit: quit the program" << endl
				<< " H                 - Help: show this information" << endl
				<< " A                 - About: tell about the programer and program" << endl
				<< " D                 - Draw: draw a card and end your turn" << endl
				<< " L <Card> <Pile>   - Lay: lay a card on a pile" << endl
				<< " M <Pile1> <Pile2> - Move: move Pile1 on top of Pile2" << endl
				<< endl
				<< "The game consists of several rounds. A round is played until a player has layed" << endl
				<< "down all cards in the hand.  The player still holding cards receives penalty " << endl
				<< "points: 10 point for each King and 1 point for every other card.  Rounds are" << endl
				<< "played until one player receives 25 or more penalty points.  The winner of the" << endl
				<< "game is the player with the fewest points.  Cards may be stacked in piles, in" << endl
				<< "descending rank and alternating color.  Piles 1-4 may begin with any card." << endl
				<< "Piles 5-8 may begin only with Kings.  For detailed rules, please visit:" << endl
				<< "http://www.pagat.com/domino/kingscorners.html" << endl
				<< endl;
			waitForUser();
		}
		// About
		else if (line == "A") cout
			<< "King's Corner by Michael Leonhard (http://tamale.net/)" << endl
			<< "This program is the 5th project for CS340 Software Design, Fall 2005." << endl
			<< "It simulates the card game called King's Corner.  One is able to play a two" << endl
			<< "player version of the game, with the computer player as the opponent." << endl
			<< endl
			<< "Michael Leonhard is a senior at the University of Illinois at Chicago.  He" << endl
			<< "plans to graduate next year, in July 2006, with a Bachelors degree in" << endl
			<< "Computer Science." << endl;
		// Draw
		else if (line == "D") break;
		// not a single letter command
		else
		{
			istringstream st(line);
			string command;
			st >> command;
			
			// Lay
			if (command == "L")
			{
				// extract parameters
				string cardString;
				int pile;
				st >> cardString >> pile;
				Card card(cardString);
				
				// bad format
				if (st.fail() || !st.eof() || card.isBlank() || pile < 1 || pile > 8)
				{
					cout << "Your \"L\" command was malformed.  Proper format: L <Card> <Pile>" << endl
						<< "For example, to play the Five of Diamonds on Pile Two, type L 5D 2" << endl;
					continue;
				}
				// try to play card
				pile--;
				userHand_.userPlayOnPile(card, laydownPile_[pile]);
			}
			// Move
			else if (command == "M")
			{
				// extract parameters
				int pile1, pile2;
				st >> pile1 >> pile2;
				
				//cout << "pile1=" << pile1 << " pile2="<<pile2<<endl;
				// bad format
				if (st.fail() || !st.eof() || pile1 == pile2 || pile1 < 1 || pile1 > 8 || pile2 < 1 || pile2 > 8)
				{
					cout << "Your \"M\" command was malformed.  Proper format: M <Pile1> <Pile2>" << endl
						<< "For example, to move pile 3 to pile 1, type M 3 1" << endl;
					continue;
				}
				// try to move the pile
				pile1--;
				pile2--;
				laydownPile_[pile1]->playerMoveToPile(laydownPile_[pile2],USER);
			}
			// no command string
			else cout << "Your command was not recognized.  Type H for help." << endl;
		}
	}
	
	// user won the round!
	if (userHand_.isEmpty())
	{
		cout << "You won the round!" << endl;
		return;
	}
	
	// try to draw a card
	Card card = drawPile_.drawCard();
	// a card was drawn
	if (!card.isBlank())
	{
		userHand_.addCard(card);
		cout << "You drew " << card << "." << endl;
	}
	// a card was not drawn
	else cout << "You could not draw a card because the draw pile is empty."
			 << endl;
}

/* Accessor for computerPenaltyPoints_ field */
int Round::getComputerPenaltyPoints() const
{
	assert(computerPenaltyPoints_ >= 0);
	return computerPenaltyPoints_;
};

/* accessor for userPenaltyPoints_ field */
int Round::getUserPenaltyPoints() const
{
	assert(userPenaltyPoints_ >= 0);
	return userPenaltyPoints_;
};

/* Facilitates printing round to outstream using << operator
 * returns: outstream (to allow expressions like: cout << round << endl)
 */
ostream& operator<< (ostream& out, const Round& r)
{
	r.print(out);
	return out;
};
