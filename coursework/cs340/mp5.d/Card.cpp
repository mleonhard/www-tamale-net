#include <cstdlib>
#include <iostream>

#include "mp5.h"

using namespace std;

/* Default Constructor, initializes a blank card
 */
Card::Card ()
{
	//cout << "Card::Card()" << endl;
	value_ = -1;
};

/* Constructor, initializes a card
 * value: the numeric value of the card, in the range 0 through 51
 */
Card::Card (int value) : value_(value)
{
	//cout << "Card::Card(value=" << value_ << ")" << endl;
	assert(value_ > -1 && value_ < 52);
};

/* Constructor, initializes a card from a string
 * value: the string name of the card, if string is invalid then 
 *        card will be blank
 */
Card::Card (string value)
{
	//cout << "Card::Card(value=" << value << ")" << endl;
	// start out making the card blank
	value_ = -1;
	// invalid length
	if (value.length() != 2) return;
	// rank
	int rank = -1;
	switch (int(value[0]))
	{
		case 'A': rank = 0; break;
		case '2': rank = 1; break;
		case '3': rank = 2; break;
		case '4': rank = 3; break;
		case '5': rank = 4; break;
		case '6': rank = 5; break;
		case '7': rank = 6; break;
		case '8': rank = 7; break;
		case '9': rank = 8; break;
		case 'T': rank = 9; break;
		case 'J': rank = 10; break;
		case 'Q': rank = 11; break;
		case 'K': rank = 12; break;
		default: return;
	}
	// suit
	int suit = -1;
	switch (int(value[1]))
	{
		case 'C': suit = 0; break;
		case 'D': suit = 1; break;
		case 'S': suit = 2; break;
		case 'H': suit = 3; break;
		defaut: return;
	}
	// value
	value_ = rank*4 + suit;
	assert(value_ > -2 && value_ < 52);
};

/* Checks if the specified card may be placed on top of this card.
 * returns: true or false
 */
bool Card::canStack (const Card& thatCard)
{
	assert(value_ > -1 && value_ < 52);
	
	//cout << "Card::canStack(" << thatCard << ") " << *this << endl;
	int thatValue = thatCard.getValue();
	int thisRank = value_ / 4; // 0=A .. 12=K
	int thatRank = thatValue / 4;
	int thisColor = value_ % 2; // 0=black 1=red
	int thatColor = thatValue % 2;
	
	// stacking is allowed if the card is of one lower rank
	// and opposite color
	return (thisRank - 1 == thatRank) && (thisColor != thatColor);
};

/* Returns true if the card is a King */
bool Card::isKing() const
{
	assert(value_ > -1 && value_ < 52);
	//cout << "Card::isKing() " << *this << endl;
	return value_ / 4 == 12;
};

/* Returns true if the card is blank */
bool Card::isBlank() const
{
	assert(value_ > -2 && value_ < 52);
	//cout << "Card::isBlank() " << *this << endl;
	return value_ == -1;
};

/* Creates a string representation of the object
 * returns: string
 */
string Card::toString () const
{
	assert(value_ > -1 && value_ < 52);
	
	string const ranks[13] = {"A","2","3","4","5","6","7","8","9","T","J","Q","K"};
	string const suits[4] = {"C","D","S","H"};
	int rank = value_ / 4;
	int suit = value_ % 4;
	return ranks[rank] + suits[suit];
};

/* Prints a string representation of the object to the specified ostream.
 * out: the ostream on which to print
 */
void Card::print (ostream& out) const
{
	out << toString();
};

/* Accessor for value_ */
int Card::getValue() const
{
	assert(value_ > -1 && value_ < 52);
	return value_;
};

/* Equality Operator, returns true if cards have the same value */
bool Card::operator== (const Card& c) const
{
	assert(value_ > -1 && value_ < 52);
	return value_ == c.getValue();
}

/* Inequality Operator, returns true if cards have differing values */
bool Card::operator!= (const Card& c) const
{
	assert(value_ > -1 && value_ < 52);
	return value_ != c.getValue();
}

/* Converts the card suit to suit order
 * returns: integer representing strength of suit
 */
int Card::suitToOrder(int suit) const
{
	assert(suit > -1 && suit < 4);
	
	switch (suit)
	{
		case 2: return 3; // Spades
		case 3: return 2; // Hearts
		case 1: return 1; // Diamonds
		case 0: return 0; // Clubs (lowest)
	}
	return -1;
}

/* Order Operator, returns true if the cards are in order  */
bool Card::operator< (const Card& thatCard) const
{
	assert(value_ > -1 && value_ < 52);
	//cout << "Card::operator<(" << thatCard << ") " << *this << endl;
	
	// find the ranks and suits
	int thisValue = value_;
	int thatValue = thatCard.getValue();
	int thisRank = thisValue / 4; // 0=A .. 12=K
	int thatRank = thatValue / 4;
	int thisSuit = thisValue % 4; // 0=C 1=D 2=S 3=H
	int thatSuit = thatValue % 4;
	
	// calculate new values that are ordered by suit
	thisValue = thisRank*4 + suitToOrder(thisSuit);
	thatValue = thatRank*4 + suitToOrder(thatSuit);
	
	// compare the values
	return thisValue > thatValue;
}

/* Facilitates printing object to outstream using << operator
 * returns: outstream (to allow expressions like: cout << card << endl)
 */
ostream& operator<< (ostream& out, const Card& c)
{
	c.print(out);
	return out;
};
