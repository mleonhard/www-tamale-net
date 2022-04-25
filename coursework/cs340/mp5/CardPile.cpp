/* King's Corner (MP5)
 * Michael Leonhard (mleonhar)
 * 2005-11-27
 * GCC 3.4.2, Dev-C++ 4.9.9.2, Windows XP
 * CS 340, Fall 2005, Instructor: Pat Troy, TA: Nitin Jindal
 *
 * CardPile.cpp - represents a collection of card
 *
 * Holds a vector of Card objects.  This class contains no
 * dynamically allocated data members, so the default copy
 * constructor and assignment operators are used.
 *
 * This is the base class of DrawPile, LaydownPile, and PlayerHand.
 * It provides methods to add cards, count, check for emptiness,
 * and print the card pile.
 */

#include "mp5.h"

/* Default Constructor, initializes an empty pile
 */
CardPile::CardPile ()
{
	//cout << "CardPile::CardPile()" << endl;
};

/* Destructor */
CardPile::~CardPile()
{
	//cout << "CardPile::~CardPile() " << *this << endl;
};

/* Adds a card to the top of the pile (end of the list)*/
void CardPile::addCard(Card card)
{
	cardList_.push_back(card);
};

/* Checks if the pile is empty. */
bool CardPile::isEmpty() const
{
	cardList_.empty();
};

/* Returns the number of cards in the pile */
int CardPile::numCards() const
{
	cardList_.size();
};

/* Facilitates printing object to outstream using << operator
 * returns: outstream (to allow expressions like: cout << pile << endl)
 */
ostream& operator<< (ostream& out, const CardPile& pile)
{
	pile.print(out);
	return out;
};

/* Print the object to the specified ostream.
 * out: the ostream on which to print
 */
void CardPile::print (ostream& out) const
{
	out << "[CardPile";
	printCards(out);
	out << "]";
};

/* Print out the cards to the specified ostream.
 * out: the ostream on which to print
 */
void CardPile::printCards (ostream& out) const
{
	vector<Card>::const_iterator iter1 = cardList_.begin();
	while (iter1 != cardList_.end())
	{
		out << " " << *iter1;
		iter1++;
	}
};
