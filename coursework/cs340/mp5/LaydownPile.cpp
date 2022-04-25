/* King's Corner (MP5)
 * Michael Leonhard (mleonhar)
 * 2005-11-27
 * GCC 3.4.2, Dev-C++ 4.9.9.2, Windows XP
 * CS 340, Fall 2005, Instructor: Pat Troy, TA: Nitin Jindal
 *
 * LaydownPile.cpp - a card pile that can be played into
 *
 * Inherits from CardPile and adds checking to make sure that
 * cards are kept in order and only valid sequences of cards
 * may be played.  Also facilitates moving the contents to
 * another CardPile.
 */

#include "mp5.h"

/* Constructor */
LaydownPile::LaydownPile (bool isCorner, int number)
 : isCorner_(isCorner), number_(number)
{
	//cout << "LaydownPile::LaydownPile(isCorner=" << isCorner
	//	<< " number=" << number << ")" << endl;
};

/* Tries to add the card to the top of the pile (end of the list)
 * returns: true if the card was added, otherwise false
 */
bool LaydownPile::laydownCard(Card card)
{
	//cout << "LaydownPile::laydownCard(" << card << ") " << *this << endl;
	
	// pile is empty
	if (cardList_.empty())
	{
		// corner pile but card is not king
		if (isCorner_ && !card.isKing()) return false;
		// throw user_exception("The card cannot go in the pile.");
	}
	// pile has a card
	else
	{
		// get the top card
		Card topCard = cardList_.back();
		// the cards cannot stack
		if (!topCard.canStack(card)) return false;
	}
		
	// add the card to the top of the pile (end of the list)
	cardList_.push_back(card);
	// added
	return true;
};

/* Returns a string name of the object */
string LaydownPile::niceName() const
{
	stringstream nicename;
	nicename << "Pile " << number_;
	return nicename.str();
};

/* Tries to move all cards to the specified pile
 * returns: true if the cards were moved
 */
bool LaydownPile::playerMoveToPile(LaydownPile* pile, Player player)
{
	//cout << "LaydownPile::playerMoveToPile(pile=" << *pile << ") "
	//	<< *this << endl;
	
	if (player == COMPUTER) assert(!isCorner_);
	
	// the pile is empty
	if (cardList_.empty()) return false;
	
	// try to move each card in pile
	// if the first one goes, then all will go.
	vector<Card>::iterator iter = cardList_.begin();
	while (iter != cardList_.end() && pile->laydownCard(*iter))
	{
		// card was laid down, so remove it from the hand
		//cout << "+" << *iter << endl;
		iter = cardList_.erase(iter);
	}
	
	// make sure that either all cards were moved or none were moved.
	assert(iter == cardList_.begin() || iter == cardList_.end());
	
	// the cards were moved
	if (iter == cardList_.end())
	{
		// inform user of move
		if (player == COMPUTER) cout << "Computer moved ";
		else if (player == USER) cout << "You moved ";
		else throw runtime_error("Unknown user moved pile.");
		cout << niceName() << " to " << pile->niceName() << "." << endl;
			
		// the play was made
		return true;
	}
	// the play was not made, so inform the user
	if (player == USER) cout << niceName() << " may not be moved to "
							<< pile->niceName() << "." << endl;
	return false;
};

/* Prints a string representation of the object to the specified ostream.
 * out: the ostream on which to print
 */
void LaydownPile::print (ostream& out) const
{
	out << "[LaydownPile" << number_;
	printCards(out);
	out << "]";
};
