/* King's Corner (MP5)
 * Michael Leonhard (mleonhar)
 * 2005-11-27
 * GCC 3.4.2, Dev-C++ 4.9.9.2, Windows XP
 * CS 340, Fall 2005, Instructor: Pat Troy, TA: Nitin Jindal
 *
 * PlayerHand.cpp - represents a player's hand of cards
 *
 * Inherits from CardPile and adds logic to play cards on piles and
 * keep cards in proper rank and suit order.
 *
 * For user plays, it accepts a card string from the user and tries
 * to play that card on the specified pile.  For computer plays,
 * it tries to play each card until one is successful.
 */

#include "mp5.h"

/* Adds the card to the hand, then sorts the hand */
void PlayerHand::addCard(Card card)
{
	cardList_.push_back(card);
	sort(cardList_.begin(), cardList_.end());
};

/* Plays the highest possible card on the specified pile
 * returns: true if a card was played
 */
bool PlayerHand::computerPlayOnPile(LaydownPile* pile)
{
	//cout << "PlayerHand::computerPlayOnPile(pile=" << *pile << ") "
	//	<< *this << endl;
	
	// iterate through our cards
	vector<Card>::iterator iter = cardList_.begin();
	while (iter != cardList_.end())
	{
		// try to lay down card
		if (pile->laydownCard(*iter))
		{
			// inform user of move
			cout << "Computer played " << *iter << " on " << pile->niceName()
				<< "." << endl;
			// remove card from hand
			cardList_.erase(iter);
			// card was laid down
			return true;
		}
		//cout << "-" << *iter << endl;
		iter++;
	}
	// no card could be laid down
	return false;
};

/* Determines the penalty points for the leftover cards in the hand
 * returns: integer
 */
int PlayerHand::countPenaltyPoints() const
{
	// start with no points
	int penaltyPoints = 0;
	// iterate through all cards in the hand
	vector<Card>::const_iterator iter = cardList_.begin();
	for (; iter != cardList_.end(); iter++)
	{
		// kings get 10 points
		if ((*iter).isKing()) penaltyPoints += 10;
		// other cards get 1 point
		else penaltyPoints++;
	}
	// done
	return penaltyPoints;
};

/* Prints the object to the specified ostream.
 * out: the ostream on which to print
 */
void PlayerHand::print (ostream& out) const
{
	out << "[PlayerHand";
	printCards(out);
	out << "]";
};

/* Plays specified card on the specified pile.
 * returns: true if a card was played
 */
void PlayerHand::userPlayOnPile(Card card, LaydownPile* pile)
{
	//cout << "PlayerHand::userPlayOnPile(card=" << card
	//	<< " pile=" << *pile << ") " << *this << endl;
	
	// iterate through all cards in the hand
	vector<Card>::iterator iter = cardList_.begin();
	for (; iter != cardList_.end(); iter++)
	{
		// the card has been found
		if (*iter == card)
		{
			// try to lay down card
			if (pile->laydownCard(card))
			{
				// remove card from hand
				cardList_.erase(iter);
				// inform user of move
				cout << "You played " << card << " on " << pile->niceName()
					<< "." << endl;
				return;
			}
			// pile cannot accept card
			else
			{
				// inform user
				cout << "Card " << card << " may not be played on "
					<< pile->niceName() << "." << endl;
				return;
			}
		}
	}
	// the card was not found in the hand
	cout << "Your hand does not contain card " << card << "." << endl;
	return;
};
