/* King's Corner (MP5)
 * Michael Leonhard (mleonhar)
 * 2005-11-27
 * GCC 3.4.2, Dev-C++ 4.9.9.2, Windows XP
 * CS 340, Fall 2005, Instructor: Pat Troy, TA: Nitin Jindal
 */

#include <algorithm>

#include "mp5.h"

using namespace std;

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
}

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
}

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
}

/* Adds the card to the hand, then sorts the hand */
void PlayerHand::addCard(Card card)
{
	cardList_.push_back(card);
	sort(cardList_.begin(), cardList_.end());
}

/* Prints the object to the specified ostream.
 * out: the ostream on which to print
 */
void PlayerHand::print (ostream& out) const
{
	out << "[PlayerHand";
	printCards(out);
	out << "]";
};

/* Facilitates printing object to outstream using << operator
 * returns: outstream (to allow expressions like: cout << hand << endl)
 */
ostream& operator<< (ostream& out, const PlayerHand& hand)
{
	hand.print(out);
	return out;
};
