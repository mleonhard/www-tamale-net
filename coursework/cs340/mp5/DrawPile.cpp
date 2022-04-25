/* King's Corner (MP5)
 * Michael Leonhard (mleonhar)
 * 2005-11-27
 * GCC 3.4.2, Dev-C++ 4.9.9.2, Windows XP
 * CS 340, Fall 2005, Instructor: Pat Troy, TA: Nitin Jindal
 *
 * DrawPile.cpp - implements a draw pile
 *
 * Inherits from CardPile.  Adds ability to remove the top
 * card from the pile.
 */

#include "mp5.h"

/* Default Constructor, initializes with random list of cards.
 */
DrawPile::DrawPile ()
{
	// TODO change num cards back to 52
	// fill card list with cards 0-51
	for (int i = 0; i < 52; i++) addCard(Card(i));
	// shuffle the cards
	random_shuffle(cardList_.begin(), cardList_.end());
	
	//cout << "DrawPile::DrawPile() " << *this << endl;
};

/* Draws a card from the top of the pile (the end of the list)
 * returns: the drawn card | blank card (if pile is empty)
 */
Card DrawPile::drawCard()
{
	// the pile is empty, so return an empty card
	if (cardList_.empty()) return Card();
	// get the card and delete it from the list
	Card card = cardList_.back();
	cardList_.pop_back();
	return card;
};

/* Prints a string representation of the object to the specified ostream.
 * out: the ostream on which to print
 */
void DrawPile::print (ostream& out) const
{
	out << "[DrawPile";
	printCards(out);
	out << "]";
};
