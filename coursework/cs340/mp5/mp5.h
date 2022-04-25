/* King's Corner (MP5)
 * Michael Leonhard (mleonhar)
 * 2005-11-27
 * GCC 3.4.2, Dev-C++ 4.9.9.2, Windows XP
 * CS 340, Fall 2005, Instructor: Pat Troy, TA: Nitin Jindal
 *
 * mp5.h - Header file for C++ program
 *
 * Declares classes, types, and utilify functions.
 * Includes standard headers, declares namespace.
 */

#ifndef MP5_H
#define MP5_H

#include <sstream>
#include <stdexcept>
#include <iostream>
#include <vector>

using namespace std;

/* Types */

// exception used to signal user's quit command
typedef int user_quit;
// exception used to abort a user initiated action
typedef invalid_argument user_exception;
// represents a player
enum Player { COMPUTER, USER };

/* Functions defined in mp5.cpp*/
bool getYesNo(string prompt);
void waitForUser();
Player operator++ (Player& player, int);
ostream& operator<< (ostream& out, Player player);

/* Classes */
class Card
{
	private:
		int value_;
		int suitToOrder(int suit) const;
	
	public:
		Card();
		Card(int value);
		Card (string value);
		bool canStack (const Card& thatCard);
		int getValue() const;
		bool isBlank() const;
		bool isKing() const;
		bool operator== (const Card& c) const;
		bool operator!= (const Card& c) const;
		bool operator< (const Card& c) const;
		void print (ostream& out) const;
		string toString() const;
};
ostream& operator<< (ostream& out, const Card& c);

class CardPile
{
	protected:
		vector<Card> cardList_;
	
	public:
		CardPile();
		~CardPile();
		virtual void addCard(Card card);
		bool isEmpty() const;
		int numCards() const;
		virtual void print (ostream& out) const;
		void printCards (ostream& out) const;
};
ostream& operator<< (ostream& out, const CardPile& pile);

class DrawPile : public CardPile
{
	public:
		DrawPile();
		Card drawCard();
		void print (ostream& out) const;
};

class LaydownPile : public CardPile
{
	private:
		bool isCorner_;
		int number_;
	
	public:
		LaydownPile (bool isCorner, int number);
		virtual bool laydownCard (Card card);
		string niceName() const;
		virtual void print (ostream& out) const;
		bool playerMoveToPile(LaydownPile* pile, Player player);
};

class PlayerHand : public CardPile
{
	public:
		void addCard(Card card);
		bool computerPlayOnPile (LaydownPile* pile);
		int countPenaltyPoints() const;
		void print (ostream& out) const;
		void userPlayOnPile(Card card, LaydownPile* pile);
};

class Game
{
	private:
		void announceWinner();
		int computerPenaltyPoints_;
		int userPenaltyPoints_;
	
	public:
		Game ();
		Player play (Player nextDealer);
		void print (ostream& out) const;
};
ostream& operator<< (ostream& out, const Game& g);

class Round
{
	private:
		Player turnTaker_; /* the player who is taking a turn */
		/* penalty points earned in the round */
		int computerPenaltyPoints_;
		int userPenaltyPoints_;
		/* piles */
		LaydownPile* laydownPile_[8];
		PlayerHand userHand_;
		PlayerHand computerHand_;
		DrawPile drawPile_;
		/* methods */
		bool computerPlayOnce();
		void computerTakesTurn();
		bool playOnPile(LaydownPile* pile);
		void printGameInfo() const;
		void userTakesTurn();
	
	public:
		Round ();
		~Round ();
		int getComputerPenaltyPoints() const;
		int getUserPenaltyPoints() const;
		void deal (Player dealer);
		void play ();
		void print (ostream& out) const;
};
ostream& operator<< (ostream& out, const Round& r);

#endif /* MP5_H */
