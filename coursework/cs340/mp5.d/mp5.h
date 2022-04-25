/* King's Corner (MP5)
 * Michael Leonhard (mleonhar)
 * 2005-11-27
 * GCC 3.4.2, Dev-C++ 4.9.9.2, Windows XP
 * CS 340, Fall 2005, Instructor: Pat Troy, TA: Nitin Jindal
 */

#ifndef MP5_H
#define MP5_H

//#include <cstdlib>
//#include <exception>
#include <algorithm>
#include <cctype>
#include <locale>
#include <sstream>
#include <string>
//#include <assert)
#include <stdexcept>
#include <iostream>
#include <vector>

using namespace std;

bool getYesNo(string prompt);
void waitForUser();

// exception used to signal user's quit command
typedef int user_quit;
// exception used to abort a user initiated action
typedef invalid_argument user_exception;

enum Player { COMPUTER, USER };
Player operator++ (Player& player, int);
ostream& operator<< (ostream& out, Player player);

class Card
{
	private:
		int value_;
		int suitToOrder(int suit) const;
	
	public:
		Card();
		Card(int value);
		Card (string value);
		string toString() const;
		void print (ostream& out) const;
		int getValue() const;
		bool isKing() const;
		bool isBlank() const;
		bool operator== (const Card& c) const;
		bool operator!= (const Card& c) const;
		bool operator< (const Card& c) const;
		bool canStack (const Card& thatCard);
};
ostream& operator<< (ostream& out, const Card& c);

class CardPile
{
	protected:
		vector<Card> cardList_;
	
	public:
		CardPile();
		~CardPile();
		bool isEmpty() const;
		int numCards() const;
		virtual void print (ostream& out) const;
		void printCards (ostream& out) const;
		virtual void addCard(Card card);
};
ostream& operator<< (ostream& out, const CardPile& pile);

class DrawPile : public CardPile
{
	public:
		DrawPile();
		Card drawCard();
		void print (ostream& out) const;
};
ostream& operator<< (ostream& out, const DrawPile& pile);

class LaydownPile : public CardPile
{
	private:
		bool isCorner_;
		int number_;
	
	public:
		LaydownPile (bool isCorner, int number);
		virtual void print (ostream& out) const;
		virtual bool laydownCard (Card card);
		bool playerMoveToPile(LaydownPile* pile, Player player);
		string niceName() const;
};
ostream& operator<< (ostream& out, const LaydownPile& pile);

class PlayerHand : public CardPile
{
	public:
		void addCard(Card card);
		void print (ostream& out) const;
		bool computerPlayOnPile (LaydownPile* pile);
		int countPenaltyPoints() const;
		void userPlayOnPile(Card card, LaydownPile* pile);
};
ostream& operator<< (ostream& out, const PlayerHand& hand);

class Game
{
	private:
		int computerPenaltyPoints_;
		int userPenaltyPoints_;
		void announceWinner();
	
	public:
		Game ();
		void print (ostream& out) const;
		Player play (Player nextDealer);
};
ostream& operator<< (ostream& out, const Game& g);

class Round
{
	private:
		Player turnTaker_;
		int computerPenaltyPoints_;
		int userPenaltyPoints_;
		LaydownPile* laydownPile_[8];
		PlayerHand userHand_;
		PlayerHand computerHand_;
		DrawPile drawPile_;
		bool computerPlayOnce();
		void computerTakesTurn();
		bool playOnPile(LaydownPile* pile);
		void userTakesTurn();
		void printGameInfo() const;
	
	public:
		Round ();
		~Round ();
		void deal (Player dealer);
		void play ();
		void print (ostream& out) const;
		int getComputerPenaltyPoints() const;
		int getUserPenaltyPoints() const;
};
ostream& operator<< (ostream& out, const Round& r);

#endif /* MP5_H */
