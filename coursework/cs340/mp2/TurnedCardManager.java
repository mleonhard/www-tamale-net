/**
 * Stores currently turned cards, allows only two two be turned at the same time,
 * also handles turning cards back down after a delay.
 *
 * Assignment: MP2
 * Class: CS 340, Fall 2005
 * TA: Nitin Jindal
 * System: jEdit, jdk-1.5.0.4, Windows XP
 * @author Michael Leonhard (CS account mleonhar)
 * @version 22 Sep 2005
*/

import java.util.Vector;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import javax.swing.Timer;

public class TurnedCardManager implements ActionListener
{
	// data fields
	private Vector turnedCards;			// list of turned non-matching cards
	private TurnCounterLabel turnCounterLabel;// turn counter is incremented at every second card turn
	private Timer turnDownTimer;				// timer is used to make a delay
	private final int turnDownDelay = 2000; 	// milliseconds to wait before turning cards down
	
	/**
	 * Constructor
	 *
	 * @param turnCounterLabel reference to turn counter label in main program window
	*/
	public TurnedCardManager(TurnCounterLabel turnCounterLabel)
	{
		// save parameter
		this.turnCounterLabel = turnCounterLabel;
		// create list
		this.turnedCards = new Vector(2);
		// make timer object
		this.turnDownTimer = new Timer(this.turnDownDelay, this);
		this.turnDownTimer.setRepeats(false);
	}
	
	/**
	 * Adds the card to the list, handles cards matching, starts timer to turn down
	 *
	 * @param card the new card to be added
	 * @return true
	*/
	private boolean doAddCard(Card card)
	{
		// add the card to the list
		this.turnedCards.add(card);
		// there are two cards
		if(this.turnedCards.size() == 2)
		{
			// record the player's turn
			this.turnCounterLabel.increment();
			// get the other card (which was already turned up)
			Card otherCard = (Card)this.turnedCards.get(0);
			// the cards match, so remove them from the list (they will remain face up)
			if( otherCard.getNum() == card.getNum())
				this.turnedCards.clear();
			// the cards do not match, so start the timer to turn them down
			else this.turnDownTimer.start();
		}
		return true;
	}
	
	/**
	 * The specified card wants to turn, add if currently less than 2 cards
	 *
	 * @param card the Card object that wants to turn
	 * @return true if the card is allowed to turn, otherwise false
	*/
	public boolean turnUp(Card card)
	{
		// the card may be turned
		if(this.turnedCards.size() < 2) return doAddCard(card);
		// there are already 2 cards in the list
		return false;
	}
	
	/**
	 * Remove the specified card from the list.
	 *
	 * @param card the Card object to be removed from the list of turned cards
	*/
	public void del(Card card)
	{
		this.turnedCards.remove(card);
	}
	
	/**
	 * Invoked when timer event occurs, turns non-matching cards down
	 *
	 * @param e the timer event information
	*/
	public void actionPerformed(ActionEvent e)
	{
		// turn each card back down
		for(int i = 0; i < this.turnedCards.size(); i++ )
		{
			Card card = (Card)this.turnedCards.get(i);
			card.turnDown();
		}
		// forget the cards
		this.turnedCards.clear();
	}
}
