/**
 * Represents a card, keeps Icons for front and back, handles mouse clicks
 *
 * Assignment: MP2
 * Class: CS 340, Fall 2005
 * TA: Nitin Jindal
 * System: jEdit, jdk-1.5.0.4, Windows XP
 * @author Michael Leonhard (CS account mleonhar)
 * @version 22 Sep 2005
*/

import java.awt.event.*;
import javax.swing.*;

public class Card extends JLabel implements MouseListener
{
	// data fields
	TurnedCardManager turnedManager;
	Icon faceIcon;
	Icon backIcon;
	boolean faceUp = false; 				// card is initially face down
	int num; 							// number corresponding to the face of the card
	int iconWidthHalf, iconHeightHalf; 	// half the dimensions of the back face icon
	boolean mousePressedOnMe = false;
	
	/**
	 * Constructor
	 * @param turnedManager object that manages list of currently turned cards
	 * @param face the face of the card, only one other card has this face
	 * @param back the back of the card, same as all other cards
	 * @param num number associated with face icon, used to compare cards for equality
	*/
	public Card(TurnedCardManager turnedManager, Icon face, Icon back, int num)
	{
		// initially show face down
		super(back);
		// save parameters
		this.turnedManager = turnedManager;
		this.faceIcon = face;
		this.backIcon = back;
		this.num = num;
		// catch mouse clicks and events
		this.addMouseListener(this);
		// store icon dimensions for mouse click testing
		this.iconWidthHalf = back.getIconWidth() / 2;
		this.iconHeightHalf = back.getIconHeight() / 2;
	}
	
	/**
	 * Try to turn face up
	*/
	public void turnUp()
	{
		MemoryGame.dprintln("Card["+num+"].turnUp()");
		// the card is already face up
		if(this.faceUp) return;
		// ask manager to allow turn
		this.faceUp = this.turnedManager.turnUp(this);
		// allowed to turn, so change the picture
		if(this.faceUp) this.setIcon(this.faceIcon);
	}
	
	/**
	 * Turn back over, face down
	 *
	*/
	public void turnDown()
	{
		MemoryGame.dprintln("Card["+num+"].turnDown()");
		// the card is already face down
		if(!this.faceUp) return;
		// change the picture
		this.setIcon(this.backIcon);
		// the card is now down
		this.faceUp = false;
	}
	
	/**
	 * return the number identifying the kind of card (the face)
	*/
	public int getNum() { return num; }
	
	/**
	 * Check if the coordinates are over the icon
	 *
	 * @param x X coordinate
	 * @param y Y coordinate
	 * @return true if coordinates are over icon, otherwise false
	*/
	private boolean overIcon(int x, int y)
	{
		// calculate the distance from the center of the label
		int distX = Math.abs(x - (this.getWidth() / 2));
		int distY = Math.abs(y - (this.getHeight() / 2));
		// outside icon region
		if(distX > this.iconWidthHalf || distY > this.iconHeightHalf )
			return false;
		// inside icon region
		return true;
	}
	
	/** Methods to implement MouseListener interface **********************/
	
	/**
	 * Invoked when the mouse button has been clicked (pressed and released) on a component.
	 *
	 * @param e object holding information about the button click
	*/
	public void mouseClicked(MouseEvent e)
	{
		// over icon, so try to turn up the card
		if(overIcon(e.getX(), e.getY())) this.turnUp();
	}
	
	/**
	 * Invoked when a mouse button has been pressed on a component.
	 *
	 * @param e object holding information about the button press
	*/
	public void mousePressed(MouseEvent e)
	{
		// over icon, so remember this is a mouse press
		if(overIcon(e.getX(), e.getY())) this.mousePressedOnMe = true;
	}
	
	/**
	 * Invoked when a mouse button has been released on a component.
	 *
	 * @param e object holding information about the button release
	*/
	public void mouseReleased(MouseEvent e)
	{
		// previous press was over icon
		if(this.mousePressedOnMe)
		{
			// mouse is no longer pressed
			this.mousePressedOnMe = false;
			// it was a click, so treat it as one
			this.mouseClicked(e);
		}
	}
	
	/**
	 * Invoked when the mouse enters a component.
	 *
	 * @param e object holding information about the mouse pointer
	*/
	public void mouseEntered(MouseEvent e) {}
	
	/**
	 * Invoked when the mouse exits a component.
	 *
	 * @param e object holding information about the mouse pointer
	*/
	public void mouseExited(MouseEvent e)
	{
		// forget any previous mouse press
		this.mousePressedOnMe = false;
	}
}
