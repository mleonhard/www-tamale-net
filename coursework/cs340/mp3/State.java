/**
 * This class implements a state machine to coordinate gameplay.
 * 
 * Assignment: MP3
 * Class: CS 340, Fall 2005
 * TA: Nitin Jindal
 * System: jdk-1.5.0.4 and Eclipse 3.1 on Windows XP
 * @author Michael Leonhard (CS account mleonhar)
 * @version 12 Oct 2005
 */

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.Timer;

public class State implements ActionListener {
	// the delay between computer turns (in milliseconds)
	private static final int COMPUTER_TURN_DELAY = 1000;
	// State: user's mouse wanders over board, hovering over lines
	private static final int WANDER_STATE = 0;
	// State: user's mouse was pressed on the hovered line and not moved off it
	private static final int PRESSED_STATE = 1;
	// State: the mouse was clicked and then moved off the hovered line
	private static final int WAITMOUSEUP_STATE = 2;
	// State: it is the computer's turn
	private static final int COMPUTER_STATE = 3;
	// State: computer made a turn and is waiting to make its next turn (so user
	// can see which turn it just made)
	private static final int COMPUTERWAIT_STATE = 4;
	// State: the game has ended
	private static final int ENDGAME_STATE = 5;

	// users's point counter
	private CounterLabel userCounter;
	// computer's point counter
	private CounterLabel computerCounter;
	// the current state
	private int currentState;
	// the line the mouse is near
	private Line hoveredLine;
	// the line chosen by the computer
	private Line chosenLine;
	// the field whose state we keep
	private Field field;
	// timer used to add a delay after computer turns
	Timer delayTimer;

	/**
	 * Finds a string representation for the specified state
	 * 
	 * @param state the state to look up
	 * @return a string representation of the state
	 */
	public static String stateString(int state) {
		if (state == WANDER_STATE) return "WANDER_STATE";
		if (state == PRESSED_STATE) return "PRESSED_STATE";
		if (state == WAITMOUSEUP_STATE) return "WAITMOUSEUP_STATE";
		if (state == COMPUTER_STATE) return "COMPUTER_STATE";
		if (state == COMPUTERWAIT_STATE) return "COMPUTERWAIT_STATE";
		if (state == ENDGAME_STATE) return "ENDGAME_STATE";
		return "UNKNOWN_STATE!!!";
	}

	/**
	 * Makes a string representation of the state object
	 * 
	 * @return a string representation of the object
	 */
	public String toString() {
		return "State[" + State.stateString(this.currentState) + "]";
	}

	/**
	 * Makes new state machine that starts in WANDER state
	 * 
	 * @param field the game field
	 * @param computerCounter counter for computer's score
	 * @param userCounter counter for user's score
	 */
	public State(Field field, CounterLabel userCount, CounterLabel computerCount) {
		// save references
		this.field = field;
		this.userCounter = userCount;
		this.computerCounter = computerCount;
		// make the timer object
		this.delayTimer = new Timer(COMPUTER_TURN_DELAY, this);
		// set initial state
		this.currentState = WANDER_STATE;
	}

	/**
	 * Called by field when the mouse button is released
	 * 
	 * @param nearestLine the mouse was released near this line
	 */
	public void mouseReleased(Line nearestLine) {
		// System.out.println("State.mouseReleased() " + this + " nearestLine="
		// + nearestLine);

		// PRESSED
		if (this.currentState == PRESSED_STATE) {
			// the mouse was released on the same line
			if (nearestLine == this.hoveredLine) {
				// draw the line
				this.setHoveredLine(null);
				int boxesMade = nearestLine.draw(true);
				// at least one box was made
				if (boxesMade > 0) {
					// add to the user's score
					this.userCounter.add(boxesMade);
					// user's gets another turn
					enterState(WANDER_STATE);
				}
				// no box was made, so it is now the computer's turn
				else enterState(COMPUTER_STATE);
			}
			// the mouse was not released on the same line where it was pressed
			else enterState(WANDER_STATE);
		}
		
		// WAITMOUSEUP
		else if (this.currentState == WAITMOUSEUP_STATE) enterState(WANDER_STATE);
		// mouse release event is ignored in all other states
	}

	/**
	 * Called by field when mouse button is pressed down
	 * 
	 * @param nearestLine the mouse was pressed near this line
	 */
	public void mousePressed(Line nearestLine) {
		// System.out.println("State.mousePressed() " + this + " nearestLine="
		// + nearestLine);

		// WANDER
		if (this.currentState == WANDER_STATE) {
			// press was not near any line
			if (nearestLine == null) enterState(WAITMOUSEUP_STATE);
			// press was near the line
			else {
				// line is drawn and cannot be chosen
				if (nearestLine.isDrawn()) enterState(WAITMOUSEUP_STATE);
				// line is not drawn, so choose it
				else {
					this.hoveredLine = nearestLine;
					enterState(PRESSED_STATE);
				}
			}
		}
		// mouse press events are ignored in all other states
	}

	/**
	 * Called by field when mouse moves.
	 * 
	 * @param nearestLine the mouse is nearest to this line
	 */
	public void mouseMoved(Line nearestLine) {
		// System.out.println("State.mouseMoved() " + this + " nearestLine="
		// + nearestLine);

		// WANDER
		if (this.currentState == WANDER_STATE) {
			// line exists and is drawn, so it cannot be chosen
			if (nearestLine != null && nearestLine.isDrawn()) this
					.setHoveredLine(null);
			// line is not drawn, so it can be chosen (and hovered over)
			else this.setHoveredLine(nearestLine);
		}

		// PRESSED
		else if (this.currentState == PRESSED_STATE) {
			// mouse has moved off hoveredLine
			if (nearestLine != this.hoveredLine) enterState(WAITMOUSEUP_STATE);
		}
		// mouse movements are ignored in all other states
	}

	/**
	 * Enters the specified state
	 * 
	 * @param newState the new state to enter
	 */
	private void enterState(int newState) {
		// System.out.println("State.enterState() " + this + "newState="
		// + State.stateString(newState));
		
		// entering WANDER
		if (newState == WANDER_STATE) {
			// field contains no undrawn line
			if (!this.field.hasUndrawnLine()) enterState(ENDGAME_STATE);
			// field contains an undrawn line
			else this.currentState = WANDER_STATE;
		}
		
		// entering PRESSED
		else if (newState == PRESSED_STATE) this.currentState = PRESSED_STATE;
		// entering WAITMOUSEUP
		else if (newState == WAITMOUSEUP_STATE) {
			// hover over no line
			this.setHoveredLine(null);
			// enter the state
			this.currentState = WAITMOUSEUP_STATE;
		}
		
		// entering COMPUTER
		else if (newState == COMPUTER_STATE) {
			// field contains no undrawn line
			if (!this.field.hasUndrawnLine()) enterState(ENDGAME_STATE);
			// field has an undrawn line
			else {
				// choose a line
				this.chosenLine = this.field.computerChooseLine();
				// show the computer chosen line (in red)
				this.chosenLine.showAsChosen();
				// wait a while so the user can see the chosen line
				enterState(COMPUTERWAIT_STATE);
			}
		}
		
		// entering COMPUTERWAIT
		else if (newState == COMPUTERWAIT_STATE) {
			// start timer (no race condition because Swing callbacks come from
			// one thread)
			this.delayTimer.start();
			// enter the state
			this.currentState = COMPUTERWAIT_STATE;
		}
		
		// entering ENDGAME
		else this.currentState = ENDGAME_STATE;
	}

	/**
	 * Called when timer event occurs, used to add delay after computer turn
	 * 
	 * @param e information about the event
	 */
	public void actionPerformed(ActionEvent e) {
		// stop timer so it doesn't trigger again
		this.delayTimer.stop();

		// COMPUTERWAIT
		if (this.currentState == COMPUTERWAIT_STATE) {
			// draw the chosen line
			int boxesMade = this.chosenLine.draw(false);
			// the computer made a box
			if (boxesMade > 0) {
				// add to the computer's score
				this.computerCounter.add(boxesMade);
				// computer gets another turn
				enterState(COMPUTER_STATE);
			}
			// the computer made no box, so now it is the user's turn
			// TODO show currently hovered line, don't wait for mouse move
			else enterState(WANDER_STATE);
		}
		// timer event is ignored by all other states
	}

	/**
	 * Handles updating the hovered line and redrawing
	 * 
	 * @param newHoveredLine the mouse is hovering over this line
	 */
	private void setHoveredLine(Line newHoveredLine) {
		// System.out.println("State.setHoveredLine() " + this.hoveredLine
		// + " -> " + newHoveredLine);

		// the line is the same, so do nothing
		if (newHoveredLine == this.hoveredLine) return;

		// old line exists, so stop showing it
		if (this.hoveredLine != null) this.hoveredLine.stopShowing();
		// new line exists, so show it
		if (newHoveredLine != null) newHoveredLine.showAsHovered();
		// change to new line
		this.hoveredLine = newHoveredLine;
	}
}