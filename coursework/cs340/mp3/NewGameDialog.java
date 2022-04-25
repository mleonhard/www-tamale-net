/**
 * This class implements a dialog box to query the user for dimensions of the
 * new game board. The constructor takes a reference to a callback object.
 * When the user makes a selection, the dialog will be disposed and the
 * selected values passed to the callback object.  The dialog may be generated
 * again by calling setVisible(true).
 * 
 * Assignment: MP3
 * Class: CS 340, Fall 2005
 * TA: Nitin Jindal
 * System: jdk-1.5.0.4 and Eclipse 3.1 on Windows XP
 * @author Michael Leonhard (CS account mleonhar)
 * @version 12 Oct 2005
 */

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.InputMap;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JSpinner;
import javax.swing.KeyStroke;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;

public class NewGameDialog extends javax.swing.JDialog {
	// version number of this class, used for serialization
	private static final long serialVersionUID = 1L;
	// default number of rows
	private final int DEFAULTROWS = 5;
	// default number of columns
	private final int DEFAULTCOLS = 5;

	// column spinner, returns Integer objects
	private JSpinner colSpinner;
	// row spinner, returns Integer objects
	private JSpinner rowSpinner;
	// callback object which receives user's selection
	private NewGameParametersCallback callback;

	/**
	 * Shows the dialog box in the right position over the parent window
	 */
	public void showDialog() {
		// TODO figure out why this works incorrectly on Linux
		// get the origin of the window
		Point location = this.getParent().getLocation();
		// move down and to the right
		location.translate(30, 30);
		// place the dialog here
		this.setLocation(location);
		// show the dialog
		this.setVisible(true);
	}

	/**
	 * Allow testing of dialog as a standalone class
	 * 
	 * @param args command line parameters (ignored)
	 */
	public static void main(String[] args) {
		// make an invisible toplevel window
		JFrame frame = new JFrame();
		// closing the window should dispose of it, allowing VM to exit
		frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		// show it
		frame.setSize(400, 300);
		frame.setVisible(true);
		// anonymous inline callback
		NewGameParametersCallback cback = new NewGameParametersCallback() {
			public void newGameParameters(int cols, int rows) {
				// print out the user's choices
				System.out.println("NewGameParametersCallback cols=" + cols
						+ " rows=" + rows);
			}
		};
		// make the modal dialog
		NewGameDialog instance = new NewGameDialog(frame, cback);
		// show the dialog
		instance.showDialog();
	}

	/**
	 * Creates the invisible modal dialog box
	 * 
	 * @param frame window that owns the dialog
	 * @param callback callback to receive the new game parameters
	 */
	public NewGameDialog(JFrame frame, NewGameParametersCallback callback) {
		// allow the super-class to initialize (JDialog)
		super(frame);
		// save data
		this.callback = callback;
		// set up the dialog
		initGUI();
	}

	/**
	 * Configure's the dialog's appearance and creates the contained widgets
	 */
	private void initGUI() {
		// configure dialog layout
		GridBagLayout thisLayout = new GridBagLayout();
		thisLayout.columnWeights = new double[] { 0.1, 0.1, 0.1 };
		thisLayout.columnWidths = new int[] { 7, 7, 2 };
		thisLayout.rowWeights = new double[] { 0.1, 0.1, 0.1, 0.1, 0.1 };
		thisLayout.rowHeights = new int[] { 7, 7, 7, 7, 7 };
		this.getContentPane().setLayout(thisLayout);

		// set other characteristics of dialog
		this.setTitle("New Game");
		this.setModal(true);
		this.setResizable(false);
		this.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		this.setSize(197, 209);

		{// "New Game" label
			JLabel newGameLabel = new JLabel();
			newGameLabel.setText("New Game");
			newGameLabel.setFont(new java.awt.Font("Dialog", 1, 18));

			GridBagConstraints newGameLabelConstraints = new GridBagConstraints();
			newGameLabelConstraints.gridy = 0;
			newGameLabelConstraints.gridwidth = 2;

			this.getContentPane().add(newGameLabel, newGameLabelConstraints);
		}

		{// "Choose dimensions:" label
			JLabel chooseLabel = new JLabel();
			chooseLabel.setText("Choose dimensions:");
			chooseLabel.setLayout(null);
			chooseLabel.setHorizontalTextPosition(SwingConstants.LEADING);
			chooseLabel.setHorizontalAlignment(SwingConstants.CENTER);

			GridBagConstraints chooseLabelConstraints = new GridBagConstraints();
			chooseLabelConstraints.gridy = 1;
			chooseLabelConstraints.gridwidth = 2;
			chooseLabelConstraints.anchor = GridBagConstraints.WEST;
			chooseLabelConstraints.ipadx = 10;

			this.getContentPane().add(chooseLabel, chooseLabelConstraints);
		}

		{// "Rows:" label
			JLabel rowsLabel = new JLabel();
			rowsLabel.setText("Rows:");

			GridBagConstraints rowsLabelConstraints = new GridBagConstraints();
			rowsLabelConstraints.gridy = 2;
			rowsLabelConstraints.anchor = GridBagConstraints.EAST;
			rowsLabelConstraints.ipadx = 10;

			this.getContentPane().add(rowsLabel, rowsLabelConstraints);
		}

		{// rows spinner
			this.rowSpinner = new JSpinner();
			SpinnerNumberModel rowSpinnerModel = new SpinnerNumberModel(
					DEFAULTROWS, 1, 50, 1);
			this.rowSpinner.setModel(rowSpinnerModel);

			GridBagConstraints rowSpinnerConstraints = new GridBagConstraints();
			rowSpinnerConstraints.gridx = 1;
			rowSpinnerConstraints.gridy = 2;
			rowSpinnerConstraints.fill = GridBagConstraints.HORIZONTAL;

			this.getContentPane().add(this.rowSpinner, rowSpinnerConstraints);
		}

		{// "Columns:" label
			JLabel colsLabel = new JLabel();
			colsLabel.setText("Columns:");

			GridBagConstraints cosLabelConstraints = new GridBagConstraints();
			cosLabelConstraints.gridy = 3;
			cosLabelConstraints.anchor = GridBagConstraints.EAST;
			cosLabelConstraints.ipadx = 10;

			this.getContentPane().add(colsLabel, cosLabelConstraints);
		}

		{// cols spinner
			this.colSpinner = new JSpinner();
			SpinnerNumberModel colSpinnerModel = new SpinnerNumberModel(
					DEFAULTCOLS, 1, 50, 1);
			this.colSpinner.setModel(colSpinnerModel);

			GridBagConstraints colSpinnerConstraints = new GridBagConstraints();
			colSpinnerConstraints.gridx = 1;
			colSpinnerConstraints.gridy = 3;
			colSpinnerConstraints.fill = GridBagConstraints.HORIZONTAL;

			this.getContentPane().add(this.colSpinner, colSpinnerConstraints);
		}

		{// "Start Game" button
			JButton startGameButton = new JButton();
			startGameButton.setText("Start Game");

			// button's action map
			ActionMap acmap = startGameButton.getActionMap();
			// map for input events that happen anywhere in the dialog
			InputMap inmap = startGameButton
					.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);

			// ENTER KEY HANDLER
			// make an anonymous object to handle dispatch action
			Action dispatchAction = new AbstractAction() {
				private static final long serialVersionUID = 1L;
				// Invoked when the action occurs. Dispatches the dialog box.
				public void actionPerformed(ActionEvent e) {
					// System.out.println("NewGameDialog:actionPerformed:
					// dispatch");
					dispatchDialog();
				}
			};
			// set object as action handler for the button (for button presses)
			startGameButton.addActionListener(dispatchAction);
			// register "dispatch" action for enter key presses
			KeyStroke EnterKey = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0);
			acmap.put("dispatch", dispatchAction);
			inmap.put(EnterKey, "dispatch");

			// ESCAPE KEY HANDLER
			// make an anonymous object to handle dispose action
			Action disposeAction = new AbstractAction() {
				private static final long serialVersionUID = 1L;
				// Invoked when an action occurs. Disposes of the dialog box.
				public void actionPerformed(ActionEvent e) {
					// System.out.println("NewGameDialog:actionPerformed:
					// dispose");
					dispose();
				}
			};
			// register "dispose" action for escape key presses
			KeyStroke EscapeKey = KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0);
			acmap.put("dispose", disposeAction);
			inmap.put(EscapeKey, "dispose");

			GridBagConstraints startGameButtonConstraints = new GridBagConstraints();
			startGameButtonConstraints.gridx = 1;
			startGameButtonConstraints.gridy = 4;

			this.getContentPane().add(startGameButton,
					startGameButtonConstraints);
		}
	}

	/**
	 * Disposes of the dialog box and passes dialog values to the callback
	 * object. The dialog may be recreated again by calling setVisible(true).
	 */
	private void dispatchDialog() {
		// get the values from the spinner widgets
		Integer colInteger = (Integer) this.colSpinner.getValue();
		Integer rowInteger = (Integer) this.rowSpinner.getValue();
		int cols = colInteger.intValue();
		int rows = rowInteger.intValue();
		// System.out.println("colSpinner=" + cols);
		// System.out.println("rowSpinner=" + rows);

		// Dispose of the dialog (hides it, removes all event handlers, VM exit
		// allowed)
		this.dispose();

		// pass the values to the callback object
		this.callback.newGameParameters(cols, rows);
	}
}