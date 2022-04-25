/**
 * This program implements a dots and boxes game.
 * 
 * Assignment: MP3
 * Class: CS 340, Fall 2005
 * TA: Nitin Jindal
 * System: jdk-1.5.0.4 and Eclipse 3.1 on Windows XP
 * @author Michael Leonhard (CS account mleonhar)
 * @version 12 Oct 2005
 */

import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.WindowConstants;

public class DotsAndBoxes extends javax.swing.JFrame implements ActionListener {
	// version number of this class, used for serialization
	private static final long serialVersionUID = 1L;
	// dialog box for starting a new game (normally hidden)
	private NewGameDialog newGameDialog;
	// the field of dots and lines
	private Field field;
	// counter widget that displays user's score
	private CounterLabel userCounterLabel;
	// counter widget that displays computer's score
	private CounterLabel computerCounterLabel;

	// gameplay instructions
	final String HOWTOPLAYTEXT = "How To Play\r\n" + "\r\n"
			+ "The game consists of a field of dots.  Take turns with the\r\n"
			+ "computer adding lines between the dots.  Complete a box to\r\n"
			+ "get another turn.  Your boxes will show O.  The computer's\r\n"
			+ "boxes get X.  Complete the most boxes to win!\r\n";

	// about box text
	final String ABOUTTEXT = "Dots and Boxes is a Java program written by "
			+ "Michael Leonhard.\r\n"
			+ "\r\n"
			+ "Michael is an undergraduate studying Computer Science at the\r\n"
			+ "University of Illinois at Chicago.  This program is a project\r\n"
			+ "for CS340 Software Design.\r\n"
			+ "\r\n"
			+ "Michael has a website at http://tamale.net/\r\n"
			+ "------------------------------------------\r\n"
			+ "Michael Leonhard (mleonhar)\r\n"
			+ "Assignment 3 (MP3)\r\n"
			+ "CS 340, Fall 2005\r\n"
			+ "Instructor: Pat Troy\r\n"
			+ "TA: Nitin Jindal\r\n"
			+ "Created 9 Oct 2005 with\r\n"
			+ "jdk-1.5.0.4 and Eclipse 3.1 on Windows XP";

	/**
	 * Set up a new game and show the frame
	 * 
	 * @param cols number of columns of dots
	 * @param rows number of rows of dots
	 */
	public void newGame(int cols, int rows) {
		// reset the counters
		this.userCounterLabel.reset();
		this.computerCounterLabel.reset();

		// if an old field exists, remove it
		if (this.field != null) this.getContentPane().remove(this.field);

		// create the new field
		this.field = new Field(cols, rows, this.userCounterLabel,
				this.computerCounterLabel);
		this.getContentPane().add(this.field);
		this.getContentPane().validate();

		// show the window (if this is the first game)
		this.setVisible(true);
	}

	/**
	 * This method is called when the class is loaded from the command line. It
	 * makes an instance of the main game class and starts a game on it.
	 * 
	 * @param argv command line parameters (ignored)
	 */
	public static void main(String[] argv) {
		// make an instance of the main game class
		DotsAndBoxes instance = new DotsAndBoxes();
		// start a new game
		instance.newGame(6, 6);
	}

	/**
	 * Make an invisible game window, with menu and status bar
	 */
	public DotsAndBoxes() {
		// allow the super class to initialize (JFrame)
		super();
		// set up the window, menu, and status bar
		initGUI();

		// Anonymous object, gets called when user dispatches new game dialog
		NewGameParametersCallback cback = new NewGameParametersCallback() {
			public void newGameParameters(int rows, int cols) {
				newGame(rows, cols);
			}
		};
		// create the hidden dialog box
		newGameDialog = new NewGameDialog(this, cback);
	}

	/**
	 * Set characteristics of main window, make menu, make status bar
	 */
	private void initGUI() {
		// closing the main window should dispose of it, allowing VM to exit
		setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		// set window title
		this.setTitle("Dots and Boxes");
		// initial size
		this.setSize(400, 300);
		// let the window manager choose location (requires Java 1.5)
		//this.setLocationByPlatform(true);

		// main window layout
		BorderLayout thisLayout = new BorderLayout();
		this.getContentPane().setLayout(thisLayout);

		{// menu
			JMenuBar menuBar = new JMenuBar();
			this.setJMenuBar(menuBar);

			// Game menu
			JMenu gameMenu = new JMenu("Game");
			gameMenu.setMnemonic(java.awt.event.KeyEvent.VK_G);
			menuBar.add(gameMenu);

			// New Game menu item
			JMenuItem newGameMenuItem = new JMenuItem("New Game");
			newGameMenuItem.setMnemonic(java.awt.event.KeyEvent.VK_N);
			newGameMenuItem.addActionListener(this);
			gameMenu.add(newGameMenuItem);

			// Exit menu item
			JMenuItem exitMenuItem = new JMenuItem("Exit");
			exitMenuItem.setMnemonic(java.awt.event.KeyEvent.VK_X);
			exitMenuItem.addActionListener(this);
			gameMenu.add(exitMenuItem);

			// Help menu
			JMenu helpMenu = new JMenu("Help");
			helpMenu.setMnemonic(java.awt.event.KeyEvent.VK_H);
			menuBar.add(helpMenu);

			// How to Play menu item
			JMenuItem howToPlayMenuItem = new JMenuItem("How to Play");
			howToPlayMenuItem.setMnemonic(java.awt.event.KeyEvent.VK_P);
			howToPlayMenuItem.addActionListener(this);
			helpMenu.add(howToPlayMenuItem);

			// About menu item
			JMenuItem aboutMenuItem = new JMenuItem("About");
			aboutMenuItem.setMnemonic(java.awt.event.KeyEvent.VK_A);
			aboutMenuItem.addActionListener(this);
			helpMenu.add(aboutMenuItem);
		}

		{// status bar
			JPanel statusBarPanel = new JPanel();
			GridBagLayout statsuBarLayout = new GridBagLayout();
			statsuBarLayout.columnWeights = new double[] { 0.05, 0.45, 0.45,
					0.05 };
			statsuBarLayout.columnWidths = new int[] { 7, 7, 7, 7 };
			statusBarPanel.setLayout(statsuBarLayout);
			this.getContentPane().add(statusBarPanel, BorderLayout.SOUTH);
			statusBarPanel.setVisible(true);
			statusBarPanel.setFocusable(false);

			// user count
			this.userCounterLabel = new CounterLabel("Your Score: ");
			GridBagConstraints userLabelConstraints = new GridBagConstraints();
			userLabelConstraints.gridx = 1;
			statusBarPanel.add(userCounterLabel, userLabelConstraints);

			// computer count
			this.computerCounterLabel = new CounterLabel("Computer's Score: ");
			GridBagConstraints computerLabelConstraints = new GridBagConstraints();
			computerLabelConstraints.gridx = 2;
			statusBarPanel.add(computerCounterLabel, computerLabelConstraints);
		}
	}

	/**
	 * Handles menu events. Implements ActionListener.actionPerformed().
	 * 
	 * @param e object with information about the event
	 */
	public void actionPerformed(ActionEvent e) {
		// System.out.println("actionPerformed: " + e.getActionCommand());

		// New Game menu item was selected
		if (e.getActionCommand().equals("New Game")) {
			// show modal new game dialog box
			newGameDialog.showDialog();
		}

		// How to Play menu item was selected
		else if (e.getActionCommand().equals("How to Play")) {
			// show the instructional modal dialog box
			JOptionPane.showMessageDialog(this, HOWTOPLAYTEXT, "How To Play",
					JOptionPane.PLAIN_MESSAGE);
		}

		// About menu item was selected
		else if (e.getActionCommand().equals("About")) {
			// show the modal about box
			JOptionPane.showMessageDialog(this, ABOUTTEXT,
					"About Dots and Boxes", JOptionPane.PLAIN_MESSAGE);
		}

		// Exit menu item was selected
		else if (e.getActionCommand().equals("Exit")) {
			// dispose of the main window. Java VM will exit if there are no
			// other threads or windows.
			this.dispose();
		}
	}
}