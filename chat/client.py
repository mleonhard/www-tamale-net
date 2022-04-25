#!/usr/bin/env python
#
# Client.py
# Copyright (C) 2003 Michael Leonhard
# http://tamale.net/
#
# version 1.1 (2003/07/07 02:09)
#  * uses HeadHunterClass now
#  * raises SanityCheckFailure exception
# version 1.0 RC1 (2003/07/06 00:42)

# standard Python modules
import errno, math, select, string, thread

# PyGame modules (http://www.pygame.org/)
import pygame, pygame.display, pygame.draw, pygame.event, pygame.font, pygame.time
from pygame.locals import *

# program modules (http://tamale.net/chat)
from widgets import *
from inetcomms import *

TITLETEXT = "Client v1.1"
CREDITTEXT = "tamale.net/client"
INITIALTEXT = """# Client.py\n# Copyright (C) 2003 Michael Leonhard\n# http://tamale.net/\n#\n"""
CONNECTINSTRUCTION = "# To join the chat, enter the server name above and\n# press CONNECT.\n"
SERVERHELPTEXT = "* invalid server address\n# Try echo.tamale.net or echo.tamale.net:12360\n"
PORTHELPTEXT = "# If you specify a port, it must be a whole number between\n# 0 and 65536.  Use a colon to separate the hostname and port.\n# EXAMPLE: chat.blah.com:2033\n"
NOTCONNECTEDTEXT = "# You are not connected\n"

FILTERCHARS = "?????????	\n?????" + "????????????????" + """ !"#$%&'()*+,-./""" + "0123456789:;<=>?" + "@ABCDEFGHIJKLMNO" + "PQRSTUVWXYZ[\\]^_" + "`abcdefghijklmno" + "pqrstuvwxyz{|}~?" + "?" * 128

DEFAULTSERVERNAME = "localhost" #"echo.tamale.net"
DEFAULTPORT = 12360
DEFAULTNICKNAME = "Default"

class SanityCheckFailure( Exception ):
	"""Sanity Check Failure Exception class"""
	def __init__( self, whatfailed ):
		"""Initialize a new exception"""
		self.reason = whatfailed
	
	def __str__( self ):
		"""String representation of exception"""
		return `self.whatfailed`

class ClientClass( WidgetWindow ):
	def __init__( self ):
		"""Initialize Client class"""
		# data
		self.headhunter = None
		self.completedheadhunters = []
		self.lock = thread.allocate_lock()
		self.liason = None
		self.nickname = DEFAULTNICKNAME
		
		# instead of pygame.init(), initialize modules manually to 
		# avoid initing pygame.sound
		pygame.display.init()
		pygame.font.init()
		
		# setup screen
		screen = pygame.display.set_mode( (512, 384), DOUBLEBUF )
		pygame.display.set_caption( 'Client' )
		
		# filter events
		self.PERIODICEVENT = USEREVENT + 1
		badevents = [NOEVENT, ACTIVEEVENT, JOYAXISMOTION, JOYBALLMOTION, JOYHATMOTION, JOYBUTTONDOWN ,JOYBUTTONUP, VIDEORESIZE, SYSWMEVENT, NUMEVENTS]
		goodevents = [self.PERIODICEVENT, KEYDOWN, KEYUP, MOUSEMOTION, MOUSEBUTTONDOWN, MOUSEBUTTONUP, QUIT ]
		pygame.event.set_blocked( badevents )
		
		# initialize the WidgetWindow base class
		WidgetWindow.__init__( self, screen )
		
		# create the widgets
		title = TextClass( self, (5, 0, 150, 35), TITLETEXT, 36 )
		credit = TextClass( self, (5, 37, 250, 25), CREDITTEXT )
		
		nicklabel = TextClass( self, (160, 7, 80, 20), "Nickname:" )
		self.nickbox = EditClass( self, self.nickeditaction, (242, 7, 155, 20 ), self.nickname, 24, 50 )
		self.nickbox.allowed = """`1234567890-=qwertyuiop[]\\asdfghjkl;'zxcvbnm,./~!@#$%^&*()_+QWERTYUIOP{}|ASDFGHJKL"ZXCVBNM<?"""
		nickbutton = ButtonClass( self, self.nickbuttonaction, (402, 2, 105, 25), "CHANGE", DARKGRAY, BLACK, WHITE, 20 )
		
		serverlabel = TextClass( self, (160, 37, 55, 20), "Server:" )
		self.serverbox = EditClass( self, self.servereditaction, (217, 37, 180, 20 ), DEFAULTSERVERNAME, 24, 200 )
		self.serverbox.allowed = ".:-1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"		
		self.serverbutton = ButtonClass( self, self.serverbuttonaction, (402, 32, 105, 25), "CONNECT", DARKGRAY, BLACK, WHITE, 20 )
		
		self.page = PageClass( self, (0, 60, 512, 304), (0x80, 0x80, 0x80, 0xFF) )
		self.edit = EditClass( self, self.editaction, (0, 364, 512, 20), "", 24, 200 )
		
		# put the widgets in the window
		self.addwidget( title )
		self.addwidget( credit )

		self.addwidget( nicklabel )
		self.addwidget( self.nickbox, TABTARGET )
		self.addwidget( nickbutton )

		self.addwidget( serverlabel )
		self.addwidget( self.serverbox, TABTARGET )
		self.addwidget( self.serverbutton )

		self.addwidget( self.page )
		self.addwidget( self.edit, TABTARGET )
		
		# set keyboard focus to the edit widget
		self.nickbox.focus()
		
		# initial text
		self.page.append( INITIALTEXT + CONNECTINSTRUCTION )
		
		# initial screen draw
		self.eventproc( pygame.event.Event( NOEVENT, {} ) )
		
		# the network will be checked at this rate
		pygame.time.set_timer( self.PERIODICEVENT, 50 ) #33 == 30fps

	def loop( self ):
		"""Program loop"""
		
		# begin looping
		self.looping = 1
		
		# loop
		while self.looping:
			# get the next event
			event = pygame.event.wait()
			
			# periodic event
			if event.type == self.PERIODICEVENT:
				# check the headhunters
				self.checkheadhunters()
				
				# get all pending PERIODICEVENTs
				pygame.event.get( self.PERIODICEVENT )
				
				# connected
				if self.liason and self.liason.connected:
					# poll server socket
					data = self.liason.selectandread( 0 )
					
					# data was read
					if data:
						# filter data to prevent ttf bombout
						data = string.translate( data, FILTERCHARS ) 
						# add it to the page
						self.page.append( data )
				# just disconnected
				elif self.liason and not self.liason.connected:
					# disconnect
					self.disconnectnow( "lost connection to server" )
			
			# user pressed the X button to close the window
			elif event.type == QUIT:
				# disconnect the network stuff
				self.doquit( "Client exiting" )
				break
			# pass the event to the widgets
			self.eventproc( event )
	
	def cleanup( self ):
		"""Clean up"""
		pygame.time.set_timer( self.PERIODICEVENT, 0 )
		pygame.quit()
		
		# connected
		if self.liason:
			# disconnect
			self.liason.disconnect()
			# done with the server
			self.liason = None

	def nickbuttonaction( self ):
		"""Function is called when CHANGE button is pressed"""
		# act as if ENTER was pressed in nick box
		self.nickeditaction( self.nickbox )
	
	def nickeditaction( self, widget ):
		"""Function is called when enter key is pressed in nickname box"""

		# new nickname is empty
		if len( widget.text ) == 0:
			# reset to old nickname
			widget.settext( self.nickname )
			# focus nick box
			widget.focus()
			return
		
		# currently connected to server
		if self.liason:
			# set keyboard focus to main edit box
			self.edit.focus()
		# not connected
		else:
			# set keyboard focus to server box
			self.serverbox.focus()
		
		# new nickname is same as old
		if self.nickname == self.nickbox.text:
			# don't do anything
			return
		
		# currently connected to server
		if self.liason:
			# send rename notification
			self.liason.dosend( self.nickname + " is now known as " + self.nickbox.text + "\n" )
		
		# set nickname
		self.nickname = self.nickbox.text
	
	def disconnectnow( self, reason ):

		# not connected
		if not self.liason: raise SanityCheckFailure( "not connected" )
		
		# notify
		self.liason.dosend( self.nickname + " leaves the chat: " +  reason + "\n" )
		self.page.append( self.nickname + " leaves the chat: " +  reason + "\n" )
		self.page.append( "* disconnecting\n\n" )
		
		# disconnect
		self.liason.disconnect()
		# done with the connection
		self.liason = None
		
		# change button
		self.serverbutton.settext( "CONNECT" )
		# change title
		pygame.display.set_caption( 'Client' )
		# focus server box
		self.serverbox.focus()
	
	def connectnow( self ):
		"""Handle connect command"""

		# not connected
		if self.liason: raise SanityCheckFailure( "already connected" )
		
		# press ENTER on nick box in case user forgot to
		self.nickbuttonaction()
		
		# empty server box
		if not len( self.serverbox.text ):
			# focus server box
			self.serverbox.focus()
			return
		
		# split the server and port
		parts = string.split( self.serverbox.text, ":", 2 )
		
		# server host name
		host = string.strip( parts[0] )
		
		# too many colon separated strings
		if len( parts ) > 2:
			# focus server box
			self.serverbox.focus()
			# give instructions
			self.page.append( SERVERHELPTEXT )
			return
		
		# no port specified
		if len( parts ) < 2 or len( string.strip( parts[1] ) ) == 0:
			# use the default port
			port = 12360
		# port was specified
		else:
			try:
				# port
				port = int( string.strip( parts[1] ) )
				# port must be between 1 and 65536
				if not port < 65536 or not port > 0: raise ValueError
			
			# invalid port specified
			except ValueError:
				# focus server box
				self.serverbox.focus()
				# give instructions
				self.page.append( PORTHELPTEXT )
				return
		
		# human readable address
		prettyaddress = host
		
		# using non-default port
		if port != 12360:
			# address includes port
			prettyaddress = prettyaddress + ":" + str( port )
		
		# show connecting message
		self.page.append( "* connecting to " + prettyaddress )
		# update serverbox with reformatted server address
		self.serverbox.settext( prettyaddress )
		# set focus to main edit box
		self.edit.focus()
		
		# create the headhunter class to do the connection
		self.headhunter = HeadHunterClass( host, port, prettyaddress, self.headhuntercallback )

	def checkheadhunters( self ):
		# acquire the lock before doing anything
		self.lock.acquire()

		# each one
		for h in self.completedheadhunters:
			# complete it
			self.completeheadhunter( h )

		# clear the list now
		self.completedheadhunters = []
		
		# release the lock
		self.lock.release()
	
	def completeheadhunter( self, headhunter ):
		# headhunter is not specified
		if not headhunter: raise SanityCheckFailure( "headhunter not specified" )
		
		# this headhunter has been forgotten
		if self.headhunter != headhunter:
			# do nothing
			return
		
		# at this point we know that self.headhunter == headhunter
		
		# headhunter failed
		if not self.headhunter.l:
			# change button
			self.serverbutton.settext( "CONNECT" )
			# change title
			pygame.display.set_caption( 'Client' )
			# focus server box
			self.serverbox.focus()
			# show error message
			self.page.append( "* " + self.headhunter.errortext )
			# done with the headhunter
			self.headhunter = None
			return
		
		# connection succeeded and produced liason
		self.liason = self.headhunter.l
		# done with headhunter
		self.headhunter = None
		# announce
		self.page.append( "* connected to " + self.liason.prettyaddress )
		self.liason.dosend( self.nickname + " joins the chat\n" )
		# change button
		self.serverbutton.settext( "DISCONNECT" )
		# change title
		pygame.display.set_caption( self.liason.prettyaddress + " - Client" )
	
	def headhuntercallback( self, headhunter ):
		# acquire the lock
		self.lock.acquire()
		
		# add headhunter to list that will be reviewed
		self.completedheadhunters.append( headhunter )
		
		# release the lock
		self.lock.release()
	
	def serverbuttonaction( self ):
		"""Function is called when CONNECT/DISCONNECT button is pressed"""
		# currently connected (DISCONNECT was pressed)
		if self.liason:
			# disconnect from the server
			self.disconnectnow( "Pressed DISCONNECT button" )
		# connection in progress (STOP was pressed)
		elif self.headhunter:
			# stop connection
			self.stopconnect()
		# not connected (CONNECT was pressed)
		else:
			# connect to the server
			self.connectnow()
		
	def servereditaction( self, widget ):
		"""Function is called when enter key is pressed in server box"""
		
		# currently connected
		if self.liason:
			# disconnect from the server
			self.disconnectnow( "Changing servers" )
		# connection in progress
		elif self.headhunter:
			# stop the connection
			self.stopconnect()
		
		# connect to the new server
		self.connectnow()

	def stopconnect( self ):
		"""Stop a connection (actually just ignore it)"""
		
		# not connecting
		if not self.headhunter: raise SanityCheckFailure( "not connecting" )
		
		# forget the headhunter
		self.headhunter = None
	
	def doquit( self, text ):
		"""Handle quit command"""
		# currently connected
		if self.liason:
			# reason is not empty
			if len( text ):
				# reason
				text = ": " + text
			# notify
			self.liason.dosend( self.nickname + " leaves the chat" +  text + "\n" )
		
		# stop looping
		self.looping = 0
	
	def dome( self, text ):
		"""Handle me command"""
		# not connected
		if not self.liason:
			# give connection instructions
			self.page.append( CONNECTINSTRUCTION )
			return
		
		# ignore empty /me command
		if not len( text ): return
		
		# send /me text
		self.liason.dosend( self.nickname + " " + text + "\n" )
	
	def docommand( self, text ):
		"""Do a command"""
		# separate the command text
		parts = string.split( text, " ", 1 )
		
		# command
		command = string.lower( parts[0] )
		
		# parameters
		if len( parts ) == 2: parms = string.strip( parts[1] )
		else: parms = ""
		
		# quit command
		if command == "quit": self.doquit( parms )
		# me command
		elif command == "me": self.dome( parms )
		# unknown command
		else:
			# give unknown command instructions
			self.page.append( UNKNOWNCOMMANDINSTRUCTION )
	
	def dotext( self, text ):
		# not connected
		if not self.liason:
			# give connection instructions
			self.page.append( CONNECTINSTRUCTION )
		# connected
		else:
			# send it to the server
			self.liason.dosend( self.nickname + ": " + text + "\n" )
	
	def editaction( self, widget ):
		"""Function called when enter key is pressed"""
		
		# clean string
		text = string.strip( widget.text )
		
		# ignore empty string
		if len( text ) == 0: return
		
		# command
		if text[0] == '/':
			# do the command
			self.docommand( text[1:] )
		# text
		else:
			# do the text
			self.dotext( text )
		
		# clear the edit box
		widget.settext( "" )

client = ClientClass()
client.loop()
client.cleanup()
