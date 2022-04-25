#!/usr/bin/env python
# Echo server program
# Copyright (C) 2002,2003 Mike Leonhard
# http://tamale.net/
#
# version 1.1 (2003/07/07 02:08)
#  * works with updated ListenClass
# version 1.0 (2003/07/06 12:32)
import select, time
from inetcomms import *

class CoordinatorClass:
	def __init__( self, port=12360 ):
		"""Initialize Coordinator class"""
		self.liasons = []
		self.pending = []
		self.listen = ListenClass( port )
		self.sockets = self.liasons + [self.listen]
	
	def liasonconnectedfilter( self, l ):
		"""Return the connected status of a liason"""
		return l.connected
	
	def liasonsendfilter( self, l ):
		"""Return the whether the liason has unsent data"""
		return l.connected and len( l.outbuffer ) > 0
	
	def cycle( self ):
		"""Check each socket"""
		
		# clean liason list
		self.liasons = filter( self.liasonconnectedfilter, self.liasons )
		
		# each new connection from the listen socket
		for (connection,address) in self.listen.newconnections:
			# create a new liason for the connection
			l = LiasonClass( connection )
			# send greeting
			l.dosend( "You have entered a strange and mysterious place.\n" )
			# add the liason to the list
			self.liasons.append( l )
		
		# all new connections have been handled
		self.listen.newconnections = []
		
		# update list of listenable sockets
		self.sockets = self.liasons + [self.listen]

		# update list of sockets with pending outgoing data
		self.pending = filter( self.liasonsendfilter, self.liasons )
	
	def dosend( self, data ):
		"""Send data through each liason"""
		# each liason
		for i in self.liasons:
			# send the data
			i.dosend( data )
		
	def cleanup( self ):
		"""Cleanup the liasons and listen socket"""
		# each liason
		for l in self.liasons:
			# disconnect
			l.disconnect()
		
		# clear list of liasons
		self.liasons = []
		
		# listen socket
		self.listen.disconnect()
	
	def loop( self ):
		"""Loop while reading liasons and listen socket"""
		
		# begin looping
		looping = 1
		
		# loop
		while looping == 1:
			# get lists of sockets that are ready to be read or written
			(readable, sendable, e) = select.select( self.sockets, self.pending, [] )
			
			# check each readable socket
			for l in readable:
				# read
				data = l.doread()
				# data was read
				if data:
					# send it out
					self.dosend( data )
			
			# check each sendable socket
			for l in sendable:
				# write
				l.dosend()
			
			# update lists
			self.cycle()
			
			# pause
			#time.sleep( 5 )
		
print 'Beginning'
coordinator = CoordinatorClass()
coordinator.loop()
coordinator.cleanup()
print 'Ending'
