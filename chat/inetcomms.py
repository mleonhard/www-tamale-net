#!/usr/bin/env python
#
# InetComms.py
# Copyright (C) 2003 Mike Leonhard
# http://tamale.net/
#
# version 1.3 (2003/07/26 23:52)
#  * ListenClass is for IPv4
#  * ListenClass6 is for IPv6
# version 1.2 (2003/07/26 23:28)
#  * moved imports into individual classes
#    this allows server to be run on old Python 1.5 installations that lack threads
# version 1.1 (2003/07/07 02:07)
#  * changed RepresentativeClass to HeadHunterClass
#    HeadHunterClass does multi-threaded connect in separate thread
#    callback is used to notify main program of success/failure
#    supports IPv6
#  * ListenClass also support IPv6 now, too
# version 1 RC2 (2003/07/06 02:05)
#  * sockets are now non-blocking
#    outgoing data is queued up
#    Coordinator must select on liasons with pending unwritten data and call liason.dosend()
# version 1 RC1 (2003/07/06 01:01)
#  * initial version

# import required modules
import socket

# try to import optional modules
try: import select, thread
except ImportError: pass

CHUNKSIZE = 512 # send chunk size

class LiasonClass:
	def __init__( self, s, prettyaddress=None, greeting=None ):
		"""Initialize the Liason class, greet, and announce"""
		self.socket = s
		self.prettyaddress = prettyaddress
		self.connected = 1
		self.outbuffer = ""

		# load the required modules
		import select
		
		# human readable address not provided
		if not self.prettyaddress:
			# lookup our peer's name and use that
			(phost, pport) = s.getpeername()
			self.prettyaddress = str( phost ) + ":" + str( pport )
		
		# set socket to non-blocking mode
		self.socket.setblocking( 0 )
		
		# announce the new connection
		print 'connected with ' + self.prettyaddress
		
		# greeting
		if greeting: self.dosend( greeting )
	
	def fileno( self ):
		"""Return the file number of the socket"""
		return self.socket.fileno()
	
	def doread( self ):
		"""Read data from the socket"""
		
		# not connected
		if not self.connected: return None
		
		# catch exceptions
		try:
			# Read
			data = self.socket.recv( 1024 )
			# treat empty read as error
			if len( data ) == 0: raise socket.error
		
		# error reading
		except socket.error:
			# disconnect the socket
			self.disconnect()
			# no data to return
			return None
		
		# return the data that was read
		return data
	
	def dosend( self, data=None ):
		"""Send data on the socket"""
		
		# not connected
		if not self.connected: return
		
		# data was specified
		if data:
			# add outgoing data to buffer
			self.outbuffer = self.outbuffer + data
		
		# data is remaining to be sent
		while len( self.outbuffer ):
			# catch exceptions
			try:
				# send first chunk
				sent = self.socket.send( self.outbuffer[:CHUNKSIZE] )
				# cut out sent data from buffer
				self.outbuffer = self.outbuffer[sent:]
			# socket is not ready
			except socket.error:
				# TODO: Disconnect slow clients
				print self.prettyaddress + 'send queue length is ' + str( len( self.outbuffer ) )
				
				# try again later
				break
	
	def disconnect( self ):
		"""Disconnect the socket, announce"""
		
		# not connected
		if not self.connected: return None
		
		# TODO: read all available data before socket close (polite way)
		
		# announce the disconnection
		print 'disconnected with ' + self.prettyaddress
		
		# close the socket
		self.socket.close()
		self.connected = 0
	
	def selectandread( self, wait ):
		# not connected
		if not self.connected: return None
		
		# poll socket
		(readable, w, e) = select.select( [self.socket.fileno()], [], [], wait )
		
		# no data
		if len( readable ) == 0: return None
		
		# read the data
		return self.doread()

class HeadHunterClass:
	"""Sockets communication class that does non-blocking connect()"""
	
	def __init__( self, host, port, prettyaddress=None, callback=None ):
		"""Initialize the Representative class, spawn the connector thread"""
		self.host = host
		self.port = port
		self.prettyaddress = prettyaddress
		self.callback = callback
		self.l = None
		self.errotext = None

		# load required modules
		import thread
		
		# no callback specified
		if self.callback == None:
			# use donothing callback
			self.callback = self.donothing
		
		# spawn the new thread
		thread.start_new_thread( self.connectorthread, () )
	
	def donothing( self, headhunter ):
		"""Generic callback function"""
		pass
	
	def connectorthread( self ):
		# resolve the address
		try:
			addrsetlist = socket.getaddrinfo( self.host, self.port, socket.AF_UNSPEC, socket.SOCK_STREAM)
		# lookup error
		except socket.gaierror, (error, text):
			self.errortext = "GAI ERROR: " + text
			return self.callback( self )
		
		# we want to create the proper socket to connect to the server
		s = None
		
		# try each address
		for addrset in addrsetlist:
			# break out the pieces of the address
			af, socktype, proto, canonname, sa = addrset
			
			print 'addrset: ' + str( addrset )
			
			# create the socket
			try: s = socket.socket( af, socktype, proto )
			# unsupported socket configuration
			except socket.error, (error, text):
				# keep the error message
				self.errortext = "SOCKET ERROR: " + text
				# try next address
				continue
			
			# connect to the server
			try: s.connect( sa )
			# connect failure
			except socket.error, (error, text):
				# close and discard the socket
				s.close()
				s = None
				# keep the error message
				self.errortext = "CONNECT ERROR: " + text
				# try the next address
				continue
			
			# connect was successful
			break
		
		# none of the addresses were successful
		if s is None:
			# report the error
			return self.callback( self )
		
		# create the liason class
		self.l = LiasonClass( s, self.prettyaddress )
		
		# done
		self.callback( self )

class ListenClass:
	def __init__( self, port, host=None, addressfamily=socket.AF_INET ):
		"""Initialize Listen class"""
		
		# data
		self.newconnections = []
		
		# resolve our own address
		print 'resolving ' + str( host ) + ':' + str( port )
		addrsetlist = socket.getaddrinfo( host, port, addressfamily, socket.SOCK_STREAM, 0, socket.AI_PASSIVE )
		
		# we want to create the proper socket to listen on the address and port
		s = None
		
		# try each address
		for addrset in addrsetlist:
			# break out the pieces of the address
			af, socktype, proto, canonname, sa = addrset
			
			print 'addrset: ' + str( addrset )
			
			# catch exceptions
			try:
				# create the socket
				s = socket.socket( af, socktype, proto )
				# set option to allow quick restart of server
				s.setsockopt( socket.SOL_SOCKET, socket.SO_REUSEADDR, 1 )
				
			# unsupported socket configuration
			except socket.error, exception:
				# keep the error
				self.exception = exception
				# try next address
				continue
			
			# catch exceptions
			try:
				# bind to the address
				s.bind( sa )
				# make the socket able to accept connections (max 5 connections can wait at a time)
				s.listen( 5 )
			
			# bind failure
			except socket.error, exception:
				# close and discard the socket
				s.close()
				s = None
				# keep the error
				self.exception = exception
				# try next address
				continue
			
			# bind was successful
			break
		
		# none of the addresses were successful
		if s is None:
			# raise the exception again
			raise socket.error, self.exception
		
		# listening on socket
		print 'Listening on', s.getsockname()
		
		# socket is ready
		self.socket = s
	
	def fileno( self ):
		"""Return the file number of the socket"""
		return self.socket.fileno()
	
	def doread( self ):
		"""Accept a new connection on the listening socket"""
		# accept the connection
		conn, addr = self.socket.accept()
		# add it to the list
		self.newconnections.append( (conn, addr) )
	
	def disconnect( self ):
		# make the socket unable to accept new connections
		self.socket.listen( 0 )
		# close the socket
		self.socket.close()
		print 'Stopped listening'

class ListenClass6( ListenClass ):
	"""IPv6 ListenClass"""
	def __init__( self, port, host=None ):
		ListenClass.__init__( self, port, host, socket.AF_INET6 )
