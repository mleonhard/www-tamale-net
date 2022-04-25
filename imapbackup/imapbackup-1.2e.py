#!/usr/bin/env python

"""IMAP Incremental Backup Script"""
__version__ = "1.2e" # Not likely to change soon
__author__ = "Rui Carmo (http://the.taoofmac.com)"
__copyright__ = "(C) 2006 Rui Carmo. Code under BSD License."
__contributors__ = "Bob Ippolito (fix for http://python.org/sf/1092502)"

# THIS IS BETA SOFTWARE - USE AT YOUR OWN RISK, I TAKE NO RESPONSIBILITY FOR ANY DATA LOSS
# See http://the.taoofmac.com/space/Projects/imapbackup.py for more information.

import getpass, os, gc, sys, time, platform, getopt
import mailbox, rfc822, imaplib, socket, email
import StringIO, re, csv, sha, gzip, bz2

# Progress spinner 
spinner_pos = 0
spinner=[c.encode("utf-8") for c in unicode("|/-\\","utf-8")]

def spin(i):
  """Display a cheesy spinner"""
  global spinner_pos
  if sys.stdin.isatty():
    sys.stdout.write("\r" + spinner[spinner_pos])
    sys.stdout.flush()
    spinner_pos+=1
    spinner_pos%=len(spinner)

def clean_exit():
  sys.stdout.write("\n")

def cli_exception(type, value, tb):
  if not issubclass(type, KeyboardInterrupt):
    sys.__excepthook__(type, value, tb)
  else:
    clean_exit()

# Make sure we get a chance to clean up the display on a tty
if sys.stdin.isatty():
  sys.excepthook=cli_exception

# Helper class for IMAP folder list parsing
class mailboxlist(csv.excel):
  """This class is a csv dialect for parsing the IMAP folder list"""
  delimiter = ' '

# Hideous fix to counteract http://python.org/sf/1092502
# (which should have been fixed ages ago.)
def _fixed_socket_read(self, size=-1):
  data = self._rbuf
  if size < 0:
      # Read until EOF
      buffers = []
      if data:
          buffers.append(data)
      self._rbuf = ""
      if self._rbufsize <= 1:
          recv_size = self.default_bufsize
      else:
          recv_size = self._rbufsize
      while True:
          data = self._sock.recv(recv_size)
          if not data:
              break
          buffers.append(data)
      return "".join(buffers)
  else:
      # Read until size bytes or EOF seen, whichever comes first
      buf_len = len(data)
      if buf_len >= size:
          self._rbuf = data[size:]
          return data[:size]
      buffers = []
      if data:
          buffers.append(data)
      self._rbuf = ""
      while True:
          left = size - buf_len
          recv_size = min(self._rbufsize, left) # the actual fix
          data = self._sock.recv(recv_size)
          if not data:
              break
          buffers.append(data)
          n = len(data)
          if n >= left:
              self._rbuf = data[left:]
              buffers[-1] = data[:left]
              break
          buf_len += n
      return "".join(buffers)

# Platform detection to enable socket patch
# (issue may be present in other Pythons, but of this combination I'm sure of)
if('Darwin' in platform.platform() and '2.3.5' == platform.python_version()):
  socket._fileobject.read = _fixed_socket_read

# Regular expressions for parsing
msgmatch = re.compile("^Message\-Id\: (.+)", re.IGNORECASE + re.MULTILINE)
filematch = re.compile("(.+)", re.MULTILINE)
blanks = re.compile(r'\s+', re.MULTILINE)
msgsize = re.compile("\d+ \(RFC822.SIZE (\d+).*\)")

# Constants
IMAP_PATH_SEPARATOR='/' # May be different, depending on IMAP server
UUID = '19AF1258-1AAF-44EF-9D9A-731079D6FAD7' # Used to generate Message-Ids

def collectFromIMAP(server, imap_folder):
  """Collects Message-Ids from a given IMAP folder"""
  server.select(imap_folder)
  sys.stdout.write("  IMAP: Scanning %s" % imap_folder)
  # List all messages
  typ, data = server.search(None, 'ALL')
  messages = {}
  i = 0
  for num in data[0].split():
    # Retrieve each individual Message-Id
    typ, data = server.fetch(num, '(BODY[HEADER.FIELDS (MESSAGE-ID)])')
    header = data[0][1].strip()
    # remove newlines inside Message-Id (a dumb Exchange trait)
    header = blanks.sub(' ', header)
    try:
      id = msgmatch.match(header).group(1) 
      if id not in messages.keys():
        # avoid adding dupes
        messages[id] = num
    except:
      # Some messages may have no Message-Id, so we'll synthesise one
      # (this usually happens with Sent, Drafts and .Mac news)
      typ, data = server.fetch(num, '(BODY[HEADER.FIELDS (FROM TO CC DATE SUBJECT)])')
      header = data[0][1].strip()
      header = header.replace('\r\n','\t')
      messages['<' + UUID + '.' + sha.sha(header).hexdigest() + '>'] = num      
      pass
    i = i + 1
    spin(i)
  sys.stdout.write("\n  IMAP: Found %d unique messages in %s.\n" % (len(messages.keys()),imap_folder))
  return messages

def collectFromFile(filename, compress):
  """Collects Message-Ids from a given mbox file"""
  # Most of this code is deprecated in Python > 2.3, since PortableUnixMailbox is no more
  messages = {}
  i = 0
  if os.path.exists(filename):
    sys.stdout.write("  FILE: Scanning %s" % filename)
    if compress == 'gzip':
      handle = gzip.GzipFile(filename,'rb')
    elif compress == 'bzip2':
        handle = bz2.BZ2File(filename,'rb')
    else:
      handle = file(filename,'rb')
    for message in mailbox.PortableUnixMailbox(handle):
      header = ''
      # We assume all messages on disk have message-ids
      try:
        header =  ''.join(message.getfirstmatchingheader('message-id'))
      except KeyError:
        # No message ID was found. Warn the user and move on
        sys.stdout.write("\n  WARNING: Message #%d on %s does not have Message-Id header: %s." % (i, filename, str(message.getfirstmatchingheader('message-id'))))
        pass
      header = blanks.sub(' ', header.strip())
      try:
        id = msgmatch.match(header).group(1)
        if id not in messages.keys():
          # avoid adding dupes
          messages[id] = id
      except AttributeError:
        # Message-Id was found but could somehow not be parsed by regexp (highly bloody unlikely)
        sys.stdout.write("\n  WARNING: Mailbox file seems not to have been generated by this program.")
        sys.stdout.write("\n           Message-Id scanning turned up '%s'" % header)
        pass
      i = i + 1
      spin(i)
    handle.close()
  sys.stdout.write("\n  FILE: Found %d unique messages in %s.\n" % (len(messages.keys()),filename))
  return messages

def updateMailbox(server, imap_folder, mailbox, messages, existing, compress, clobber):
  """Append messages from IMAP folder to existing mailbox"""
  server.select(imap_folder)
  # Check if server supports PEEK
  # (bit redundant to do it every time, I know...)
  fetch_command = "(RFC822.PEEK)"
  response = server.fetch("1:1", fetch_command)
  if response[0] != "OK":
    fetch_command = "RFC822"
  else:
    fetch_command = "RFC822.PEEK"
  i = 0
  maxlength = total = 0
  if clobber == True:
    sys.stdout.write('  COPY: Copying from %s to %s' % (imap_folder, mailbox))
  else:
    sys.stdout.write('  APPEND: Appending from %s to %s' % (imap_folder, mailbox))
  # Open disk file
  if compress == 'gzip':
    mbx = gzip.GzipFile(mailbox,'ab',9)
  elif compress == 'bzip2':
    mbx = bz2.BZ2File(mailbox,'wb',512*1024,9)
  else:
    mbx = file(mailbox,'ab')
  for id in messages.keys():
    # If IMAP message is not in mbox file
    if id not in existing.keys():
      # Get raw message size
      typ, data = server.fetch(messages[id], '(RFC822.SIZE)')
      length = int(msgsize.match(data[0]).group(1))
      maxlength = max(length, maxlength)
      total = total + length
      # This "From" and the terminating newline below delimit messages in mbox files
      buffer = "From nobody %s\n" % time.strftime('%a %m %d %H:%M:%S %Y') 
      # If this is one of our synthesised Message-Ids, insert it before the other headers
      if UUID in id:
        buffer = buffer + "Message-Id: %s\n" % id
      mbx.write(buffer)
      buffer = ''
      typ, data = server.fetch(messages[id], fetch_command)
      mbx.write(data[0][1].strip().replace('\r',''))
      del data
      gc.collect()
      mbx.write('\n\n')
      i = i + 1
      spin(i)
  mbx.close()
  if i == 0:
    sys.stdout.write('\n  INFO: No new messages.\n')
  else:
    sys.stdout.write('\n  SUMMARY: Appended %d messages to %s\n  (%d bytes, of which the largest message was %d bytes)\n' % (i, mailbox, total, maxlength))

def scanTree(server, compress, clobber):
  """Parse folder listing and loop over it"""
  # Obtain folder listing
  typ, data = server.list(pattern='*')
  # Parse folder listing as a CSV dialect (automatically removes quotes)
  reader = csv.reader(StringIO.StringIO('\r\n'.join(data)),dialect='mailboxlist')
  # Iterate over each folder
  for row in reader:
    imap_folder = row[2]
    # generate a pathname for the mailbox file
    # (we assume that folders can contain messages, so we store messages in a '.mbox' file
    # inside a pathname generated from the IMAP mailbox name)
    path = '/'.join(imap_folder.split(IMAP_PATH_SEPARATOR))
    filename = '.'.join(imap_folder.split(IMAP_PATH_SEPARATOR)) + '.mbox'
    if compress == 'gzip':
      filename = filename + '.gz'
    elif compress == 'bzip2':
      filename = filename + '.bz2'
      
    existing = {}
    # Collect Message-Ids from each folder
    messages = collectFromIMAP(server, imap_folder)
    if os.path.exists(filename):
      if clobber == True:
        os.remove(filename)
      elif compress != 'bzip2':
        # Collect pre-existing Message-Ids from disk file
        existing = collectFromFile(filename, compress)
    # now copy messages across
    updateMailbox(server, imap_folder, filename, messages, existing, compress, clobber)

def main():
  """Main entry point"""
  try:
    opts, args = getopt.getopt(sys.argv[1:], "z:s:u:p:y", ["compress=","server=", "username=","password=","yes-i-want-to-clobber-files"])
  except getopt.GetoptError:
    print "Usage: imapbackup [OPTIONS]"
    print "-y --yes-i-want-to-clobber-files does not try to append, or warn about bzip2 clobbering"
    print "-z (gzip|bzip2) --compress=(gzip|bzip2) create/append to compressed files (EXPERIMENTAL)"
    print "   WARNING: bzip2 does not allow for appending, existing files will be clobbered."
    print "-s HOSTNAME --server=HOSTNAME    connect to HOSTNAME"
    print "-u USERNAME --username=USERNAME  with USERNAME"
    print "-p PASSWORD --password=PASSWORD  with PASSWORD (you will be prompted for one if missing)"
    print "\nMailbox files will be created IN THE CURRENT WORKING DIRECTORY"
    sys.exit(2)
  username = password = server = None
  clobber = False
  compress = 'plain'
  for option, value in opts:
    if option in ("-y", "--yes-i-want-to-clobber-files"):
      print "WARNING: All existing mailbox files will be overwritten!"
      clobber = True
    if option in ("-z", "--compress"):
      if value in ('gzip','bzip2'):
        compress = value
      else:
        print "ERROR: Invalid compression type specified."
        sys.exit(2)
    if option in ("-s", "--server"):
      server = value
    if option in ("-u", "--username"):
      username = value
    if option in ("-p", "--password"):
      password = value
  if compress == 'bzip2' and clobber == False:
    print "ERROR: bzip2 compression does not allow for appending."
    print"        Please specify -y with it if you want to remove existing archives."
    sys.exit(2)
  elif compress == 'gzip' and clobber == False:
    print "WARNING: Appending will work, but .mbox.gz scanning is VERY slow."
    print "         You may want to consider using uncompressed files and"
    print "         running gzip -9 *.mbox after the backup run."
  if(server is None):
    print "ERROR: No server specified."
    sys.exit(2)
  if(username is None):
    print "ERROR: No username specified."
    sys.exit(2)
  if(password is None):
    password = getpass.getpass()
  server = imaplib.IMAP4(server)
  server.login(username, password)
  scanTree(server, compress, clobber)
  server.logout()

if __name__ == '__main__':
  csv.register_dialect('mailboxlist',mailboxlist)
  gc.enable()
  main()
