#!/usr/bin/env python

"""IMAP Incremental Backup Script"""
__version__ = "1.3b"
__author__ = "Rui Carmo (http://the.taoofmac.com)"
__copyright__ = "(C) 2006 Rui Carmo. Code under BSD License."
__contributors__ = "Bob Ippolito (fix for http://python.org/sf/1092502), Michael Leonhard (overhaul)"

# THIS IS BETA SOFTWARE - USE AT YOUR OWN RISK, I TAKE NO RESPONSIBILITY FOR ANY DATA LOSS
# For more information, see http://the.taoofmac.com/space/Projects/imapbackup
# or http://tamale.net/imapbackup

# TODO:
# - Add proper exception handlers to scanFile() and downloadMessages()
# - Migrate mailbox usage from rfc822 module to email module
# - Investigate using the noseek mailbox/email option to improve speed
# - Use the email module to normalize downloaded messages and add missing Message-Id
# - Test parseList() and its descendents on other imapds
# - Test bzip2 support
# - Add option to download only subscribed folders
# - Add regex option to filter folders
# - Use a single IMAP command to get Message-IDs
# - Use a single IMAP command to fetch the messages
# - Improve imaplib module with LIST parsing code, submit patch
# - Submit patch of socket._fileobject.read
import getpass, os, gc, sys, time, platform, getopt
import mailbox, rfc822, imaplib, socket, email
import re, sha, gzip, bz2

class SkipFolderException(Exception): pass

class Spinner:
  def __init__(self, message):
    """Spinner constructor"""
    self.glyphs = [c.encode("utf-8") for c in unicode("|/-\\","utf-8")]
    self.pos = 0
    self.message = message
    sys.stdout.write(message)
    sys.stdout.flush()
    self.spin()
  
  def spin(self):
    """Rotate the spinner"""
    if sys.stdin.isatty():
      sys.stdout.write("\r" + self.message + " " + self.glyphs[self.pos])
      sys.stdout.flush()
      self.pos = (self.pos+1) % len(self.glyphs)

  def stop(self):
    """Erase the spinner from the screen"""
    if sys.stdin.isatty():
      sys.stdout.write("\r" + self.message + "  ")
      sys.stdout.write("\r" + self.message)
      sys.stdout.flush()

def prettyBytes(n):
  """Converts integer into a human friendly count of bytes, eg: 12.243 MB"""
  if n == 1:
    return "1 byte"
  elif n < 1024:
    return "%s bytes" % (n)
  elif n < 1048576:
    return "%.2f KB" % (n/1024.0)
  elif n < 1073741824:
    return "%.3f MB" % (n/1048576.0)
  elif n < 1099511627776:
    return "%.3f GB" % (n/1073741824.0)
  else:
    return "%.3f TB" % (n/1099511627776.0)


# Regular expressions for parsing
msgmatchRE = re.compile("^Message\-Id\: (.+)", re.IGNORECASE + re.MULTILINE)
blanksRE = re.compile(r'\s+', re.MULTILINE)

# Constants
UUID = '19AF1258-1AAF-44EF-9D9A-731079D6FAD7' # Used to generate Message-Ids

def downloadMessages(server, foldername, filename, messages, compress, overwrite):
  """Download messages from folder and append to mailbox"""
  
  if overwrite:
    if os.path.exists(filename):
      print "Deleting", filename
      os.remove(filename)
    return []
  else:
    assert('bzip2' != compress)

  # Open disk file
  if compress == 'gzip':
    mbox = gzip.GzipFile(filename,'ab',9)
  elif compress == 'bzip2':
    mbox = bz2.BZ2File(filename,'wb',512*1024,9)
  else:
    mbox = file(filename,'ab')

  # the folder has already been selected by scanFolder()

  # nothing to do
  if not len(messages):
    print "New messages: 0"
    mbox.close()
    return
  
  spinner = Spinner("Downloading %s new messages to %s" % (len(messages), filename))
  total = biggest = 0
  
  # each new message
  for ID in messages.keys():
    # This "From" and the terminating newline below delimit messages in mbox files
    buf = "From nobody %s\n" % time.strftime('%a %m %d %H:%M:%S %Y') 
    # If this is one of our synthesised Message-IDs, insert it before the other headers
    if UUID in ID:
      buf = buf + "Message-Id: %s\n" % ID
    mbox.write(buf)

    # fetch message
    typ, data = server.fetch(messages[ID], "RFC822")
    assert('OK' == typ)
    text = data[0][1].strip().replace('\r','')
    mbox.write(text)
    mbox.write('\n\n')
    
    size = len(text)
    biggest = max(size, biggest)
    total += size
    
    del data
    gc.collect()
    spinner.spin()
  
  mbox.close()
  spinner.stop()
  print ": %s total, %s for largest message" % (prettyBytes(total), prettyBytes(biggest))

def scanFile(filename, compress, overwrite):
  """Gets IDs of messages in the specified mbox file"""
  # file will be overwritten
  if overwrite:
    return []
  else:
    assert('bzip2' != compress)

  # file doesn't exist
  if not os.path.exists(filename):
    print "File %s: not found" % (filename)
    return []

  spinner = Spinner("File %s" % (filename))

  # open the file
  if compress == 'gzip':
    mbox = gzip.GzipFile(filename,'rb')
  elif compress == 'bzip2':
    mbox = bz2.BZ2File(filename,'rb')
  else:
    mbox = file(filename,'rb')

  messages = {}

  # each message
  i = 0
  for message in mailbox.PortableUnixMailbox(mbox):
    header = ''
    # We assume all messages on disk have message-ids
    try:
      header =  ''.join(message.getfirstmatchingheader('message-id'))
    except KeyError:
      # No message ID was found. Warn the user and move on
      print
      print "WARNING: Message #%d in %s has no Message-Id header." % (i, filename)
    
    header = blanksRE.sub(' ', header.strip())
    try:
      ID = msgmatchRE.match(header).group(1)
      if ID not in messages.keys():
        # avoid adding dupes
        messages[ID] = ID
    except AttributeError:
      # Message-Id was found but could somehow not be parsed by regexp (highly bloody unlikely)
      print
      print "WARNING: Message #%d in %s has a malformed Message-Id header." % (i, filename)
    spinner.spin()
    i = i + 1

  # done
  mbox.close()
  spinner.stop()
  print ": %d messages" % (len(messages.keys()))
  return messages

def scanFolder(server, foldername):
  """Gets IDs of messages in the specified folder, returns id:num dict"""
  messages = {}
  spinner = Spinner("Folder %s" % (foldername))
  try:
    typ, data = server.select(foldername, readonly=True)
    if 'OK' != typ:
      raise SkipFolderException("SELECT failed: %s" % (data))
    num_msgs = int(data[0])
    
    # each message
    for num in range(1, num_msgs+1):
      # Retrieve Message-Id
      typ, data = server.fetch(num, '(BODY[HEADER.FIELDS (MESSAGE-ID)])')
      if 'OK' != typ:
        raise SkipFolderException("FETCH %s failed: %s" % (num, data))
      
      header = data[0][1].strip()
      # remove newlines inside Message-Id (a dumb Exchange trait)
      header = blanksRE.sub(' ', header)
      try:
        ID = msgmatchRE.match(header).group(1) 
        if ID not in messages.keys():
          # avoid adding dupes
          messages[ID] = num
      except:
        # Some messages may have no Message-Id, so we'll synthesise one
        # (this usually happens with Sent, Drafts and .Mac news)
        typ, data = server.fetch(num, '(BODY[HEADER.FIELDS (FROM TO CC DATE SUBJECT)])')
        if 'OK' != typ:
          raise SkipFolderException("FETCH %s failed: %s" % (num, data))
        header = data[0][1].strip()
        header = header.replace('\r\n','\t')
        messages['<' + UUID + '.' + sha.sha(header).hexdigest() + '>'] = num
        pass
      spinner.spin()
  finally:
    spinner.stop()
    print ":",
  
  # done
  print "%d messages" % (len(messages.keys()))
  return messages

def parseParenList(row):
  """Parses the nested list of attributes at the start of a LIST response"""
  # eat starting paren
  assert(row[0] == '(')
  row = row[1:]

  result = []

  # NOTE: RFC3501 doesn't fully define the format of name attributes :(
  nameAttribRE = re.compile("^\s*(\\\\[a-zA-Z0-9_]+)\s*")

  # eat name attributes until ending paren
  while row[0] != ')':
    # recurse
    if row[0] == '(':
      paren_list, row = parseParenList(row)
      result.append(paren_list)
    # consume name attribute
    else:
      m = nameAttribRE.search(row)
      assert(m != None)
      nameAttrib = row[m.start():m.end()]
      row = row[m.end():]
      #print "MATCHED '%s' '%s'" % (nameAttrib, row)
      nameAttrib = nameAttrib.strip()
      result.append(nameAttrib)

  # eat ending paren
  assert(')' == row[0])
  row = row[1:]
  
  # done!
  return result, row

def parseStringList(row):
  """Parses the quoted and unquoted strings at the end of a LIST response"""
  result = []
  stringRE = re.compile('\s*(?:"([^"]+)")\s*|\s*(\S+)\s*')
  slist = stringRE.split(row)
  slist = [s for s in slist if s]
  return slist

def parseLIST(row):
  """Prases response of LIST command into a list"""
  row = row.strip()
  paren_list, row = parseParenList(row)
  string_list = parseStringList(row)
  assert(len(string_list) == 2)
  return [paren_list] + string_list

def getHierarchyDelimiter(server):
  """Queries the imapd for the hierarchy delimiter, eg. '.' in INBOX.Sent"""
  # see RFC 3501 page 39 paragraph 4
  typ, data = server.list('', '')
  assert(typ == 'OK')
  assert(len(data) == 1)
  lst = parseLIST(data[0]) # [attribs, hierarchy delimiter, root name]
  hierarchy_delim = lst[1]
  # NIL if there is no hierarchy
  if 'NIL' == hierarchy_delim:
    hierarchy_delim = '.'
  return hierarchy_delim

def getNames(server, compress):
  """Get list of folders, returns [(FolderName,FileName)]"""

  spinner = Spinner("Folders")
  
  # Get hierarchy delimiter
  delim = getHierarchyDelimiter(server)
  spinner.spin()
  
  # Get LIST of all folders
  typ, data = server.list()
  assert(typ == 'OK')
  spinner.spin()
  
  names = []

  # parse each LIST, find folder name
  for row in data:
    lst = parseLIST(row)
    foldername = lst[2]
    suffix = {'none':'', 'gzip':'.gz', 'bzip2':'.bz2'}[compress]
    filename = '.'.join(foldername.split(delim)) + '.mbox' + suffix
    names.append((foldername, filename))

  # done
  spinner.stop()
  print ": %s folders" % (len(names))
  return names

def printUsage():
    """Prints usage, exits"""
    #     "                                                                               "
    print "Usage: imapbackup [OPTIONS] -s HOST -u USERNAME [-p PASSWORD]"
    print " -a --append-to-mboxes     Append new messages to mbox files. (default)"
    print " -y --yes-overwrite-mboxes Overwite existing mbox files instead of appending."
    print " -n --compress=none        Use one plain mbox file for each folder. (default)"
    print " -z --compress=gzip        Use mbox.gz files.  Appending may be very slow."
    print " -b --compress=bzip2       Use mbox.bz2 files. Appending not supported: use -y."
    print " -s HOST --server=HOST     Name or IP address of the IMAP4rev1 server"
    print " -u USER --user=USER       Username to log into server"
    print " -p PASS --pass=PASS       Prompts for password if not specified."
    print "\nNOTE: mbox files are created in the current working directory."
    sys.exit(2)

def getConfig():
  """Reads command line, returns config dict"""
  # config = {
  #   'compress': 'none' or 'gzip' or 'bzip2'
  #   'overwrite': True or False
  #   'server': String
  #   'user': String
  #   'pass': String
  # }
  
  # read command line
  try:
    opts, args = getopt.getopt(sys.argv[1:], "aynzbs:u:p:", ["append-to-mboxes","yes-overwrite-mboxes","compress=","server=","user=","pass="])
    if not len(opts):
      print "Empty opts"
      printUsage()
  except getopt.GetoptError:
    print "GetoptError"
    printUsage()

  errors = []
  warnings = []
  
  # config dict
  #   Must appear on command line: server, user
  #   May appear: compress, overwrite, pass (prompted)
  config = {'compress':'none', 'overwrite':False}
  
  # get options from command line, save in config
  for option, value in opts:
    if option in ("-a", "--append-to-mboxes"):
      config['overwrite'] = False
    elif option in ("-y", "--yes-overwrite-mboxes"):
      warnings.append("Existing mbox files will be overwritten!")
      config["overwrite"] = True
    elif option == "-n":
      config['compress'] = 'none';
    elif option == "-z":
      config['compress'] = 'gzip';
    elif option == "-b":
      config['compress'] = 'bzip2';
    elif option == "--compress":
      if value in ('none','gzip','bzip2'):
        config['compress'] = value
      else:
        errors.append("Invalid compression type specified.")
    elif option in ("-s", "--server"):
      config['server'] = value
    elif option in ("-u", "--user"):
      config['user'] = value
    elif option in ("-p", "--pass"):
      config['pass'] = value
    else:
      errors.append("Unknown option: " + option)

  # check config
  if config['compress'] == 'bzip2' and config['overwrite'] == False:
    errors.append("Cannot append new messages to mbox.bz2 files.  Please specify -y.")
  if config['compress']== 'gzip' and config['overwrite'] == False:
    warnings.append("Appending new messages to mbox.gz files is very slow.  Please Consider\n"
                    "  using -y and compressing the files yourself with gzip -9 *.mbox")
  if('server' not in config):
    errors.append("No server specified.")
  if('user' not in config):
    errors.append("No username specified.")

  # show warnings
  for warning in warnings:
    print "WARNING:", warning
  
  # show errors, exit
  for error in errors:
    print "ERROR", error
  if len(errors):
    sys.exit(2)

  # prompt for password, if necessary
  if('pass' not in config):
    config['pass'] = getpass.getpass()
  
  # done!
  return config

def connectAndLogin(config):
  try:
    print "Connecting to IMAP server '%s' TCP port 143" % (config['server'])
    server = imaplib.IMAP4(config['server'])
    print "Logging in as '%s'" % (config['user'])
    server.login(config['user'], config['pass'])
  except socket.gaierror, e:
    (err, desc) = e
    print "ERROR: problem looking up server '%s' (%s %s)" % (config['server'], err, desc)
    sys.exit(3)
  except socket.error, e:
    (err, desc) = e
    print "ERROR: could not connect to '%s' (%s %s)" % (config['server'], err, desc)
    sys.exit(4)

  return server

def main():
  """Main entry point"""
  try:
    config = getConfig()
    server = connectAndLogin(config)
    names = getNames(server, config['compress'])
    names.reverse()
    #for n in range(len(names)):
    #  print n, names[n]
    
    for name_pair in names:
      try:
        foldername, filename = name_pair
        fol_messages = scanFolder(server, foldername)
        fil_messages = scanFile(filename, config['compress'], config['overwrite'])
        
        new_messages = {}
        for ID in fol_messages:
          if ID not in fil_messages:
            new_messages[ID] = fol_messages[ID]
        
        #for f in new_messages:
        #  print "%s : %s" % (f, new_messages[f])

        downloadMessages(server, foldername, filename, new_messages, config['compress'], config['overwrite'])
      
      except SkipFolderException, e:
        print e
    
    print "Disconnecting"
    server.logout()
  except socket.error, e:
    (err, desc) = e
    print "ERROR: %s %s" % (err, desc)
    sys.exit(4)
  except imaplib.IMAP4.error, e:
    print "ERROR:", e
    sys.exit(5)

def clean_exit():
  sys.stdout.write("\n")
  sys.stdout.flush()

def cli_exception(type, value, tb):
  if not issubclass(type, KeyboardInterrupt):
    sys.__excepthook__(type, value, tb)
  else:
    clean_exit()

# Make sure we get a chance to clean up the display on a tty
if sys.stdin.isatty():
  sys.excepthook=cli_exception

# Hideous fix to counteract http://python.org/sf/1092502
# (which should have been fixed ages ago.)
# Also see http://python.org/sf/1441530
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
if 'Darwin' in platform.platform() and '2.3.5' == platform.python_version():
  socket._fileobject.read = _fixed_socket_read
if 'Windows' in platform.platform():
  socket._fileobject.read = _fixed_socket_read

if __name__ == '__main__':
  gc.enable()
  main()
