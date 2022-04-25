CS 340 Project 1
Michael Leonhard, 2005/09/08
Java 1.5.0.4 on Windows XP

KeyGen.java

This is program 1.  It is a key generator program.  It writes keys in XML format.
The primary data structures in the class consist of several pieces of private data.

Filenames of the public and private keys are stored here:
 private String M_pubkeyFilename
 private String M_privkeyFilename

Integer values used in the key creation process are also stored here.  The P and Q
values are obtained from the command line.  If not present on the command line,
the user is prompted to enter them.  The other values are obtained from (P,Q) through
various algorithms.
 private int M_p;
 private int M_q;
 private int M_n;
 private int M_phi;
 private int M_e;
 private int M_d;

RsaCipher.java

This program reads a key file and an input file and encrypts or decrypts the input
file, writing the result to an output file.  Like KeyGen.java, data is stored
in private class data fields.

File names:
 private String M_inFilename
 private String M_outFilename
 private String M_keyFilename
These values are read from the key file:
 private int M_n
 private int M_e
 private int M_d
The operation (encrypt or decrypt) is stored in this variable
 private int M_operation
 static int OPERATION_ENCRYPT = 0
 static int OPERATION_DECRYPT = 1


TException.java class is used as a generic description carrying exception.  It
is used by KeyGen.java and RsaCipher.java to report errors.