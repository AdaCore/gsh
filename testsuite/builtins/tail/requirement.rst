    
Here is the copy of the standard:

The Standard
============

NAME
----

.. program:: tail
    
tail - copy the last part of a file
    
SYNOPSIS
--------

.. parsed-literal:: 
        tail [-f][ -c number| -n number][file]
    
DESCRIPTION
-----------
    
The tail utility shall copy its input file to the standard output beginning at a designated place.
    
Copying shall begin at the point in the file indicated by the -c number or -n number options. The option-argument number shall be counted in units of lines or bytes, according to the options -n and -c. Both line and byte counts start from 1.
    
Tails relative to the end of the file may be saved in an internal buffer, and thus may be limited in length. Such a buffer, if any, shall be no smaller than {LINE_MAX}*10 bytes.
    
OPTIONS
-------
    
The tail utility shall conform to the Base Definitions volume of IEEE Std 1003.1-2001, Section 12.2, Utility Syntax Guidelines.
    
The following options shall be supported:
    
.. option:: -c  number

   The application shall ensure that the number option-argument is a decimal
   integer whose sign affects the location in the file, measured in bytes, to
   begin the copying:

   .. csv-table::
      :header: "Sign", "Copying Start"      
    
      "\+", "Relative to the beginning of the file."
      "\-", "Relative to the end of the file."
      "none", "Relative to the end of the file."
    
   The origin for counting shall be 1; that is, -c +1 represents the first byte of the file, -c -1 the last.
        
.. option:: -f

   If the input file is a regular file or if the file operand specifies a FIFO, do not terminate after the last line of the input file has been copied, but read and copy further bytes from the input file when they become available. If no file operand is specified and standard input is a pipe, the -f option shall be ignored. If the input file is not a FIFO, pipe, or regular file, it is unspecified whether or not the -f option shall be ignored.

.. option:: -n  number
            
   This option shall be equivalent to -c number, except the starting location in the file shall be measured in lines instead of bytes. The origin for counting shall be 1; that is, -n +1 represents the first line of the file, -n -1 the last.
    
If neither -c nor -n is specified, -n 10 shall be assumed.
    
OPERANDS
--------
    
        The following operand shall be supported:
    
        file
            A pathname of an input file. If no file operands are specified, the standard input shall be used.
    
STDIN
-----
    
        The standard input shall be used only if no file operands are specified. See the INPUT FILES section.
    
INPUT FILES
-----------
    
        If the -c option is specified, the input file can contain arbitrary data; otherwise, the input file shall be a text file.
    
ENVIRONMENT VARIABLES
---------------------
    
        The following environment variables shall affect the execution of tail:
    
        LANG
            Provide a default value for the internationalization variables that are unset or null. (See the Base Definitions volume of IEEE Std 1003.1-2001, Section 8.2, Internationalization Variables for the precedence of internationalization variables used to determine the values of locale categories.)
        LC_ALL
            If set to a non-empty string value, override the values of all the other internationalization variables.
        LC_CTYPE
            Determine the locale for the interpretation of sequences of bytes of text data as characters (for example, single-byte as opposed to multi-byte characters in arguments and input files).
        LC_MESSAGES
            Determine the locale that should be used to affect the format and contents of diagnostic messages written to standard error.
        NLSPATH
            [XSI] [Option Start] Determine the location of message catalogs for the processing of LC_MESSAGES . [Option End]
    
ASYNCHRONOUS EVENTS
-------------------
    
Default.
    
STDOUT
------
    
The designated portion of the input file shall be written to standard output.
    
STDERR
------
    
The standard error shall be used only for diagnostic messages.
    
OUTPUT FILES
------------
    
None.
    
EXTENDED DESCRIPTION
--------------------
    
None.
    
EXIT STATUS
-----------
   
The following exit values shall be returned:

.. csv-table::
    
   "0", "Successful completion."
   ">0", "An error occurred."
    
CONSEQUENCES OF ERRORS
----------------------
    
        Default.
    
