Todo List
#########

#. Implement cp as a builtin.

   **issue**:
   On Windows the gnuwin32 version of cp is not
   reliable and thus force us to use **Cygwin**

   **specifications**:

   refer to: `cp spec <http://pubs.opengroup.org/onlinepubs/9699919799/utilities/cp.html>`_

   * '-i' is not required to be implemented (no interactive mode)
   * '-r' must be supported and have the same behaviour than '-R'
          should be in a separate file in 'builtin'

   Should be implemented in priority following options:
   * -r and -R,
   * -p ,
   * -f

   and contrary to the standard, environment variables (LANG, LC_ALL, LC_COLLATE,
   LC_CTYPE,  LC_MESSAGES, NLSPATH) do not affect the behaviour of `cp`.

   See Ticket: N724-010


#. Add implementation of arithmetic expansion

   **specifications**:

   refer to: `arithmetic expansion spec <http://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html#tag_02_06_04>`_
