# Variables

## Model

*GSH* use the same model as declared in *POSIX Shell*. So you can refer to
that documentation for operation like setting, unsetting and exporting
variables.

## Windows Specificities

The main difference when handling variables is that on Windows OS
variables are not case sensitive whereas in *POSIX Shell* standard they are.

### Variable existence

When doing:

    echo $A_Variable

GSH will do the following:

1- Check if `A_Variable` exists in GSH internal state with specified casing
2- If `A_Variable` does not exist check the existence of `A_VARIABLE` and 
   if value if coming from Windows environment.
3- If both previous existence checks fail then consider the variable
   as non existant.

Thus the following command launched for example from a Cygwin shell:

    A_VARIABLE='upper' gsh.exe -c 'echo $A_Variable'

Will output:

    upper

Inside the shell a similar command will have a different effect:

    gsh.exe -c 'A_VARIABLE="upper"; echo $A_Variable'

Will output just a newline.

To summarize inside *GSH* variables behave as expected in a *POSIX* Shell but
variables coming from the process environment are seen by all variables that
differ just by casing.

### Import/Export of Environment

During the initialisation of the *GSH* state, the process environment is
imported the following way:

1- Import all environments variables into the shell state
2- For environment variables, if the upper case version of that variable is
   not in the environment create an entry for the upper case version of
   that variable.

During export of environment to external process the following actions are
done:

1- Add at the beginning of the environment all upper case variables
2- Append to that environment all other variables from the shell state

These methods for import/export of environment ensure that:

1- *GSH* processes can pass between each other the full shell environment
   (case sensitive)
2- Interaction with other processes is intuitive


For example if from a *CMD* Windows shell the following is done:

     $ set My_Path=c:/tmp
     $ gsh -c "echo $MY_PATH"


Then the following will be displayed:

     c:/tmp

If from a *GSH* interactive shell the following commands are executed:

     $ export DUMMY=value1
     $ export Dummy=value2
     $ gsh -c 'echo $DUMMY; echo $Dummy'

The the following will be displayed:

     value1
     value2

Note that *Cygwin* use a similar mechanism, thus when spawning *GSH* from a
*Cygwin* process should preserve all the variables even if only casing differ.


### Special Variables

The `PATH` and `HOME` variables are automatically transform back and forth to a
UNIX friendly representation (with `/`).
