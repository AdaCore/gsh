# The test performs two tests:
# 1- on null commands redirections operators should be executed
# 2- these redirections should not be done in the current environment
>toto.txt
if [ ! -f toto.txt ]; then
   echo "FAILED (cannot find totot.txt)"
else
   echo "PASSED"
fi

