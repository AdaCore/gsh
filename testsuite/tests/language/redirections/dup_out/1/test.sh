echo "======================================="
(
echo "+ check basic descriptor duplication"
echo ".. create fd 2 that point to error.out"
exec 2>error.out
echo "..   > status=$? (expect 0)"
echo ".. echo PASSED by duplicating fd 2 into 1"
echo "PASSED" >&2
echo ".. check that error.out contains PASSED"
content=`cat error.out 2>/dev/null`
if [ "$content" = "PASSED" ]; then
   echo "..   > PASSED"
else
   echo "..   > FAILED (wrong content)"
   echo "== error.out =="
   cat error.out
   echo "==============="
fi
)

echo "======================================="
(
echo "+ check fd duplication 5>&word were word is a dynamic value"
echo ".. create fd 5 that point to file5.txt"
exec 5>file5.txt
echo "..   > status=$? (expect 0)"
echo '.. echo PASSED to fd "`echo 5`" (that should evaluate to 5)'
echo "PASSED" 1>&`echo 5`
echo "..   > status=$? (expect 0)"
echo ".. check content of file5"
content=`cat file5.txt 2>/dev/null`
if [ "$content" = "PASSED" ]; then
   echo "..   > PASSED"
else
   echo "..   > FAILED (wrong content)"
   echo "== file5.txt =="
   cat file5.txt
   echo "==============="
fi
)

echo "======================================="
(
echo "+ check error handling on duplication of invalid fd"
echo FAILED > file3.txt
echo PASSED > file4.txt
echo ".. do an exec with invalid duplication"
exec 2>/dev/null
exec 3>file3.txt 1>&6 4>file4.txt
echo "..   > status=$? (expect 1)"
echo ".. check that first redirection was executed (3>file3.txt)"
content3=`cat file3.txt 2>/dev/null`
content4=`cat file4.txt 2>/dev/null`
if [ ! -f file3.txt -o "$content3" = "FAILED" ]; then
   echo "..   > FAILED (content of file3.txt should have been reset)"
   echo "== file3.txt =="
   cat file3.txt 2>/dev/null
   echo "==============="
else
   echo "..   > PASSED"
fi 
echo ".. check that third redirection was not executed (4>file4.txt)"
if [ "$content4" = "PASSED" ]; then
   echo "..   > PASSED"
else
   echo "..   > FAILED (file4.txt should not have changed)"
   echo "== file4.txt =="
   cat file4.txt >/dev/null
   echo "==============="
fi
echo "..  check that none of the redirection are still in effect"
(echo "FAILED (fd:3)" >&3 ) 2>/dev/null
echo "..   > status=$? (expected 1 because of invalid fd)"
(echo "FAILED (fd:4)" >&4 ) 2>/dev/null
echo "..   > status=$? (expected 1 because of invalid fd)"
)

echo "======================================="
(
echo "+ check that shell current command is not run in case of redirection error"
echo ".. launch subshell with bad fd duplication redirection"
(( touch should_not_be_here.txt; echo "test" ) 2>&6) 2>/dev/null
echo "..   > status=$? (expected 1 because of invalid fd)"
echo ".. check that file should_not_be_here.txt does not exist"
if [ -f should_not_be_here.txt ]; then
   echo "..   > FAILED (file exists)"
else
   echo "..   > PASSED (command was interrupted)"
fi
)

echo "======================================="
