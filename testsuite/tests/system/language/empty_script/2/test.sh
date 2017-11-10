$SHELL ./test_sub.sh
if [ $? -ne 0 ]; then
   echo "FAILED (script with only comments should return 0)"
else
   echo "PASSED"
fi
