(eval exit 1
 exit 0)
if [ $? -ne 1 ]; then
   echo "FAILED"
else
   echo "PASSED"
fi
