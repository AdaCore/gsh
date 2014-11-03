# Test of if clause

if true; then
  echo "PASSED"
fi
echo $?

if false; then
  echo "FAILED"
fi
echo $?

if false; then
  echo "FAILED"
else
  echo "PASSED"
fi

if true; then
  echo "PASSED"
else
  echo "FAILED"
fi

if false; then
  echo "FAILED"
elif true; then
  echo PASSED
else
  echo FALSE
fi

if true; then echo PASSED; else echo FAILED; fi
