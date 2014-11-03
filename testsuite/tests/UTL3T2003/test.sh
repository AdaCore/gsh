echo "==== Test basic value ===="
tail -4 test.data; echo "status: $?"

echo "==== Test tail -0 ===="
tail -0 test.data; echo "status: $?"

echo "==== Test tail -4t (should return error) ===="
tail -4t test.data; echo "status:$?"

echo "==== Test tail -4 -5 (should return error) ===="
tail -4 -5 test.data; echo "status:$?"


