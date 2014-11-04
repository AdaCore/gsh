echo "==== Test basic value ===="
tail -n 4 test.data; echo "status: $?"

echo "==== Test tail -0 ===="
tail -n 0 test.data; echo "status: $?"

echo "==== Test tail -4t (should return error) ===="
tail -n 4t test.data; echo "status:$?"

echo "==== Test tail -4 -n 5 (should not return error) ===="
tail -4 -n 5 test.data; echo "status:$?"

echo "==== Test tail -n 4 -5 (should return error) ===="
tail -n 4 -5 test.data; echo "status:$?"

echo "==== Test tail -n 4 -n 3 (should output 3 lines) ===="
tail -n 4 -n 3 test.data; echo "status:$?"

echo "==== Tail tail -n +10 ===="
tail -n +10 test.data; echo "status:$?"

