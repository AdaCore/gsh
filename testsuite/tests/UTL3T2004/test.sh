echo "==== Get last line of file cr/lf line ending ===="
tail -2 test.data; echo "status: $?"

echo "==== Get last line of file cr/lf line ending (last line without line ending) ===="
tail -2 test2.data; echo "status: $?"

