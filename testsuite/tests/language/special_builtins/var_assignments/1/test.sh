VAR=FAILED
VAR=PASSED1 . ./subtest.sh
echo "[builtin: .     ] expect VAR=PASSED1; got VAR=$VAR"
VAR=PASSED2 . ./subtest2.sh
echo "[builtin: source] expect VAR=PASSED5; got VAR=$VAR"
VAR=PASSED3 eval 'echo $VAR=PASSED3'
echo "[builtin: eval  ] expect VAR=PASSED3; got VAR=$VAR"
VAR=PASSED4 echo 'echo'
echo "[builtin: echo  ] expect VAR=PASSED3; got VAR=$VAR"

