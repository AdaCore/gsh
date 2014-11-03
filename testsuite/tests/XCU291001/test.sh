TOTO="FAILED"
TATA="FAILED"
export TOTO

d=`dirname $0`
TOTO="PASSED" TATA="PASSED" $d/sub.sh
