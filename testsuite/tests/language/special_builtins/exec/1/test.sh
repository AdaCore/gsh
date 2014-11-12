
(
 exec > toto-new.txt
 echo "toto"
 exec 1>&2
 mv toto-new.txt toto.txt
) > tata.txt 2>&1
cat tata.txt

(
 (exec > tata-new.txt
  echo "toto"
 )
 echo "hello"
) > tutu.txt 2>&1
cat tutu.txt

