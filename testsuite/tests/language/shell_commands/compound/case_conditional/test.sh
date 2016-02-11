dummy="hello"

# Check handling of quoted '*', '?', '[', ']'
case $dummy in
  *'*'*) echo FAILED;;
      *) echo PASSED;;
esac

toto='?'
case $dummy in
   *"$toto"*) echo FAILED;;
     *$toto*) echo PASSED;;
esac

# set exit status to 1 and ensure that if no case is matched then exit
# status is 0 and not 1
false
case $dummy in
   nomatch*) echo FAILED;;
   *nomatch) echo FAILED;;
 no[mat]*ch) echo FAILED;;
esac
echo $?

case $dummy in
   [!h]*) echo FAILED;;
       *) echo PASSED;;
esac

true
case $dummy in
  hello) echo "PASSED";;
      *) echo "FAILED";;
esac
echo $?

true
case $dummy in
  hella)         echo "FAILED";;
  hello | tutu ) echo "PASSED"; false;;
esac
echo $?

false
case $dummy in
  h[el]*o) ;;
esac
echo $?

echo "before case"
case $dummy in
  hello)  echo "hello from case"
          echo "hello1 from case" > match.txt
          echo "hello3 from case"
esac > case.txt
echo "outside case"
cat case.txt
cat match.txt

x='\x'

case x in
\x)	echo ok 1;;
*)	echo bad 1;;
esac

case x in
$x)	echo ok 2;;
*)	echo bad 2;;
esac

case $x in
\x)	echo bad 3;;
\\x)	echo ok 3 ;;
*)	echo bad 3.1 ;;
esac

case $x in
\\$x)	echo ok 4 ;;
x)	echo bad 4;;
$x)	echo bad 4.1 ;;
*)	echo bad 4.2;;
esac

case x in
\\x)	echo bad 5;;
\x)	echo ok 5;;
*)	echo bad 5.1;;
esac

case x in
\\x)	echo bad 6;;
x)	echo ok 6;;
*)	echo bad 6.1;;
esac

case x in
$x)	echo ok 7 ;;
\\$x)	echo bad 7 ;;
*)	echo bad 7.1 ;;
esac

case x in
\x)	echo ok 8 ;;
\\x)	echo bad 8 ;;
*)	echo bad 8.1 ;;
esac

case \x in
\x)	echo ok 9 ;;
\\x)	echo bad 9 ;;
*)	echo bad 9.1 ;;
esac

case $x in
$x)	echo "bad 10 backaslash in a pattern has special meaning" ;;
*)	   echo "ok 10" ;;
esac

case \x in
\x)	echo "ok 11" ;;
*)	   echo "bad 11";;
esac
