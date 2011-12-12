dummy="hello"

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
