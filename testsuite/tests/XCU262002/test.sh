echo `echo "abc"`
echo `echo 'a b c'`
echo $(echo "avc")
echo $(echo 'a b c')

echo `echo $(echo abc)`

toto="FAILED"
echo `toto=PASSED; echo $toto`
echo `toto='PASSED and OK'; echo "$toto"`
echo "`toto=PASSED; echo $toto`" 

toto="FAILED"
echo $(toto=PASSED; echo $toto)
echo "$toto"
echo $(toto='PASSED and OK'; echo "$toto")
echo "$toto"
echo "$(toto=PASSED; echo $toto)"
echo "$toto"
echo $( echo $(echo youyou $toto))
echo $(echo "hello" # a comment in command sub
)
