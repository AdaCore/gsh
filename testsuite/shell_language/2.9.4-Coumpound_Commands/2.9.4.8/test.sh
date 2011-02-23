for a in "a a" b c d 'e e'; do
   echo "$a"
   false
done
echo $?

for a in "a a" b c d 'e e'; do
   echo "$a"
done > redirection.out
echo $?
cat redirection.out

