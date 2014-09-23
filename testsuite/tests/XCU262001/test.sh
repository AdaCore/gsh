echo "Operators - = ? and +"
echo "===================="

echo ""
echo "expansion with -"
param="abcd"
empty_str=""
echo '${param-myword}:     ' ${param-myword} '     / expected abcd'
echo '${empty_str-myword}: ' ${empty_str-myword} '          / expected null'
unset unset_var
echo '${unset_var-myword}: ' ${unset_var-myword} '   / expected myword'

echo ""
echo "expansion with ="
param="abcd"
empty_str=""
echo '${param=myword}:     ' ${param=myword} '      / expected abcd'
echo '${empty_str=myword}: ' ${empty_str=myword} '           / expected null'
unset unset_var
echo '${unset_var=myword}: ' ${unset_var=myword} '    / expected unset_var is assigned myword:' $unset_var

echo ""
echo "expansion with ?"
param="abcd"
empty_str=""
echo '${param?myword}:      ' ${param?myword} '     / expected abcd'
echo '${empty_str?myword}:  ' ${empty_str?myword} '          / expected null'
unset unset_var
(
echo '${unset_var?myword}:  ' ${unset_var?myword} '     / expected error'
)

echo ""
echo "expansion with +"
param="abcd"
empty_str=""
echo '${param+myword}:      ' ${param+myword} '   / expected myword'
echo '${empty_str+myword}:  ' ${empty_str+myword} '   / expected myword'
unset unset_var
echo '${unset_var+myword}:  ' ${unset_var+myword} '          / expected null'

echo ""
echo "Operators - = ? and + introduced by COLON ':'"
echo "=============================================="
echo ""
echo "expansion with :-"
param="abcd"
empty_str=""
echo '${param:-myword}:      ' ${param:-myword} '     / expected abcd'
echo '${empty_str:-myword}:  ' ${empty_str:-myword} '   / expected myword'
unset unset_var
echo '${unset_var:-myword}:  ' ${unset_var:-myword} '   / expected myword'

echo ""
echo "expansion with :="
param="abcd"
empty_str=""
echo '${param:=myword}:      ' ${param:=myword} '     / expected abcd'
echo '${empty_str:=myword}:  ' ${empty_str:=myword} '   / expected empty_str is assigned myword:' $empty_str
unset unset_var
echo '${unset_var:=myword}:  ' ${unset_var:=myword} '   / expected unset_var is assigned myword:' $unset_var

echo ""
echo "expansion with :?"
param="abcd"
empty_str=""
echo '${param:?myword}:     ' ${param:?myword} '     / expected abcd'
(
echo '${empty_str:?myword}: ' ${empty_str:?myword} / expected error
)
(
unset unset_var
echo '${unset_var?myword}:  ' ${unset_var:?myword} / expected error
)

echo ""
echo "expansion with :+"
param="abcd"
empty_str=""
echo '${param:+myword}:     ' ${param:+myword}  '   / expected myword'
echo '${empty_str:+myword}: ' ${empty_str:+myword} '          / expected null'
unset unset_var
echo '${unset_var:+myword}: ' ${unset_var:+myword} '          / expected null'


echo ""
echo "Length (subst starting with by #)"
echo "================================="
param="abcd"
empty_str=""
unset unset_var
echo '${#param}:     ' ${#param} / expected 4
echo '${#empty_str}: ' ${#empty_str} / expected 0
echo '${#unset_var}: '  ${#unset_var} / expected 0


echo ""
echo "Substrings processing"
echo "====================="

echo ""
echo "expansion with %"
x=file.c
empty_str=""
echo currently x is $x
echo '${x%.c}.o:         ' ${x%.c}.o '    / expected file.o'
echo '${empty_str%.c}:   ' ${empty_str%.c} '           / expected null'
unset unset_var
echo '${unset_var%abc}:  ' ${unset_var%abc} '           / expected null'

echo ""
echo "expansion with %%"
x=posix/src/std
empty_str=""
echo currently x is $x
echo '${x%%/*}:                     ' ${x%%/*} '           / expected posix'
x=posix/src/std
echo currently x is $x
echo '${x%%*/}:                     ' ${x%%*/} '   / expected posix/src/std'
echo '${empty_str%%/*}:             ' ${empty_str%%/*} '                 / expected null'
unset unset_var
echo '${unset_var%%abc}:            ' ${unset_var%%abc} '                 / expected null'

echo ""
echo "expansion with #"
x=$HOME/src/cmd
empty_str=""
echo currently x is '$HOME/src/cmd'
echo '${x#$HOME}:                  ' ${x#$HOME} '   / expected /src/cmd'
x=posix/src/std
echo currently x is $x
echo '${x#*/}:                     ' ${x#*/} '   / expected src/std'
echo '${empty_str#abc}:            ' ${empty_str#abc} '           / expected null'
unset unset_var
echo '${unset_var#abc}:            ' ${unset_var#abc} '           / expected null'

echo ""
echo "expansion with ##"
x=/one/two/three
empty_str=""
echo currently x is $x
echo '${x##*/}:          ' ${x##*/} '   / expected three'
echo '${empty_str##abc}: ' ${empty_str##abc} '         / expected null'
unset unset_var
echo '${unset_var##abc}: ' ${unset_var##abc} '         / expected null'

