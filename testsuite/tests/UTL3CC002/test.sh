type ls
command -v ls
command -V ls
which ls

type cp
command -v cp
command -V cp
which cp


echo "* test 'command' on cp a copy_a"
command cp a copy_a

echo "* ls to see the creation of copy_a"
command ls
echo "* content of copy_a"
command cat copy_a

echo "* several builtins and elem"
command -v cp echo printf
which cp echo printf
type cp echo printf
