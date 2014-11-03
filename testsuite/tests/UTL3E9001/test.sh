echo "test export var=value var2=value2"
export VAR=var_value VAR2
VAR2=var2_value
python -c "import os; print os.environ['VAR']"
python -c "import os; print os.environ['VAR2']"


