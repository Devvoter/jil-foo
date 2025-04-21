# Code analyzer for SOL25 (`parse.py`)

A filter-type script (parse.py in Python 3.11) reads source code in SOL25 
(see section 3 in ipp25spec) from standard input, checks the lexical, syntactic, and static 
semantic correctness of the code, and outputs the XML representation of the program's 
abstract syntax tree to standard output according to the specification in section 4.1.

## Setup:
```
..clone repo

python -m venv .venv # create virtual environment
source .venv/bin/activate
pip install -r tests/requirements.txt # install required packages
```

## Usage
```
-> (myvenv) ~$ cat sol_1.sol25
class Main : Object {
    run [|
        x := 42.
    ]
}
-> (myvenv) ~$ python3.11 parse.py < sol_1.sol25 
<?xml version="1.0" encoding="UTF-8"?>
<program language="SOL25">
    <class name="Main" parent="Object">
        <method selector="run">
            <block arity="0">
                <assign order="1">
                    <var name="x"/>
                    <expr>
                        <literal class="Integer" value="42"/>
                    </expr>
                </assign>
            </block>
        </method>
    </class>
</program>
-> (myvenv) ~$ 
```

## Tests
The tests used for this project were kindly provided by a colleague — many thanks! :Koteseni: \
You can find the tests here: [Kubikuli/IPP_proj2-tests](https://github.com/Kubikuli/IPP_proj2-tests)
