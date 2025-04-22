# Code analyzer for SOL25 (`parse.py`)

A filter-type script (parse.py in Python 3.11) reads source code in SOL25 
(see section 3 in ipp25spec) from standard input, checks the lexical, syntactic, and static 
semantic correctness of the code, and outputs the XML representation of the program's 
abstract syntax tree to standard output according to the specification in section 4.1.

## 📁 Project Structure
- `parse.py` – main script that parses SOL25 source code and outputs an XML AST
- `ipp25spec.pdf` – official specification of the IPP project (includes SOL25 language rules)
- `readme1.pdf` – report/documentation describing the implementation and design
- `requirements.txt` – dependencies for  development
- `sol_1.sol25` – example SOL25 source file used for demonstration or testing
- `README.md` – this file with usage instructions and project info


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
You can find the tests here: [Marek324/butfit-ipp1-tests](https://github.com/Marek324/butfit-ipp1-tests/tree/master)
