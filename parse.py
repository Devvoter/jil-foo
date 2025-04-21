############################## START OF FILE ##############################
###########################################################################
#       PROJECT: IPP, Code analyzer in SOL25
#       FILE: parse.py
#       AUTOR: Denys Pylypenko - xpylypd00
#
#       INFO: This script parses the SOL25 source code and produces
#               an XML representation of its abstract syntax tree.
###########################################################################
################################# IMPORTS #################################
import sys
import codecs
from xml.dom import minidom
import xml.etree.ElementTree as ET
from typing import Optional, List, Union
from dataclasses import dataclass, field
from lark import Lark, Transformer, Token, UnexpectedCharacters, UnexpectedToken
###########################################################################
################################# GRAMMAR #################################
sol25_grammar = r"""

%ignore /[ \t\r\n]+/
%ignore COMMENT
COMMENT: /"([^"\\]|\\.)*"/s

//
CLASS: "class"
CNAME: /[A-Z][a-zA-Z0-9_]*/
NAME: /[a-z_][a-zA-Z0-9_]*/
TRUE: "true"
FALSE: "false"
NIL: "nil"
INT: /[-+]?\d+/
STRING: /'(?:[^'\\]|\\'|\\\\|\\n)*'/
NAME_COLON: /[a-z_][a-zA-Z0-9_]*:/
COLON_NAME: /:[a-z_][a-zA-Z0-9_]*/


?start: program
program: class_def*

class_def: CLASS CNAME ":" CNAME "{" method_def* "}"

method_def: selector block

selector: NAME
        | NAME_COLON (NAME_COLON)*

block: "[" block_params? "|" block_stats? "]"

block_params: COLON_NAME (COLON_NAME)*

block_stats: statement*

statement: NAME ":=" expr "."

expr: expr_base expr_tail?

expr_base: INT
         | STRING
         | TRUE
         | FALSE
         | NIL
         | NAME
         | CNAME
         | block
         | "(" expr ")"

expr_tail: NAME -> message_no_args
         | NAME_COLON expr (NAME_COLON expr)* -> message_with_args
"""
###########################################################################
################################ HELP TEXT ################################
help_text = """
Usage: parse.py [--help]

This script parses the SOL25 source code and produces 
an XML representation of its abstract syntax tree.
 
Parameters:
  --help        Display this help message and exit 
  (cannot be combined with any other parameter).
"""
###########################################################################
############################## LIST AND DICT ##############################
BUILT_IN_CLASSES = {
    "Object", "Integer", "String",
    "Block", "Nil", "True", "False"
}

RESERVED_WORDS = {
    "class", "self", "super",
    "nil", "true", "false"
}

BUILTIN_CLASS_METHODS = {
    "String":  ["new", "from:", "read"],
    "Object":  ["new", "from:"],
    "Nil":     ["new", "from:"],
    "Integer": ["new", "from:"],
    "Block":   ["new", "from:"],
    "True":    ["new", "from:"],
    "False":   ["new", "from:"],
}

BUILTIN_INSTANCE_METHODS = {
    "Object": [
        "identicalTo:", "equalTo:", "asString",
        "isNumber", "isString", "isBlock", "isNil",
    ],
    "Nil": [
        "asString",
    ],
    "Integer": [
        "plus:", "minus:", "divBy:", "multiplyBy:",
        "equalTo:", "asString", "asInteger", "timesRepeat:",
    ],
    "String": [
        "equalTo:", "asString", "concatenateWith:",
        "startsWith:endsBefore:", "print", "asInteger",
    ],
    "Block": [
        "whileTrue:",
    ],
    "True": [
        "not", "and", "or", "ifTrue:ifFalse:",
    ],
    "False": [
        "not", "and", "or", "ifTrue:ifFalse:",
    ],
}
###########################################################################
################################# CLASSES #################################
@dataclass
class ProgramNode:
    # Represents the entire SOL25 program, containing a list of user-defined classes.
    classes: List["ClassNode"] = field(default_factory=list)

@dataclass
class ClassNode:
    # Represents a class definition with its name, parent (superclass),
    # and a list of its methods.
    name: str
    parent: str
    methods: List["MethodNode"]

@dataclass
class MethodNode:
    # Represents a method in a class.
    # 'selector' is the method name (which may include colons),
    # and 'body' is the block (i.e., the method's code).
    selector: str
    body: "BlockNode"

@dataclass
class BlockNode:
    # Represents a block of code (e.g. the body of a method).
    # 'params' contains the names of formal parameters of the block,
    # and 'statements' is the list of statements within the block.
    params: List[str]               # parameter names
    statements: List["Statement"]   # list of statements

# Statement types – currently, only assignment is supported.
Statement = Union["AssignNode"]

@dataclass
class AssignNode:
    # Represents an assignment statement where a variable is assigned the value of an expression.
    var_name: str
    expr: "Expr"

# Expression types – various types of expressions that can appear in SOL25.
Expr = Union["LiteralNode", "VarNode", "ClassLiteralNode", "SendNode", BlockNode]

@dataclass
class LiteralNode:
    # Represents a literal value (e.g., a number, string, true, false, or nil).
    # 'lit_type' indicates the type and 'value' holds the actual textual representation.
    lit_type: str  # 'Integer', 'String', 'True', 'False', 'Nil'
    value: str     # textual part (e.g. '42', 'hello\n', ...)

@dataclass
class VarNode:
    # Represents a variable usage. The type of the variable is not determined statically.
    name: str

@dataclass
class ClassLiteralNode:
    # Represents a class literal (e.g., Integer, String).
    # This is used when referring to a class directly in the code.
    name: str

@dataclass
class SendNode:
    # Represents a method call (sending a message) in SOL25.
    # 'selector' is the method name, 'receiver' is the expression on which the method is called,
    # and 'args' is the list of argument expressions.
    selector: str
    receiver: Expr
    args: List[Expr]

###########################################################################
class ASTTransformer(Transformer):
    # Root rule
    @staticmethod
    def program(children):
        """
        Matches the top-level rule: program: class_def*
        Children is a list of ClassNode objects.
        Returns a ProgramNode that encapsulates all user-defined classes.
        """
        return ProgramNode(classes=children)

    @staticmethod
    def class_def(children):
        """
        Matches: class_def: CLASS CNAME ":" CNAME "{" method_def* "}"
        Collects exactly two CNAME tokens (the class name and its parent),
        and gathers any MethodNode objects.
        """
        cname_tokens = []
        method_nodes = []
        for ch in children:
            if isinstance(ch, Token) and ch.type == 'CNAME':
                cname_tokens.append(ch)
            elif isinstance(ch, MethodNode):
                method_nodes.append(ch)
            # otherwise ignore (these are 'class', ':', '{', '}')

        class_name = str(cname_tokens[0])
        parent_name = str(cname_tokens[1])
        return ClassNode(name=class_name,
                         parent=parent_name,
                         methods=method_nodes)

    @staticmethod
    def method_def(children):
        """
        Matches: method_def: selector block
        children = [selector_string, BlockNode].
        If the selector is a reserved word (and doesn't contain a colon) -
        exit_with_code(22). Otherwise, return a MethodNode.
        """
        selector_str = children[0]
        block = children[1]
        if ":" not in selector_str and selector_str in RESERVED_WORDS:
            exit_with_code(22)
        return MethodNode(selector=selector_str,
                          body=block)

    @staticmethod
    def selector(children):
        """
        Matches: selector: NAME | NAME_COLON (NAME_COLON)*
        Joins the parts (Tokens) into one string.
        """
        # For example:
        #   - no params: [Token(NAME,"run")] => 'run'
        #   - with params: [Token(NAME_COLON,"add:"), Token(NAME_COLON,"to:")] => 'add:to:'

        parts = [str(ch) for ch in children]
        return "".join(parts)

    @staticmethod
    def block(children):
        """
        Matches: block: "[" block_params? "|" block_stats? "]"
        After parsing, 'children' might contain up to two items:
          - If both block_params and block_stats are present, 'children' has length 2.
          - If only block_params or only block_stats is present, 'children' has length 1.
          - If the block is completely empty (no params and no statements), 'children' can be an empty list.
        For example:
          - "[|]" => no parameters, no statements -> children = []
          - "[ :x | ]" => only parameters -> children = [[ "x" ]]
          - "[ | x := 1. ]" => only statements -> children = [[ AssignNode(...) ]]
          - "[ :x :y | x := 1. y := 2. ]" => params and statements -> children = [[ "x","y" ], [ AssignNode(...), ... ]]
        We distinguish 'params' from 'statements' by checking the content:
          - If it's a list of strings (like ["x", "y"]), we treat it as parameters.
          - Otherwise, it's a list of statements (AssignNode, etc.).
        Returns a BlockNode with two fields:
          - params: the list of parameter names (strings), or empty if none
          - statements: the list of statements in the block (or empty if none)
        """
        params = []
        statements = []
        if len(children) == 1:
            item = children[0]
            if item and isinstance(item[0], str):
                params = item
            else:
                statements = item
        elif len(children) == 2:
            params = children[0]
            statements = children[1]

        return BlockNode(params=params,
                         statements=statements)

    @staticmethod
    def block_params(children):
        """
        Matches: block_params: COLON_NAME (COLON_NAME)*
        Each token is like ":x". Remove the leading colon and store the result.
        Check if there are no duplicates and no reserved words.
        """
        params = [str(tok)[1:] for tok in children]

        if len(params) != len(set(params)):
            exit_with_code(35)

        for p in params:
            if p in RESERVED_WORDS:
                exit_with_code(22)
        return params

    @staticmethod
    def block_stats(children):
        """
        Matches: block_stats: statement*
        Returns a list of statement objects.
        """
        # children = [stmt, stmt, ...]
        return children

    @staticmethod
    def statement(children):
        """
        Matches: statement: NAME ":=" expr "."
        children = [Token(NAME), expression]
        Creates an AssignNode with var_name and the parsed expression.
        If var_name is a reserved word - error.
        """
        var_name = str(children[0])
        if var_name in RESERVED_WORDS:
            exit_with_code(22)
        expr_node = children[1]
        return AssignNode(var_name=var_name,
                          expr=expr_node)

    @staticmethod
    def expr(children):
        """
        Matches: expr: expr_base expr_tail?
        If there's only expr_base, return it directly.
        If there's expr_tail, it represents a method call (SendNode).
        """

        if len(children) == 1:
            return children[0]  # just expr_base
        else:
            base = children[0]
            tail = children[1]

            if isinstance(tail, str):
                # no arguments
                return SendNode(selector=tail,
                                receiver=base,
                                args=[])
            else:
                # tail = (selectorStr, [expr, expr, ...])
                sel, arg_exprs = tail
                return SendNode(selector=sel,
                                receiver=base,
                                args=arg_exprs)

    @staticmethod
    def expr_base(children):
        """
        Matches: expr_base: INT | STRING | TRUE | FALSE | NIL | NAME | CNAME | block | "(" expr ")"
        If it's a token, test its type (INT, STRING, etc.) and create the appropriate node.
        If it's a BlockNode or a parenthesized expression, return it as is.
        """
        child = children[0]
        if isinstance(child, Token):
            # check token type
            ttype = child.type
            tval = str(child)
            if ttype == 'INT':
                return LiteralNode(lit_type='Integer', value=tval)
            elif ttype == 'STRING':
                inner_text = tval[1:-1]
                return LiteralNode(lit_type='String', value=inner_text)
            elif ttype == 'TRUE':
                return LiteralNode(lit_type='True', value='true')
            elif ttype == 'FALSE':
                return LiteralNode(lit_type='False', value='false')
            elif ttype == 'NIL':
                return LiteralNode(lit_type='Nil', value='nil')
            elif ttype == 'NAME':
                return VarNode(name=tval)
            elif ttype == 'CNAME':
                # interpret this as class literal
                return ClassLiteralNode(name=tval)
        else:
            # either a BlockNode if child is block, or expr if '(' expr ')'
            return child

    @staticmethod
    def message_no_args(children):
        """
        Matches: expr_tail: NAME -> message_no_args
        For method calls without arguments. Just a single token (e.g. "run").
        If it's a reserved word, exit_with_code(22).
        """
        method_name = str(children[0])
        if method_name in RESERVED_WORDS:
            exit_with_code(22)
        return method_name

    @staticmethod
    def message_with_args(children):
        """
        Matches: expr_tail: NAME_COLON expr (NAME_COLON expr)*
        children might look like [Token(NAME_COLON, "add:"), Expr, Token(NAME_COLON, "to:"), Expr, ...].
        Collecting all selector parts into a string (like "add:to:") and arguments into a list.
        Returns a tuple (selector_str, [expr1, expr2, ...]).
        """
        parts = []
        args = []
        i = 0
        while i < len(children):
            if isinstance(children[i], Token):
                sel_part = str(children[i])
                parts.append(sel_part)
                i += 1
                expr_arg = children[i]
                args.append(expr_arg)
                i += 1
        selector_str= "".join(parts)
        return selector_str, args

###########################################################################
################################ FUNCTIONS ################################
def infer_type(expr: Expr) -> Optional[str]:
    """
    Infers and returns the type of expression as a string.

    - ClassLiteralNode('Integer') -> "class-Integer"
    - LiteralNode('Integer') -> "Integer", LiteralNode('String') -> "String"
      'True' or 'False' -> same value, 'Nil' -> "Nil"
    - VarNode -> None (type is unknown)
    - BlockNode -> "Block"
    - SendNode (method call):
        - If the receiver's type starts with "class-" and the selector is "from:" or "new",
            return the class name (meaning we get an instance of that class).
        - If the receiver is an instance (e.g. "Integer") and the method is in
            BUILTIN_INSTANCE_METHODS, return the same type; otherwise, exit_with_code(32).
        - If the receiver is a user-defined class or instance, it checks user-defined methods.

    This function is used during static analysis to determine the type of expressions in SOL25.
    That allows us to validate method calls by ensuring that only defined methods are invoked
    on the correct types (class vs. instance). If a method is missing, call exit_with_code(32).
    """

    if isinstance(expr, ClassLiteralNode):
        # e.g. ClassLiteralNode('Integer') => "class-Integer"
        return f"class-{expr.name}"

    if isinstance(expr, LiteralNode):
        # Return "Integer", "String", "True", "False", or "Nil" depending on lit_type
        if expr.lit_type == 'Integer':
            return "Integer"
        if expr.lit_type == 'String':
            return "String"
        if expr.lit_type in ("True", "False"):
            return expr.lit_type
        if expr.lit_type == 'Nil':
            return "Nil"
        return None

    if isinstance(expr, VarNode):
        # Unknown type for variables
        return None

    if isinstance(expr, BlockNode):
        # Blocks are "Block"
        return "Block"

    if isinstance(expr, SendNode):
        # Method call: determine the receiver's type first
        base_type = infer_type(expr.receiver)
        if base_type is None:
            # Can't determine the type -> skip further checks
            return None

        if base_type.startswith("class-"):
            # The receiver is a class literal, e.g. "class-Integer"
            class_name = base_type[len("class-"):]
            # Check if the selector is in the built-in class methods
            if class_name in BUILTIN_CLASS_METHODS:
                if expr.selector in BUILTIN_CLASS_METHODS[class_name]:
                    # If it's "from:" or "new", we get an instance of the class
                    if expr.selector in ("from:", "new"):
                        return class_name
                    # Otherwise, we can't infer a more specific type
                    return None
            # If no matching method found, return None or handle error
            return None
        else:
            # The receiver is an instance type, e.g. "Integer"
            if base_type in BUILTIN_INSTANCE_METHODS:
                # If method is known, keep the same type
                if expr.selector in BUILTIN_INSTANCE_METHODS[base_type]:
                    return base_type
                else:
                    exit_with_code(32)  # Method doesn't exist
            else:
                # For user-defined types, check them recursively
                if not has_method_recursively(base_type, expr.selector, {}):
                    exit_with_code(32)
                return base_type

    return None

def parsing_arguments() -> None:
    """
    This function parses the command line arguments.
    """
    args = sys.argv[1:]  # get args without script name
    if not args:
        # return if no args
        return

    if len(args) > 1 or args[0] not in ("--help", "-h"):
        exit_with_code(10)

    # '--help' or '-h' was passed
    exit_with_code(0)

def exit_with_code(code)-> None:
    """
        Function to print error message and exit with code.
    """
    if code == 0:
        print(help_text)
        sys.exit(0)
    if code == 10:
        print("Error: missing script parameter or bad combination of parameters",
              file=sys.stderr)
        sys.exit(10)
    if code == 21:
        print("Error: Lexical error in source code in SOL25.",
              file=sys.stderr)
        sys.exit(21)
    elif code == 22:
        print("Error: Syntax error in source code in SOL25.",
              file=sys.stderr)
        sys.exit(22)
    elif code == 31:
        print("Semantic error - missing Main class or its instance method run.",
              file=sys.stderr)
        sys.exit(31)
    elif code == 32:
        print("Error: semantic error - use of an undefined (and therefore uninitialized) variable, formal parameter, "
              "class, or class method.",
              file=sys.stderr)
        sys.exit(32)
    elif code == 33:
        print("Error: Semantic arity error (bad arity of the block assigned to the selector when defining the "
              "instance method)",
              file=sys.stderr)
        sys.exit(33)
    elif code == 34:
        print("Error: Semantic error - collision variable (local variable collides with formal block parameter).",
              file=sys.stderr)
        sys.exit(34)
    elif code == 35:
        print("Error: Semantic error - others",
              file=sys.stderr)
        sys.exit(35)

def on_comment(token):
    """
    This callback is triggered by Lark whenever it sees a COMMENT.
    Only store the text of the first comment and ignore the rest.
    """
    global first_comment
    if first_comment is None:
        raw = token.value[1:-1]
        unescaped = codecs.decode(raw, 'unicode-escape')
        final = unescaped.replace('\n', '&#10;')
        first_comment = final

def check_block_for_undefined_class(block: BlockNode,
                                    all_classes: set[str],
                                    user_classes: dict[str, ClassNode]):
    """
    This function recursively checks all statements in a block for references to undefined classes.
    For each assignment statement in the block, it examines the right-hand side expression.
    """
    for statement in block.statements:
        # Assume each statement is an AssignNode
        check_expr_for_undefined_class(statement.expr,
                                       all_classes,
                                       user_classes)

def check_expr_for_undefined_class(expr: Expr,
                                   all_classes: set[str],
                                   user_classes: dict[str, ClassNode]):
    """
    Recursively checks an expression for references to undefined classes.
    If a ClassLiteralNode is encountered, it verifies that its name is in all_classes.
    For a SendNode, it checks the receiver and all arguments, then verifies that if the receiver
    is a class literal, the called method exists.
    """
    if isinstance(expr, ClassLiteralNode):
        if expr.name not in all_classes:
            exit_with_code(32)
    elif isinstance(expr, SendNode):
        check_expr_for_undefined_class(expr.receiver, all_classes, user_classes)
        for arg in expr.args:
            check_expr_for_undefined_class(arg, all_classes, user_classes)
        check_class_method(expr, user_classes)
    elif isinstance(expr, BlockNode):
        # recursively check nested statements
        check_block_for_undefined_class(expr, all_classes, user_classes)
    # LiteralNode and VarNode don't contain class references; no check needed.

def check_block_for_undefined_vars(block: BlockNode,
                                   known_vars: set[str]):
    """
       Function recursively checks that all variables used in the block are defined (i.e., have been assigned)
       before use. It processes each assignment statement in the block:
         - First, it checks the right-hand side expression to ensure all variables used are already declared.
         - Then, it ensures that the variable being assigned is not one of the formal parameters (which are immutable).
         - Finally, it adds the variable to the set of known variables.
       """
    # known_vars are normal local variables
    # block.params are formal parameters (cannot be changed)
    for statement in block.statements:
        # first, check the expression for usage of undeclared vars
        check_expr_for_undefined_vars(statement.expr, known_vars)

        # next, see if we're assigning to a parameter
        if statement.var_name in block.params:
            exit_with_code(34) # Error: Attempting to assign to a formal parameter

        # if it's okay, declare statement.var_name
        known_vars.add(statement.var_name)

def check_expr_for_undefined_vars(expr: Expr,
                                  known_vars: set[str]):
    """
    Recursively checks that all variable references in the expression refer to variables
    that have been declared (i.e., present in known_vars). For SendNode, both the receiver
    and all arguments are checked. For nested blocks, a new scope is created that includes
    the block parameters.
    """
    if isinstance(expr, VarNode):
        if expr.name == "self":
            return
        if expr.name not in known_vars:
            exit_with_code(32)  # undeclared variable
    elif isinstance(expr, SendNode):
        # check receiver
        check_expr_for_undefined_vars(expr.receiver, known_vars)
        for arg in expr.args:
            check_expr_for_undefined_vars(arg, known_vars)
    elif isinstance(expr, BlockNode):
        # recursively check nested block
        # block parameters are considered declared
        new_known = {"self"} | set(expr.params)
        check_block_for_undefined_vars(expr, new_known)
    # LiteralNode / ClassLiteralNode do not contain VarNode, so no checks needed

def check_class_method(expr: SendNode,
                       user_classes: dict[str, ClassNode]):
    """
    This function checks that a method call (a SendNode) is valid by doing a simple static type check.

    First, it calls 'infer_type' on the receiver (the object on which the method is called) to figure out its type.
    - If the type starts with "class-", it means the receiver is a class literal. In that case, the function
      checks that the method (the selector from the SendNode) is defined as a class method for that class.
    - Otherwise, the receiver is treated as an instance, and the function checks that the selector is among
      the instance methods for that type.

    If the method is not found in the appropriate set (either in BUILTIN_CLASS_METHODS or in BUILTIN_INSTANCE_METHODS,
    or via inheritance in user-defined classes), the function calls exit_with_code(32).
    """
    # Use a type inference function to determine the type of the receiver.
    inferred_type = infer_type(expr.receiver)
    if inferred_type is None:
        # If we cant infer the type, we skip this check (and will check dynamic later)
        return
    if inferred_type.startswith("class-"):
        # The receiver is a class literal
        # for example: "class-Integer"
        class_name = inferred_type[len("class-"):]
        # Check if the class is built-in and the method is defined among its class methods.
        if class_name in BUILTIN_CLASS_METHODS:
            if expr.selector not in BUILTIN_CLASS_METHODS[class_name]:
                exit_with_code(32)
        else:
            # For user-defined classes, recursively check whether the method is defined.
            if not has_method_recursively(class_name, expr.selector, user_classes):
                exit_with_code(32)
    else:
        # The inferred type represents an instance type ("Integer", "String", ...)
        # Check if the selector is among the built-in instance methods.
        if inferred_type in BUILTIN_INSTANCE_METHODS:
            if expr.selector not in BUILTIN_INSTANCE_METHODS[inferred_type]:
                exit_with_code(32)
        else:
            # For user-defined instance types, search recursively through the inheritance chain.
            if not has_method_recursively(inferred_type, expr.selector, user_classes):
                exit_with_code(32)



def has_method_recursively(class_name: str,
                           selector: str,
                           user_classes: dict[str, ClassNode]) -> bool:
    """
    This function checks recursively if a user-defined class (given by class_name) or any of its parent classes
    has a method with the given selector.

    Steps for this:
    - Look up the class in the user_classes dictionary.
    - Check if this class has a method whose selector matches.
    - If not found, get the parent's name. If the parent is a built-in class, check in BUILTIN_CLASS_METHODS.
      Otherwise, call the function recursively on the parent class.

    It returns True if the method is found, or False if is not.
    """

    cls_node = user_classes.get(class_name)
    if not cls_node:
        # The class was not found in the user-defined classes.
        return False
    # Check if this class has a method with the given selector.
    if any(m.selector == selector for m in cls_node.methods):
        return True
    # If not, try to check in its parent.
    parent_name = cls_node.parent
    if parent_name in BUILTIN_CLASS_METHODS:
        # If the parent is built-in, look up in the built-in class methods.
        return selector in BUILTIN_CLASS_METHODS[parent_name]
    else:
        # Otherwise, recursively check the parent's methods.
        return has_method_recursively(parent_name, selector, user_classes)

def check_circular_inheritance(user_classes: dict[str, ClassNode]):
    """
        This function checks for circular inheritance in user classes.
    """
    for class_name in user_classes:
        visited = set()
        current = class_name
        while current in user_classes:
            if current in visited:
                exit_with_code(35)
            visited.add(current)
            current = user_classes[current].parent
            # If the current parent is not a user class, then it is a built-in class — a cycle is impossible
            if current not in user_classes:
                break


def code_check(program: ProgramNode):
    """
    This function make some work, such as:
         1) Make sure there are no duplicate class definitions
         2) Looking for 'Main' class
         3) Make sure every other method has the number of colons
             in its selector equal to the number of parameters
         4) Look for 'run' method that has zero parameters
         5) Check if the parent is defined
         6) Check references to classes in expressions
         7) Check for undefined variables in blocks
         8) Check for circular inheritance in user classes
         9) Check that a method with the same selector is not defined twice in one class.
    """

    # make a dictionary of user classes
    user_classes_dict = {cls.name: cls for cls in program.classes}
    # make a set of all classes
    all_classes = BUILT_IN_CLASSES.union(user_classes_dict.keys())

    # there are some flags
    seen_classes = set()
    main_class = None
    run_found = False

    for cls in program.classes:

        # if we have already seen this class
        if cls.name in seen_classes:
            exit_with_code(35)

        # found Main class
        if cls.name == "Main":
            main_class = cls

        seen_selectors = set()

        # add class to seen classes
        seen_classes.add(cls.name)

        for method in cls.methods:

            # 9)
            if method.selector in seen_selectors:
                exit_with_code(35)
            seen_selectors.add(method.selector)

            # found run method
            if method.selector == "run":
                run_found = True
                # check if run method has zero parameters
                if len(method.body.params) != 0:
                    exit_with_code(33)
                continue

            # check if the number of colons in the selector is equal to the number of parameters
            colon_count = method.selector.count(':')
            if colon_count != len(method.body.params):
                exit_with_code(33)

        # 5) Check if the parent is defined
        if cls.parent not in all_classes:
            exit_with_code(32)


    # 2) Check for Main class
    if main_class is None:
        exit_with_code(31)  # no Main class

    # 4) Check for run method
    if not run_found:
        exit_with_code(31) # no run method

    # 3) Check references to classes in expressions:
    #

    # 6) Check references to classes in expressions
    #     For every method, scan its block to ensure that
    #     every referenced class is defined.
    # 7) Check for undefined variables in blocks
    #     For every method, go through the block statements in order,
    #     ensuring that any variable used has been previously assigned.
    for cls in program.classes:
        for method in cls.methods:

            # 6)
            check_block_for_undefined_class(method.body, all_classes, user_classes_dict)

            # 7)
            known_vars = set()
            known_vars.add("self") # 'self' is always available within instance methods
            # Add block parameters (they are considered declared)
            known_vars.update(method.body.params)
            check_block_for_undefined_vars(method.body, known_vars)


    # 8)Check for circular inheritance in user classes
    check_circular_inheritance(user_classes_dict)

###########################################################################
############################## XML FUNCTION ###############################
def build_program_xml(program: ProgramNode,
                      first_comment=None) -> ET.Element:
    """
    Creates the root XML element for the entire SOL25 program.

    Steps:
    - Create a <program> element with the attribute language="SOL25".
        If there's a non-empty first_comment, also set description=first_comment.
    - For each user-defined class in program.classes, we create a <class> child element
        in the root. This element has two attributes: 'name' (the class name)
        and 'parent' (its superclass).
    - For each method in a given class, we create a <method> element with
        selector=method.selector, and then call build_block_xml() to attach
        the method’s code (BlockNode) as XML.
    """
    attrs = {"language": "SOL25"}
    if first_comment is not None:
        attrs["description"] = first_comment
    root = ET.Element("program", attrs)

    # Create a <class> element for each class in the AST.
    for cls_node in program.classes:
        class_el = ET.SubElement(root,
                                 "class",
                                 {
                                    "name": cls_node.name,
                                    "parent": cls_node.parent,}
        )

        # Create a <method> element inside each <class> for every method in that class.
        for method_node in cls_node.methods:
            meth_el = ET.SubElement(class_el,
                                    "method",
                                    {
                                        "selector": method_node.selector,}
            )

            # Add the block body (block of statements) to the <method> element.
            build_block_xml(method_node.body, meth_el)
    return root

def build_block_xml(block: BlockNode,
                    parent_el: ET.Element):
    """
    Builds an XML representation of a block of code and appends it to a parent element.

    Steps:
    - Create a <block> element with arity=the number of parameters in the block.
    - For each parameter in block.params, generate a <parameter> element,
        assigning it the attribute name=the parameter name and order=i (1-based index).
    - For each statement in block.statements, we create an <assign> element
        with a unique order (based on the statement’s position).
        This <assign> element will contain:
            - a <var> sub-element specifying the target variable of the assignment,
            - an <expr> sub-element representing the expression to be assigned.
    - build_block_xml() delegates the construction of each expression to build_expr_xml().
    """
    block_el = ET.SubElement(parent_el, "block", {
        "arity": str(len(block.params))
    })

    # Create <parameter> elements for each formal parameter in this block.
    for i, param_name in enumerate(block.params, start=1):
        ET.SubElement(block_el,
                      "parameter",
                      {
                          "name": param_name,
                          "order": str(i),}
        )

    # Create <assign> elements for each statement (an assignment) in the block.
    for i, stmt in enumerate(block.statements, start=1):

        assign_el = ET.SubElement(block_el,
                                  "assign",
                                  {
                                      "order": str(i),}
        )
        ET.SubElement(assign_el,
                      "var",
                      {
                          "name": stmt.var_name,}
        )
        expr_el = ET.SubElement(assign_el, "expr")
        build_expr_xml(stmt.expr, expr_el)

def build_expr_xml(expr: Expr,
                   parent_el: ET.Element):
    """
    Converts an expression node into its corresponding XML representation
    and appends it to the given parent element.

    Steps:
    - If the expr is a LiteralNode, create a <literal> element with:
       - class=(expr.lit_type)
       - value=(expr.value)
    - If the expr is a VarNode, create a <var> element indicating
        the variable's name.
    - If the expr is a ClassLiteralNode, create a <literal> element
        but with class="class" and value=(the class name).
    - If the expr is a SendNode, create a <send> element with attribute
        selector=(expr.selector). Within it:
           - An <expr> child for the receiver of the message,
           - An <arg> child (with an <expr> child) for each argument.
    - If the expr is a BlockNode, call build_block_xml again to append
        a nested <block> element. This allows for nested blocks.
    """
    if isinstance(expr, LiteralNode):
        ET.SubElement(parent_el,
                      "literal",
                      {
                          "class": expr.lit_type,
                          "value": expr.value,}
        )
    elif isinstance(expr, VarNode):
        ET.SubElement(parent_el,
                      "var",
                      {
                          "name": expr.name,}
        )
    elif isinstance(expr, ClassLiteralNode):
        ET.SubElement(parent_el,
                      "literal",
                      {
                          "class": "class",
                          "value": expr.name,}
        )
    elif isinstance(expr, SendNode):
        send_el = ET.SubElement(parent_el,
                                "send",
                                {
                                    "selector": expr.selector,}
        )

        # First, build the expression for the receiver of the message.
        recv_expr_el = ET.SubElement(send_el,
                                     "expr"
        )
        build_expr_xml(expr.receiver, recv_expr_el)

        # Then, build expression nodes for each argument.
        for i, arg_expr in enumerate(expr.args, start=1):
            arg_el = ET.SubElement(send_el,
                                   "arg",
                                   {"order": str(i),}
            )
            arg_expr_el = ET.SubElement(arg_el,
                                        "expr"
            )
            build_expr_xml(arg_expr, arg_expr_el)
    elif isinstance(expr, BlockNode):
        # Recursively build a nested block inside the expression’s parent.
        build_block_xml(expr, parent_el)

###########################################################################
################################## MAIN ###################################
if __name__ == "__main__":
    first_comment = None
    try:
        # Parse command-line arguments.
        #    If '--help' is found, display the usage information
        #    and then exit. If invalid parameters are supplied,
        #    parsing_arguments() may call exit_with_code(10).
        parsing_arguments()

        # Read the SOL25 source code from standard input.
        sol25_code = sys.stdin.read()

        # Set up the Lark parser, passing:
        #    - the grammar (sol25_grammar),
        #    - the start symbol ('start'),
        #    - the parser type ('lalr'),
        #    - and the comment callback (on_comment), which handles storing
        #      the first encountered comment.
        parser = Lark(sol25_grammar,
                      start='start',
                      parser='lalr',
                      lexer_callbacks={"COMMENT": on_comment})

        # Parse the SOL25 source code into a parse tree.
        tree = parser.parse(sol25_code)

        # Transform the parse tree into an abstract syntax tree (AST)
        ast = ASTTransformer().transform(tree)

        # Run checks on the AST to verify
        # there are no lexical, syntactic, or semantic errors.
        code_check(ast)

        # Build an XML tree from the AST
        # If a first_comment was captured, include it in the description.
        xml_root = build_program_xml(ast,
                                     first_comment=first_comment)

        # Convert to a pretty-printed XML string
        xml_str = ET.tostring(xml_root,
                              encoding="UTF-8")
        pretty_xml = minidom.parseString(xml_str).toprettyxml(indent="    ",
                                                              encoding="UTF-8")

        # Replace &amp;#10; with &#10; in the final XML
        final_xml_str = pretty_xml.decode("UTF-8")
        final_xml_str = final_xml_str.replace("&amp;#10;", "&#10;")

        # Output the result
        sys.stdout.write(final_xml_str)
    except UnexpectedCharacters:
        exit_with_code(21)
    except UnexpectedToken:
        exit_with_code(22)

###########################################################################
############################### END OF FILE ###############################
