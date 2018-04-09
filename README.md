# uofl-scheme-calculator

## Description:
This is an infix programmable calculated implemented using racket.

## Files: 
#### main.rkt
This is the main entry point for the application that provides the REPL functionality. 

#### parser.rkt
Information received from the REPL or other string sources are fed to the parser. The
parser will evaluate the information and attempt to determine what to do.

#### tokenizer.rkt
The tokenizer is responsible for breaking the string input down into the bare minimum 
lexemes for evaluation.

#### functions.rkt
Information passed from the parser is taken as input. Given the command "#definefunc", a 
new activation record for the user-defined function will be created and pushed onto the stack. 

