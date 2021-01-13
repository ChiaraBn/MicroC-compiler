# Languages, Compilers and Interpreters

## Final project: compiler for MicroC

eval $(opam config env)

ocamldoc

## Elementi di teoria da mettere nel report

## FRONT END - MENHIR
 - **menhir** - an LR(1) parser generator for OCaml 

 - A **reduce/reduce** conflict occurs if there are two or more rules that apply to the same sequence of input.
 - A **shift-reduce** conflict occurs in a state that requests both a shift action and a reduce action.

    The way that **precedence** works to resolve shift/reduce conflicts is that it compares the precedence of the rule to be reduced with the precedence of the token to be shifted.

- **Shift**
    Accept the basic symbol as the corresponding terminal, push a new state onto the stack, and examine the next basic symbol.

- **Reduce**
    Note that a specific phrase has been recognized, remove a number of states equal to the number of symbols in the sequence of the corresponding production from the stack, push a new state onto the stack, and examine the current basic symbol again. 

- **factor** generates  expressions that cannot be  “pulled apart,”  that is,  a  factor is either a single operand or any parenthesized expression.
- **term** generates a product or quotient of factors.<br> A single factor is a term, and thusis a sequence of factors separated by the operators*or/.  
- **Expression** generates a sum or difference of one or more terms. A single term isan expression, and thus is a sequence of terms separated by the operators+or-.<br>
Examples of expressions are12,12/3*45and12+3*45-6.
<p>

- **Error handling** ( Parser **incrementale** )
    - A .__messages__ file is a text file. It is composed of a list of entries. 
    - **Blank** lines are significant: they are used as separators, both between entries, and (within an entry) between the sentences and the message. Thus, there cannot be a blank line between two sentences. 
    - A special **error token** is made available for use within productions. <br>
    The LR automaton is constructed exactly as if error was a regular terminal symbol.<br>
    However, error is never produced by the lexical analyzer. Instead, when an error is detected, the current lookahead token is discarded and replaced with the error token, which becomes the current lookahead token. At this point, the parser enters error handling mode.<p>

    The **workflow** is:

    - Run menhir --list-errors on the parser to get a template file with error states (parser.messages).
    - Edit the .messages file and add error message descriptions.
    - Run menhir --compile-errors parser.messages parser.mly to generate the error handling module.

    Il parser **incrementale** viene guidato dal codice del programmatore, mentre quello **monolitico** prende in input un token e restituisce un risultato, invocando da sè il lexer.<br>
    Quindi in quello incrementale, è il programmatore che fornisce i token al parser.
    - presenta dei **checkpoint**, ovvero dei dati algebrici che permettono di fare pattern matching, che descrivono gli stati del parser.

    

<p>

## Punto 4 
(scegline quattro)

    --> do-while loops;

    --> pre/post increment/decrement operators, i.e., ++ and --;

    --> abbreviation for assignment operators, i.e., +=, -=, *=, /= and %=;

    --> floating point arithmetic;
    - stessi operandi aritmetici delle operazioni intere
    - accetta rappresentazione exp
    

    variable declaration with initialization, e.g., int i = 0;
    multi-dimensional arrays;
    strings as in C, i.e. null-terminated arrays of characters;
    structs.
