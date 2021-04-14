# Languages, Compilers and Interpreters

## Final project: compiler for MicroC

eval $(opam config env)

ocamldoc

## Elementi di teoria da mettere nel report

## PUNTO 1 - front end - menhir
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

## PUNTO 2 - Analisi semantica
- **semant.ml** per l'analisi semantica
- **symbol table** per memorizzare i tipi delle variabili

- TypFun (t, id, tlst) usato per controllo semantico delle funzioni.<br>
Cosi il confronto è sempre tra Ast.typ
<p>

## PUNTO 3 - Code Generation

LLVM IR {
    - codice a tre indirizzi
    - valori tipati
    - registri infiniti
}

opt_pass.ml -> file che implementa le possibili ottimizzazioni del codice creato con LLVM
LLVM crea codice macchina non ottimizzato in quanto questo è compito delle fasi successive
(quindi è possibile non implementarle a questo livello) <br>

A questo punto dello sviluppo, si assume che i costrutti siano semanticamente corretti,
quindi non vengono eseguiti **controlli** di tipo. <br>

Viene usata la **tabella dei simboli** per tenere traccia dei tipi in maniera più semplice. <br>

**inizializzazione**
- valori primitivi al loro zero
- booleani a false
- array e puntatori alle struct vuote
<br>

<p>


## PUNTO 4 
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
