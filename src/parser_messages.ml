
(* This file was auto-generated based on "src/parser.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 0 ->
        "Missing clause for WHILE token.\n"
    | 12 ->
        "Wrong ID for the function.\n"
    | 13 ->
        "Wrong parameters for TIMES token.\n"
    | 15 ->
        "Missing ID for the function declaration.\n"
    | 16 ->
        "Unbalanced use of parenthesis.\n"
    | 18 ->
        "Missing arguments for the FUNCTION declaration.\n"
    | 160 ->
        "Missing arguments for the FUNCTION declaration.\n"
    | 7 ->
        "Unbalanced use of parenthesis.\n"
    | 19 ->
        "Missing clause for WHILE token.\n"
    | 21 ->
        "Unbalanced use of parenthesis.\n"
    | 9 ->
        "Missing clause for WHILE token.\n"
    | 10 ->
        "Unbalanced use of parenthesis.\n"
    | 11 ->
        "Missing semicolon.\n"
    | 26 ->
        "Missing clause for WHILE token.\n"
    | 28 ->
        "Missing clause for WHILE token.\n"
    | 29 ->
        "Missing clause for WHILE token.\n"
    | 108 ->
        "NULL value before WHILE token.\n"
    | 109 ->
        "NULL value between parenthesis.\n"
    | 156 ->
        "Error in VOID ID.\n"
    | 30 ->
        "Wrong use of TIMES token.\n"
    | 52 ->
        "Wrong use of TIMES token.\n"
    | 107 ->
        "Wrong use of TIMES token.\n"
    | 27 ->
        "Unbalanced use of parenthesis.\n"
    | 31 ->
        "Wrong use of referenced value.\n"
    | 32 ->
        "Wrong use of referenced value.\n"
    | 34 ->
        "Wrong use of referenced value.\n"
    | 105 ->
        "Wrong use of referenced value ID.\n"
    | 110 ->
        "Missing return value.\n"
    | 111 ->
        "NULL statement before WHILE token.\n"
    | 132 ->
        "NULL statement before TIMES token.\n"
    | 55 ->
        "NULL statement before TIMES token.\n"
    | 155 ->
        "NULL statement before SEMICOLON token.\n"
    | 66 ->
        "PLUS token must have not NULL values.\n"
    | 67 ->
        "PLUS token must have not NULL values.\n"
    | 57 ->
        "OR_BIT token must have not NULL values.\n"
    | 58 ->
        "OR_BIT token must have not NULL values.\n"
    | 38 ->
        "Missing value after NOT token.\n"
    | 39 ->
        "Missing value after MINUS token.\n"
    | 40 ->
        "Unbalanced use of parenthesis.\n"
    | 97 ->
        "NULL statement after left parenthesis.\n"
    | 99 ->
        "Unbalanced use of parenthesis.\n"
    | 45 ->
        "Missing value after INCR token.\n"
    | 96 ->
        "INCR token must be followed by a not null value.\n"
    | 113 ->
        "Missing clause for the IF statement.\n"
    | 114 ->
        "Missing clause for the IF statement.\n"
    | 115 ->
        "NULL value for the clause for the IF statement.\n"
    | 143 ->
        "NULL value for the clause for the IF statement.\n"
    | 116 ->
        "NULL value for the IF statement.\n"
    | 141 ->
        "NULL value for the IF statement.\n"
    | 144 ->
        "NULL value for the ELSE statement.\n"
    | 46 ->
        "Unbalanced use of parenthesis.\n"
    | 36 ->
        "Unbalanced use of parenthesis.\n"
    | 103 ->
        "Null expression for the type of the array.\n"
    | 47 ->
        "Unbalanced use of parenthesis.\n"
    | 93 ->
        "Null expression.\n"
    | 94 ->
        "Null expression.\n"
    | 53 ->
        "Missing assignment.\n"
    | 71 ->
        "Missing assignment.\n"
    | 72 ->
        "Could not assign NULL value.\n"
    | 73 ->
        "Missing assignment.\n"
    | 75 ->
        "Missing assignment.\n"
    | 76 ->
        "Could not assign NULL value.\n"
    | 77 ->
        "Missing assignment.\n"
    | 79 ->
        "Missing assignment.\n"
    | 80 ->
        "Could not assign NULL value.\n"
    | 117 ->
        "Unbalanced use of parenthesis.\n"
    | 118 ->
        "Unbalanced use of parenthesis.\n"
    | 119 ->
        "Unbalanced use of parenthesis.\n"
    | 120 ->
        "Unbalanced use of parenthesis.\n"
    | 121 ->
        "Unbalanced use of parenthesis.\n"
    | 122 ->
        "Unbalanced use of parenthesis.\n"
    | 123 ->
        "Unbalanced use of parenthesis.\n"
    | 124 ->
        "The body of a block shouldn't be of NULL type.\n"
    | 125 ->
        "Unbalanced use of parenthesis.\n"
    | 126 ->
        "The body of a block shouldn't be of VOID type.\n"
    | 134 ->
        "The body of a block shouldn't be of NULL type.\n"
    | 136 ->
        "The body of a block shouldn't be of NULL type.\n"
    | 137 ->
        "The body of a block shouldn't be of NULL type.\n"
    | 135 ->
        "The body of a block shouldn't be of NULL type.\n"
    | 128 ->
        "Unbalanced use of parenthesis.\n"
    | 130 ->
        "Unbalanced use of parenthesis.\n"
    | 131 ->
        "Unbalanced use of parenthesis.\n"
    | 129 ->
        "Unbalanced use of parenthesis.\n"
    | 48 ->
        "The DECR token must be followed by a not NULL expression.\n"
    | 89 ->
        "The DECR token must be followed by a not NULL expression.\n"
    | _ ->
        raise Not_found
