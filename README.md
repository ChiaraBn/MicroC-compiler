# Languages, Compilers and Interpreters

## Final project: compiler for MicroC

The main goal of the Languages, Compilers and Interpreters' course was to analyze and study the behaviour of a generic compiler, in order to achieve the realization of an ad hoc compiler for a subset language of C: **MicroC**.

### Building the project

Firstly, it is necessary to have the environment updated
```
eval $(opam config env)
``` 

To create the **executable**
```
make
``` 
This command creats the **microc.native** executable that can be lauched with different options: <br>
``` 
-p file.mc  - Create and print the AST
-s file.mc  - Perform semantic checks
-d file.mc  - Print the generated code
-o out.txt  - Place the output into a file
-0          - Optimize the generated code
-c file.mc  - Compile (default)
``` 
To link the external libraries
``` 
make ext
```
Ultimately, to compile and run the file:
```
make run f=file.mc
```
To clean the folder from the genered files
```
make clean
```


### Structure of the files
Inside the top level folder: <br>

| File         | Description                                      |
| -----------  | -----------                                      |
|  /src        | The folder for the source files                  |
|  /test       | The folder for the various tests for the program |
|  /doc        | The folder for the Ocamldoc files                |
| Makefile     | The Makefile for the compilation of the program  |
| README.md    | File for the overall documentation               |
| MicroC.pdf   | Final PDF report for the project                 |
<br>

Inside the **src** folder: <br>
| File         | Description                                      |
| -----------  | -----------                                      |
| microcc.ml   | The file from which build the executable         |
| ast.ml       | The AST structure                                |
| scanner.mll  | Ocamllex specification of the scanner            |
| parser.ml    | Menhir specification of the grammar              | 
| parser_engine.ml | The module that interacts with the parser    |          
| semant.ml | Module that implements the semantic checker         |           
| codegen.ml | Module that implements the code generation         |            
| opt_pass.ml  | Module that implements some simple optimizations |             
| symbol_table.mli | Interface of a polymorphic symbol table data type |          
| symbol_table.ml | Implementation of a polymorphic symbol table data type |
| util.ml | It contains some utily functions                         |               
| error_msg.ml | The strings representing the semantic errors     |           
| rt-support.c | The run-time support to be linked to bitcode files|