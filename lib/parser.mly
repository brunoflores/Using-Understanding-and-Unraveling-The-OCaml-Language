%{
  (* open Syntax *)
%}

%token EOF

%start <int> main

%%

main:
  | EOF { 0 }
