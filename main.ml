open Lexing
open Parser
open Lexer
open Poly
open Expr

let filename = Sys.argv.(1)

let () = 
  (*do_print (Times([Term(1,1); Term(1,2); Term(1,3); Term(1,4)]));
  print_newline ();*)
  print_int (total_exponent 0 ([Term(1,2); Times([Term(1,2); Term(1,2)]); Term(1,1); Plus([Term(1, 11); Term(1,10)]); Term(1,6); Term(1,0); Term(1,4)]));
  print_newline ();
  (*open_in filename |>
  Lexing.from_channel |>
  Parser.main Lexer.token |>
  print_expr |>
  from_expr |>
  simplify |>
  print_pExp*)