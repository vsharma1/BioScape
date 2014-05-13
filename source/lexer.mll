{
  exception Illegal_character
  type token = Parser.token
  type proc = Process.t
  type action = Action.t
  type position = Lexing.position

  let add_line (lexbuf) =
    let pos:position = Lexing.lexeme_end_p lexbuf in
    let pos:position = {pos with Lexing.pos_lnum = pos.Lexing.pos_lnum+1; Lexing.pos_bol = pos.Lexing.pos_cnum}
    in
       (*IF-FSHARP Lexing.lexbuf_set_curr_p lexbuf pos ENDIF-FSHARP*)
       (*IF-OCAML*) lexbuf.Lexing.lex_curr_p <- pos(*ENDIF-OCAML*)

  let remove_ends (s:string) = String.sub s 1 ((String.length s)-2)
  let char_of_string (s:string) = String.get s 0
  let keyword_table = Hashtbl.create 53
      let _ =
        List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
        [
          "and", Parser.AND ;
          "as" , Parser.AS ;
          "bool" , Parser.BOOL ;
          "case" , Parser.CASE ;
          "chan" , Parser.CHAN ;
          "char" , Parser.CHAR ;
          "cuboid", Parser.CUBOID ;
          "coord", Parser.COORD ;
          "delay" , Parser.DELAY ;
          "directive", Parser.DIRECTIVE ;
          "do" , Parser.DO ;
          "else" , Parser.ELSE ;
          "float" , Parser.FLOAT ;
          "false",Parser.FALSE;
          "if" , Parser.IF ;
          "in" , Parser.IN ;
          "int" , Parser.INT ;
          "let" , Parser.LET ;
          "list" , Parser.LIST ;
          "match" , Parser.MATCH ;
          "mov" , Parser.MOV ;
          "new", Parser.NEW ;
          "out" , Parser.OUT ;
          "or" , Parser.OR ;
          "of" , Parser.OF ;
          "plot", Parser.PLOT ;
          "proc" , Parser.PROC ;
          "replicate" , Parser.REPLICATE ;
          "run" , Parser.RUN ;
          "sample", Parser.SAMPLE ;
          "show" , Parser.SHOW ;
          "space", Parser.SPACE ;
          "sphere", Parser.SPHERE ;
          "string", Parser.STRING ;
          "sub", Parser.SUB ;
          "then" , Parser.THEN ;
          "true", Parser.TRUE;
          "type" , Parser.TYPE ;
          "val" , Parser.VAL ;
          "with" , Parser.WITH ;
          "float_of_int" , Parser.INT2FLOAT ;
          "int_of_float" , Parser.FLOAT2INT ;
          "sqrt" , Parser.SQRT ;
        ]
}
rule token = parse
    [' ''\t''\r']	{ token lexbuf }	(* skip blanks *)
  | '\n' { add_line lexbuf; token lexbuf}
  | "("  { Parser.LPAREN }
  | ")"  { Parser.RPAREN }
  | "."  { Parser.DOT }
  | "["  { Parser.LBRACKET }
  | "]"  { Parser.RBRACKET }
  | "<"  { Parser.LT }
  | ">"  { Parser.GT }
  | "!"  { Parser.BANG }
  | "^"  { Parser.CARAT }
  | "|"  { Parser.BAR }
  | ","  { Parser.COMMA }
  | ";"  { Parser.SEMI }
  | ":"  { Parser.COLON }
  | "::" { Parser.CONS }
  | "[]" { Parser.EMPTYLIST }
  | "="	 { Parser.EQUAL }
  | "+"	 { Parser.PLUS }
  | "-"	 { Parser.MINUS }
  | "/"	 { Parser.SLASH }
  | "_"	 { Parser.UNDERSCORE }
  | "'"  { Parser.PRIME }
  | "*"  { Parser.STAR }
  | "<=" { Parser.LTEQUAL }
  | ">=" { Parser.GTEQUAL }
  | "<>" { Parser.LTGT }
  | "()" { Parser.NULL }
  | "->" { Parser.ARROW }
  | "@"  { Parser.AT }
  | "?"  { Parser.QUESTION }

  | ['0'-'9']+   	{ Parser.INTVALUE(int_of_string(Lexing.lexeme lexbuf)) }
  | ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\''] *
    { let s = Lexing.lexeme lexbuf in
        try Hashtbl.find keyword_table s
        with Not_found -> Parser.NAME(s)
    }
  | ['"'] ([^'"'] | "\\\"")* ['"'] {Parser.STRINGVALUE(remove_ends (Lexing.lexeme lexbuf))}
  | ['\''] [^ '\'' '\\'] ['\''] { Parser.CHARVALUE(char_of_string (remove_ends (Lexing.lexeme lexbuf))) }
  | "'\\''" { Parser.CHARVALUE('\'') }
  | "'\\n'" { Parser.CHARVALUE('\n') }
  | "'\\r'" { Parser.CHARVALUE('\r') }
  | "'\\t" { Parser.CHARVALUE('\t') }
  | "'\\b'" { Parser.CHARVALUE('\b') }
  | "'\\" ['0'-'9']['0'-'9']['0'-'9'] "'"
      { Parser.CHARVALUE(char_of_string (remove_ends (Lexing.lexeme lexbuf)))
      }
  | ['0'-'9']+ ['.'] ['0'-'9']+ (['e' 'E'] ['+' '-'] ['0'-'9']+)?
      { Parser.FLOATVALUE(float_of_string (Lexing.lexeme lexbuf))
      }
  | eof            	{ Parser.EOF }
  | "(*"  		{ comment 1 lexbuf }
  | _ 			{ raise Illegal_character }

and comment n = parse
  | "(*"  		{ comment (n+1) lexbuf }
  | '\n' { add_line lexbuf; comment n lexbuf}
  | "*)" 		{ if (n-1) > 0 then comment (n-1) lexbuf else token lexbuf}
  |  _ 			{ comment n lexbuf }

(*****************************************************************************)
{
  let format (pos:position) =
    "Line " ^ string_of_int pos.Lexing.pos_lnum ^
    " char " ^ string_of_int (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) ^ ": "

  let parse_file (str:string) =
    let ic:in_channel = open_in str in
    let lexbuf = Lexing.from_channel ic in
      try Parser.main token lexbuf
      with
	  Illegal_character -> failwith (format (Lexing.lexeme_start_p lexbuf) ^ "Illegal character")
	| Parsing.Parse_error -> failwith (format (Lexing.lexeme_start_p lexbuf) ^ "Syntax error")

  let parse_string (str:string) =
    let lexbuf = Lexing.from_string str in
      try Parser.main token lexbuf
      with
	  Illegal_character -> failwith (format (Lexing.lexeme_start_p lexbuf) ^ "Illegal character")
	| Parsing.Parse_error -> failwith (format (Lexing.lexeme_start_p lexbuf) ^ "Syntax error")

}
(*****************************************************************************)

