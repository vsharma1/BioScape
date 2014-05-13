type token =
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LT
  | GT
  | EOF
  | BANG
  | CARAT
  | BAR
  | EQUAL
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | LTGT
  | LTEQUAL
  | GTEQUAL
  | COMMA
  | DOT
  | AT
  | SEMI
  | COLON
  | UNDERSCORE
  | PRIME
  | NEW
  | IF
  | THEN
  | ELSE
  | STRING
  | INT
  | NULL
  | LET
  | TYPE
  | IN
  | OUT
  | SYSTEM
  | ARROW
  | REC
  | BOOL
  | CHAR
  | LIST
  | FLOAT
  | SUB
  | EMPTYLIST
  | TRUE
  | FALSE
  | CONS
  | SHOW
  | INT2FLOAT
  | FLOAT2INT
  | SQRT
  | TAU
  | AND
  | VAL
  | QUESTION
  | DO
  | REPLICATE
  | RUN
  | SAMPLE
  | MOV
  | DIRECTIVE
  | DELAY
  | OR
  | WITH
  | CHAN
  | PROC
  | PLOT
  | OF
  | AS
  | ALL
  | MATCH
  | CASE
  | SPACE
  | SHAPE
  | CUBOID
  | SPHERE
  | ORIGIN
  | COORD
  | INTVALUE of (int)
  | STRINGVALUE of (string)
  | NAME of (string)
  | FLOATVALUE of (float)
  | CHARVALUE of (char)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (float*int*bool*bool* ((Action.t*string) list * (Species.t*string) list)) * (Environment.t * Volume.t * Process.t * string)
val main2 :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (float*int*bool*bool* ((Action.t*string) list * (Species.t*string) list)) * (Environment.t * Volume.t * Process.t * string)
val plots :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ((Action.t*string) list * (Species.t*string) list)
val v :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Value.t
val vs :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Value.t list
val m :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Pattern.t
val ms :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Pattern.t list
val x :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Value.t
val t :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Typ.t
