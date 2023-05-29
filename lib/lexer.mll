{
	(* lexer head *)
	open Parser
	let reserved_small = [
		(* ("keyword", token);*)
		("ite", ITE);
		("fix", REC);
		("unit", TM_UNIT);
		("true", TM_BOOL);
		("false", TM_BOOL);
		("fst", FST);
		("snd", SND);
		("case", CASE);
		("of", OF);
	]
	let reserved_large = [
		("Unit", TY_UNIT);
		("Bool", TY_BOOL);
		("Int", TY_INT)
	]
	exception Quit
	exception RecTime of int
	exception TypeDeclarement of string
	exception AsTeX
	exception AsString
	exception Macro of string * string
}

let space = ['\t' ' ']
let small = ['a'-'z']
let large = ['A'-'Z']
let char = ['A'-'Z' 'a'-'z' '_']
let num = ['0'-'9']
let any = [' '-'~']

rule token = parse
| space+ { token lexbuf (* use recursion to ignore *) }
| '\\' {LAMBDA}
| '.' {DOT}
| '(' {LPAREN}
| ')' {RPAREN}
| '{' {LBRACE}
| '}' {RBRACE}
| ',' {COMMA}
| '<' {LANGLE}
| '>' {RANGLE}
| ':' {COLON}
| '=' {EQUAL}
| '|' {PIPE}
| "->" {ARROW}
| "#quit" {raise Quit}
| "#rectime" space* (num+ as num_string) {raise (RecTime (int_of_string(num_string)))}
| "#type" space* (any+ as type_string) {raise (TypeDeclarement (type_string))}
| "#tex" {raise AsTeX}
| "#string" {raise AsString}
| "#macro" space* (char+ as key) space* '=' space* (any* as body) {raise (Macro(key, body))}
| num+ as lexeme {NUM (int_of_string lexeme)}
| small char* as lexeme
    { try
        List.assoc lexeme reserved_small
      with
				_ -> TM_VAR lexeme
     }
| large char* as lexeme
    { try
        List.assoc lexeme reserved_large
      with
      _ -> failwith "NotFoundLargeChars"
     }
| '`' small char* as lexeme {TAG lexeme}
| '\n' {EOL}
| eof {EOF}

{
	(* lexer end *)
}