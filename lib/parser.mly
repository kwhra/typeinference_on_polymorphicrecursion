%{
  (* head of parser *)
%}

%token EOF EOL
%token <Term.var> TM_VAR
%token TM_BOOL TM_UNIT ITE TM_INT
%token LPAREN RPAREN
%token LAMBDA DOT
%token REC
%token COMMA LBRACE RBRACE FST SND
%token LANGLE RANGLE EQUAL COLON CASE OF PIPE
%token <Type.var> TY_VAR
%token ARROW
%token TY_BOOL TY_UNIT TY_INT 
%token <Tag.t> TAG
%token <int> NUM
%start <Term.t> program_term
%start <Type.t> program_type

%%

program_term:
  | t=term EOF {t}
  | t=term EOL {t}

program_type:
  | t=ty EOF {t}
  | t=ty EOL {t}

term:
  | t1=term t2=term_base {Term.App(t1, t2)}
  | t=term_base {t}

term_base:
  | x=TM_VAR {Term.Var x}
  | TM_BOOL {Term.Con Term.Bool}
  | TM_INT {Term.Con Term.Int}
  | TM_UNIT {Term.Con Term.Unit}
  | FST {Term.Con Term.Fst}
  | SND {Term.Con Term.Snd}
  | NUM {Term.Con Term.Int}
  | ITE {Term.Con Term.Ite}
  | LPAREN t=term RPAREN {t}
  | LAMBDA x=TM_VAR DOT t=term{Term.Abs(x, t)}
  | REC x=TM_VAR DOT t=term {Term.Rec(x, t)}
  | LBRACE t1=term COMMA t2=term RBRACE {Term.Tuple(t1, t2)}
  | LANGLE tag=TAG EQUAL t=term RANGLE {Term.Variant(tag, t)}
  | CASE t1=term OF f=tm_field {Term.CaseOf(t1, f)}

tm_field:
  | LANGLE tag = TAG EQUAL x = TM_VAR RANGLE t=term PIPE tl = tm_field {(tag, x, t)::tl}
  | LANGLE tag = TAG EQUAL x = TM_VAR RANGLE t=term {[(tag, x, t)]}

ty:
  | t1=ty_base ARROW t2=ty {Type.Arr(t1, t2)}
  | t=ty_base {t}

ty_base:
  | x=TY_VAR {Type.Var x}
  | TY_BOOL {Type.Con Type.Bool}
  | TY_INT {Type.Con Type.Int}
  | TY_UNIT {Type.Con Type.Unit}
  | LPAREN t=ty RPAREN {t}
  | LBRACE t1=ty COMMA t2=ty RBRACE {Type.Tuple(t1, t2)}
  | LANGLE f=ty_field RANGLE {Type.Variant f}

ty_field:
  | tag=TAG COLON ty=ty COMMA tl=ty_field {(tag, ty)::tl}
  | tag=TAG COLON ty=ty {[(tag, ty)]}