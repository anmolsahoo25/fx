{
    open Parser

    exception Lexing_error of string
}

rule token = parse
| [' ' '\t' '\n']
    { token lexbuf }
| eof
    { EOF }
| "fn"
    { FN }
| "let"
    { LET }
| "in"
    { IN }
| '='
    { EQUAL }
| "handle"
    { HANDLE }
| "with"
    { WITH }
| "mut"
    { MUT }
| '_'
    { ANY }
| '"' (['a'-'z' 'A'-'Z' '0'-'9' ' ']* as s) '"'
    { SCONST s }
| (['a'-'z'] ['a'-'z' '0'-'9']*) as i
    { ID i }
| ['0'-'9']+ as n
    { NCONST n }
| ":="
    { SET }
| "->"
    { ARROW }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '{'
    { LCPAREN }
| '}'
    { RCPAREN }
| ':'
    { COLON }
| ';'
    { SEMICOLON }
| ','
    { COMMA }
| '|'
    { BAR }
| '+'
    { PLUS }
| '!'
    { EXCL }
