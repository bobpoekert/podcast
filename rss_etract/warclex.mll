{
  open Lexing
}

(* translated from page 10 of http://bibnum.bnf.fr/WARC/WARC_ISO_28500_version1_latestdraft.pdf *)

let sp = ' '
let ht = '\t'
let cr = '\r'
let lf = '\n'
let token_character = [ ^ '\x00' - ' ' '(' ')' '<' '>' '@' ',' ';' ':' '\\' '"' '/' '[' ']' '?' '=' '{' '}' ' ' '\t']
let token = token_character +
let lws = [ ' ' '\t' ]
let crlf = cr lf
let text = ( token_character | lws ) +
let field_name = token
let field_value = text 
let named_field = field_name ": " field_value ?
let field_content = text
let field_value = field_content | lws
let version = "WARC/1.0\r\n"

rule warc_record = parse 
  | version { VERSION }
  | ":" { COLON }
  | crlf { CRLF }
  | eof { EOF }
  | field_name { FIELD_NAME }
  | field_value { FIELD_VALUE }