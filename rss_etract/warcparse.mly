%token FIELD_NAME FIELD_VALUE
%token COLON CRLF EOF
%token VERSION

%start <Warc.warc_record option> main
%%

main:
  | v = entries { Some v }
  | EOF { None }
  ;

entries:
  VERSION; fields = separated_list(CRLF, separated_pair(FIELD_NAME, COLON, FIELD_VALUE)); CRLF; CRLF; { fields } ;

