(* ported from nltk.tokenize.treebank.word_tokenize *)

let rt v =
  (fun _ -> v)

let rep1 s =
  Printf.sprintf " %s " (Re.Group.get s 0)

let rep2 s =
  Printf.sprintf "%s %s" (Re.Group.get s 0) (Re.Group.get s 1)

let fmt1 fmt =
  (fun s -> Printf.sprintf fmt (Re.Group.get s 0))

let fmt2 fmt =
  (fun s -> Printf.sprintf fmt (Re.Group.get s 0) (Re.Group.get s 1))

let fmt3 fmt =
  (fun s -> Printf.sprintf fmt (Re.Group.get s 0) (Re.Group.get s 1) (Re.Group.get s 2))

let contractions2 = [
  Re.Pcre.regexp "(?i)\\b(can)(?#X)(not)\\b";
  Re.Pcre.regexp "(?i)\\b(d)(?#X)('ye)\\b";
  Re.Pcre.regexp "(?i)\\b(gim)(?#X)(me)\\b";
  Re.Pcre.regexp "(?i)\\b(gon)(?#X)(na)\\b";
  Re.Pcre.regexp "(?i)\\b(got)(?#X)(ta)\\b";
  Re.Pcre.regexp "(?i)\\b(lem)(?#X)(me)\\b";
  Re.Pcre.regexp "(?i)\\b(mor)(?#X)('n)\\b";
  Re.Pcre.regexp "(?i)\\b(wan)(?#X)(na)\\s";
]

let contractions3 = [
  Re.Pcre.regexp "(?i) ('t)(?#X)(is)\\b";
  Re.Pcre.regexp "(?i) ('t)(?#X)(was)\\b";
]

let starting_quotes = [
 ((Re.Pcre.regexp "([«“‘„]|[`]+)"), rep1);
 ((Re.Pcre.regexp "^\""), (rt "``"));
 ((Re.Pcre.regexp "(``)"), rep1);
 ((Re.Pcre.regexp "([ \\(\\[{<])(\"|\\'{2})"), (fmt1 "%s `` "));
 ((Re.Pcre.regexp "(?i)(\')(?!re|ve|ll|m|t|s|d)(\\w)\\b"), rep2)
]

let punctuation = [
  ((Re.Pcre.regexp "([^\\.])(\\.)([\\]\\)}>\"\'»”’ ]*)\\s*$"), (fmt3 "%s %s %s "));
  ((Re.Pcre.regexp "([:,])([^\\d])"), (fmt2 " %s %s"));
  ((Re.Pcre.regexp "([:,])$"), rep1);
  ((Re.Pcre.regexp "\\.\\.\\."), (rt " ... "));
  ((Re.Pcre.regexp "[;@#$%&]"), (rt "\\g<0>"));
  ((Re.Pcre.regexp "([^\\.])(\\.)([\\]\\)}>\"\\']*)\\s*$"), (fmt3 "%s %s%s "));
  ((Re.Pcre.regexp "[?!]"), (rt " \\g<0> "));
  ((Re.Pcre.regexp "([^'])' "), (fmt1 "%s ' "))
]

let parens_brackets = [
  ((Re.Pcre.regexp "[\\]\\[\\(\\)\\{\\}\\<\\>]"), (rt " \\g<0> "))
]

let ending_quotes = [
  ((Re.Pcre.regexp "([»”’])"), rep1);
  ((Re.Pcre.regexp "\""), (rt " '' "));
  ((Re.Pcre.regexp "(\\S)('')"), (fmt2 "%s %s "));
  ((Re.Pcre.regexp "([^' ])('[sS]|'[mM]|'[dD]|') "), (fmt2 "%s %s "));
  ((Re.Pcre.regexp "([^' ])('ll|'LL|'re|'RE|'ve|'VE|n't|N'T) "), (fmt2 "%s %s "))
]

let double_dashes = [
  (Re.Pcre.regexp "--", (rt " -- "))
]

let apply_subs subs text =
  let _folder s re =
    let rx, rep = re in 
    Re.replace rx ~f:rep s in 
  List.fold_left _folder text subs

let apply_contractions ctr text =
  let _folder s rx =
    Re.replace rx ~f:rep2 s in
  List.fold_left _folder text ctr

let apply_all_subs text =
  text
  |> apply_subs starting_quotes
  |> apply_subs punctuation
  |> apply_subs parens_brackets
  |> apply_subs double_dashes
  |> apply_subs ending_quotes
  |> apply_contractions contractions2
  |> apply_contractions contractions3

let python_splitter = Re.Pcre.regexp "\\s+"

let rec _get_re_tokens tokens res =
  match tokens with
  | [] -> res
  | (tok : Re.split_token) :: rst ->
    match tok with
    | `Text(s) -> _get_re_tokens rst (s :: res)
    | `Delim(_) -> _get_re_tokens rst res

let get_re_tokens tokens = _get_re_tokens tokens []

let tokenize s =
  s
  |> String.lowercase_ascii
  |> apply_all_subs
  |> Re.split_full python_splitter
  |> get_re_tokens