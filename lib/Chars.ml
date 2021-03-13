open Parser
open Combinator
open Stream

let is_lower c =
  let code = Char.code c in
  code >= 97 && code <= 122

let is_upper c =
  let code = Char.code c in
  code >= 65 && code <= 90

let is_alpha c =
  is_lower c || is_upper c

let is_digit c =
  let code = Char.code c in
  code >= 48 && code <= 57

let is_whitespace c =
  let code = Char.code c in
  List.mem code [32; 9; 10; 11; 12; 13]

let newline = "\n"

let crlf    = "\r\n"

let eof     = "\000"

let char f =
  let stream = get_stream () in
  match peek stream with
  | Some a when f a ->
    let _ = process_char stream in
    Ok a
  | _ -> Fail

let str s () =
  let s_len = String.length s in
  let stream = get_stream () in
  let chars = Stream.npeek s_len stream in
  match s_len == List.length chars with
  | true  ->
    if String.compare (stringify chars) s == 0 then
      let _ = process_chars stream s_len in
      Ok chars
    else
      Fail
  | false -> Fail

let any_char () =
  char (fun _ -> true)

let one_of cs =
  char (fun c -> List.mem c cs)

let none_of cs =
  char (fun c -> not (List.mem c cs))

let digit () =
  char (fun c -> is_digit c)

let alpha () =
  char (fun c -> is_alpha c)

let alpha_digit () =
  char (fun c -> is_alpha c || is_digit c)

let whitespace () =
  char (fun c -> is_whitespace c)

let lower () =
  char (fun c -> is_lower c)

let upper () =
  char (fun c -> is_upper c)

let spaces = many whitespace

let ignore_spaces () = spaces () |> ignore

let match_char uc () =
  char (fun c -> c == uc)

let tab = match_char '\t'

let comma = match_char ','

let l_paren = match_char '('

let r_paren = match_char ')'

let end_of_line () =
  str newline <?> str crlf

let parens e () = between l_paren r_paren e
