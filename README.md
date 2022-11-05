# Monadic Parser Combinator

A simple monadic parser combinator library with position information.

## Example

`bin/main.ml` demonstrates using the parser to parse a simple untyped lambda calculus.

```ocaml
open Chungus.Parser
open Chungus.Combinator
open Chungus.Chars

type exp =
  | Var     of position * string
  | Int     of position * int
  | Lambda  of position * exp * exp
  | Apply   of position * exp * exp

let rec show_exp e =
  match e with
  | Var (_, id) -> id
  | Int (_, i)  -> string_of_int i
  | Lambda (_, i, b) -> "λ " ^ show_exp i ^ " -> " ^ show_exp b
  | Apply (_, f, a)  -> "(" ^ show_exp f ^ ") (" ^ show_exp a ^ ")"

let identifier () =
  let  pos   = get_position () in
  let* ident = lower <@> many alpha_digit in
  return @@ Var (pos, stringify ident)

let int () =
  let pos = get_position () in
  let* i  = many1 digit in
  return @@ Int (pos, stringify i |> int_of_string)

let terminal () =
  return =<< (identifier <?> int)

let rec lambda () =
  let pos = get_position () in
  let* id = str "λ" >> spaces >> identifier in
  let* ex = spaces >> str "->" >> expression in
  return @@ Lambda (pos, id, ex)

and non_app () =
  lambda <?> terminal <?> parens expression >>= return

and app l r =
  Apply (get_position (), l, r)

and expression () = return =<< chainl (spaces >> non_app) (spaces >> lift app)

let () =
  from_string "( λ chrisNevers -> chrisNevers (λ shamone -> hehe) ) (λ whenTheImposterIsSus -> 54235)";
  match expression () with
  | Ok e -> print_endline (show_exp e)
  | _    -> print_endline "Failed to parse expression"

```
