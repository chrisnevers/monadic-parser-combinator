exception StreamNotSet

let stringify cs =
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) cs;
  Buffer.contents buf

(* Result types *)
type 'a result =
  | Ok  of 'a
  | Fail
  | Fatal of string

(* Monad definitions *)
let bind x f =
  match x () with
  | Ok r    -> f r
  | Fail    -> Fail
  | Fatal s -> Fatal s

let return a = Ok a

let ( >>= )  x f = bind x f
let ( =<< )  x f = bind f x

let ( let* ) x f = bind x f

let ( <?> )  x f =
  fun () ->
    match x () with
    | Ok a    -> Ok a
    | Fail    -> f ()
    | Fatal s -> Fatal s

let ( >> ) (x: unit -> 'a result) (f: unit -> 'b result) =
  fun () ->
    x >>= fun _ -> f ()

let ( << ) x f = f >> x

let ( <@> ) l r =
  fun () ->
    let* start = l in
    let* rest  = r in
    return (start :: rest)

let const x () = return x

let lift f =
  fun () -> return f

(* Operate on streams *)
let stream = ref None

type position = {
  line : int;
  col  : int;
}

let show_position pos =
  "(" ^ string_of_int pos.line ^ ", " ^ string_of_int pos.col ^ ")"

let position = ref {
  line = 0;
  col  = 0;
}

let make _stream =
  stream := Some _stream

let from_channel chan =
  stream := Some (Stream.of_channel chan)

let from_string str =
  stream := Some (Stream.of_string str)

let get_stream () =
  match !stream with
  | None   -> raise StreamNotSet
  | Some a -> a

let new_line pos =
  pos := {
    line = !pos.line + 1;
    col = 0
  }

let incr_col pos =
  pos := {
    line = !pos.line;
    col  = !pos.col + 1;
  }

let get_position () = !position

let process_char stream =
  let chr = Stream.next stream in
  if chr == '\n' then
    new_line position
  else
    incr_col position

let rec process_chars stream = function
  | 0 -> ()
  | n ->
    process_char stream;
    process_chars stream (n - 1)
