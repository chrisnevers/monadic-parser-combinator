open Parser

let many p () =
  let rec aux () =
    match p () with
    | Ok e    ->
      let* rst = aux in
      return (e :: rst)
    | _ -> return []
  in
  aux ()

let many1 p () =
  let rec aux () =
    match p () with
    | Ok e    ->
      let* rst = aux in
      return (e :: rst)
    | _ -> return []
  in
  let* fst = p in
  let* rst = aux in
  return (fst :: rst)

let rec choice ps =
  match ps with
  | [] -> Fail
  | h :: t ->
    match h () with
    | Ok a -> Ok a
    | _    -> choice t

let count i p =
  let rec aux i () =
    match i with
    | 0 -> return []
    | n ->
      let* a = p in
      let* rst = aux (n - 1) in
      Ok (a :: rst)
  in
  aux i ()

let sepBy p sep =
  let rec aux () =
    match p () with
    | Ok e ->
      begin match sep () with
      | Ok _ ->
        let* rst = aux in
        return (e :: rst)
      | _ -> return [e]
      end
    | _ -> return []
  in
  aux ()

let between s e p =
  let* _  = s in
  let* p' = p in
  let* _  = e in
  return p'

let chainl p op () =
  let rec aux x : 'a result =
    let work () =
      let* f = op in
      let* y = p in
      aux (f x y)
    in
    let* res = work <?> const x in
    return res
  in
  let* fst = p in
  aux fst
