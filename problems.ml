(* https://ocaml.org/problems *)

(* 1. Tail of a List  *)
let rec last = function 
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t
;;

(* 2. Last Two Elements of a List  *)
let rec last_two = function
    | [] | [_] -> None
    | [x; y] -> Some (x,y)
    | _ :: t -> last_two t
;;

(* 3. N'th Element of a List  *)
let nth n lst  =
  let rec loop lst n idx =
    match lst with
    | [] -> None
    | x :: tail -> 
        if n = idx then
          Some x
        else 
          loop tail n (idx+1)
  in
  loop lst n 0
;;

(* 4. Length of a List *)
let len lst = 
  let rec loop lst n =
    match lst with
    | [] -> n
    | _ :: tail -> loop tail (n+1)
  in
  loop lst 0
;;

(* 5. Reverse a List *)
let rev lst =
  let rec loop input output =
    match input with
    | head :: tail -> loop (tail) (head :: output)
    | [] -> output
  in
  loop lst []
;;

(* 6. Palindrome *)
let is_palindrome lst =
  lst = List.rev lst
;;

(* 7. Flatten a List *)
type 'a node =
  | One of 'a 
  | Many of 'a node list

let flatten nodes =
  let rec loop acc = function 
    | One one :: tail -> 
        loop (one :: acc) tail
    | Many many :: tail ->
        loop (loop acc many) tail
    | [] -> acc
  in
  loop [] nodes
  |> List.rev
;;

(* 8. Eliminate Duplicates *)
let compress lst =
  let rec loop acc last = function
    | [] -> acc
    | head :: tail -> 
        match last with
        | Some last -> 
          if head <> last then
            loop (head :: acc) (Some head) tail
          else
            loop acc (Some head) tail
        | None -> loop (head :: acc) (Some head) tail
  in
  loop [] None lst
  |> List.rev
;;

(* 9. Pack Consecutive Duplicates *)
let pack lst =
  let rec loop acc total last = function 
    | [] -> (acc :: total)
    | head :: tail ->
        match last with
        | Some last ->
            if head = last then
              loop (head :: acc) total (Some head) tail
            else
              loop [head] (acc :: total) (Some head) tail
        | None ->
            loop [head] total (Some head) tail
  in
  loop [] [] None lst
  |> List.rev
;;

(* 10. Run-Length Encoding *)
let encode lst =
  let rec loop (acc: int) (total: (int * 'a) list) last = function 
    | [] -> 
        (match last with
        | Some last ->
          ((acc, last) :: total)
        | None ->
            [])
    | head :: tail ->
        match last with
        | Some last ->
            if head = last then
              loop (1 + acc) total (Some head) tail
            else
              let pair = (acc, last) in
              loop 1 (pair :: total) (Some head) tail
        | None ->
            loop 1 total (Some head) tail
  in
  loop 0 [] None lst
  |> List.rev
;;

(* 11. Modified Run-Length Encoding *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode2 lst =
  let create count item =
    match count with
    | 1 -> One item
    | _ -> Many (count, item)
  in
  let rec loop (acc: int) total last = function 
    | [] -> 
        (match last with
        | Some last ->
          let item = create acc last in
          (item :: total)
        | None ->
            [])
    | head :: tail ->
        match last with
        | Some last ->
            if head = last then
              loop (1 + acc) total (Some head) tail
            else
              let item = create acc last in
              loop 1 (item :: total) (Some head) tail
        | None ->
            loop 1 total (Some head) tail
  in
  loop 0 [] None lst
  |> List.rev
;;

(* 12. Decode a Run-Length Encoded List *)
let decode (lst: 'a rle list) =
  let extract = function
    | One one -> [one]
    | Many (count, item) -> List.init count (fun _ -> item)
  in
  lst
  |> List.map extract
  |> List.flatten
;;

(* 13. Run-Length Encoding of a List (Direct Solution) *)
let encode list =
    let rle count x = if count = 0 then One x else Many (count + 1, x) in
    let rec aux count acc = function
      | [] -> [] (* Can only be reached if original list is empty *)
      | [x] -> rle count x :: acc
      | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                              else aux 0 (rle count a :: acc) t
    in
      List.rev (aux 0 [] list)
;;

(* 14. Duplicate the Elements of a List *)
let duplicate lst =
  let rec loop acc = function
    | head :: tail ->
        loop (head :: head :: acc) tail
    | [] -> acc
  in
  List.rev (loop [] lst)
;;

(* 15. Replicate the Elements of a List a Given Number of Times *)
let replicate lst n =
  let rep item n = 
    List.init n (fun _ -> item)
  in
  let rec loop acc = function 
    | head :: tail -> 
        let copies = rep head n in
        loop (copies @ acc) tail
    | [] -> acc
  in
  List.rev (loop [] lst)
;;

let () =
  ()
