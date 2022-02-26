(** Problem 1 *) 

(** returns x^n where n is any non-negative integer and x is an integer *)

let rec pow x n =
if n = 0 then 1
else if n = 1 then x
else x * pow x (n-1);;

(**returns x^n where n is any non-negative integer and x is a float*)
let rec float_pow x n =
if n = 0 then 1.0
else if n = 1 then x
else x *. float_pow x (n-1);;

(** Problem 2 *)

(** Removes the head of the given list *)
let remove_head lst = function
| [] -> []
| h::t -> t;;

(** returns a list of any type without consecutive matches *)
let rec compress lst = match lst with
| [] -> []
| [x] -> [x]
| h::t -> if h = List.hd t then compress t
else h::(compress t);;


