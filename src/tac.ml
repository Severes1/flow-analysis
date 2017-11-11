(* Specification of the Three-Address-Code IR *)

open Printf

type id = string
type lbl = int

type atom =
  | Int of int
  | Var of id

type exp =
  | Atom of atom
  | Plus of atom * atom
  | Times of atom * atom

type stmt =
  | Assign of id * exp
  | Branch of lbl list
  | Label of lbl                (* Label 0 is reserved as start. *)
                                (* Label ~-1 is reserved as unreachable *)

type tac = stmt list

let atom_to_string atom =
  match atom with
  | Int int -> string_of_int int
  | Var id -> id

                
let exp_to_string exp =
  match exp with
  | Atom(atom) -> atom_to_string atom
  | Plus(e1, e2) -> (atom_to_string e1) ^ " + " ^ (atom_to_string e2)
  | Times(e1, e2)-> (atom_to_string e1) ^ " * " ^ (atom_to_string e2)
                                                   
let stmt_to_string stmt =
  match stmt with
  | Assign(id, exp) ->
      let exp_str = exp_to_string exp
      in "    " ^ id ^ " := " ^ exp_str
  (* | Jump(lbl) -> "    goto " ^ (string_of_int lbl) *)
  | Label(lbl) -> string_of_int lbl ^ ":"
  | Branch (lbl::[]) -> "    goto " ^ (string_of_int lbl)
  | Branch lbls -> List.fold_left
                     (fun acc lbl -> acc ^ string_of_int lbl ^ " ")
                     "    branch "
                     lbls


let prog_to_string prog =
  List.fold_left (fun str stmt -> str ^ (stmt_to_string stmt) ^ "\n")
                 ""
                 prog

                 
let test_prog : tac = [
    Label 1;
    Assign("x", Plus(Var "y", Var "z"));
    Assign("a", Plus(Var "x", Int 2));
    Label 2;
    Assign("b", Plus(Var "y", Var "z"));
    Assign("a", Plus(Var "1", Int 2));
    Assign("x", Atom(Int 5));
    Branch [1; 2]
  ];;
                 
(* let () =  *)
(*      printf "%s\n" (prog_to_string test_prog) *)
