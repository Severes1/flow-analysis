
open Tac
open Printf
open Blocks
open Dom

let test_prog = [
    Label 1;
    Assign("x", Plus(Var "a", Var "b"));
    Branch [3];
    
    (* Label 2; *)
    Label 3;
    
    Label 4;
    Branch [5; 9; 10;];
    
    Label 5;
    
    Label 6;

    Label 7;
    Assign("a", Atom (Int 5));

    Label 8;
    Assign("y", Plus(Var "a", Var "b"));
    Branch[6; 10];
    
    Label 9;
    Assign("w", Plus(Var "a", Var "b"));
    Assign("q", Plus(Var "a", Var "b"));

    Label 10;
    (* Exit *)
  ];;

let () =
  let program = prog_to_string test_prog in
  printf "%s\n" program;;

let () =
  (* let name = Sys.argv.(1) in *)
  (* let program = compile_file_to_string name in *)
  (* let program = prog_to_string test_prog in *)
  let blocks = basic_blocks test_prog in
  let block_string = List.fold_left
                       (fun str block ->
                         str ^ "\n" ^ block#to_string)
                       ""
                       blocks
  in
  (* let cfg = Cfg.make_cfg test_prog in *)
  (* let dom = dom_tree cfg in *)
  (* let df = dom_frontier dom cfg in *)
  printf "%s\n" block_string;
  (* printf "%s\n" (to_dot blocks); *)
  (* print_dom dom; *)
  (* print_dom_frontier df *)

  let file = "flow.dot" in
  let message = to_dot blocks in
  (* Write message to file *)
  let oc = open_out file in    (* create or truncate file, return channel *)
  fprintf oc "%s\n" message;   (* write something *)
  close_out oc;                (* flush and close the channel *)

  let dom = new dom_tree (List.hd blocks) in
  printf "%s\n" (dom#to_string ^ "\n");
  List.iter
    (fun block -> printf "%s\n" ((string_of_int block#get_label) ^ ": "
                  ^ (string_of_int (dom#idom block)#get_label)))
    blocks;
  List.iter
    (fun block -> printf "%s\n" ((string_of_int block#get_label) ^ ": "
                                 ^ (List.fold_left
                                      (fun str x ->
                                        str ^ " "
                                        ^ (string_of_int (x#get_label)))
                                      ""
                                      (dom#dominance_frontier block))))
    blocks;

      List.iter
    (fun block -> printf "%s\n" ((string_of_int block#get_label) ^ ": "
                                 ^ (List.fold_left
                                      (fun str x ->
                                        str ^ " "
                                        ^ (string_of_int (x#get_label)))
                                      ""
                                      (block#predecessors))))
    blocks


         
