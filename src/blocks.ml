open Tac
open Printf

class basic_block (label : lbl) =
object (self)
  val mutable instrs : tac = []
  val mutable entrances : basic_block list = []
  val mutable exits : basic_block list = []
                                           
  method is_start =
    List.length entrances = 0

  method code =
    instrs
                              
  method get_label =
    label
      
  method predecessors : basic_block list =
    entrances                   (* is this mutable outside the object? *)
      
  method successors : basic_block list =
    exits
      
  method add_instr instr =
    instrs <- (instrs @ [instr])
                
  method add_predecessor entrance =
    if not (List.mem entrance entrances)
    then entrances <- entrance::entrances
                             
  method add_successor exit =
    exits <- exit::exits
                     
  method to_string =
    (List.fold_left
       (fun str ex -> str ^ " " ^ string_of_int ex#get_label)
       ((string_of_int label)
        ^ ":\n"
        ^ prog_to_string instrs
        ^ "Exits:")
       (List.sort
          (fun a b -> a#get_label - b#get_label)
          exits)) ^ "\n"

                
end                      


exception Unreachable of string
exception DuplicateLabel of string
                  
                                   
let rec basic_blocks (prog : tac) =

  let get_block blocks exit =
    List.find (fun block -> block#get_label = exit) blocks in

  
  let label_exists blocks label =
    List.exists (fun block -> block#get_label = label) blocks
  in
  
  let guard_unique_label current rest label =
    if label_exists (current::rest) label
    then raise (DuplicateLabel ("Label "
                                ^ (string_of_int label)
                                ^ " is not unique")) in
    
  let guard_reachable current reachable =
    if not reachable
    then raise (Unreachable ("Code after exit of block "
                             ^ (string_of_int current#get_label)
                             ^ " is unreachable")) in

  let (current, rest_blocks, _, branches) =
    List.fold_left
      (fun (current,  rest, reachable, branches) stmt ->
        (match stmt with
         | Label int -> guard_unique_label current rest int;
                        let new_block = new basic_block int in

                        (new_block,
                         rest @ [current],
                         true,
                         if reachable
                              (* last instr was not a branch *)
                         then (current, [int])::branches
                         else branches)
                          
         | Branch lbls -> guard_reachable current reachable;
                          (current, rest, false, (current, lbls)::branches)
                            
         | _ -> guard_reachable current reachable;
                current#add_instr stmt;
                (current, rest, reachable, branches)))
      
      ((new basic_block 0), [], true, [])
      prog in
  let blocks = rest_blocks @ [current] in

  
  (* Patch branches *)
  let () = List.iter (fun (block, exits) ->
               List.iter (fun exit ->
                   let next = (get_block blocks exit) in
                   block#add_successor next;
                   next#add_predecessor block)
                         exits)
                     branches in


  (* Check that all blocks are reachable *)
  let () =
    let rec get_reachable_blocks (block : basic_block) visited =
      block::
        (List.fold_left
           (fun blocks succ ->
             (get_reachable_blocks succ (block::visited)) @ blocks)
           []
           (List.filter
              (fun block -> not (label_exists visited block#get_label))
              block#successors)) in

    let reachable_blocks = get_reachable_blocks (get_block blocks 0) [] in
    
    List.iter 
      (fun block ->
        if not (label_exists reachable_blocks block#get_label)
        then raise (Unreachable ("Block "
                                 ^ (string_of_int (block#get_label)))))
      blocks in

  blocks


let to_dot (blocks : basic_block list) =
  let edges = List.fold_left (fun edges block ->
                  edges @ (List.map (fun succ -> 
                               (block#get_label, succ#get_label))
                                    block#successors))
                             []
                             blocks in

  let gen_text block =
    List.fold_left
      (fun str instr -> str ^ String.trim (stmt_to_string instr) ^ "\\n" )
      ""
      block#code in
  
  let node_string =
    List.fold_left
      (fun str block -> str ^ (string_of_int block#get_label)
                        ^ "[label=\""
                        ^ (if block#get_label = 0
                          then "start"
                          else (string_of_int block#get_label))
                        ^ "\\n"
                        ^ gen_text block ^"\"];\n")
      ""
      blocks in

  let edges_string =
    List.fold_left
      (fun str (from, t) -> str ^ (string_of_int from) ^ " -> " ^ (string_of_int t)
                            ^ ";\n")
      ""
      edges

  in "digraph {\n node[shape=box labelloc=t]\n" ^ node_string ^ edges_string ^ "}"
      
