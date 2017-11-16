open Blocks
open Printf
       
class dom_tree (start_node : basic_block) =
object(self)
  val dom_table = Hashtbl.create 50
  val dom_frontier = Hashtbl.create 50 
                                 
  val all_nodes =
      let rec all_nodes accum curr =
        let new_succs = Util.diff curr#successors accum in
        match new_succs with
        | [] -> accum
        | _ ->  List.fold_left
                  (fun accum succ -> all_nodes accum succ)
                  (List.append new_succs accum)
                  new_succs
      in all_nodes [start_node] start_node 

  (* To be populated at iniatialize *)
  val mutable all_node_labels = []

  method private label_to_block label =
    List.find (fun (x : basic_block) -> x#get_label = label)
              all_nodes

  method idom (block : basic_block) : basic_block =
    let doms = Hashtbl.find dom_table block#get_label in
    match doms with
    | [] -> block
    | _ -> 
       let idoms = List.fold_left
                     Util.diff
                     doms
                     (List.map (Hashtbl.find dom_table) doms)
       in
       let idom_label = List.hd idoms in
       self#label_to_block idom_label

  method does_dominate (a:basic_block) (b:basic_block) : bool =
    List.mem a#get_label
             (Hashtbl.find dom_table b#get_label)

  method dominance_frontier (block : basic_block) : basic_block list =
    List.map self#label_to_block
             (Hashtbl.find dom_frontier
                           block#get_label)

  method private setup =
    Hashtbl.replace dom_table
                start_node#get_label
                [];
    List.iter
      (fun node -> if node != start_node#get_label
                   then Hashtbl.replace dom_table node all_node_labels)
      all_node_labels;

    List.iter
      (fun node -> Hashtbl.replace dom_frontier node [])
      all_node_labels
                

  method private compute_dom_tree =
    
    let lookup_doms (node_label : Tac.lbl) : Tac.lbl list =
      Hashtbl.find dom_table node_label in

    let rec process_node node =
      let label = node#get_label in
      (* There must be at least one if we're not start *)
      let pred::preds = List.map (fun node -> node#get_label)
                                 node#predecessors in
      let old_dominators = lookup_doms node#get_label in
      let dominators : Tac.lbl list =
        List.fold_left
          (fun doms pred ->
            Util.intersect doms
                      (pred::(lookup_doms pred)))
          (pred::(lookup_doms pred))
          preds in
      
      Hashtbl.replace dom_table
                  label
                  dominators;

      if (List.length (Util.intersect old_dominators dominators))
         < (List.length old_dominators)
      then List.iter process_node node#successors in

    List.iter process_node start_node#successors

  method private compute_dominance_frontier =
    List.iter
      (fun (node : basic_block) ->
        let node_dominators = Hashtbl.find dom_table
                                           node#get_label in
        List.iter
          (fun pred ->
            let pred_dominators = Hashtbl.find dom_table
                                               pred#get_label in
            (List.iter
               (fun (ancestor : Tac.lbl) ->
                 let old_frontier = Hashtbl.find dom_frontier ancestor in 
                 if not (List.mem node#get_label old_frontier)
                 then Hashtbl.replace dom_frontier
                                      ancestor
                                      (node#get_label::old_frontier))

               (Util.diff (pred#get_label::pred_dominators)
                          node_dominators)))
          node#predecessors)
      all_nodes
              
  initializer all_node_labels <- (List.map (fun node -> node#get_label) all_nodes);
              self#setup;
              self#compute_dom_tree;
              self#compute_dominance_frontier
                 
  method to_string : string  =
    let hashtbl_fold_in_order f tbl base =
      let keys = Hashtbl.fold
                   (fun key value acc ->
                     if List.mem key acc
                     then acc
                     else key::acc)
                   tbl
                   [] in
      List.fold_left
        (fun acc key -> f key (Hashtbl.find tbl key) acc)
        base
        (List.sort
           (fun a b -> a - b)
           keys) in

    hashtbl_fold_in_order (fun node_label doms str ->
        str ^ "\n" ^
        (string_of_int node_label) ^ ": " ^
        match doms with
        | [] -> "[]" 
        | _ ->
          (List.fold_left (fun str node_label -> str ^ ", " ^ (string_of_int node_label))
                          (string_of_int (List.hd doms))
                          (List.tl(doms))))
                          dom_table
                          ""
end
