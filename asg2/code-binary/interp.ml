(* $Id: interp.ml,v 1.8 2020-01-24 11:42:24-08 - - $ *)

open Absyn

exception Unimplemented of string
let no_expr reason = raise (Unimplemented reason)
let no_stmt reason continuation = raise (Unimplemented reason)

let want_dump = ref false 

let eval_memref (memref : Absyn.memref) = match memref with
    | Arrayref (ident, expr) -> 0.1
    | Variable (ident)-> 0.2 

let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number
    | Memref memref -> (match memref with
                       | Arrayref (ident, expr) -> Array.get 
                       (Hashtbl.find Tables.array_table ident) (int_of_float (eval_expr expr))
                       | Variable (ident)-> Hashtbl.find Tables.variable_table ident)
    | Unary (oper, expr) -> (Hashtbl.find Tables.unary_fn_table oper) (eval_expr expr)
    | Binary (oper, expr1, expr2) -> (Hashtbl.find Tables.binary_fn_table oper) 
    (eval_expr expr1) (eval_expr expr2)

let rec interpret (program : Absyn.program) =
    match program with
    | [] -> ()
    | firstline::continuation -> match firstline with
      | _, _, None -> interpret continuation
      | _, _, Some stmt -> (interp_stmt stmt continuation)

and interp_stmt (stmt : Absyn.stmt) (continuation : Absyn.program) =
    match stmt with
    | Dim (ident, expr) -> interp_dim ident expr continuation
    | Let (memref, expr) -> interp_let memref expr continuation
    | Goto label -> interp_goto label continuation
    | If (expr, label) -> interp_if expr label continuation
    | Print print_list -> interp_print print_list continuation
    | Input memref_list -> interp_input memref_list continuation
 
and interp_let (memref : Absyn.memref)
               (expr : Absyn.expr)
               (continuation : Absyn.program) =
    (let v = (eval_expr expr)
    in match memref with
       | Arrayref (ident, expr) -> Array.set (Hashtbl.find Tables.array_table ident) 
       (int_of_float (eval_expr expr)) v
       | Variable (ident)-> Hashtbl.add Tables.variable_table ident (eval_expr expr));
    interpret continuation

and interp_dim (ident : string)
               (expr : Absyn.expr)
               (continuation : Absyn.program) = 
    Hashtbl.add Tables.array_table ident 
    (Array.make ((int_of_float (eval_expr expr)) + 1) 0.0);
    interpret continuation

and interp_goto (label : string)
                (continuation : Absyn.program) =
    interpret (Hashtbl.find Tables.label_table label)


and interp_if (expr : Absyn.expr)
              (label : string)
              (continuation : Absyn.program) =
    let v = (eval_expr expr) in
        if v > 0. then 
           interp_stmt (Goto label) continuation
        else 
           interpret continuation

and interp_print (print_list : Absyn.printable list)
                 (continuation : Absyn.program) =
    let print_item item =
        (print_string " ";
         match item with
         | String string ->
           let regex = Str.regexp "\"\\(.*\\)\""
           in print_string (Str.replace_first regex "\\1" string)
         | Printexpr expr ->
           print_float (eval_expr expr))
    in (List.iter print_item print_list; print_newline ());
    interpret continuation

and interp_input (memref_list : Absyn.memref list)
                 (continuation : Absyn.program)  =
    let input_number memref =
        try  let number = Etc.read_number ()
             in ((match memref with
                | Arrayref (i, e) ->
                    let arr = Hashtbl.find Tables.array_table i in
                    let idx = int_of_float (eval_expr e) in
                    Array.set arr idx number
                | Variable i -> Hashtbl.add Tables.variable_table i number);
                (*print_float number;
                print_newline ()*))
        with End_of_file -> 
             (Hashtbl.replace Tables.variable_table "eof" 1.0;
              print_string "End_of_file"; print_newline ())
    in List.iter input_number memref_list;
    interpret continuation
 
let interpret_program program =
    (Tables.init_label_table program; 
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
     interpret program)

