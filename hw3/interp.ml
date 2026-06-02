(*
  William Ee
  I pledge my honor that I have abided by the Stevens Honor System.
*)

open Parser_plaf.Ast
open Parser_plaf.Parser
open Ds
    
(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> exp_val ea_result =
  fun e ->
  match e with
  | Int(n) ->
    return (NumVal n)
  | Var(id) ->
    apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1+n2))
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1-n2))
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1*n2))
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return (NumVal (n1/n2))
  | Let(id,def,body) ->
    eval_expr def >>= 
    extend_env id >>+
    eval_expr body 
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b 
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return (BoolVal (n = 0))
  | Debug(_e) ->
    string_of_env >>= fun str ->
    print_endline str; 
    error "Debug called"

(* Added code starts here... *)

  | EmptyTree _ -> return (TreeVal Empty)
  | Node (e1, e2, e3) ->
      eval_expr e1 >>= fun v1 ->
      eval_expr e2 >>= fun v2 ->
      eval_expr e3 >>= fun v3 ->
      if is_tree v2 && is_tree v3 then
        return (TreeVal (Node (v1, v2, v3)))
      else
        error "Node - Children must be trees!"
  | IsEmpty e ->
      eval_expr e >>= fun v ->
      (match v with
       | TreeVal Empty -> return (BoolVal true)
       | TreeVal _ -> return (BoolVal false)
       | _ -> error "IsEmpty - Not a tree!")
  | CaseT (e1, e2, id1, id2, id3, e3) ->
      eval_expr e1 >>= fun v ->
      (match v with
       | TreeVal Empty -> eval_expr e2
       | TreeVal (Node (val1, left, right)) ->
           extend_env id1 val1 >>= fun () ->
           extend_env id2 (TreeVal left) >>= fun () ->
           extend_env id3 (TreeVal right) >>= fun () ->
           eval_expr e3
       | _ -> error "CaseT - Not a tree!")
  | Record fs ->
      eval_exprs (List.map second fs) >>= fun vals ->
      let fields = List.map first fs
      in
        if dupe_check (List.combine fields vals)
          then error "Record - Duplicate fields!"
        else
          return (RecordVal (List.combine fields vals))
  | Proj (e, id) ->
      eval_expr e >>= fun v ->
      (match v with
       | RecordVal fields -> (
           match lookup id fields with
           | Some value -> return value
           | None -> error "Proj - Nonexistent field!"
         )
       | _ -> error "Proj - Expected a record!")
and
  eval_exprs : expr list -> (exp_val list) ea_result =
  fun es ->
  match es with
  | [] -> return []
  | h::t -> eval_expr h > >= fun i ->
    eval_exprs t > >= fun l ->
    return (i::l)

(* ...and ends here *)

(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e

(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_prog
  in run c
  


