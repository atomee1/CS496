(*
  Author: William Ee
  Pledge: I pledge my honor that I have abided by the Stevens Honor System.
*)

open ReM
open Dst
open Parser_plaf.Ast
open Parser_plaf.Parser
       
let rec chk_expr : expr -> texpr tea_result = function 
  | Int _n -> return IntType
  | Var id -> apply_tenv id
  | IsZero(e) ->
    chk_expr e >>= fun t ->
    if t=IntType
    then return BoolType
    else error "isZero: expected argument of type int"
  | Add(e1,e2) | Sub(e1,e2) | Mul(e1,e2)| Div(e1,e2) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    if (t1=IntType && t2=IntType)
    then return IntType
    else error "arith: arguments must be ints"
  | ITE(e1,e2,e3) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    chk_expr e3 >>= fun t3 ->
    if (t1=BoolType && t2=t3)
    then return t2
    else error "ITE: condition not boolean or types of then and else do not match"
  | Let(id,e,body) ->
    chk_expr e >>= fun t ->
    extend_tenv id t >>+
    chk_expr body
  | Proc(var,Some t1,e) ->
    extend_tenv var t1 >>+
    chk_expr e >>= fun t2 ->
    return @@ FuncType(t1,t2)
  | Proc(_var,None,_e) ->
    error "proc: type declaration missing"
  | App(e1,e2) ->
    chk_expr e1 >>=
    pair_of_funcType "app: " >>= fun (t1,t2) ->
    chk_expr e2 >>= fun t3 ->
    if t1=t3
    then return t2
    else error "app: type of argument incorrect"
  | Letrec([(_id,_param,None,_,_body)],_target) | Letrec([(_id,_param,_,None,_body)],_target) ->
    error "letrec: type declaration missing"
  | Letrec([(id,param,Some tParam,Some tRes,body)],target) ->
    extend_tenv id (FuncType(tParam,tRes)) >>+
    (extend_tenv param tParam >>+
     chk_expr body >>= fun t ->
     if t=tRes 
     then chk_expr target
     else error
         "LetRec: Type of recursive function does not match
declaration")
  | Debug(_e) ->
    string_of_tenv >>= fun str ->
    print_endline str;
    error "Debug: reached breakpoint"
(* Reference implementations start here... *)
  | NewRef(e) ->
    chk_expr e >>= fun t ->
    return (RefType t)
  | DeRef(e) ->
    chk_expr e >>= fun t ->
    begin
      match t with
      | RefType subtype -> return subtype
      | _ -> error "deref: expected reference type"
    end
  | SetRef(e1, e2) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
      begin
        match t1 with
        | RefType subtype ->
          if (subtype = t2)
            then return UnitType
          else error "setref: type mismatch"
        | _ -> error "setref: expected reference type"
      end
  | BeginEnd([]) ->
    return UnitType
  | BeginEnd(es) ->
    List.fold_left (fun _ x -> chk_expr x >>= fun t -> return t) (return UnitType) es
(* ...and end here *)

(* List implementations start here... *)
  | EmptyList(t) ->
    begin
      match t with 
      | Some _t -> return (ListType _t)
      | _ -> error "emptylist: invalid argument"
    end
  | Cons(e1, e2) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    begin
      match t2 with
      | ListType _t2 ->
        if (t1 = _t2)
          then return (ListType t1)
        else error "cons: head and tail type mismatch"
      | _ -> error "cons: second argument must be list"
    end
  | IsEmpty(e) ->
    begin
      chk_expr e >>= fun t ->
        match t with
        | ListType _ -> return BoolType
        | TreeType _ -> return BoolType
        | _ -> error "isempty: expected list or tree"
    end
  | Hd(e) ->
    begin
      chk_expr e >>= fun t ->
        match t with
        | ListType subtype -> return subtype
        | _ -> error "hd: expected list"
    end
  | Tl(e) ->
    begin
      chk_expr e >>= fun t ->
        match t with
        | ListType subtype -> return (ListType subtype)
        | _ -> error "tl: expected list"
    end
(* ...and end here *)

(* Tree implementations start here... *)
  | EmptyTree(t) ->
    begin
      match t with 
      | Some _t -> return (TreeType _t)
      | _ -> error "emptytree: invalid argument"
    end
  | Node(de, le, re) ->
    chk_expr de >>= fun tt ->
      chk_expr le >>=
      begin function
        | TreeType tl ->
          if tl <> tt
            then error "node: invalid left node type"
          else chk_expr re >>=
            begin function
              | TreeType tr ->
                if tr <> tt
                  then error "node: invalid right node type"
                else return (TreeType tt)
              | _ -> error "node: right node not tree type"
            end
        | _ -> error "node: left node not tree type"
      end
  | CaseT(target, emptycase, id1, id2, id3, nodecase) ->
    chk_expr target >>= fun t ->
      begin
        match t with
        | TreeType tt ->
          extend_tenv id1 tt >>+
          extend_tenv id2 (TreeType tt) >>+
          extend_tenv id3 (TreeType tt) >>+
          chk_expr nodecase >>= fun evalt ->
          chk_expr emptycase >>= fun emptyt ->
          if evalt = emptyt
            then return emptyt
          else error "caset: case function type mismatch"
        | _ -> error "caset: expected tree"
        end
(* ...and end here *)
  | _ -> failwith "chk_expr: implement"
and
  chk_prog (AProg(_,e)) =
  chk_expr e

(* Type-check an expression *)
let chk (e:string) : texpr result =
  let c = e |> parse |> chk_prog
  in run_teac c

let chkpp (e:string) : string result =
  let c = e |> parse |> chk_prog
  in run_teac (c >>= fun t -> return @@ string_of_texpr t)
