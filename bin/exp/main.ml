[@@@warning "-20..70"]

module Eff : sig
  type db_type =
    | Nil
    | String of string

  type column_name = string
  type query = (column_name * db_type) list
  type query_result = db_type list

  val find_one : string -> query -> query_result
  val find_all : string -> query -> query_result list

  type _ Effect.t +=
    | QueryOne : (query * string) -> query_result Effect.t
    | QueryMultiple : (query * string) -> query_result list Effect.t
end = struct
  open! Effect
  open! Effect.Deep

  type db_type =
    | Nil
    | String of string

  type column_name = string
  type query_result = db_type list
  type query = (column_name * db_type) list

  type _ Effect.t +=
    | QueryOne : (query * string) -> query_result Effect.t
    | QueryMultiple : (query * string) -> query_result list Effect.t

  let find_one tbl string_list = perform (QueryOne (string_list, tbl))
  let find_all tbl string_list = perform (QueryMultiple (string_list, tbl))
end

module Handler : sig
  type record = (string * Eff.db_type) list
  type db = (string * record list) list
  type table_name = string

  (* NOTE: Abstrair db_type e usar GADT talvez seja melhor. *)
  val filter : Eff.query -> table_name -> db -> Eff.query_result list
  val find : Eff.query -> table_name -> db -> Eff.query_result
  val compute : db -> (unit -> unit) -> unit
end = struct
  open! Effect
  open! Effect.Deep
  open Eff

  type table_name = string
  type record = (string * db_type) list

  (* Map which keys are table names and the values are lists of maps
     which keys are columns and the values are db_type *)
  type db = (string * record list) list

  let equal type1 type2 =
    match type1, type2 with
    | Nil, Nil -> true
    | String x, String y -> String.equal x y
    | _, _ -> false
  ;;

  let get_columns (columns : string list) (record : record) : db_type list =
    List.map
      (fun key ->
        match List.assoc_opt key record with
        | Some result -> result
        | None -> failwith "get columns")
      columns
  ;;

  let compare_dbtype_list (list1 : db_type list) (list2 : db_type list) =
    List.fold_left2 (fun acc vl1 vl2 -> acc && equal vl1 vl2) true list1 list2
  ;;

  (* TODO: Usar o functor Map *)

  let find query table_name db =
    let table = List.assoc table_name db in
    let keys : string list = List.map (fun (k, _) -> k) query in
    let values : db_type list = List.map (fun (_, v) -> v) query in
    let result =
      List.find
        (fun record ->
          let columns = get_columns keys record in
          compare_dbtype_list columns values)
        table
    in
    result |> List.map (fun (_, v) -> v)
  ;;

  let filter query table_name db =
    let table = List.assoc table_name db in
    let keys : string list = List.map (fun (k, _) -> k) query in
    let values : db_type list = List.map (fun (_, v) -> v) query in
    let result =
      List.filter
        (fun record ->
          let columns = get_columns keys record in
          compare_dbtype_list columns values)
        table
    in
    List.map (fun l -> List.map (fun (_, v) -> v) l) result
  ;;

  let compute (db : db) f =
    (* TODO: Trocar por match_with *)
    try_with
      f
      ()
      { effc =
          (fun (type a) (eff : a t) ->
            match eff with
            | QueryOne (query, table_name) ->
              let result = find query table_name db in
              Some (fun (k : (a, _) continuation) -> continue k result)
            | QueryMultiple (query, table_name) ->
              let result = filter query table_name db in
              Some (fun (k : (a, _) continuation) -> continue k result)
            | _ -> None)
      }
  ;;
end

module App = struct
  open Eff

  let str = function
    | Nil -> failwith "meh"
    | String string -> string
  ;;

  let logic () =
    let result = find_one "hokages" [ "name", String "Naruto" ] in
    let favorite_food = List.hd @@ List.tl result in
    print_endline @@ str favorite_food
  ;;
end

let () =
  let db =
    [ "hokages", [ [ "name", Eff.String "Naruto"; "favorite_food", Eff.String "ramen" ] ]
    ]
  in
  Handler.compute db App.logic
;;
