open Yojson

exception NotANoteBook of string
let notANoteBook s = raise (NotANoteBook s)

let files = ref []
let markdown = ref true

let spec = [
    ("-r", Arg.Clear markdown, "forget markdown cells")
  ]
let anon_fun = fun fname -> files := fname :: !files
let usage = Printf.sprintf "usage: %s files ..." Sys.argv.(0)

let _ = Arg.parse spec anon_fun usage

type cellType = Markdown | Code

let remove_final_newline s =
  let len = String.length s in
  if len > 0 && s.[len-1] = '\n' then
    String.sub s 0 (len - 1)
  else s

let eval_inline_re = Re.Posix.(compile (re  "[{][|](([\n]|[^|]|([|][^}]))+)[|][}]"))

let treat_cell lines out json =
  let (ty, metadata, src) =
    let ty =
      try
        match List.assoc  "cell_type" json with
        | `String "markdown" -> Markdown
        | `String "code" -> Code
        | (s:Basic.t) -> notANoteBook (Printf.sprintf "Bad cell type: %s"
                                         (Basic.to_string s))
      with Not_found ->
        Printf.eprintf "%s\n%!" (Basic.to_string (`Assoc json));
        notANoteBook "missing cell_type"
    in
    let metadata =
      try
        match List.assoc "metadata" json with
          `Assoc m -> m
        | _ -> raise Not_found
      with Not_found -> []
    in
    let src =
      try
        match List.assoc "source" json with
        | `List l ->
           let fn = function `String s -> remove_final_newline s | _ -> raise Not_found in
           List.map fn l
        | `String s -> String.split_on_char '\n' s
        | _ -> raise Not_found
      with Not_found ->
        Printf.eprintf "%s\n%!" (Basic.to_string (`Assoc json));
        notANoteBook "missing source"
    in
    let src = List.flatten (List.map (String.split_on_char '\n') src) in
    (ty, metadata, src)
  in
  match ty with
  | Code ->
     lines := List.rev_append src !lines;
     lines := "" :: !lines;
  | Markdown when !markdown ->
     let src_total = String.concat "\n" src in
     let eval_inline = Re.all eval_inline_re src_total in
     List.iter (fun g ->
         Printf.fprintf out "%s\n%!" (String.trim (Re.Group.get g 1)))
       eval_inline;
     lines := List.rev_append (List.map (fun (k,v) ->
                                   let v = Basic.to_string v in
                                   Printf.sprintf "#$ %s: %s" k v) metadata) !lines;
     lines := List.rev_append (List.map (Printf.sprintf "## %s") src) !lines;
     lines := "" :: !lines;
  | Markdown -> ()

let treat_file fname =
  try
    let bname =
      match Filename.chop_suffix_opt ~suffix:".ipynb" fname with
      | None -> notANoteBook "no extension ipynb"
      | Some s -> s
    in
    let ch = open_in fname in
    let json = Basic.from_channel ch in
    close_in ch;
    let dico = match json with
      | `Assoc d -> d
      | _ -> failwith "bad notebook"
    in
    let metadata =
      try
        match List.assoc "metadata" dico with
        | `Assoc d -> d
        | _ -> raise Not_found
      with
        Not_found -> []
    in
    let userdata =
      try
        match List.assoc "userdata" metadata with
        | `Assoc d -> d
        | _ -> raise Not_found
      with
        Not_found -> []
    in
    let seed =
      try
        match List.assoc "seed" metadata with
        | `Int n -> n
        | _ -> raise Not_found
      with Not_found -> 0
    in
    let cells =
      try
        match List.assoc "cells" dico with
        | `List l ->
           let fn = function
               `Assoc d -> d | _ -> raise Not_found
           in
           List.map fn l
        | `Assoc d -> [d]
        | _ -> raise Not_found
      with Not_found ->
        Printf.eprintf "%s\n%!" (Basic.to_string json);
        notANoteBook "No cells at top"
    in
    let out = open_out (bname ^ ".py") in
    let lines = ref [] in
    Printf.fprintf out "#SEED: %d\n\
                        #HEAD\n\
                        try:\n  if type(seed) != int: seed=%d\n\
                        except NameError: seed=%d\n\
                        import random\n\
                        random.seed(seed)\n%!" seed seed seed;
    List.iter (fun (k,v) ->
        let v = match v with
            | `Int n -> string_of_int n
            | `String s -> Printf.sprintf "%S" s
            | `Bool b -> if b then "True" else "False"
            | `Float f -> string_of_float f
            | `Null -> "None"
            | _ -> failwith "Bad notebook"
        in
        Printf.fprintf out "%s = %s #USERDATA\n%!" k v) userdata;
    List.iter (treat_cell lines out) cells;
    Printf.fprintf out "#HEAD\n";
    List.iter (Printf.fprintf out "%s\n%!") (List.rev !lines);
    close_out out

  with
    e -> Printf.eprintf "Can not translate file: %s (%s)"
           fname
           (Printexc.to_string e);
         exit 1

let _ = List.iter treat_file !files
