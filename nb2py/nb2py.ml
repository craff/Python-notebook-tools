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

let treat_cell out json =
  let (ty, src) =
    let ty =
      try
        match List.assoc  "cell_type" json with
        | `String "markdown" -> Markdown
        | `String "code" -> Code
        | (s:Basic.t) -> notANoteBook (Printf.sprintf "Bad cell type: %s"
                                         (Basic.to_string s))
      with Not_found -> notANoteBook "missing cell_type"
    in
    let src =
      try
        match List.assoc "source" json with
        | `List l ->
           let fn = function `String s -> s | _ -> raise Not_found in
           List.map fn l
        | _ -> raise Not_found
      with Not_found -> notANoteBook "missing source"
    in
    (ty, src)
  in
  match ty with
  | Code ->
     List.iter (Printf.fprintf out "%s%!") src;
     Printf.fprintf out "\n\n%!"
  | Markdown when !markdown ->
     List.iter (Printf.fprintf out "## %s%!") src;
     Printf.fprintf out "\n\n%!"
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
    let cells =
      try
        match json with
        | `Assoc dico ->
           begin
             match List.assoc "cells" dico with
               `List l ->
                let fn = function
                    `Assoc d -> d | _ -> raise Not_found
                in
                List.map fn l
             | _ -> raise Not_found
           end
        | _ -> raise Not_found
      with Not_found -> notANoteBook "No cells at top"
    in
    let out = open_out (bname ^ ".py") in
    List.iter (treat_cell out) cells;
    close_out out

  with
    e -> Printf.eprintf "Can not translate file: %s (%s)"
           fname
           (Printexc.to_string e);
         exit 1

let _ = List.iter treat_file !files
