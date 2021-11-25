open Yojson

exception BadPython of string
let badPython s = raise (BadPython s)

let files = ref []
let markdown = ref true

let spec = []
let anon_fun = fun fname -> files := fname :: !files
let usage = Printf.sprintf "usage: %s files ..." Sys.argv.(0)

let _ = Arg.parse spec anon_fun usage

type out  = All | Corrige | Question
type cell = Markdown of string list | Code of string list | None

let strip_dashes l =
  let len =  String.length l in
  if len < 3 then "" else String.sub l 3 (len - 3)

let is_empty s =
  let len = String.length s in
  try
    for i = 0 to len - 1 do
      if s.[i] <> ' ' || s.[i] <> '\t' then raise Exit
    done;
    true
  with Exit -> false

let starts_with ~prefix:p s =
  let l = String.length s in
  let i = ref 0 in
  while !i < l && (s.[!i] = ' ' || s.[!i] = '\t') do incr i done;
  let i = !i in
  let lp = String.length p in
  if l - i < lp then false
  else
    try
      for j = 0 to lp - 1 do
        if s.[i + j] <> p.[j] then raise Exit
      done;
      true
    with Exit -> false

let json_string_list l =
  List.rev (
      List.mapi (fun i l ->
          let l = if i = 0 then l else l ^ "\n" in
          `String l) l)

let output_python ch cells =
  let fn = function
    | Code l ->
       let src = json_string_list l in
       `Assoc [ "cell_type", `String "code"
              ; "source", `List src
              ; "execution_count", `Null
              ; "metadata", `Assoc []
              ; "outputs", `List [] ]
    | Markdown l ->
       let src = json_string_list l in
       `Assoc [ "cell_type", `String "markdown"
              ; "source", `List src
              ; "metadata", `Assoc [] ]
    | None -> assert false
  in
  let cells = List.map fn cells in
  let info : t = `Assoc
    [ "codemirror_mode", `Assoc [ "name", `String "ipython"
                                ; "version", `Int 3]
    ; "file_extension", `String ".py"
    ; "mime_type", `String "text/x-python"
    ; "name", `String "python"
    ; "nbconvert_exporter", `String "python"
    ; "pygments_lexer", `String "ipython3"
    ; "verstion", `String "3.8.6" ]
  in
  let metadata : t = `Assoc
    [ "kernelspec", `Assoc [ "display_name", `String "Python 3"
                           ; "language", `String "python"
                           ; "name", `String "python3" ]
    ; "language_info", info ]
  in
  let infos = [ "metadata", metadata
              ; "nbformat", `Int 4
              ; "nbformat_minor", `Int 2 ]
  in
  to_channel ch (`Assoc (("cells", `List cells) :: infos))

let treat_file out_mode fname =
  try
    let bname =
      match Filename.chop_suffix_opt ~suffix:".py" fname with
      | None -> badPython "no extension .py"
      | Some s -> s
    in
    let ch = open_in fname in
    let oname = if out_mode = Corrige then bname ^ "_corrige.ipynb"
                else bname ^ ".ipynb"
    in
    let out = open_out oname in
    let cells =
      let mode = ref (None:cell) in
      let cur = ref All in
      let cells = ref [] in
      let line_num = ref 0 in
      let write_cell () = if !mode <> None then cells := !mode :: !cells; in
      try
        while true do
          let line = input_line ch in
          incr line_num;
          if starts_with ~prefix:"#CORRIGE" line then
            begin
              if !cur != All then
                badPython (Printf.sprintf "line %d: bad #CORRIGE" !line_num);
              cur := Corrige
            end
          else if starts_with ~prefix:"#QUESTION" line then
            begin
              if !cur != All then
                badPython (Printf.sprintf "line %d: bad #QUESTION" !line_num);
              cur := Question
            end
          else if starts_with ~prefix:"#FIN" line then
            begin
              if !cur = All then
                badPython (Printf.sprintf "line %d: bad #FIN" !line_num);
              cur := All
            end
          else
            begin
              let is_md = String.length line >= 2 && line.[0] = '#' && line.[1] = '#' in
              let line = if is_md then strip_dashes line else line in
              match (is_md, !mode) with
              | true , Markdown _ when line = "" -> ()
              | true , Markdown l -> mode := Markdown (line :: l)
              | false, Code     l -> if !cur = All || out_mode = !cur then
                                       mode := Code (line :: l)
              | _    , _          -> write_cell ();
                                     if not (is_empty line) then
                                       mode := if is_md then Markdown [line]
                                               else Code [line]
                                     else mode := None
            end
        done;
        assert false
      with End_of_file ->
        close_in ch;
        write_cell ();
        List.rev !cells
    in
    output_python out cells
  with
    e -> Printf.eprintf "Can not translate file: %s (%s)"
           fname
           (Printexc.to_string e);
         exit 1

let _ =
  List.iter (treat_file Corrige) !files;
  List.iter (treat_file Question) !files
