
let first_line str =
  let pos = String.index str '\n' in
  let pos = if str.[pos] = '\r' then pos-1 else pos in
  String.sub str 0 (pos-1)

let last_line str =
  let len = String.length str in
  let ln = String.rindex str '\n' in
  let ln = if ln = len - 1 then String.rindex_from str (ln-2) '\n' else ln in
  String.sub str (ln+1) (len - ln - 1)

let decode_parts str =
  try
    let _ = Str.(search_forward (regexp "\r?\n\r?\n") str 0) in
    let header = String.sub str 0 Str.(match_beginning ()) in
    let fin = Str.(match_end ()) in
    let len = String.length str in
    let rm = if str.[len-1] = '\n' then
               if str.[len-2] = '\r' then 2 else 1 else 0
    in
    let len = len - fin - rm in
    let content = String.sub str fin len in
    let open Str in
    let content_disposition_regexp =
      regexp "Content-Disposition: \\([^;\r\n]*\\)\\(;[ ]*\\)?\\([^\r\n]*\\)"
    in
    let content_type_regexp =
      regexp "Content-Type: \\([^\r\n]*\\)"
    in
    let _ = search_forward content_disposition_regexp header 0 in
    let disposition = matched_group 1 header in
    let values = matched_group 3 header in
    let values =
      match Tiny_httpd_util.parse_query values with
      | Ok l -> List.map (fun (k,v) ->
                    let open String in
                    let k = trim k in
                    let v = trim v in
                    let len = String.length v in
                    let v =
                      if v.[0] = '"' && v.[len-1] = '"' && len > 1 then
                        String.sub v 1 (len-2)
                      else v
                    in
                    (k,v)) l
      | _ -> []
    in
    let mime_type =
      try
        let _ = search_forward content_type_regexp header 0 in
        Some (matched_group 1 header)
      with
        Not_found -> None
    in
    Some(disposition, mime_type, values, content)
  with Not_found -> None

let decode_multipart str =
  let sep = first_line str in
  let parts = Str.(split (regexp (sep ^ "\\(--\\)?\r?\n")) str) in
  List.filter_map decode_parts parts

let rec new_temp_dir ?(perm=0o700) () =
  let temp_dir = Filename.get_temp_dir_name () in
  let dirname = Filename.concat temp_dir
                  ("pynotebooktools" ^
                     (Printf.sprintf "%x" (Random.int 0x1_000_000))) in
  if Sys.file_exists dirname then new_temp_dir () else
    begin
      Sys.mkdir dirname perm;
      dirname
    end

let read_all ch =
  let buf = Buffer.create 1024 in
  try
    while true do Buffer.add_channel buf ch 1024 done;
    assert false
  with End_of_file -> close_in ch; Buffer.contents buf

let read_file fname =
  let ch = open_in fname in
  let r = read_all ch in
  close_in ch;
  r

let run_cmd cmd =
  Printf.eprintf "run: %S\n%!" cmd;
  let (stdout,stdin,stderr as chs) =
    Unix.open_process_full ("firejail " ^ cmd) (Unix.environment ())
  in
  try
    let pid = Unix.process_full_pid chs in
    let out = read_all stdout in
    let err = read_all stderr in
    close_out stdin; close_in stdout; close_in stderr;
    let (_,status) = Unix.(waitpid [WUNTRACED] pid) in
    if status <> Unix.WEXITED 0 then
      Printf.eprintf "out:\n%serr:\n%s\n%!" out err;
    (out,err,status)

  with e ->
    close_out stdin; close_in stdout; close_in stderr;
    raise e

let string_of_time time =
  let ((y,n,d),((h,m,s),_)) = Ptime.to_date_time ~tz_offset_s:Options.tz time in
  Printf.sprintf "%d/%d/%d, %d:%d:%d" d n y h m s
