
let first_line str =
  let pos = String.index str '\n' in
  let pos = if str.[pos] = '\r' then pos-1 else pos in
  String.sub str 0 (pos-1)

let last_line str =
  let len = String.length str in
  let ln = String.rindex str '\n' in
  let ln = if ln = len - 1 then String.rindex_from str (ln-2) '\n' else ln in
  String.sub str (ln+1) (len - ln - 1)

(* Decoding of multipart encoded post forms according to rfc7578 *)
type multipart_part =
  { disposition : string
  ; mime_type   : string option
  ; charset     : string option
  ; filename    : string option
  ; content     : string }

let regexp str = Re.Posix.(compile (re str))

let content_disposition_regexp =
  regexp "Content-Disposition: ([^;\r\n]*)(;[ ]*)?([^\r\n]*)"

let content_type_regexp =
  regexp "Content-Type: ([^\r\n]*)(;[ ]*charset=([^ \n\r]+))?"

let empty_line_re =
  regexp "\r?\n\r?\n"

let decode_parts str =
  try
    Printf.eprintf "coucou 0\n%!";
    let grps =
      Re.exec empty_line_re str
    in
    Printf.eprintf "coucou 1\n%!";
    let header_end = Re.Group.start grps 0 in
    let content_begin = Re.Group.stop grps 0 in
    Printf.eprintf "coucou 2\n%!";
    let header = String.sub str 0 header_end in
    let len = String.length str in
    let rm = if str.[len-1] = '\n' then
               if str.[len-2] = '\r' then 2 else 1 else 0
    in
    let len = len - content_begin - rm in
    let content = String.sub str content_begin len in
    let grps =
      Re.exec content_disposition_regexp header
    in
    Printf.eprintf "coucou 3\n%!";
    let disposition = Re.Group.get grps 1 in
    let values = Re.Group.get grps 3 in
    Printf.eprintf "coucou 4\n%!";
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
    let (mime_type, charset) =
      try
        Printf.eprintf "coucou A %S %S\n%!" header content;
        let grps = Re.exec content_type_regexp header in
        Printf.eprintf "coucou 5\n%!";
        let mime_type = Re.Group.get_opt grps 1 in
        let charset = Re.Group.get_opt grps 3 in
        (mime_type, charset)
      with Not_found ->
        None, None
    in
    Printf.eprintf "coucou 6\n%!";
    let name = List.assoc "name" values in
    let filename =
      try Tiny_httpd_util.percent_decode (List.assoc "filename" values)
      with Not_found -> None
    in
    Some(name,{disposition; mime_type; charset; filename; content})
  with Not_found -> None

let decode_multipart str =
  let sep = first_line str in
  let parts = Re.split (regexp (sep ^ "(--)?\r?\n")) str in
  let res = List.filter_map decode_parts parts in
  let default_charset, res =
    List.partition (fun (name,_) -> name = "_charset_") res
  in
  match default_charset with
  | (_,{content=charset; _})::_ ->
     List.map (function
           (name, part as c) ->
           if part.charset = None then
             (name, {part with charset = Some charset})
           else c) res
  | [] -> res

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

let run_cmd cmd ?stdin ?stdout ?stderr ?jail args =
  let cmd = Filename.quote_command cmd ?stdin ?stdout ?stderr args in
  let cmd = match jail, Options.docker_image with
    | Some tmpdir, Some image ->
       Printf.sprintf "docker run -v %s:%s -w %s %s %s" tmpdir tmpdir tmpdir image cmd
    | (None, _) | (_, None) -> cmd
  in
  Printf.eprintf "run: %S\n%!" cmd;
  let (stdout,stdin,stderr as chs) =
    Unix.open_process_full cmd (Unix.environment ())
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

let time_of_string str =
  Printf.eprintf "%S\n%!" str;
  match Ptime.of_rfc3339 str with
  | Ok (r,_,_) -> r
  | Error _ -> failwith "time_of_string"
