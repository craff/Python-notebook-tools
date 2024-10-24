
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

let run_cmd cmd ?jail args =
  let open Simple_httpd in
  let cmd, args = match jail, Options.docker_image () with
    | Some tmpdir, Some image ->
       "docker", Array.append
                   (Array.append [|"docker"; "run"; "-v"; tmpdir^":"^tmpdir; "-w"; tmpdir|]
                      (Options.docker_options ()))
                   (Array.append [|image; cmd|]
                      (Array.of_list args))
    | (None, _) | (_, None) -> cmd, Array.of_list (cmd::args)
  in
  Log.f (Log.Exc 0) (fun k -> k "run: %S %s" cmd (String.concat " " (Array.to_list args)));
  let (proc, io) = Process.create cmd args in
  let out = Input.of_io io in
  try
    let out = Input.read_all out ~buf:(Buffer.create 4096) in
    Log.f (Log.Exc 0) (fun k -> k "end run: %S %s" cmd (String.concat " " (Array.to_list args)));
    let msg = match Process.(proc.status) with
      | Process.Exited d -> Printf.sprintf "exited(%d)" d
      | Process.Exn e -> Printf.sprintf "exception(%s)" (Printexc.to_string e)
      | Process.Running -> Printf.sprintf "running!"
    in
    Log.f (Log.Exc 0) (fun k -> k "run exited: %s" msg);
    (out,proc.Process.status)

  with e ->
    Log.f (Log.Exc 0) (fun k -> k "run excited with exception %s" (Printexc.to_string e));
    Io.close io;
    raise e

let string_of_time time =
  let ((y,n,d),((h,m,s),_)) = Ptime.to_date_time ~tz_offset_s:(Options.tz ()) time in
  Printf.sprintf "%d/%d/%d, %d:%d:%d" d n y h m s

let time_of_string str =
  match Ptime.of_rfc3339 str with
  | Ok (r,_,_) -> r
  | Error _ -> failwith "time_of_string"
