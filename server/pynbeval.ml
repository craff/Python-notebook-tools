open Tiny_httpd

let sessions = Hashtbl.create 1024

let prefix = Options.prefix
let mk_cookie code =
  let headers = Headers.empty in
  let headers = Headers.set_cookie "PYNBTOOLS_SESSION" code headers in
  headers

let redirect ?(headers=Headers.empty) url =
  let url = Pages.href url in
  let headers = Headers.set "Location" url headers in
  Response.make_string ~headers (Error(303, "Redirect to "^url))

let new_session login id (role:Db.Role.t) =
  let code = Digest.(to_hex (string (string_of_int (Random.int 1_000_000_000) ^
                                       string_of_int id))) in
  Hashtbl.add sessions code (login,id,role);
  mk_cookie code

let get_session_code req =
  match Request.get_cookie req "PYNBTOOLS_SESSION" with
  | None -> raise Not_found
  | Some c -> c

let get_session req =
  try
    let code = get_session_code req in
    let session = Hashtbl.find sessions code in
    Some (session, mk_cookie code)
  with
    Not_found -> None

let is_admin req =
  match get_session req with
  | None -> false
  | Some ((_,_,role),_) -> role = Db.Role.Admin

let is_prof req =
  match get_session req with
  | None -> false
  | Some ((_,_,role),_) -> role = Db.Role.Admin || role = Db.Role.Prof

let error ?(message=(("Erreur: %s\n"): (_,_,_,_) format4)) e =
  let headers = Headers.set
                  "Content-Type" "text/plain; charset=utf-8"
                  Headers.empty
  in
  let error_msg = match e with
    | Failure s -> s
    | Db.Bad_login -> "Mauvais login ou mot de passe"
    | _ -> Printexc.to_string e
  in
  Response.fail ~headers ~code:404 message error_msg

let chrono fn req =
  let t0 = Request.start_time req in
  let r = fn req in
  let t1 = Unix.gettimeofday () in
  let d = (t1 -. t0) *. 1e6 in
  Printf.printf "Time: %.0fµs for %s\n%!" d (Request.path req);
  r

let no_session fn req =
  try
    let session = get_session req in
    match session with
    | None ->
       fn req Headers.empty
    | Some (_,headers) ->
       fn req headers
  with e -> error e

let no_session fn req = chrono (no_session fn) req

let rm_cookie code =
  let headers = Headers.empty in
  let headers = Headers.unset_cookie "PYNBTOOLS_SESSION" code headers in
  headers

let check_session fn req =
  let session = get_session req in
  match session with
  | None ->
     Response.make_string (Ok (Pages.home_page ()))
  | Some session ->
     try
       fn req session
     with e -> error e

let check_session fn req = chrono (check_session fn) req

let top_handler =
  check_session (fun _ ((login,id,role),headers) ->
     Response.make_string ~headers
       (Ok (Pages.home_page_logged login id role)))

let basthon_handler =
  check_session (fun req ((_,userid,role),headers) ->
    let query = Request.query req in
    let exoid = int_of_string (List.assoc "exoid" query) in
    let ty = Db.exo_of_string (List.assoc "type" query) in
    let timestamp = try Some (List.assoc "time" query)
                    with Not_found -> None
    in
    let userid =
      try
        if role = Db.Role.Student then raise Not_found;
        int_of_string (List.assoc "userid" query)
      with Not_found -> userid
    in
    let (fname, solution) =
      match timestamp with
      | Some ts -> Printf.eprintf "get specific\n%!";
                   Db.get_specific_solution exoid userid ts
      | None    -> Printf.eprintf "get latest\n%!";
                   Db.get_latest_solution exoid userid
    in
    (* FIXME: record solution type *)
    let is_nb = String.length solution > 0 && solution.[0] = '{' in
    let solution =
      match is_nb, ty with
      | true, Db.Question_ipynb -> solution
      | false, Db.Question_py   -> solution
      | false, Db.Question_ipynb   ->
         begin
           let temp_dir = Util.new_temp_dir () in
           try
             let py_name = Filename.concat temp_dir fname ^ ".py" in
             let ipynb_name = Filename.concat temp_dir fname ^ ".ipynb" in
             let ch = open_out py_name in
             output_string ch solution;                                                            close_out ch;
             let (_out,_err,status) = Util.run_cmd "py2nb" [py_name] in
             if status <> Unix.WEXITED 0 then failwith "bad python file";
             let r = Util.read_file ipynb_name in
             let _ = Sys.command ("rm -rf "^temp_dir) in
             r
           with e ->
                 let _ = Sys.command ("rm -rf "^temp_dir) in
                 raise e
         end
      | true, Db.Question_py   ->
         begin
           let temp_dir = Util.new_temp_dir () in
           try
             let py_name = Filename.concat temp_dir fname ^ ".py" in
             let ipynb_name = Filename.concat temp_dir fname ^ ".ipynb" in
             let ch = open_out ipynb_name in
             output_string ch solution;                                                            close_out ch;
             let (_out,_err,status) = Util.run_cmd "nb2py" [ipynb_name] in
             if status <> Unix.WEXITED 0 then failwith "bad notebook file";
             let r = Util.read_file py_name in
             let _ = Sys.command ("rm -rf "^temp_dir) in
             r
           with e ->
                 let _ = Sys.command ("rm -rf "^temp_dir) in
                 raise e
         end
      | _ -> failwith "bad request"

    in
    let headers = Headers.set "Content-Type"
                  "application/x-python"
                  headers
    in
    Response.make_string ~headers (Ok solution))

let download_handler = no_session (fun req headers ->
  let query = Request.query req in
  let exoid = int_of_string (List.assoc "exoid" query) in
  let ty = Db.exo_of_string (List.assoc "type" query) in
  if not (is_prof req) && ty <> Db.Question_py && ty <> Db.Question_ipynb then
    failwith "you are trying to hack";
  let content = Db.download exoid ty in
  let headers = Headers.set "Content-Type"
                  "application/x-python"
                  headers
  in
  Response.make_string ~headers (Ok content))

let add_solution headers userid exoid fname solution =
  let open Util in
  let dirname = new_temp_dir () in
  try
    let extension = Filename.extension fname in
    let (fname, extension) =
      if extension = ".txt" then
        let fname = Filename.remove_extension fname in
        (fname, Filename.extension fname)
      else
        (fname, extension)
    in
    if extension <> ".py" && extension <> ".ipynb" then
      failwith "seulement les fichiers '.py' et '.ipynb' sont acceptés";
    let bname = Filename.remove_extension fname in
    let fname = Filename.concat dirname fname in
    let rname = Filename.concat dirname bname in
    let ch = open_out fname in
    output_string ch solution;
    close_out ch;
    let fname =
      if extension = ".ipynb" then
        begin
          let (_out,_err,status) = run_cmd "nb2py" [fname] in
          if status <> Unix.WEXITED 0 then failwith "bad python notebook";
          rname ^ ".py"
        end
      else fname
    in
    let (test_file,stdin) = Db.to_check exoid in
    let test_name = rname ^ "_test.py" in
    let ch = open_out test_name in
    output_string ch test_file;
    close_out ch;
    let stdin_name =
      match stdin with
      | None | Some "" -> ""
      | Some str ->
         let fname = Filename.concat dirname "stdin.txt" in
         let ch = open_out fname in
         output_string ch str;
         close_out ch;
         fname
    in
    let last = if stdin_name = "" then [] else [stdin_name] in
    let (result,_err,status) = run_cmd ~jail:dirname "python3"
                               ("/usr/local/lib/pynbtools/evaluation.py"::
                                test_name:: fname:: last) in
    if status <> Unix.WEXITED 0 then failwith "bad python file";
    let note =
      try
        let ln = last_line result in
        let regexp = Util.regexp "Total: ([0-9.eE-]*)" in
        let grps = Re.exec regexp ln in
        float_of_string (Re.Group.get grps 1)
      with _ -> failwith "Syntax error in solution"
    in

    Db.record_solution userid exoid solution note result;
    let _ = Sys.command ("rm -rf "^dirname) in
    let url = "show_solution?exoid=" ^ string_of_int exoid in
    redirect ~headers url
  with
    e ->
    let _ = Sys.command ("rm -rf "^dirname) in
    raise e

let () =
  let server = create ~max_connections:200 ~timeout:1.0
                 ~port:Options.port ()
  in
  let _ = Db.init () in

  begin
    set_top_handler server top_handler;
    if prefix != "" then
      add_route_handler server
        Route.(exact prefix @/ return) top_handler
  end;

  let pexact str =
    let open Route in
    if prefix = "" then exact str @/ return
    else exact prefix @/ exact str @/ return
  in

  add_route_handler server
    (pexact "status")
    (fun _ ->
      let str = Printf.sprintf "active connections: %d\n%!"
              (active_connections server)
      in
      Response.make_string
        (Ok str));

  add_route_handler server
    (pexact "top") top_handler;

  add_route_handler server
    (pexact "download") download_handler;

  add_route_handler server
    (pexact "basthon") basthon_handler;

  add_route_handler server
    (pexact "style.css") (fun _req ->
      let headers = Headers.set "Content-Type" "text/css" Headers.empty in
      Response.make_string ~headers
        (Ok Pages.css));

  add_route_handler server
    (pexact "scripts.js") (fun _req ->
      let headers = Headers.set "Content-Type" "application/javascript" Headers.empty in
      Response.make_string ~headers
        (Ok Pages.scripts));

  add_route_handler server
    (pexact "login")
    (no_session (fun req _ ->
      let open Db in
      let query = Tiny_httpd_util.parse_query req.Request.body in
      let query = match query with
        | Ok q -> q
        | _ -> raise Bad_login
      in
      let (login, id, role) = login query in
      let headers = new_session login id role in
      redirect ~headers "/"));

  (* echo request *)
  add_route_handler server
    (pexact "new_user")
    (no_session (fun req headers ->
      let open Db in
      let query = Tiny_httpd_util.parse_query req.Request.body in
      let query = match query with
        | Ok q -> q
        | _ -> failwith "Ne peut pas créer l'utilisateur"
      in
      new_user query (is_admin req);
      redirect ~headers "/"));

  (* echo request *)
  add_route_handler server
    (pexact "creation_classe")
    (check_session (fun req (_,headers) ->
      let open Db in
      let query = Tiny_httpd_util.parse_query req.Request.body in
      let query = match query with
        | Ok q -> q
        | _ -> failwith "can not create class"
      in
      if not (is_prof req) then failwith "Non autorisé";
      creation_classe query;
      redirect ~headers "/"));

  add_route_handler server
    (pexact "logout")
    (check_session (fun req _ ->
      let code = get_session_code req in
      Hashtbl.remove sessions code;
      redirect ~headers:(rm_cookie code) "/"));

  add_route_handler server
    (pexact "show_solution")
    (check_session (fun req ((login,userid,_),headers) ->
          let exoid = int_of_string (List.assoc "exoid" (Request.query req)) in
          Response.make_string ~headers
            (Ok (Pages.show_solution login userid exoid)))
    );

  add_route_handler server
    (pexact "show_notes")
    (check_session (fun req ((login,userid,_),headers) ->
          let exoid = int_of_string (List.assoc "exoid" (Request.query req)) in
          let exoname = List.assoc "exoname" (Request.query req) in
          let classid =  List.assoc_opt "classid" (Request.query req) in
          let classid = match classid
            with | None | Some "" -> None
                 | Some s -> Some (int_of_string s)
          in
          let debut =  List.assoc_opt "debut" (Request.query req) in
          let fin =  List.assoc_opt "fin" (Request.query req) in
          Response.make_string ~headers
            (Pages.show_notes login ?debut ?fin ?classid userid exoid exoname))
    );

  add_route_handler server
    (pexact "exists_login")
    (no_session (fun req headers ->
      let login = List.assoc "login" (Request.query req) in
      let b = Db.exists_login login in
      let str = if b then "true" else "false" in
      let headers = Headers.set "Content-Type" "text/plain" headers in
      Response.make_string ~headers (Ok str)));

  add_route_handler server
    (pexact "change_user_classe")
    (check_session (fun req ((_,_,role),headers) ->
       if role = Db.Role.Student then failwith "Not allowed";
       let query = Request.query req in
       let _ = Db.change_user_classe query in
       let headers = Headers.set "Content-Type" "text/plain" headers in
       Response.make_string ~headers (Ok "true")));

  add_route_handler server
    (pexact "change_user_name")
    (check_session (fun req ((_,_,role),headers) ->
       if role = Db.Role.Student then failwith "Not allowed";
       let query = Request.query req in
       let _ = Db.change_user_name query in
       let headers = Headers.set "Content-Type" "text/plain" headers in
       Response.make_string ~headers (Ok "true")));

  add_route_handler server
    (pexact "change_user_firstname")
    (check_session (fun req ((_,_,role),headers) ->
       if role = Db.Role.Student then failwith "Not allowed";
       let query = Request.query req in
       let _ = Db.change_user_firstname query in
       let headers = Headers.set "Content-Type" "text/plain" headers in
       Response.make_string ~headers (Ok "true")));

  add_route_handler server
    (pexact "change_user_password")
    (check_session (fun req ((_,_,role),headers) ->
       if role = Db.Role.Student then failwith "Not allowed";
       let query = Tiny_httpd_util.parse_query req.Request.body in
       let query = match query with
        | Ok q -> q
        | _ -> raise Not_found
       in
       let _ = Db.change_user_password query in
       let headers = Headers.set "Content-Type" "text/plain" headers in
       Response.make_string ~headers (Ok "true")));

  add_route_handler server
    (pexact "change_exo_name")
    (check_session (fun req ((_,_,role),headers) ->
       if role = Db.Role.Student then failwith "Not allowed";
       let query = Request.query req in
       let _ = Db.change_exo_name query in
       let headers = Headers.set "Content-Type" "text/plain" headers in
       Response.make_string ~headers (Ok "true")));

  add_route_handler server
    (pexact "change_exo_visibility")
    (check_session (fun req ((_,_,role),headers) ->
       if role = Db.Role.Student then failwith "Not allowed";
       let query = Request.query req in
       let _ = Db.change_exo_visibility query in
       let headers = Headers.set "Content-Type" "text/plain" headers in
       Response.make_string ~headers (Ok "true")));

  add_route_handler server
    (pexact "change_exo_classe")
    (check_session (fun req ((_,_,role),headers) ->
       if role = Db.Role.Student then failwith "Not allowed";
       let query = Request.query req in
       let _ = Db.change_exo_classe query in
       let headers = Headers.set "Content-Type" "text/plain" headers in
       Response.make_string ~headers (Ok "true")));

  add_route_handler server
    (pexact "submit_solution")
    (check_session (fun req ((_,userid,_),headers) ->
       let open Util in
       let parts = decode_multipart req.Request.body in
       Printf.eprintf "decode OK\n%!";
       let exoid = ref (-1) in
       let solution = ref None in
       let fn (name, {filename; content; _}) =
         Printf.eprintf "scan: %s %s\n%!" name content;
         match name with
         | "solution" ->
            let fname = match filename with
            | Some f -> f
            | None -> "untitled.ipynb"
            in
            solution := Some (fname, content)
         | "exoid" ->
            exoid := int_of_string content
         | "submit" -> ()
         | _ -> failwith "bad form"
       in
       List.iter fn parts;
       Printf.eprintf "scan OK\n%!";
       let fname, content = match !solution with
         | None -> failwith "bad form"
         | Some c -> c
       in
       Printf.eprintf "fname and content OK\n%!";
       add_solution headers userid !exoid fname content));

  add_route_handler server
    (pexact "new_exo")
    (check_session (fun req (_,headers) ->
       let open Util in
       let parts = decode_multipart req.Request.body in
       let exo_name = ref "" in
       let description = ref "" in
       let visible = ref false in
       let tpy = ref None in
       let stdin = ref None in
       let fn (name, {filename; content; _}) =
         match name with
         | "tpy"         ->
            let filename = match filename with
              | Some f -> f
              | None -> "untitled.tpy"
            in
            tpy := Some (filename, content)
         | "stdin"       -> stdin := Some content
         | "name"        -> exo_name := content
         | "visible"     -> visible := true
         | "submit"      -> ()
         | "description" -> description := content
         | _ -> failwith "bad form"
       in
       List.iter fn parts;
       let fname, content = match !tpy with
         | None -> failwith "bad form"
         | Some c -> c
       in
       Response.make_string ~headers
         (Pages.add_exo ?stdin:!stdin !exo_name !description !visible fname content)));

  (*
  add_route_handler server
    (pexact "update_exo")
    (check_session (fun req _ ->
       let query = Tiny_httpd_util.parse_query req.Request.body in
       let query = match query with
         | Ok q -> q
         | _ -> failwith "bad form"
       in
       let rec gn acc =
         function [] -> List.rev acc
                | (name,value) as c :: rest ->
                   let kn (name',_) = name' <> name in
                   let (acc, rest) =
                     if value = "on" && String.contains name '_' then
                       (List.filter kn acc, List.filter kn rest)
                     else
                       (acc, rest)
                   in
                   gn (c::acc) rest
       in
       let query = gn [] query in
       let fn (name,value) =
         if name <> "submit" then
           begin
             if String.starts_with ~prefix:"name_" name then
               begin
                 let len = String.length name in
                 let exoid = int_of_string (String.sub name 5 (len - 5)) in
                 Db.exo_name exoid value
               end
             else if String.contains name '_' then
               begin
                 match String.split_on_char '_' name with
                 | exoid::classid::_ ->
                    let exoid = int_of_string exoid in
                    let userid = int_of_string classid in
                    let checked = value = "on" in
                    Db.change_exo_classe exoid userid checked
                 | _ -> assert false
               end
             else
               begin
                 let exoid = int_of_string name in
                 match value with
                 | "visible" -> Db.exo_visibility exoid true
                 | "hidden" -> Db.exo_visibility exoid false
                 | "remove" -> Db.remove_exo exoid
                 | _ -> failwith "bad form"
               end
           end
       in
       List.iter fn query;
       top_handler req));
   *)
  add_route_handler server
    (pexact "record_exo")
    (check_session (fun req ((_,id,_),headers) ->
      let query = Tiny_httpd_util.parse_query req.Request.body in
      let query = match query with
        | Ok q -> q
        | _ -> failwith "bad record exo"
      in
      let _ = Db.record_exo id query in
      redirect ~headers "/"));

  (* run the server *)
  Printf.eprintf "listening on http://%s:%d\n%!" (addr server) (port server);
  try
    match run server with
    | Ok () -> ()
    | Error e -> raise e
  with
    e ->
    Printf.eprintf "Unexpected exception (server crash): %s\n%!"
      (Printexc.to_string e)
