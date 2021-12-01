open Tiny_httpd

let sessions = ref []

let prefix = Options.prefix
let mk_cookie code =
  let headers = Headers.empty in
  let headers = Headers.set "Set-Cookie"
                  (Format.asprintf "PYNBTOOLS_SESSION=%s" code)
                  headers
  in
  headers

let new_session login id (role:Db.Role.t) =
  let code = Digest.(to_hex (string (string_of_int (Random.int 1_000_000_000) ^
                                       string_of_int id))) in
  sessions := (code, (login,id,role)) :: !sessions;
  mk_cookie code

let get_session req =
  let cookies = Request.get_header req "cookie" in
  let fn_eq s =
    match String.split_on_char '=' s with
    | k::v::_ -> Some String.(trim k,trim v)
    | _       -> None
  in
  let cookies =
    match cookies with
    | None -> []
    | Some s -> List.filter_map fn_eq (String.split_on_char ';' s)
  in
  try
    let code = List.assoc "PYNBTOOLS_SESSION" cookies in
    let session = List.find (fun (c,_) -> c=code) !sessions in
    Some (session, mk_cookie code)
  with
    Not_found -> None

let is_admin req =
  match get_session req with
  | None -> false
  | Some ((_,(_,_,role)),_) -> role = Db.Role.Admin

let is_prof req =
  match get_session req with
  | None -> false
  | Some ((_,(_,_,role)),_) -> role = Db.Role.Admin || role = Db.Role.Prof

let check_session req fn =
  let session = get_session req in
  match session with
  | None ->
     Response.make_string
       (Ok (Pages.home_page ()))
  | Some session -> fn session

let top_handler req =
  check_session req (fun ((_,(login,id,role)),headers) ->
     Response.make_string ~headers
       (Ok (Pages.home_page_logged login id role)))

let download_handler req =
  try
    let query = Request.query req in
    let exoid = int_of_string (List.assoc "exoid" query) in
    let ty = Db.exo_of_string (List.assoc "type" query) in
    if not (is_prof req) && ty <> Db.Question_py && ty <> Db.Question_ipynb then
      failwith "you are trying to hack";
    let content = Db.download exoid ty in
    let headers = Headers.empty in
    let headers = Headers.set "Content-Type"
                  "application/x-python"
                  headers
    in
    Response.make_string ~headers (Ok content)
  with
    e ->
    Response.fail ~code:500 "couldn't download file: %s"
      (Printexc.to_string e)

let () =
  let server = create ~port:Options.port () in
  let _ = Db.init () in

  begin
    if prefix = "" then
      set_top_handler server top_handler
    else
      add_route_handler server
        Route.(exact prefix @/ return) top_handler
  end;

  let pexact str =
    let open Route in
    if prefix = "" then exact str @/ return
    else exact prefix @/ exact str @/ return
  in

  add_route_handler server
    (pexact "top") top_handler;

  add_route_handler server
    (pexact "download") download_handler;

  add_route_handler server
    (pexact "style.css") (fun _req ->
      Response.make_string
        (Ok Pages.css));

  add_route_handler server
    (pexact "login")
    (fun req ->
      let open Db in
      try
        let query = Tiny_httpd_util.parse_query req.Request.body in
        let query = match query with
          | Ok q -> q
          | _ -> raise Bad_login
        in
        let (login, id, role) = login query in
        let headers = new_session login id role in
        Response.make_string ~headers
          (Ok (Pages.home_page_logged login id role))
      with Bad_login ->
         Response.make_string (Error (450,"Bad login or password")));

  (* echo request *)
  add_route_handler server
    (pexact "new_user")
    (fun req ->
      let open Db in
      try
        let query = Tiny_httpd_util.parse_query req.Request.body in
        let query = match query with
          | Ok q -> q
          | _ -> raise Bad_login
        in
        new_user query (is_admin req);
        top_handler req
      with Bad_login ->
         Response.make_string (Error (450,"Bad login or password")));

  add_route_handler server
    (pexact "logout")
    (fun req ->
      let session = get_session req in
      begin
        match session with
        | None -> ()
        | Some((code,_),_) ->
           try sessions := List.filter (fun (c,_) -> c != code) !sessions
           with Not_found -> ()
      end;
      top_handler req);

  add_route_handler server
    (pexact "show_solution")
    (fun req ->
      try
        check_session req (fun ((_,(_,userid,_)),headers) ->
            let exoid = int_of_string (List.assoc "exoid" (Request.query req)) in
            Response.make_string ~headers
              (Ok (Pages.show_solution userid exoid)))
      with e ->
        Response.fail ~code:500 "couldn't upload file: %s"
          (Printexc.to_string e)
    );

  add_route_handler server
    (pexact "show_notes")
    (fun req ->
      try
        check_session req (fun (_,headers) ->
            let exoid = int_of_string (List.assoc "exoid" (Request.query req)) in
            let exoname = List.assoc "exoname" (Request.query req) in
            Response.make_string ~headers
              (Pages.show_notes exoid exoname))
      with e ->
        Response.fail ~code:500 "couldn't upload file: %s"
          (Printexc.to_string e)
    );

  (* file upload *)
  add_route_handler server
    (pexact "submit_solution")
    (fun req ->
      try
        check_session req (fun ((_,(_,userid,_)),headers) ->
            let parts = Util.decode_multipart req.Request.body in
            let exoid = ref (-1) in
            let solution = ref None in
            let fn (_disposition, _mime, values, content) =
              let name = List.assoc "name" values in
              match name with
              | "solution" ->
                 let fname = List.assoc "filename" values in
                 solution := Some (fname, content)
              | "exoid" ->
                 exoid := int_of_string content
              | "submit" -> ()
              | _ -> failwith "bad form"
            in
            List.iter fn parts;
            let fname, content = match !solution with
              | None -> failwith "bad form"
              | Some c -> c
            in
            Response.make_string ~headers
              (Pages.add_solution userid !exoid fname content))
      with e ->
        Response.fail ~code:500 "couldn't upload file: %s"
          (Printexc.to_string e)
    );

  add_route_handler server
    (pexact "new_exo")
    (fun req ->
      try
        check_session req (fun (_,headers) ->
            let parts = Util.decode_multipart req.Request.body in
            let exo_name = ref "" in
            let visible = ref false in
            let tpy = ref None in
            let stdin = ref None in
            let fn (_disposition, _mime, values, content) =
              let name = List.assoc "name" values in
              match name with
              | "tpy" ->
                 let fname = List.assoc "filename" values in
                 tpy := Some (fname, content)
              | "stdin" ->
                 stdin := Some content
              | "name" ->
                 exo_name := content
              | "visible" ->
                 visible := true
              | "submit" -> ()
              | _ -> failwith "bad form"
            in
            List.iter fn parts;
            let fname, content = match !tpy with
              | None -> failwith "bad form"
              | Some c -> c
            in
            Response.make_string ~headers
              (Pages.add_exo ?stdin:!stdin !exo_name !visible fname content)
          )
      with e ->
        Response.fail ~code:500 "couldn't upload file: %s"
          (Printexc.to_string e)
    );

  add_route_handler server
    (pexact "update_exo")
    (fun req ->
      try
        check_session req (fun _ ->
            let query = Tiny_httpd_util.parse_query req.Request.body in
            let query = match query with
              | Ok q -> q
              | _ -> failwith "bad form"
            in
            let fn (name,value) =
              if name <> "submit" then
                begin
                  if String.starts_with ~prefix:"name_" name then
                    begin
                      let len = String.length name in
                      let exoid = int_of_string (String.sub name 5 (len - 5)) in
                      Db.exo_name exoid value
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
            top_handler req)
      with e ->
        Response.fail ~code:500 "couldn't upload file: %s"
          (Printexc.to_string e)
    );

  add_route_handler server
    (pexact "record_exo")
    (fun req ->
      let open Db in
      try
        check_session req (fun ((_,(login,id,admin)),headers) ->
        let query = Tiny_httpd_util.parse_query req.Request.body in
        let query = match query with
          | Ok q -> q
          | _ -> failwith "bad record exo"
        in
        let _ = Db.record_exo id query in
        Response.make_string ~headers
          (Ok (Pages.home_page_logged login id admin)))
      with Bad_login ->
         Response.make_string (Error (450,"Bad login or password")));

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
