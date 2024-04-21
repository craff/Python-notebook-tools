open Simple_httpd
open MyUtil

module Options = Options

let redirect ?(headers=Headers.empty) url =
  let url = Pages.href url in
  let headers = Headers.set Headers.Location url headers in
  Response.make_raw ~headers ~code:Response_code.found ("Redirect to "^url)

let filter_zip =
    Camlzip.filter ~compress_above:1024 ~buf_size:(16*1024) ()
let filter_stat, get_stats = Stats.filter ()
let filter_unauth = Filter.compose_cross filter_zip filter_stat

module Login = Auth.Make(
   struct
     type t = string * int * Db.Role.t
     let check = Db.login
     let login_url = "/" ^ Options.prefix ()
     let cookie_policy = Session.{ default_cookie_policy with
                                   life = 7200.0;
                                   base = "Pynbtools";
                                   filter = fun _ -> None }
   end)

let check_data role (_,_,role') =
  let open Db.Role in
  match role, role' with
  | Admin, Admin   -> true
  | Admin, _       -> false
  | Prof , Student -> false
  | Prof , _       -> true
  | Student, _     -> true

let filter_login role =
  let check_data = check_data role in
  let check_filter req = Login.check_filter ~check_data req in
  Filter.compose_cross filter_unauth check_filter

let filter role =
  let nologin = (Response_code.ok,
                 [(Headers.Content_Type, "text/plain")], "Error: disconnected") in
  let check_data = check_data role in
  let check_filter = Login.check_filter ~nologin ~check_data in
  Filter.compose_cross filter_unauth check_filter

let error ?(message=(("Erreur: %s\n"): (_,_,_,_) format4)) e =
  let headers = Headers.(set
                           Content_Type "text/plain; charset=utf-8"
                                     empty)
  in
  let error_msg = match e with
    | Failure s -> s
    | Db.Bad_login -> "Mauvais login ou mot de passe"
    | _ -> Printexc.to_string e
  in
  Response.fail_raise ~headers ~code:Response_code.not_found message error_msg

let forbidden _ =
  Response.fail_raise ~code:Response_code.forbidden "Forbidden"

let top_handler =
  Login.check_handler
    ~not_logged:(fun req ->
      Response.make_string (fst (Pages.home_page () req)))
    (fun req cookies (login,id,role) ->
      Response.make_string ~cookies (Pages.home_page_logged login id role req))

let basthon_handler =
  Login.check_handler (fun req cookies (_,userid,role) ->
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
      | Some ts -> Log.f (Log.Exc 0)
                     (fun k -> k  "basthon_handler: exoid:%d ty:%s time:%s userid:%d"
                                 exoid (List.assoc "type" query) ts userid);
                   Db.get_specific_solution exoid userid ts req
      | None    -> Log.f (Log.Exc 0)
                     (fun k -> k  "basthon_handler: exoid:%d ty:%s time:latest userid:%d"
                                 exoid (List.assoc "type" query) userid);
                   Db.get_latest_solution exoid userid req
    in
    (* FIXME: record solution type *)
    let is_nb = String.length solution > 0 && solution.[0] = '{' in
    let solution =
      match is_nb, ty with
      | true, Db.Question_ipynb -> solution
      | false, Db.Question_py   -> solution
      | false, Db.Question_ipynb   ->
         begin
           let temp_dir = MyUtil.new_temp_dir () in
           try
             let py_name = Filename.concat temp_dir fname ^ ".py" in
             let ipynb_name = Filename.concat temp_dir fname ^ ".ipynb" in
             let ch = open_out py_name in
             output_string ch solution;
             close_out ch;
             let (_out,status) = MyUtil.run_cmd "py2nb" [py_name] in
             if status <> Simple_httpd.Process.Exited 0 then failwith "bad python file";
             let r = MyUtil.read_file ipynb_name in
             let _ = Sys.command ("rm -rf "^temp_dir) in
             r
           with e ->
             let _ = Sys.command ("rm -rf "^temp_dir) in
             raise e
         end
      | true, Db.Question_py   ->
         begin
           let temp_dir = MyUtil.new_temp_dir () in
           try
             let py_name = Filename.concat temp_dir fname ^ ".py" in
             let ipynb_name = Filename.concat temp_dir fname ^ ".ipynb" in
             let ch = open_out ipynb_name in
             output_string ch solution;                                                            close_out ch;
             let (_out,status) = MyUtil.run_cmd "nb2py" [ipynb_name] in
             if status <> Simple_httpd.Process.Exited 0 then failwith "bad notebook file";
             let r = MyUtil.read_file py_name in
             let _ = Sys.command ("rm -rf "^temp_dir) in
             r
           with e ->
                 let _ = Sys.command ("rm -rf "^temp_dir) in
                 raise e
         end
      | _ -> failwith "bad request"

    in
    let headers = Headers.(set Content_Type
                  "application/x-python"
                  empty)
    in
    Response.make_string ~cookies ~headers solution)

let download_handler = Login.check_handler (fun req cookies (_,_,role) ->
  let query = Request.query req in
  let exoid = int_of_string (List.assoc "exoid" query) in
  let ty = Db.exo_of_string (List.assoc "type" query) in
  if ty <> Db.Question_py && ty <> Db.Question_ipynb && role = Db.Role.Student then
    failwith "you are trying to hack";
  let content = Db.download exoid ty req in
  let headers = Headers.(set Content_Type
                  "application/x-python"
                  empty)
  in
  Response.make_string ~cookies ~headers content)

let add_solution headers userid exoid fname solution req =
  let dirname = MyUtil.new_temp_dir () in
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
          let (_out,status) = MyUtil.run_cmd "nb2py" [fname] in
          if status <> Simple_httpd.Process.Exited 0 then failwith "bad python notebook";
          rname ^ ".py"
        end
      else fname
    in
    let (test_file,stdin) = Db.to_check exoid req in
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
    Log.f (Log.Exc 0) (fun k -> k "Start python test");
    let (result,status) = run_cmd ~jail:dirname "python3"
                               ("/usr/local/lib/pynbtools/evaluation.py"::
                                test_name:: fname:: last) in
    Log.f (Log.Exc 0) (fun k -> k "End python test");
    if status <> Simple_httpd.Process.Exited 0 then failwith "bad python file";
    let note =
      try
        let ln = last_line result in
        let regexp = regexp "Total: ([0-9.eE-]*)" in
        let grps = Re.exec regexp ln in
        float_of_string (Re.Group.get grps 1)
      with _ -> failwith "Error in solution"
    in

    Db.record_solution userid exoid solution note result req;
    let _ = Sys.command ("rm -rf "^dirname) in
    (* TODO: remove dirname if this form is not submitted after some time ?
       If user leaves that page? Both?
     *)
    let headers = Headers.set Headers.Content_Type "text/plain" headers in
    Response.make_string ~headers (Printf.sprintf "Note: %.2f" note)
  with e ->
    let _ = Sys.command ("rm -rf "^dirname) in
    Response.make_string ~headers (Printf.sprintf "Evaluation failed (%s)" (Printexc.to_string e))

module Make(Init: Host.Init) = struct

  let _ = Db.init ()

  let prefix = Options.prefix ()

  let pexact str =
    let open Route in
    if prefix = "" then exact str @/ return
    else exact prefix @/ exact str @/ return

  let root =
    let open Route in
    if prefix = "" then return
    else exact prefix @/ return

  let _ = Init.add_route_handler  ~filter:filter_unauth root top_handler

  let _ = Init.add_route_handler
    (pexact "download") download_handler

  let _ = Init.add_route_handler ~filter:(filter Db.Role.Student)
    (pexact "basthon_exo") basthon_handler

  let top_dir = Options.top_dir ()

  let _ =
    let vfs = Basthon.make ~top_dir () in
    Init.add_vfs ~filter:filter_unauth ~config:(Dir.config ~download:true
                                                  ~dir_behavior:Dir.Index ()) ~prefix:"pynbtools/basthon"  vfs

  let _ = Init.add_route_handler ~filter:filter_unauth
    (pexact "style.css") (fun _req ->
      let headers = Headers.(set Content_Type "text/css" Headers.empty) in
      Response.make_string ~headers Pages.css)

  let _ = Init.add_route_handler ~filter:filter_unauth
    (pexact "scripts.js") (fun _req ->
      let headers = Headers.(set Content_Type "application/javascript" Headers.empty) in
      Response.make_string ~headers (Pages.scripts ()))

  let _ = Init.add_route_handler_chaml ~filter:filter_unauth
            (pexact "login") (fun req header ->
              let page = snd (Pages.home_page () req) in
              Login.login_page ~destination:("/"^prefix) ~page req header)

  let respbool b =
    let b = if b then "true" else "false" in
    let headers = Headers.(set Content_Type "text/plain" empty) in
    Response.make_string ~headers b

  let resptrue = respbool true

  let _ = Init.add_route_handler ~filter:filter_unauth ~meth:Method.POST
    (pexact "new_user")
    (fun req ->
      Db.new_user (Request.query req) false req;
      resptrue)

  let _ = Init.add_route_handler ~filter:(filter Db.Role.Admin) ~meth:Method.POST
    (pexact "new_user_logged")
    (fun req ->
      Db.new_user (Request.query req) true req;
      resptrue)

  let _ = Init.add_route_handler ~filter:(filter Db.Role.Prof) ~meth:Method.POST
    (pexact "creation_classe")
    (fun req ->
      Db.creation_classe (Request.query req) req;
      resptrue)

  let _ = Init.add_route_handler ~filter:(filter Db.Role.Prof) ~meth:Method.POST
    (pexact "creation_course")
    (fun req ->
      Db.creation_course (Request.query req) req;
      resptrue)

  let _ = Init.add_route_handler_chaml ~filter:filter_unauth
            (pexact "logout") (fun req -> Login.logout_page req)

  let _ = Init.add_route_handler ~filter:(filter_login Db.Role.Student) ~meth:Method.GET
    (pexact "show_solution")
    (Login.check_handler (fun req cookies (login,userid,_) ->
          let exoid = int_of_string (List.assoc "exoid" (Request.query req)) in
          Response.make_string ~cookies
            (Pages.show_solution login userid exoid req)))

  let _ = Init.add_route_handler ~filter:(filter_login Db.Role.Prof) (* Needs both GET and POST *)
    (pexact "show_notes")
    (Login.check_handler (fun req cookies (login,userid,_) ->
          let exoid = int_of_string (List.assoc "exoid" (Request.query req)) in
          let exoname = List.assoc "exoname" (Request.query req) in
          let classid =  List.assoc_opt "classid" (Request.query req) in
          let detailid = List.assoc_opt "detailid" (Request.query req) in
          let classid = match classid
            with | None | Some "" -> None
                 | Some s -> Some (int_of_string s)
          in
          let detailid = match detailid
            with | None | Some "" -> None
                 | Some s -> Some (int_of_string s)
          in
          let debut =  List.assoc_opt "debut" (Request.query req) in
          let fin =  List.assoc_opt "fin" (Request.query req) in
          Response.make_string ~cookies
            (Pages.show_notes login ?debut ?fin ?classid ?detailid userid exoid exoname req)))

  let _ = Init.add_route_handler ~filter:(filter Db.Role.Prof) ~meth:Method.GET
    (pexact "exists_classe")
    (fun req ->
      let name = List.assoc "name" (Request.query req) in
      let b = Db.exists_classe name req in
      respbool b)

  let _ = Init.add_route_handler ~filter:(filter Db.Role.Prof) ~meth:Method.GET
    (pexact "exists_course")
    (fun req ->
      let name = List.assoc "name" (Request.query req) in
      let b = Db.exists_course name req in
      respbool b)

  let _ = Init.add_route_handler ~filter:filter_unauth ~meth:Method.GET
    (pexact "exists_login")
    (fun req ->
      let login = List.assoc "login" (Request.query req) in
      let b = Db.exists_login login req in
      respbool b)

  let _ = Init.add_route_handler ~filter:(filter Db.Role.Prof) ~meth:Method.POST
    (pexact "change_user_classe")
    (fun req  ->
      let query = Request.query req in
      let _ = Db.change_user_classe query req in
      resptrue)

  let _ = Init.add_route_handler ~filter:(filter Db.Role.Prof) ~meth:Method.POST
    (pexact "change_courses_classes")
    (fun req  ->
      let query = Request.query req in
      let _ = Db.change_courses_classes query req in
      resptrue)

  let _ = Init.add_route_handler ~filter:(filter Db.Role.Prof) ~meth:Method.POST
    (pexact "change_prof_course")
    (fun req  ->
      let query = Request.query req in
      let _ = Db.change_prof_course query req in
      resptrue)

  let _ = Init.add_route_handler ~filter:(filter Db.Role.Prof) ~meth:Method.POST
    (pexact "change_course_name")
    (fun req ->
      let query = Request.query req in
      let _ = Db.change_course_name query req in
      resptrue)

  let _ = Init.add_route_handler ~filter:(filter Db.Role.Prof) ~meth:Method.POST
    (pexact "change_user_name")
    (fun req ->
      let query = Request.query req in
      let _ = Db.change_user_name query req in
      resptrue)

  let _ = Init.add_route_handler ~filter:(filter Db.Role.Prof) ~meth:Method.POST
    (pexact "change_user_firstname")
    (fun req ->
      let query = Request.query req in
      let _ = Db.change_user_firstname query req in
      resptrue)

  let _ = Init.add_route_handler ~filter:(filter Db.Role.Prof) ~meth:Method.POST
    (pexact "change_user_password")
    (fun req ->
      let _ = Db.change_user_password (Request.query req) req in
      resptrue)

  let _ = Init.add_route_handler ~filter:(filter Db.Role.Prof) ~meth:Method.POST
    (pexact "change_exo_name")
    (fun req ->
      let query = Request.query req in
      let _ = Db.change_exo_name query req in
      resptrue)

  let _ = Init.add_route_handler ~filter:(filter Db.Role.Prof)
    (pexact "change_exo_visibility")
    (fun req ->
       let query = Request.query req in
       let _ = Db.change_exo_visibility query req in
       resptrue)

  let _ = Init.add_route_handler ~filter:(filter Db.Role.Prof) ~meth:Method.POST
    (pexact "change_exo_course")
    (fun req ->
       let query = Request.query req in
       let _ = Db.change_exo_course query req in
       resptrue)

  let _ = Init.add_route_handler ~filter:(filter Db.Role.Student) ~meth:Method.POST
    (pexact "submit_solution")
    (Login.check_handler (fun req _ (_,userid,_) ->
       let exoid = ref (-1) in
       let solution = ref None in
       let fn (name, content) =
         let filename = List.assoc_opt (name, Headers.Filename_Multipart)
                          (Request.multipart_headers req)
         in
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
       List.iter fn (Request.query req);
       let fname, content = match !solution with
         | None -> failwith "bad form"
         | Some c -> c
       in
       try add_solution Headers.empty userid !exoid fname content req
       with Failure str -> Response.make_string str
    ))

  let _ = Init.add_route_handler ~filter:(filter_login Db.Role.Prof) ~meth:Method.POST
    (pexact "new_exo")
    (fun req ->
       let exo_name = ref "" in
       let description = ref "" in
       let visible = ref false in
       let tpy = ref None in
       let stdin = ref None in
       let fn (name, content) =
         let filename = List.assoc_opt (name, Headers.Filename_Multipart)
                          (Request.multipart_headers req)
         in
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
       List.iter fn (Request.query req);
       let fname, content = match !tpy with
         | None -> failwith "bad form"
         | Some c -> c
       in
       Response.make_string
         (Pages.add_exo ?stdin:!stdin !exo_name !description !visible fname content))

  (*
  add_route_handler server
    (pexact "update_exo")
    (Login.check_handler (fun req _ ->
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

  let _ = Init. add_route_handler ~filter:(filter_login Db.Role.Prof) ~meth:Method.POST
    (pexact "record_exo")
    (Login.check_handler (fun req _ (_,id,_) ->
      let query = Request.query req in
      let _ = Db.record_exo id query req in
      redirect "/" ))

end
