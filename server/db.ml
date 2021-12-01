open Lwt.Infix

module Role = struct
  type t = Admin | Prof | Student

  let decode = function
    | "admin" -> Admin
    | "prof" -> Prof
    | "student" -> Student
    | _   -> raise Not_found

  let t =
    let encode = function
      | Admin -> Ok "admin"
      | Prof -> Ok "prof"
      | Student -> Ok "student"
    in
    let decode s = try Ok (decode s) with Not_found -> Error "invalid role" in
    Caqti_type.(custom ~encode ~decode string)
end

let db =
  Caqti_lwt.connect
    (Uri.of_string
       "postgresql://pynbtools:g5HTE3Ddgj258fJr@localhost")
  >>= Caqti_lwt.or_fail |> Lwt_main.run

let init () =
  let query = [%rapper execute
                  {sql| CREATE TYPE role AS ENUM ('admin','prof','student')|sql}]
  in
  let _ =
    try query () db >>= Caqti_lwt.or_fail |> Lwt_main.run
    with _ -> ()
  in

  let query =
    [%rapper execute
        {sql|
         CREATE TABLE IF NOT EXISTS users (
           userid SERIAL PRIMARY KEY,
           name text NOT NULL,
           firstname text NOT NULL,
           login text NOT NULL,
           password text NOT NULL,
           role role NOT NULL)
         |sql}]
  in

  let _ = query () db >>= Caqti_lwt.or_fail |> Lwt_main.run in

  let query =
    [%rapper execute
        {sql|
         CREATE TABLE IF NOT EXISTS classes (
           classid SERIAL PRIMARY KEY,
           name text NOT NULL,
           password text NOT NULL)
         |sql}]
  in

  let _ = query () db >>= Caqti_lwt.or_fail |> Lwt_main.run in

  let query =
    [%rapper execute
        {sql|
         CREATE TABLE IF NOT EXISTS exos (
           exoid SERIAL PRIMARY KEY,
           created_timestamp timestamp NOT NULL DEFAULT current_timestamp,
           name text NOT NULL,
           fname text NOT NULL,
           visible boolean NOT NULL,
           userid int NOT NULL,
           tpy text NOT NULL,
           question_ipynb text NOT NULL,
           corrige_ipynb text NOT NULL,
           question_py text NOT NULL,
           corrige_py text NOT NULL,
           test_py text NOT NULL,
           stdin text,
           CONSTRAINT userid_exos
             FOREIGN KEY(userid)
   	     REFERENCES users(userid))
         |sql}]
  in

  let _ = query () db >>= Caqti_lwt.or_fail |> Lwt_main.run in

  let query =
    [%rapper execute
        {sql|
         CREATE TABLE IF NOT EXISTS solutions (
           created_timestamp timestamp NOT NULL DEFAULT current_timestamp,
           exoid int,
           userid int,
           solution text,
           note real,
           result text,
           CONSTRAINT userid_solutions
             FOREIGN KEY(userid)
   	     REFERENCES users(userid),
           CONSTRAINT exoid_solutions
             FOREIGN KEY(exoid)
   	     REFERENCES exos(exoid) ON DELETE CASCADE
         )|sql}]
  in

  let _ = query () db >>= Caqti_lwt.or_fail |> Lwt_main.run in

  ()

exception Bad_login

let login query =
  let login = List.assoc "login" query in
  let password = Digest.(to_hex (string (List.assoc "password" query))) in
  let query =
    [%rapper get_opt
        {sql|SELECT @int{userid},@Role{role} FROM users WHERE login=%string{login} AND password = %string{password}|sql}]
  in

  let result = query ~login ~password db
               >>= Caqti_lwt.or_fail |> Lwt_main.run in

  match result with
  | None -> raise Bad_login
  | Some (id, role) -> (login, id, role)

let new_user query is_admin =
  let login = List.assoc "login" query in
  let name = List.assoc "name" query in
  let firstname = List.assoc "firstname" query in
  let role = try Role.decode (List.assoc "role" query)
             with Not_found -> Role.Student
  in
  let password = Digest.(to_hex (string (List.assoc "password" query))) in
  let password2 = Digest.(to_hex (string (List.assoc "password2" query))) in
  if password <> password2 then failwith "Password mismatch";
  if role <> Role.Student && not is_admin then failwith "You are trying to hack!";
  let query = [%rapper execute
              {sql|INSERT INTO users (login, name, firstname, password, role)
                   VALUES (%string{login}, %string{name}, %string{firstname},
                           %string{password}, %Role{role})|sql}]
  in
  query ~login ~name ~firstname ~password ~role db
               >>= Caqti_lwt.or_fail |> Lwt_main.run

let get_exo_by_id exoid =
  let query =
    [%rapper get_opt
        {sql|SELECT @ptime{created_timestamp},@string{name},@string{fname},@bool{visible} FROM exos WHERE exoid=%int{exoid}|sql}]
  in
  let result = query ~exoid db
               >>= Caqti_lwt.or_fail |> Lwt_main.run in
  match result with
  | Some r -> r
  | None -> failwith "Bad exoid"

let get_exos_by_id userid =
  let query =
    [%rapper get_many
        {sql|SELECT @int{exoid},@ptime{created_timestamp},@string{name},@string{fname},@bool{visible} FROM exos WHERE userid=%int{userid}|sql}]
  in
  let result = query ~userid db
               >>= Caqti_lwt.or_fail |> Lwt_main.run in
  result

let get_visible_exos () =
  let query =
    [%rapper get_many
        {sql|SELECT @int{exoid},@ptime{created_timestamp},@string{name},@string{fname} FROM exos WHERE visible=true|sql}]
  in
  let result = query () db
               >>= Caqti_lwt.or_fail |> Lwt_main.run in
  result

let get_note exoid userid =
  let query =
    [%rapper get_opt
        {sql|SELECT @float{note} FROM solutions WHERE exoid=%int{exoid} AND userid=%int{userid} ORDER BY note DESC LIMIT 1|sql}]
  in
  let result = query ~exoid ~userid db
               >>= Caqti_lwt.or_fail |> Lwt_main.run in
  result

let get_notes exoid =
  let query =
    [%rapper get_many
        {sql|SELECT @string{name},@string{firstname},@string{result}
             FROM users as u, solutions as s WHERE
               u.userid = s.userid AND (s.exoid,s.userid, s.created_timestamp) IN
                  (SELECT exoid,userid,MAX(created_timestamp) as ct FROM solutions
                   WHERE (exoid,userid,note) IN
                     (SELECT exoid,userid,MAX(note) FROM solutions WHERE exoid=%int{exoid}
                     GROUP BY exoid,userid)
                   GROUP BY exoid,userid)|sql}]
  in
  let result = query ~exoid db
               >>= Caqti_lwt.or_fail |> Lwt_main.run in
  result

let has_solution exoid =
  let query =
    [%rapper get_opt
        {sql|SELECT @int{COUNT(*)} FROM solutions WHERE exoid=%int{exoid}|sql}]
  in
  let result = query ~exoid db
               >>= Caqti_lwt.or_fail |> Lwt_main.run in
  match result with
  | None -> false
  | Some n -> n > 0

type exo = Question_py | Question_ipynb
           | Corrige_py | Corrige_ipynb
           | Test_py | Template

let exo_of_string = function
  | "question_py" -> Question_py
  | "question_ipynb" -> Question_ipynb
  | "corrige_py" -> Corrige_py
  | "corrige_ipynb" -> Corrige_ipynb
  | "test_py" -> Test_py
  | "template" -> Template
  | _ -> failwith "bad exo type"

let string_of_exo = function
  | Question_py -> "question_py"
  | Question_ipynb -> "question_ipynb"
  | Corrige_py -> "corrige_py"
  | Corrige_ipynb -> "corrige_ipynb"
  | Test_py -> "test_py"
  | Template -> "Template"

let to_check exoid =
  let query =
    [%rapper get_opt {sql|SELECT @string{test_py}, @string?{stdin} FROM exos WHERE exoid=%int{exoid}|sql}]
  in
  let result = query ~exoid db
               >>= Caqti_lwt.or_fail |> Lwt_main.run
  in
  match result with
  | None -> raise Not_found
  | Some s -> s

let download exoid ty =
  let query = match ty with
    | Question_py ->
       [%rapper get_opt {sql|SELECT @string{question_py} FROM exos WHERE exoid=%int{exoid}|sql}]
    | Question_ipynb ->
       [%rapper get_opt {sql|SELECT @string{question_ipynb} FROM exos WHERE exoid=%int{exoid}|sql}]
    | Corrige_py ->
       [%rapper get_opt {sql|SELECT @string{corrige_py} FROM exos WHERE exoid=%int{exoid}|sql}]
    | Corrige_ipynb ->
       [%rapper get_opt {sql|SELECT @string{corrige_ipynb} FROM exos WHERE exoid=%int{exoid}|sql}]
    | Test_py ->
       [%rapper get_opt {sql|SELECT @string{test_py} FROM exos WHERE exoid=%int{exoid}|sql}]
    | Template ->
       [%rapper get_opt {sql|SELECT @string{tpy} FROM exos WHERE exoid=%int{exoid}|sql}]
  in
  let result = query ~exoid db
               >>= Caqti_lwt.or_fail |> Lwt_main.run
  in
  match result with
  | None -> raise Not_found
  | Some s -> s

let best_solution userid exoid =
  let query =
    [%rapper get_opt
        {sql|SELECT @string{result},@ptime{created_timestamp} FROM solutions
             WHERE exoid = %int{exoid} AND userid = %int{userid}
             ORDER BY note DESC, created_timestamp ASC LIMIT 1|sql}]
  in
  let result = query ~exoid ~userid db
               >>= Caqti_lwt.or_fail |> Lwt_main.run
  in
  match result with
  | Some(r,t) -> (r,Util.string_of_time t)
  | None -> ("","")

let latest_solution userid exoid =
  let query =
    [%rapper get_opt
        {sql|SELECT @string{result},@ptime{created_timestamp} FROM solutions
             WHERE exoid = %int{exoid} AND userid = %int{userid}
             ORDER BY created_timestamp DESC LIMIT 1|sql}]
  in
  let result = query ~exoid ~userid db
               >>= Caqti_lwt.or_fail |> Lwt_main.run
  in
  match result with
  | Some(r,t) -> (r,Util.string_of_time t)
  | None -> ("","")

let record_solution userid exoid solution note result =
  let query =
    [%rapper execute
        {sql|INSERT INTO solutions (userid, exoid, solution, note, result)
             VALUES (%int{userid}, %int{exoid}, %string{solution}, %float{note}, %string{result})|sql}]
  in
  query ~exoid ~userid ~solution ~note ~result db
      >>= Caqti_lwt.or_fail |> Lwt_main.run

let record_exo userid query =
  let name = List.assoc "name" query in
  let fname = List.assoc "fname" query in
  let visible = List.assoc "visible" query = "true" in
  let dirname = List.assoc "dirname" query in
  let tpy = Util.read_file (List.assoc "tpy" query) in
  let question_ipynb = Util.read_file (List.assoc "question_ipynb" query) in
  let corrige_ipynb = Util.read_file (List.assoc "corrige_ipynb" query) in
  let question_py = Util.read_file (List.assoc "question_py" query) in
  let corrige_py = Util.read_file (List.assoc "corrige_py" query) in
  let test_py = Util.read_file (List.assoc "test_py" query) in
  let stdin =
    let name = List.assoc "stdin" query in
    if name = "" then None else Some(Util.read_file name)
  in
  let submit = List.assoc "submit" query in
  if submit <> "Annuler" then
    begin
      let query =
       [%rapper execute
        {sql|INSERT INTO exos (name, fname, visible, userid, tpy, question_ipynb,
                               corrige_ipynb, question_py, corrige_py,
                               test_py, stdin)
             VALUES (%string{name}, %string{fname}, %bool{visible},
                     %int{userid}, %string{tpy},
                     %string{question_ipynb}, %string{corrige_ipynb},
                     %string{question_py}, %string{corrige_py},
                     %string{test_py},%string?{stdin})|sql}]
      in
      query ~name ~fname ~visible ~userid ~tpy ~question_ipynb
        ~corrige_ipynb ~question_py ~corrige_py ~test_py ~stdin db
      >>= Caqti_lwt.or_fail |> Lwt_main.run;
    end;
  Sys.command ("rm -rf "^dirname)

let exo_visibility exoid visible =
  let query =
    [%rapper execute
        {sql|UPDATE exos SET visible = %bool{visible} WHERE exoid=%int{exoid}|sql}]
  in
  query ~exoid ~visible db
    >>= Caqti_lwt.or_fail |> Lwt_main.run

let exo_name exoid name =
  let query =
    [%rapper execute
        {sql|UPDATE exos SET name = %string{name} WHERE exoid=%int{exoid} AND name != %string{name} |sql}]
  in
  query ~exoid ~name db
    >>= Caqti_lwt.or_fail |> Lwt_main.run

let remove_exo exoid =
  let query =
    [%rapper execute
        {sql|DELETE FROM exos WHERE exoid=%int{exoid}|sql}]
  in
  query ~exoid db
    >>= Caqti_lwt.or_fail |> Lwt_main.run
