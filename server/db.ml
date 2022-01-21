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
           login text NOT NULL UNIQUE,
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
           name text NOT NULL UNIQUE,
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
           description text,
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
         CREATE TABLE IF NOT EXISTS user_exos (
           userid int,
           exoid int,
           CONSTRAINT userid_user_exos
             FOREIGN KEY(userid)
   	     REFERENCES users(userid),
           CONSTRAINT exoid_user_exos
             FOREIGN KEY(exoid)
   	     REFERENCES exos(exoid))
         |sql}]
  in

  let _ = query () db >>= Caqti_lwt.or_fail |> Lwt_main.run in

  let query =
    [%rapper execute
        {sql|
         CREATE TABLE IF NOT EXISTS user_classes (
           userid int,
           classid int,
           CONSTRAINT userid_user_classes
             FOREIGN KEY(userid)
   	     REFERENCES users(userid),
           CONSTRAINT classid_user_classes
             FOREIGN KEY(classid)
   	     REFERENCES classes(classid))
         |sql}]
  in

  let _ = query () db >>= Caqti_lwt.or_fail |> Lwt_main.run in

  let query =
    [%rapper execute
        {sql|
         CREATE TABLE IF NOT EXISTS exo_classes (
           exoid int,
           classid int,
           CONSTRAINT exoid_exo_classes
             FOREIGN KEY(exoid)
   	     REFERENCES exos(exoid),
           CONSTRAINT classid_exo_classes
             FOREIGN KEY(classid)
   	     REFERENCES classes(classid))
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
   	     REFERENCES exos(exoid)
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

let exists_login login =
  let query =
    [%rapper get_many
        {sql|SELECT FROM users WHERE login=%string{login}|sql}]
  in
  let result = query ~login db
               >>= Caqti_lwt.or_fail |> Lwt_main.run in

  match result with
  | _::_   -> true
  | []     -> false

let exists_classe name =
  let query =
    [%rapper get_many
        {sql|SELECT FROM users WHERE name=%string{name}|sql}]
  in
  let result = query ~name db
               >>= Caqti_lwt.or_fail |> Lwt_main.run in

  match result with
  | _::_   -> true
  | []     -> false

let all_classes () =
  let query =
    [%rapper get_many
        {sql|SELECT @string{name}, @int{classid}
             FROM classes|sql}]
  in
  query () db >>= Caqti_lwt.or_fail |> Lwt_main.run

let all_classes_exo exoid =
  let query =
    [%rapper get_many
        {sql|SELECT @string{c.name}, @int{c.classid}, @int{COUNT(u.exoid)}
             FROM classes as c
             LEFT JOIN exo_classes as u
             ON u.classid = c.classid AND u.exoid = %int{exoid}
             GROUP BY c.classid, c.name|sql}]
  in
  let result = query ~exoid db
               >>= Caqti_lwt.or_fail |> Lwt_main.run in

  List.map (fun (n,i,c) -> (n,i,c>0)) result

let change_exo_classe query =
  let classid = int_of_string (List.assoc "classid" query) in
  let exoid = int_of_string (List.assoc "exoid" query) in
  let checked = List.assoc "checked" query = "true" in
  let query =
    if checked then
      [%rapper execute
        {sql|INSERT INTO exo_classes (classid,exoid)
             VALUES (%int{classid},%int{exoid})|sql}]
    else
      [%rapper execute
        {sql|DELETE FROM exo_classes
             WHERE classid = %int{classid} AND exoid = %int{exoid}|sql}]
  in
  let _ = query ~classid ~exoid db
               >>= Caqti_lwt.or_fail |> Lwt_main.run in
  ()

let all_classes_user userid =
  let query =
    [%rapper get_many
        {sql|SELECT @string{c.name}, @int{c.classid}, @int{COUNT(u.userid)}
             FROM classes as c
             LEFT JOIN user_classes as u
             ON u.classid = c.classid AND u.userid = %int{userid}
             GROUP BY c.classid, c.name|sql}]
  in
  let result = query ~userid db
               >>= Caqti_lwt.or_fail |> Lwt_main.run in

  List.map (fun (n,i,c) -> (n,i,c>0)) result

let change_user_classe query =
  let classid = int_of_string (List.assoc "classid" query) in
  let userid = int_of_string (List.assoc "userid" query) in
  let checked = List.assoc "checked" query = "true" in
  let query =
    if checked then
      [%rapper execute
        {sql|INSERT INTO user_classes (classid,userid)
             VALUES (%int{classid},%int{userid})|sql}]
    else
      [%rapper execute
        {sql|DELETE FROM user_classes
             WHERE classid = %int{classid} AND userid = %int{userid}|sql}]
  in
  let _ = query ~classid ~userid db
               >>= Caqti_lwt.or_fail |> Lwt_main.run in
  ()

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
  let squery = [%rapper execute
              {sql|INSERT INTO users (login, name, firstname, password, role)
                   VALUES (%string{login}, %string{name}, %string{firstname},
                           %string{password}, %Role{role})|sql}]
  in
  squery ~login ~name ~firstname ~password ~role db
    >>= Caqti_lwt.or_fail |> Lwt_main.run;
  let classid = int_of_string (List.assoc "classe" query) in
  let squery =
    [%rapper get_opt
        {sql|SELECT @int{userid} FROM users WHERE login=%string{login}|sql}]
  in
  let result = squery ~login db
    >>= Caqti_lwt.or_fail |> Lwt_main.run in
  match result with
  | None -> ()
  | Some userid ->
     let squery =
     [%rapper execute
        {sql|INSERT INTO user_classes (userid, classid)
             VALUES (%int{userid}, %int{classid})|sql}]
     in
   squery ~userid ~classid db
    >>= Caqti_lwt.or_fail |> Lwt_main.run

let get_users admin =
  let query =
    if admin then
      [%rapper get_many
          {sql| SELECT @int{userid}, @string{login}, @string{name}, @string{firstname}
           FROM users |sql}]
    else
      [%rapper get_many
          {sql| SELECT @int{userid}, @string{login}, @string{name}, @string{firstname}
           FROM users
           WHERE role = 'student'|sql}]
  in
  let result = query () db
               >>= Caqti_lwt.or_fail |> Lwt_main.run in
  result


let creation_classe query =
  let name = List.assoc "name" query in
  let password = Digest.(to_hex (string (List.assoc "password" query))) in
  let password2 = Digest.(to_hex (string (List.assoc "password2" query))) in
  if password <> password2 then failwith "Password mismatch";
  let query = [%rapper execute
              {sql|INSERT INTO classes (name, password)
                   VALUES (%string{name}, %string{password})|sql}]
  in
  query ~name ~password db
               >>= Caqti_lwt.or_fail |> Lwt_main.run

let get_exo_by_id exoid =
  let query =
    [%rapper get_opt
        {sql|SELECT @ptime{created_timestamp},@string{name},
                    @string{fname},@bool{visible}
             FROM exos
             WHERE exoid=%int{exoid}|sql}]
  in
  let result = query ~exoid db
               >>= Caqti_lwt.or_fail |> Lwt_main.run in
  match result with
  | Some r -> r
  | None -> failwith "Bad exoid"

let get_exos_by_id userid =
  let query =
    [%rapper get_many
        {sql|SELECT @int{exoid},@ptime{created_timestamp},@string{name},
                    @string{fname},@bool{visible}
             FROM exos WHERE userid=%int{userid}
             ORDER BY created_timestamp DESC|sql}]
  in
  let result = query ~userid db
               >>= Caqti_lwt.or_fail |> Lwt_main.run in
  result

let get_visible_exos userid =
  let query =
    [%rapper get_many
        {sql|SELECT @int{exoid},@ptime{created_timestamp},@string{name},
                    @string{fname}
             FROM exos
             WHERE visible=true AND exoid IN (
               SELECT e.exoid FROM exo_classes as e, user_classes as u
                 WHERE u.userid = %int{userid} AND e.classid = u.classid)
             ORDER BY created_timestamp DESC|sql}]
  in
  let result = query ~userid db
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

let get_notes userid exoid =
  let query =
    [%rapper get_many
        {sql|SELECT @string{u.name}, @string{u.firstname}, @string{c.name},
                    @string{s.result}, @string{s.created_timestamp}, @int{s.userid}
             FROM solutions AS s, classes AS c, users AS u, user_classes AS uc,
                  user_classes AS pc, (
               SELECT s.note, max(s.created_timestamp), s.userid
               FROM solutions AS s, (
                 SELECT max(s.note),s.userid
                 FROM solutions AS s
                 WHERE s.exoid = %int{exoid}
                 GROUP BY s.userid) AS tmp
               WHERE s.note = tmp.max AND s.userid = tmp.userid AND s.exoid=%int{exoid}
               GROUP BY s.note, s.userid) AS tmp
             WHERE s.exoid = %int{exoid} AND u.userid = tmp.userid AND u.userid = uc.userid
               AND c.classid = uc.classid AND s.created_timestamp = max
               AND c.classid = pc.classid AND pc.userid = %int{userid}
             ORDER BY c.name, u.name, u.firstname
        |sql}]
  in
  let result = query ~userid ~exoid db
               >>= Caqti_lwt.or_fail |> Lwt_main.run in
  result

let get_latest_solution exoid userid =
  let query =
    [%rapper get_opt
        {sql|SELECT @string{e.fname},@string{s.solution}
             FROM solutions as s, exos as e WHERE e.exoid=%int{exoid}
                  AND s.userid=%int{userid} AND e.exoid = s.exoid
             ORDER BY s.created_timestamp DESC LIMIT 1|sql}]
  in
  let result = query ~exoid ~userid db
               >>= Caqti_lwt.or_fail |> Lwt_main.run in
  match result with
  | Some r -> r
  | None ->
     let query =
       [%rapper get_opt
            {sql|SELECT @string{fname},@string{question_py}
                 FROM exos WHERE exoid=%int{exoid}|sql}]
     in
     let result = query ~exoid db
               >>= Caqti_lwt.or_fail |> Lwt_main.run in
     match result with
     | Some r -> r
     | None -> failwith "can not find exercice"

let get_specific_solution exoid userid timestamp =
  let query =
    [%rapper get_opt
        {sql|SELECT @string{e.fname},@string{s.solution}
             FROM solutions as s, exos as e WHERE e.exoid=%int{exoid}
                  AND s.userid=%int{userid} AND e.exoid = s.exoid
                  AND s.created_timestamp = %string{timestamp}|sql}]
  in
  let result = query ~exoid ~userid ~timestamp db
               >>= Caqti_lwt.or_fail |> Lwt_main.run in
  match result with
  | Some r -> r
  | None -> raise Not_found

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
  Printf.eprintf "name: %s\n%!" name;
  let description = List.assoc "description" query in
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
        {sql|INSERT INTO exos (name, fname, visible, description,
                               userid, tpy, question_ipynb,
                               corrige_ipynb, question_py, corrige_py,
                               test_py, stdin)
             VALUES (%string{name}, %string{fname}, %bool{visible},
                     %string{description}, %int{userid}, %string{tpy},
                     %string{question_ipynb}, %string{corrige_ipynb},
                     %string{question_py}, %string{corrige_py},
                     %string{test_py},%string?{stdin})|sql}]
      in
      query ~name ~fname ~visible ~description ~userid ~tpy ~question_ipynb
        ~corrige_ipynb ~question_py ~corrige_py ~test_py ~stdin db
      >>= Caqti_lwt.or_fail |> Lwt_main.run;
    end;
  Sys.command ("rm -rf "^dirname)

let change_user_name query =
  let userid = int_of_string (List.assoc "userid" query) in
  let name = List.assoc "name" query in
  let query =
    [%rapper execute
        {sql|UPDATE users SET name = %string{name} WHERE userid=%int{userid} AND name != %string{name} |sql}]
  in
  query ~userid ~name db
    >>= Caqti_lwt.or_fail |> Lwt_main.run

let change_user_firstname query =
  let userid = int_of_string (List.assoc "userid" query) in
  let firstname = List.assoc "firstname" query in
  let query =
    [%rapper execute
        {sql|UPDATE users SET firstname = %string{firstname} WHERE userid=%int{userid} AND firstname != %string{firstname} |sql}]
  in
  query ~userid ~firstname db
    >>= Caqti_lwt.or_fail |> Lwt_main.run

let change_user_password query =
  let userid = int_of_string (List.assoc "userid" query) in
  let password = Digest.(to_hex (string (List.assoc "password" query))) in
  let password2 = Digest.(to_hex (string (List.assoc "password2" query))) in
  if password <> password2 then failwith "Password mismatch";
  let query =
    [%rapper execute
        {sql|UPDATE users SET password = %string{password} WHERE userid=%int{userid} |sql}]
  in
  query ~userid ~password db
    >>= Caqti_lwt.or_fail |> Lwt_main.run

let change_exo_name query =
  let exoid = int_of_string (List.assoc "exoid" query) in
  let name = List.assoc "name" query in
  let query =
    [%rapper execute
        {sql|UPDATE exos SET name = %string{name} WHERE exoid=%int{exoid} AND name != %string{name} |sql}]
  in
  query ~exoid ~name db
    >>= Caqti_lwt.or_fail |> Lwt_main.run

let change_exo_visibility query =
  let exoid = int_of_string (List.assoc "exoid" query) in
  let visible = List.assoc "visible" query in
  if visible = "remove" then
    let query =
      [%rapper execute
          {sql|DELETE FROM exos WHERE exoid=%int{exoid}|sql}]
    in
    query ~exoid db
    >>= Caqti_lwt.or_fail |> Lwt_main.run
  else
    begin
      let visible = visible = "visible" in
      let query =
        [%rapper execute
            {sql|UPDATE exos SET visible = %bool{visible} WHERE exoid=%int{exoid}|sql}]
      in
      query ~exoid ~visible db
      >>= Caqti_lwt.or_fail |> Lwt_main.run
    end
