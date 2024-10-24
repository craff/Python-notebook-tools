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

let db_config = Uri.of_string "postgresql://pynbtools:g5HTE3Ddgj258fJr@localhost"

module Rapper_helper = Rapper.Make_helper (struct
  type +'a t = 'a

  let return x = x

  let map x = x

  module Stream = Simple_httpd_caqti.Stream
end)

open Simple_httpd

let raise_error res =
  match res with
  | Ok x -> x
  | Error err ->
     Log.(f (Req 0)) (fun k -> k "error in request: %a" Caqti_error.pp err);
     raise (Caqti_error.Exn err)


let init () =
  Simple_httpd_caqti.with_connection db_config (fun db ->
  let query = [%rapper execute
                  {sql| CREATE TYPE role AS ENUM ('admin','prof','student')|sql}]
  in
  let _ =
    try ignore (query () db)
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

  let _ = raise_error @@ query () db in

  let query =
    [%rapper execute
        {sql|
         CREATE TABLE IF NOT EXISTS classes (
           classid SERIAL PRIMARY KEY,
           name text NOT NULL UNIQUE,
           password text NOT NULL)
         |sql}]
  in

  let _ = raise_error @@ query () db in

  let query =
    [%rapper execute
        {sql|
         CREATE TABLE IF NOT EXISTS courses (
           courseid SERIAL PRIMARY KEY,
           name text NOT NULL UNIQUE)
         |sql}]
  in

  let _ = raise_error @@ query () db in

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

  let _ = raise_error @@ query () db in

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

  let _ = raise_error @@ query () db in

  let query =
    [%rapper execute
        {sql|
         CREATE TABLE IF NOT EXISTS prof_courses (
           userid int,
           courseid int,
           CONSTRAINT userid_prof_courses
             FOREIGN KEY(userid)
   	     REFERENCES users(userid),
           CONSTRAINT courseid_prof_courses
             FOREIGN KEY(courseid)
   	     REFERENCES courses(courseid))
         |sql}]
  in

  let _ = raise_error @@ query () db in

  let query =
    [%rapper execute
        {sql|
         CREATE TABLE IF NOT EXISTS exo_courses (
           exoid int,
           courseid int,
           CONSTRAINT exoid_exo_courses
             FOREIGN KEY(exoid)
   	     REFERENCES exos(exoid),
           CONSTRAINT courseid_exo_courses
             FOREIGN KEY(courseid)
   	     REFERENCES courses(courseid))
         |sql}]
  in

  let _ = raise_error @@ query () db in

  let query =
    [%rapper execute
        {sql|
         CREATE TABLE IF NOT EXISTS courses_classes (
           classid int,
           courseid int,
           CONSTRAINT classid_courses_classes
             FOREIGN KEY(classid)
   	     REFERENCES classes(classid),
           CONSTRAINT courseid_courses_classes
             FOREIGN KEY(courseid)
   	     REFERENCES courses(courseid))
         |sql}]
  in

  let _ = raise_error @@ query () db in

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

  let _ = raise_error @@ query () db in

  Ok ())

exception Bad_login

let rec do_with_db2 ?(retry=3) fn =
  try raise_error @@ Simple_httpd_caqti.with_connection db_config (fun db -> Ok (fn db))
  with _ when retry > 0 -> do_with_db2 ~retry:(retry - 1) fn

let rec do_with_db ?(retry=3) fn req =
  try
    Simple_httpd_caqti.with_session ~db_config req (fun conn ->
        let res = fn (conn : Simple_httpd_caqti.connection) in
        res)
  with
  | exn when retry > 0 ->
     Log.(f (Req 0)) (fun k -> k "Retry db because of %s" (Printexc.to_string exn));
     do_with_db ~retry:(retry - 1) fn req
  | exn ->
     Log.(f (Req 0)) (fun k -> k "Exception in DB request %s" (Printexc.to_string exn));
     raise exn

let (>>) x f = f (raise_error @@ x)

let login ~login ~password =
  do_with_db2 (fun db ->
      let query =
        [%rapper get_opt
            {sql|SELECT @int{userid},@Role{role} FROM users WHERE login=%string{login} AND password = %string{password}|sql}]
      in
      let password = Digest.(to_hex (string password)) in
      query ~login ~password db >> function
      | None -> None
      | (Some (id, role)) -> (Some (login, id, role)))

let exists_login login =
  do_with_db (fun db ->
      let query =
        [%rapper get_many
            {sql|SELECT FROM users WHERE login=%string{login}|sql}]
      in
      query ~login db >> function
      | _::_   -> true
      | []     -> false)

let exists_classe name =
  do_with_db (fun db ->
      let query =
        [%rapper get_many
            {sql|SELECT FROM classes WHERE name=%string{name}|sql}]
      in
      query ~name db >> function
      | _::_   -> true
      | []     -> false)

let exists_course name =
  do_with_db (fun db ->
      let query =
        [%rapper get_many
            {sql|SELECT FROM courses WHERE name=%string{name}|sql}]
      in
      query ~name db >> function
      | _::_   -> true
      | []     -> false)

let all_classes () =
  do_with_db (fun db ->
      let query =
        [%rapper get_many
            {sql|SELECT @string{name}, @int{classid}
             FROM classes ORDER BY name|sql}]
      in
      raise_error @@ query () db)

let all_courses () =
  do_with_db (fun db ->
      let query =
        [%rapper get_many
            {sql|SELECT @string{name}, @int{courseid}
             FROM courses ORDER BY name|sql}]
      in
      raise_error @@ query () db)

let all_courses_exo exoid =
  do_with_db (fun db ->
      let query =
        [%rapper get_many
            {sql|SELECT @string{c.name}, @int{c.courseid}, @int{COUNT(u.exoid)}
             FROM courses as c
             LEFT JOIN exo_courses as u
             ON u.courseid = c.courseid AND u.exoid = %int{exoid}
             GROUP BY c.courseid, c.name|sql}]
      in
      query ~exoid db >> List.map (fun (n,i,c) -> (n,i,c>0)))

let is_in query =
  try
    List.assoc "is_in" query = "on"
  with Not_found -> false

let change_exo_course query =
  do_with_db (fun db ->
      let courseid = int_of_string (List.assoc "courseid" query) in
      let exoid = int_of_string (List.assoc "exoid" query) in
      let checked = is_in query in
      let query =
        if checked then
          [%rapper execute
              {sql|INSERT INTO exo_courses (courseid,exoid)
               VALUES (%int{courseid},%int{exoid})|sql}]
        else
          [%rapper execute
              {sql|DELETE FROM exo_courses
               WHERE courseid = %int{courseid} AND exoid = %int{exoid}|sql}]
      in
      raise_error @@ query ~courseid ~exoid db)

let all_classes_user userid =
  do_with_db (fun db ->
      let query =
        [%rapper get_many
            {sql|SELECT @string{c.name}, @int{c.classid}, @int{COUNT(u.userid)}
             FROM classes as c
             LEFT JOIN user_classes as u
             ON u.classid = c.classid AND u.userid = %int{userid}
             GROUP BY c.classid, c.name
             ORDER BY c.name|sql}]
      in
      query ~userid db >> List.map (fun (n,i,c) -> (n,i,c>0)))

let all_classes_course courseid =
  do_with_db (fun db ->
      let query =
        [%rapper get_many
            {sql|SELECT @string{c.name}, @int{c.classid}, @int{COUNT(u.courseid)}
             FROM classes as c
             LEFT JOIN courses_classes as u
             ON u.classid = c.classid AND u.courseid = %int{courseid}
             GROUP BY c.classid, c.name
             ORDER BY c.name|sql}]
      in
      query ~courseid db >> List.map (fun (n,i,c) -> (n,i,c>0)))

let all_courses_prof userid =
  do_with_db (fun db ->
      let query =
        [%rapper get_many
            {sql|SELECT @string{c.name}, @int{c.courseid}, @int{COUNT(u.userid)}
             FROM courses as c
             LEFT JOIN prof_courses as u
             ON u.courseid = c.courseid AND u.userid = %int{userid}
             GROUP BY c.courseid, c.name
             ORDER BY c.name|sql}]
      in
      query ~userid db >> List.map (fun (n,i,c) -> (n,i,c>0)))

let all_courses_student userid =
  do_with_db (fun db ->
      let query =
        [%rapper get_many
            {sql|SELECT @string{c.name}, @int{c.courseid}, @int{COUNT(u.userid)}
             FROM courses as c, courses_classes as cc, user_classes as u
             WHERE cc.courseid = c.courseid AND cc.classid = u.classid AND u.userid = %int{userid}
             GROUP BY c.courseid, c.name
             ORDER BY c.name|sql}]
      in
      query ~userid db >> List.map (fun (n,i,c) -> (n,i,c>0)))

let change_user_classe query =
  do_with_db (fun db ->
      let classid = int_of_string (List.assoc "classid" query) in
      let userid = int_of_string (List.assoc "userid" query) in
      let checked = is_in query in
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
      raise_error @@ query ~classid ~userid db)

let change_courses_classes query =
  do_with_db (fun db ->
      let classid = int_of_string (List.assoc "classid" query) in
      let courseid = int_of_string (List.assoc "courseid" query) in
      let checked = is_in query in
      let query =
        if checked then
          [%rapper execute
              {sql|INSERT INTO courses_classes (classid,courseid)
               VALUES (%int{classid},%int{courseid})|sql}]
        else
          [%rapper execute
              {sql|DELETE FROM courses_classes
               WHERE classid = %int{classid} AND courseid = %int{courseid}|sql}]
      in
      raise_error @@ query ~classid ~courseid db)

let change_prof_course query =
  do_with_db (fun db ->
      let courseid = int_of_string (List.assoc "courseid" query) in
      let userid = int_of_string (List.assoc "userid" query) in
      let checked = is_in query in
      let query =
        if checked then
          [%rapper execute
              {sql|INSERT INTO prof_courses (courseid,userid)
               VALUES (%int{courseid},%int{userid})|sql}]
        else
          [%rapper execute
              {sql|DELETE FROM prof_courses
               WHERE courseid = %int{courseid} AND userid = %int{userid}|sql}]
      in
      raise_error @@ query ~courseid ~userid db)

let new_user query is_admin =
  do_with_db (fun db ->
      let login = List.assoc "login" query in
      let name = List.assoc "name" query in
      let firstname = List.assoc "firstname" query in
      let role = try Role.decode (List.assoc "role" query)
                 with Not_found -> Role.Student
      in
      let password = Digest.(to_hex (string (List.assoc "password" query))) in
      if role <> Role.Student && not is_admin then failwith "You are trying to hack!";
      let classid =
        try
          match List.mem_assoc "classe" query with
          | true ->
             let classid = int_of_string (List.assoc "classe" query) in
             if not is_admin then
               begin
                 let classpass = List.assoc "classpass" query in
                 let squery =
                   [%rapper get_opt
                       {sql|SELECT @string{password} FROM classes WHERE classid = %int{classid}|sql}]
                 in
                 ignore (squery ~classid db >> function
                 | None -> raise Not_found
                 | Some p ->
                    let q = Digest.(to_hex (string classpass)) in
                    if p <> q then raise Not_found)
               end;
             Some classid
          | false -> None
        with _ -> failwith "Class password wrong";
      in
      let squery = [%rapper execute
                       {sql|INSERT INTO users (login, name, firstname, password, role)
                        VALUES (%string{login}, %string{name}, %string{firstname},
                        %string{password}, %Role{role})|sql}]
      in
      raise_error @@ squery ~login ~name ~firstname ~password ~role db;
      match classid with
      | None -> ()
      | Some classid ->
         let squery =
           [%rapper get_opt
               {sql|SELECT @int{userid} FROM users WHERE login=%string{login}|sql}]
         in
         squery ~login db >> (function
         | None -> ()
         | Some userid ->
            let squery =
              [%rapper execute
                  {sql|INSERT INTO user_classes (userid, classid)
                   VALUES (%int{userid}, %int{classid})|sql}]
            in
            raise_error @@ squery ~userid ~classid db))

let get_users admin =
  do_with_db (fun db ->
      let query =
        if admin then
          [%rapper get_many
              {sql| SELECT @int{userid}, @string{login}, @string{name}, @string{firstname}
               FROM users ORDER BY login |sql}]
        else
          [%rapper get_many
              {sql| SELECT @int{userid}, @string{login}, @string{name}, @string{firstname}
               FROM users
               WHERE role = 'student' ORDER BY login |sql}]
      in
      raise_error @@ query () db)

let creation_classe query =
  do_with_db (fun db ->
      let name = List.assoc "name" query in
      let password = Digest.(to_hex (string (List.assoc "password" query))) in
      let password2 = Digest.(to_hex (string (List.assoc "password2" query))) in
      if password <> password2 then failwith "Password mismatch";
      let query = [%rapper execute
                      {sql|INSERT INTO classes (name, password)
                       VALUES (%string{name}, %string{password})|sql}]
      in
      raise_error @@ query ~name ~password db)

let creation_course query =
  do_with_db (fun db ->
      let name = List.assoc "name" query in
      let query = [%rapper execute
                      {sql|INSERT INTO courses (name)
                       VALUES (%string{name})|sql}]
      in
      raise_error @@ query ~name db)

let get_exo_by_id exoid =
  do_with_db (fun db ->
      let query =
        [%rapper get_opt
            {sql|SELECT @ptime{created_timestamp},@string{name},
             @string{fname},@bool{visible}
             FROM exos
             WHERE exoid=%int{exoid}|sql}]
      in
      query ~exoid db >>
        (function
         | Some r -> r
         | None -> failwith "Bad exoid"))

let get_exos_by_id userid =
  do_with_db (fun db ->
      let query =
        [%rapper get_many
            {sql|SELECT @int{exoid},@ptime{created_timestamp},@string{name},
             @string{fname},@bool{visible}
             FROM exos WHERE userid=%int{userid}
             ORDER BY created_timestamp DESC|sql}]
      in
      raise_error @@ query ~userid db)

let get_visible_exos userid courseid =
  do_with_db (fun db ->
      let query =
        [%rapper get_many
        {sql|SELECT @int{exoid},@ptime{created_timestamp},@string{name},
                    @string{fname}
             FROM exos
             WHERE visible=true AND exoid IN (
               SELECT e.exoid FROM exo_courses as e, courses_classes c, user_classes as u
                 WHERE u.userid = %int{userid} AND e.courseid = %int{courseid} AND e.courseid = c.courseid AND c.classid = u.classid)
             ORDER BY created_timestamp DESC|sql}]
      in
      raise_error @@ query ~userid ~courseid db)

let get_note exoid userid =
  do_with_db (fun db ->
      let query =
        [%rapper get_opt
            {sql|SELECT @float{note} FROM solutions WHERE exoid=%int{exoid} AND userid=%int{userid} ORDER BY note DESC LIMIT 1|sql}]
      in
      raise_error @@ query ~exoid ~userid db)

let get_notes ?debut ?fin ?classid userid exoid =
  do_with_db (fun db ->
      let debut = match debut with
        | None   | Some "" -> "1970-01-01T00:00:00+00:00"
        | Some d -> d ^ "T00:00:00+00:00"
      in
      let fin = match fin with
        | None   | Some "" -> "2100-01-01T00:00:00+00:00"
        | Some d -> d ^ "T00:00:00+00:00"
      in
      let debut = MyUtil.time_of_string debut in
      let fin   = MyUtil.time_of_string fin   in
      match classid with
      | None ->
         let query =
           [%rapper get_many
           {sql|SELECT @string{u.name}, @string{u.firstname}, @string{c.name},
                    @string{s.result}, @string{s.created_timestamp}, @int{s.userid}
             FROM solutions AS s, classes AS c, users AS u, user_classes AS uc,
                   prof_courses AS pc, exo_courses AS ec, courses_classes AS cc, (
               SELECT s.note, max(s.created_timestamp), s.userid
               FROM solutions AS s, (
                 SELECT max(s.note),s.userid
                 FROM solutions AS s
                 WHERE s.exoid = %int{exoid}
                 AND s.created_timestamp >= %pdate{debut}
                 AND s.created_timestamp <= %pdate{fin}
                 GROUP BY s.userid) AS tmp
               WHERE s.note = tmp.max AND s.userid = tmp.userid
                 AND s.exoid=%int{exoid}
                 AND s.created_timestamp >= %pdate{debut}
                 AND s.created_timestamp <= %pdate{fin}
               GROUP BY s.note, s.userid) AS tmp
             WHERE s.exoid = %int{exoid} AND u.userid = tmp.userid
               AND u.userid = uc.userid
               AND c.classid = uc.classid AND s.created_timestamp = max
               AND pc.courseid = ec.courseid AND pc.userid = %int{userid}
               AND ec.exoid = s.exoid AND pc.courseid = cc.courseid
               AND C.classid = cc.classid
               AND s.created_timestamp >= %pdate{debut}
               AND s.created_timestamp <= %pdate{fin}
             ORDER BY c.name, u.name, u.firstname
            |sql}]
         in
         raise_error @@ query ~debut ~fin ~userid ~exoid db
      | Some classid ->
         let query =
           [%rapper get_many
        {sql|SELECT @string{u.name}, @string{u.firstname}, @string{c.name},
                    @string{s.result}, @string{s.created_timestamp}, @int{s.userid}
             FROM solutions AS s, classes AS c, users AS u, user_classes AS uc,
                  prof_courses AS pc, exo_courses AS ec, courses_classes AS cc, (
               SELECT s.note, max(s.created_timestamp), s.userid
               FROM solutions AS s, (
                 SELECT max(s.note),s.userid
                 FROM solutions AS s
                 WHERE s.exoid = %int{exoid}
                 AND s.created_timestamp >= %pdate{debut}
                 AND s.created_timestamp <= %pdate{fin}
                 GROUP BY s.userid) AS tmp
               WHERE s.note = tmp.max AND s.userid = tmp.userid AND s.exoid=%int{exoid}
               GROUP BY s.note, s.userid) AS tmp
             WHERE s.exoid = %int{exoid} AND u.userid = tmp.userid AND u.userid = uc.userid
               AND c.classid = uc.classid AND s.created_timestamp = max
               AND pc.courseid = ec.courseid AND pc.userid = %int{userid}
               AND ec.exoid = s.exoid AND pc.courseid = cc.courseid
               AND C.classid = cc.classid
               AND c.classid = %int{classid}
             ORDER BY c.name, u.name, u.firstname
         |sql}]
         in
         raise_error @@ query ~debut ~fin ~userid ~exoid ~classid db)

let get_notes_detail ?debut ?fin profid userid exoid =
  do_with_db (fun db ->
      let debut = match debut with
        | None   | Some "" -> "1970-01-01T00:00:00+00:00"
        | Some d -> d ^ "T00:00:00+00:00"
      in
      let fin = match fin with
        | None   | Some "" -> "2100-01-01T00:00:00+00:00"
        | Some d -> d ^ "T00:00:00+00:00"
      in
      let debut = MyUtil.time_of_string debut in
      let fin   = MyUtil.time_of_string fin   in
      let query =
        [%rapper get_many
            {sql|SELECT @string{s.result}, @string{s.created_timestamp}
             FROM solutions AS s, classes AS c, users AS u, user_classes AS uc,
                   prof_courses AS pc, exo_courses AS ec, courses_classes AS cc
             WHERE s.exoid = %int{exoid} AND s.userid = u.userid
               AND u.userid = uc.userid AND u.userid = %int{userid}
               AND c.classid = uc.classid
               AND pc.courseid = ec.courseid AND pc.userid = %int{profid}
               AND ec.exoid = s.exoid AND pc.courseid = cc.courseid
               AND c.classid = cc.classid
               AND s.created_timestamp >= %pdate{debut}
               AND s.created_timestamp <= %pdate{fin}
             ORDER BY s.created_timestamp
            |sql}]
         in
         raise_error @@ query ~debut ~fin ~userid ~exoid ~profid db)

let get_latest_solution exoid userid =
  do_with_db (fun db ->
      let query =
        [%rapper get_opt
        {sql|SELECT @string{e.fname},@string{s.solution}
             FROM solutions as s, exos as e WHERE e.exoid=%int{exoid}
                  AND s.userid=%int{userid} AND e.exoid = s.exoid
             ORDER BY s.created_timestamp DESC LIMIT 1|sql}]
      in
      query ~exoid ~userid db >>
      (function
      | Some r -> r
      | None ->
         let query =
           [%rapper get_opt
               {sql|SELECT @string{fname},@string{question_py}
                 FROM exos WHERE exoid=%int{exoid}|sql}]
         in
         query ~exoid db >>
           (function
            | Some r -> r
            | None -> failwith "can not find exercice")))

let get_specific_solution exoid userid timestamp =
  do_with_db (fun db ->
      let query =
        [%rapper get_opt
        {sql|SELECT @string{e.fname},@string{s.solution}
             FROM solutions as s, exos as e WHERE e.exoid=%int{exoid}
                  AND s.userid=%int{userid} AND e.exoid = s.exoid
                  AND s.created_timestamp = %string{timestamp}|sql}]
      in
      query ~exoid ~userid ~timestamp db >>
        (function
         | Some r -> r
         | None -> raise Not_found))

let has_solution exoid =
  do_with_db (fun db ->
      let query =
        [%rapper get_opt
            {sql|SELECT @int{COUNT(*)} FROM solutions WHERE exoid=%int{exoid}|sql}]
      in
      query ~exoid db >>
        (function
         | None -> false
         | Some n -> n > 0))

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
  do_with_db (fun db ->
      let query =
        [%rapper get_opt {sql|SELECT @string{test_py}, @string?{stdin} FROM exos WHERE exoid=%int{exoid}|sql}]
      in
      query ~exoid db >>
        (function
         | None -> raise Not_found
         | Some s -> s))

let download exoid ty =
  do_with_db (fun db ->
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
      query ~exoid db >>
        (function
         | None -> raise Not_found
         | Some s -> s))

let best_solution userid exoid =
  do_with_db (fun db ->
      let query =
        [%rapper get_opt
            {sql|SELECT @string{result},@ptime{created_timestamp} FROM solutions
                 WHERE exoid = %int{exoid} AND userid = %int{userid}
                 ORDER BY note DESC, created_timestamp ASC LIMIT 1|sql}]
      in
      query ~exoid ~userid db >>
      (function
      | Some(r,t) -> (r,MyUtil.string_of_time t)
      | None -> ("","")))

let latest_solution userid exoid =
  do_with_db (fun db ->
      let query =
        [%rapper get_opt
        {sql|SELECT @string{result},@ptime{created_timestamp} FROM solutions
             WHERE exoid = %int{exoid} AND userid = %int{userid}
             ORDER BY created_timestamp DESC LIMIT 1|sql}]
      in
      query ~exoid ~userid db >>
              (function
               | Some(r,t) -> (r,MyUtil.string_of_time t)
               | None -> ("","")))

let record_solution userid exoid solution note result =
  do_with_db (fun db ->
      let query =
        [%rapper execute
        {sql|INSERT INTO solutions (userid, exoid, solution, note, result)
             VALUES (%int{userid}, %int{exoid}, %string{solution}, %float{note}, %string{result})|sql}]
      in
      raise_error @@ query ~exoid ~userid ~solution ~note ~result db)

let record_exo userid query =
  do_with_db (fun db ->
      let name = List.assoc "name" query in
      let description = List.assoc "description" query in
      let fname = List.assoc "fname" query in
      let visible = List.assoc "visible" query = "true" in
      let dirname = List.assoc "dirname" query in
      let tpy = MyUtil.read_file (List.assoc "tpy" query) in
      let question_ipynb = MyUtil.read_file (List.assoc "question_ipynb" query) in
      let corrige_ipynb = MyUtil.read_file (List.assoc "corrige_ipynb" query) in
      let question_py = MyUtil.read_file (List.assoc "question_py" query) in
      let corrige_py = MyUtil.read_file (List.assoc "corrige_py" query) in
      let test_py = MyUtil.read_file (List.assoc "test_py" query) in
      let stdin =
        let name = List.assoc "stdin" query in
        if name = "" then None else Some(MyUtil.read_file name)
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
          raise_error @@ query ~name ~fname ~visible ~description ~userid ~tpy ~question_ipynb
            ~corrige_ipynb ~question_py ~corrige_py ~test_py ~stdin db
        end;
      Sys.command ("rm -rf "^dirname))

let change_course_name query =
  do_with_db (fun db ->
      let courseid = int_of_string (List.assoc "courseid" query) in
      let name = List.assoc "name" query in
      let query =
        [%rapper execute
            {sql|UPDATE courses SET name = %string{name} WHERE courseid=%int{courseid} AND name != %string{name} |sql}]
      in
      raise_error @@ query ~courseid ~name db)

let change_user_name query =
  do_with_db (fun db ->
      let userid = int_of_string (List.assoc "userid" query) in
      let name = List.assoc "name" query in
      let query =
        [%rapper execute
            {sql|UPDATE users SET name = %string{name} WHERE userid=%int{userid} AND name != %string{name} |sql}]
      in
      raise_error @@ query ~userid ~name db)

let change_user_firstname query =
  do_with_db (fun db ->
      let userid = int_of_string (List.assoc "userid" query) in
      let firstname = List.assoc "firstname" query in
      let query =
        [%rapper execute
            {sql|UPDATE users SET firstname = %string{firstname} WHERE userid=%int{userid} AND firstname != %string{firstname} |sql}]
      in
      raise_error @@ query ~userid ~firstname db)

let change_user_password query =
  do_with_db (fun db ->
      let userid = int_of_string (List.assoc "userid" query) in
      let password1 = List.assoc "password1" query in
      let password = Digest.(to_hex (string password1)) in
      let query =
        [%rapper execute
            {sql|UPDATE users SET password = %string{password} WHERE userid=%int{userid} |sql}]
      in
      raise_error @@ query ~userid ~password db)

let change_exo_name query =
  do_with_db (fun db ->
      let exoid = int_of_string (List.assoc "exoid" query) in
      let name = List.assoc "name" query in
      let query =
        [%rapper execute
            {sql|UPDATE exos SET name = %string{name} WHERE exoid=%int{exoid} AND name != %string{name} |sql}]
      in
      raise_error @@ query ~exoid ~name db)

let change_exo_visibility query =
  do_with_db (fun db ->
      let exoid = int_of_string (List.assoc "exoid" query) in
      let visible = List.assoc "visible" query in
      if visible = "remove" || visible = "forced_remove" then
        begin
          let query =
            [%rapper execute
                {sql|DELETE FROM exos WHERE exoid=%int{exoid}|sql}]
          in
          raise_error @@ query ~exoid db
        end
      else
        begin
          let visible = visible = "visible" in
          let query =
            [%rapper execute
                {sql|UPDATE exos SET visible = %bool{visible} WHERE exoid=%int{exoid}|sql}]
          in
          raise_error @@ query ~exoid ~visible db
        end)
