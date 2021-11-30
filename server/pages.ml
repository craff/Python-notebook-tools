open Tyxml

let href s =
  let prefix = Options.prefix in
  if prefix = "" then s else "/" ^ prefix ^ "/" ^ s

let css =
  {css|
   table, th, td {
     border: 1px solid black;
   }
   body {
     background-color: #D0D0FF;
   }
   |css}

let html_wrapper body =
  let style = href "style.css" in
  Format.asprintf "%a" (Html.pp ()) [%html
      {|<html>
         <head>
          <title>Pynbtools</title>
          <meta charset="utf-8"/>
          <link rel="stylesheet" href="|}style{|">
         </head>
        <body>|}body{|
        </body>
       </html>|}]

let inscription is_admin =
  let role =
    if is_admin then [%html {|
       <label for="role">role</label>
         <select name="role" id="role">
           <option value="student">Élève</option>
           <option value="prof">Professeur</option>
           <option value="admin">Administrateur</option>
         </select>|}]
    else []
  in
  [%html {|
      <h3>Ajout d'un utilisateur:</h3>
      <form method="Post" action="|}(href "new_user"){|">
       <label for="login">Login</label>
         <input type="text" name="login" id="login"/><br/>
       <label for="name">Nom</label>
         <input type="text" name="name" id="name"/><br/>
       <label for="firstname">Prénom</label>
         <input type="text" name="firstname" id="firstname"/><br/>
       <label for="password">Mot de passe</label>
         <input type="password" name="password" id="password"/><br/>
       <label for="password2">Répèter le mot de passe</label>
          <input type="password" name="password2" id="password2"/><br/>|}
       role{|
       <input type="submit" value="Ajoute l'utilisateur"/><br/>
            </form>|}]

let home_page () =
  let contents = ref [] in
  let add_contents x = contents := !contents @ x @ [[%html {|<br/>|}]] in

  add_contents [%html
     {|<h3>Connexion</h3>
       <form method="Post" action="|}(href "login"){|">
          <label for="login">Login</label>
           <input type="text" name="login" id="login"/>
          <label for="password">Mot de passe</label>
           <input type="password" name="password" id="password"/>
          <input type="submit" value="Entrer"/>
      </form>|}];

  add_contents (inscription false);

  html_wrapper !contents

let exos_conf id =
  let exos = Db.get_exos_by_id id in
  let fn (id,time,name,fname,visible) =
    let exoid = string_of_int id in
    let time = Util.string_of_time time in
    let dt = [Html.txt (name ^ " (" ^ time ^ ")")] in
    let py_url = href ("download?exoid="^exoid^"&type=question_py") in
    let ipynb_url = href ("download?exoid="^exoid^"&type=question_ipynb") in
    let cpy_url = href ("download?exoid="^exoid^"&type=corrige_py") in
    let cipynb_url = href ("download?exoid="^exoid^"&type=corrige_ipynb") in
    let tpy_url = href ("download?exoid="^exoid^"&type=test_py") in
    let tpl_url = href ("download?exoid="^exoid^"&type=template") in
    let html1 =
      if visible then
        [%html {|
            <input type="radio" name="|}exoid{|" id="visible" value="visible" checked/>
          <label for="visible">visible</label>
            <input type="radio" name="|}exoid{|" id="hidden" value="hidden"/>|}]
      else
        [%html {|
            <input type="radio" name="|}exoid{|" id="visible" value="visible"/>
          <label for="visible">visible</label>
            <input type="radio" name="|}exoid{|" id="hidden" value="hidden" checked/>|}]
    in
    let show_notes =
      if Db.has_solution id then
        begin
          let show_url = Printf.sprintf "show_notes?exoid=%s&exoname=%s"
                           exoid (Tiny_httpd_util.percent_encode name)
          in
          [[%html {|<a href="|}show_url{|">voir les notes</a>|}]]
        end
      else
        []
    in
    [%html {|
              <label for="|}exoid{|">|}dt{|</label><br/>
              <a href="|}py_url{|" download="|}(Some (fname^".py")){|">python code</a>
              <a href="|}ipynb_url{|" download="|}(Some (fname^".ipynb")){|">python notebook</a>
              <a href="|}cpy_url{|" download="|}(Some (fname^".py")){|">python corrigé</a>
              <a href="|}cipynb_url{|" download="|}(Some (fname^".ipynb")){|">corrigé notebook</a>
              <a href="|}tpy_url{|" download="|}(Some (fname^".py")){|">python test</a>
              <a href="|}tpl_url{|" download="|}(Some (fname^".tpy")){|">template</a><br/>
              |}(show_notes @ html1){|
              <label for="hidden">caché</label>
               <input type="radio" name="|}exoid{|" id="remove" value="remove"/>
              <label for="remove">supprimé</label><br/>|}]
  in
  let exos_form = List.flatten (List.map fn exos) in
  [%html {|
          <h3>Visibilité et suppression des exercices</h3>
          <form method="Post" action="update_exo">|}exos_form{|
           <input type="submit" name="submit" value="Effectuer les modifications"/>
          </form>|}]

let show_notes exoid exoname =
  let notes = Db.get_notes exoid in
  let headers = ref [] in
  let fn (name,firstname,result) =
    let l = String.split_on_char '\n' result in
    let gn l = match String.split_on_char ':' l with
      | h::n::_ -> Some (h, float_of_string n)
      | _      -> None
    in
    let l = List.filter_map gn l in
    let (h,l) = List.split l in
    headers := h;
    let l = List.map (fun f -> let f = Printf.sprintf "%.1f" f in
                               [%html {|<td>|}[Html.txt f]{|</td>|}]) l
    in
    [%html {|<tr><td>|}[Html.txt name]{|</td>
             <td>|}[Html.txt firstname]{|</td>|}l{|</tr>|}]
  in
  let notes = List.map fn notes in
  let headers = "Nom" :: "Prénom" :: !headers in
  let headers = List.map (fun s -> [%html {|<th>|}[Html.txt s]{|</th>|}]) headers in
  let html =
    [%html
        {|<h3>Notes de l'exercice «|}[Html.txt exoname]{|»</h3>
          <table><tr>|}headers{|</tr>|}notes{|</table>|}]
  in
  Ok (html_wrapper html)

let form_exo userid exoid name time fname =
  let note = Db.get_note exoid userid in
  let exoid = string_of_int exoid in
  let time = Util.string_of_time time in
  let dt = [Html.txt (name ^ " (" ^ time ^ ")")] in
  let py_url = href ("download?exoid="^exoid^"&type=question_py") in
  let ipynb_url = href ("download?exoid="^exoid^"&type=question_ipynb") in
  let show_url = href (Printf.sprintf "show_solution?exoid=%s" exoid) in
  let note = match note with
    | None -> []
    | Some n ->
       let n = [Html.txt (Printf.sprintf "%.1f" n)] in
       [%html {|&nbsp;<a href="|}show_url{|">Note: |}n{|</a>|}]
  in
    [%html
      dt{|:<br/>
        <form enctype="multipart/form-data" method="Post" action="|}(href "submit_solution"){|">
           <a href="|}py_url{|" download="|}(Some (fname^".py")){|">python code</a>
           <a href="|}ipynb_url{|" download="|}(Some (fname^".ipynb")){|">python notebook</a>|}note{|<br/>
           <input type="hidden" name="exoid" value="|}exoid{|"/>
           <label for="file">Envoyer une solution</label>
              <input type="file" id="file" name="solution"/>
           <input type="submit" name="submit" value="Envoyer"/>
        </form><br/>|}]

let exos_student userid =
  let exos = Db.get_visible_exos () in
  let fn (exoid,time,name,fname) =
    form_exo userid exoid name time fname
  in
  [%html {|<h3>Exercices disponibles</h3>|}] ::
    List.flatten (List.map fn exos)

let home_page_logged login id role =
  let contents = ref [] in
  let add_contents x = contents := !contents @ x @ [[%html {|<br/>|}]] in
  add_contents [[%html {|
       <form method="Get" action="|}(href "logout"){|">
         <label for="submit">Connecter comme: |}[Html.txt login]{|</label>
          <input type="submit" id="submit" value="Déconnexion"/><br/>
       </form>|}]];

  begin
    let open Db.Role in
    match role with
    | Student ->
       add_contents (exos_student id);
    | Prof | Admin ->
       add_contents (exos_conf id);

       add_contents [%html {|
      <h3>Ajouter un exercice:</h3>
       <form enctype="multipart/form-data" method="Post" action="|}(href "new_exo"){|">
        <label for="name">Nom de l'exercice</label>
         <input type="text" name="name" id="name"/><br/>
        <label for="visible">Visible</label>
         <input type="checkbox" name="visible" id="visible"/>
        <label for="tpy">Télécharger le template python</label>
         <input type="file" name="tpy" id="tpy"/><br/>
        <label for="stdin">stdin pour ce test(optionnel)</label>
         <input type="file" name="stdin" id="stdin"/><br/>
        <input type="submit" value="Envoyer le ou les fichiers"/><br/>
       </form>|}];

       add_contents  (inscription true);

  end;

  html_wrapper !contents

let show_solution ?(now="") userid exoid =
  let (time,name,fname,_visible) = Db.get_exo_by_id exoid in
  let (best,bt) = Db.best_solution userid exoid in
  let (latest,lt) = Db.latest_solution userid exoid in
  let nowh = if now <> "" then [[%html {|<th>Actuelle</th>|}]] else [] in
  let nowt = if now <> "" then [[%html {|<td>maintenant</td>|}]] else [] in
  let nowd = if now <> "" then [[%html {|<td><pre>|}[Html.txt now]{|</pre></td>|}]] else [] in
  let html = [%html {|
    <h3>Résultat de l'exercice</h3>
    <table>
    <tr>|}nowh{|<th>Meilleure</th><th>Dernière</th></tr>
    <tr>|}nowt{|<td>|}[Html.txt bt]{|</td><td>|}[Html.txt lt]{|</td></tr>
    <tr>|}nowd{|
        <td><pre>|}[Html.txt best]{|</pre></td>
        <td><pre>|}[Html.txt latest]{|</pre></td>
        </tr></table><br/>|}
    (form_exo userid exoid name time fname)]
  in
  html_wrapper html

let add_solution userid exoid fname solution =
  let open Util in
  let dirname = new_temp_dir () in
  try
    let extension = Filename.extension fname in
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
          let (_out,_err,status) = run_cmd ("nb2py " ^ fname) in
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
    let (result,_err,status) = run_cmd ("python3 /usr/local/lib/pynbtools/evaluation.py "
                                     ^ test_name ^" " ^ fname ^" " ^stdin_name) in
    if status <> Unix.WEXITED 0 then failwith "bad python file";
    let note =
      try
        let ln = last_line result in
        let open Str in
        let regexp = regexp "Total: \\([0-9.eE-]*\\)" in
        if string_match regexp ln 0 then
          float_of_string (matched_group 1 ln)
        else 0.0
      with _ -> 0.0
    in
    let html = show_solution ~now:result userid exoid in
    Db.record_solution userid exoid solution note result;
    let _ = Sys.command ("rm -rf "^dirname) in
    Ok html
  with
    e ->
    let _ = Sys.command ("rm -rf "^dirname) in
    Error(404,Printexc.to_string e)

let add_exo ?stdin name visible fname content =
  let open Util in
  let dirname = new_temp_dir () in
  try
    let bname = Filename.remove_extension fname in
    let fname = Filename.concat dirname fname in
    let rname = Filename.concat dirname bname in
    let ch = open_out fname in
    output_string ch content;
    close_out ch;
    let stdin_name =
      match stdin with
      | None -> ""
      | Some str ->
         let fname = Filename.concat dirname "stdin.txt" in
         let ch = open_out fname in
         output_string ch str;
         close_out ch;
         fname
    in
    let (_out,_err,status) = run_cmd ("py2nb " ^ fname) in
    if status <> Unix.WEXITED 0 then failwith "bad python template";
    let question_ipynb_fname = rname ^ ".ipynb" in
    let corrige_ipynb_fname = rname ^ "_corrige.ipynb" in
    let test_ipynb_fname = rname ^ "_test.ipynb" in
    let ipynbs = [question_ipynb_fname; corrige_ipynb_fname; test_ipynb_fname] in
    let (_out,_err,status) = run_cmd ("nb2py " ^ String.concat " " ipynbs) in
    if status <> Unix.WEXITED 0 then failwith "bad python template";
    let question_py_fname = rname ^ ".py" in
    let corrige_py_fname = rname ^ "_corrige.py" in
    let test_py_fname = rname ^ "_test.py" in
    let (out1,_err,status) = run_cmd ("python3 /usr/local/lib/pynbtools/evaluation.py " ^ test_py_fname ^" " ^ question_py_fname ^" " ^stdin_name) in
    if status <> Unix.WEXITED 0 then failwith "bad python template";
    let (out2,_err,status) = run_cmd ("python3 /usr/local/lib/pynbtools/evaluation.py " ^ test_py_fname ^" " ^ corrige_py_fname ^" "^stdin_name) in
    if status <> Unix.WEXITED 0 then failwith "bad python template";
    let visible = if visible then "true" else "false" in
    let html = html_wrapper [%html {|
      Évaluation de l'exercice: |}[Html.txt name]{|<br/>
      <table>
       <tr><th>Correction</th><th>Question</th></tr>                                         <tr><td><pre>|}[Html.txt out2]{|</pre></td>
           <td><pre>|}[Html.txt out1]{|</pre></td></tr>
      </table>
      <form method="Post" action="|}(href "record_exo"){|">
        <input type="hidden" name="name" value="|}name{|"/>
        <input type="hidden" name="fname" value="|}bname{|"/>
        <input type="hidden" name="visible" value="|}visible{|"/>
        <input type="hidden" name="dirname" value="|}dirname{|"/>
        <input type="hidden" name="tpy" value="|}fname{|"/>
        <input type="hidden" name="question_ipynb" value="|}question_ipynb_fname{|"/>
        <input type="hidden" name="corrige_ipynb" value="|}corrige_ipynb_fname{|"/>
        <input type="hidden" name="question_py" value="|}question_py_fname{|"/>
        <input type="hidden" name="corrige_py" value="|}corrige_py_fname{|"/>
        <input type="hidden" name="test_py" value="|}test_py_fname{|"/>
        <input type="hidden" name="stdin" value="|}stdin_name{|"/>
        <input type="submit" name="submit" value="Enregitrer"/>
        <input type="submit" name="submit" value="Annuler"/>
      </form>|}]
    in
    (* TODO: remove dirname if this form is not submitted after some time ?
       If user leaves that page? Both?
    *)
    Ok html;
  with
    e ->
    let _ = Sys.command ("rm -rf "^dirname) in
    Error(404,Printexc.to_string e)
