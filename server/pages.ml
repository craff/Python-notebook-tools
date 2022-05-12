open Tyxml

(** SVG *)
let home_svg =
  Html.svg ~a:[Svg.a_width (30., Some `Px); Svg.a_height (30., Some `Px);
               Svg.a_viewBox (0.,0.,50.,40.)]
    [[%svg {|
         <polygon points="10,40 40,40 40,15 50,15 25,0 0,15 10,15 10,40" style="fill:#3030FF;stroke-width:0"/>|}]]

let hide_svg =
  Html.svg ~a:[Svg.a_width (30., Some `Px); Svg.a_height (30., Some `Px);
               Svg.a_viewBox (0.,0.,50.,40.)]
    [[%svg {|
         <polygon points="0,0 10,15 0,30 0,0" style="fill:#3030FF;stroke-width:0"/>|}]]

let show_svg =
  Html.svg ~a:[Svg.a_width (30., Some `Px); Svg.a_height (30., Some `Px);
               Svg.a_viewBox (0.,0.,50.,40.)]
    [[%svg {|
         <polygon points="0,0 15,10 30,0 0,0" style="fill:#3030FF;stroke-width:0"/>|}]]

(** General *)
let href s =
  let prefix = Options.prefix in
  if prefix = "" then s else
    if String.length s > 0 && s.[0] = '/' then
      "/" ^ prefix ^ s
    else
      "/" ^ prefix ^ "/" ^ s

let html_wrapper body =
  let style = href "style.css" in
  let scripts = href "scripts.js" in
  Format.asprintf "%a" (Html.pp ()) [%html
      {|<html>
         <head>
          <title>Pynbtools</title>
          <meta charset="utf-8"/>
          <link rel="stylesheet" href="|}style{|">
          <script src="|}scripts{|"></script>
         </head>
        <body>|}body{|
        </body>
       </html>|}]

let header ?(is_home=false) login =
  let home_button =
    if is_home then [] else
      [[%html {|<div style="position:absolute; top:1px; left:5px;" onclick="|}
           ("location.href='" ^ href "" ^ "';"){|">|}[home_svg]{|</div>|}]]
  in home_button @
    [%html {|
     <div style="position:absolute; top:5px; right:5px;">
       <form method="Get" action="|}(href "logout"){|">
         <label for="submit">Connecter comme: |}[Html.txt login]{|</label>
          <input type="submit" id="submit" value="Déconnexion"/><br/>
       </form></div>|}] :: []

let onglet ?(visible=true) title content =
  let id = Digest.(to_hex (string title)) in
  let id_show = id ^ "_show" in
  let id_hide = id ^ "_hide" in
  let click_onglet hide =
    let (ht,id2,id3) = if hide then ("true",id_hide,id_show)
                      else ("false",id_show,id_hide)
    in
    "click_onglet('"^id^"','"^id2^"','"^id3^"',"^ht^");"
  in
  let style = if visible then "display:block;" else "display:none;" in
  let style' = if visible then "display:none;" else "display:block;" in
  let style_hide = "position:absolute; top:5px; left:5px; " ^ style in
  let style_show = "position:absolute; top:5px; left:5px; " ^ style' in
  [%html {|
    <div style="position:relative; padding:0px; margin:0px;">
      <div style="|}style_hide{|" id="|}id_hide{|"
           onclick="|}(click_onglet true){|">
        |}[hide_svg]{|
      </div>
      <div style="|}style_show{|" id="|}id_show{|"
           onclick="|}(click_onglet false){|">
        |}[show_svg]{|
      </div>
      <h3 class="onglet_h3">|}[Html.txt title]{|</h3>
        <div id="|}id{|" class="onglet_content" style="|}style{|">|}
          content{|
      </div>
    </div>|}]

(** CSS *)
let css =
  {css|
   .table, .table th, .table td  {
     border: 2px solid #0000FF;
     border-collapse: collapse;
   }
   td, th {
     padding-left: 20px;
     padding-right: 20px;
   }
   .fill {
     text-align: center;
     width: 100%;
   }
   .fill td {
     text-align: left;
   }
   .center {
     display: flex;
     justify-content: center;
   }
   .cl {
     text-align:center;
     line-height:1.8;
   }
   .cl-item {
     display:inline-block;
     border:2px solid #2020A0;
     margin-right:2px;
     margin-left:2px;
     margin-top:1px;
     margin-bottom:1px;
     padding-right:2px;
     padding-left:2px;
     padding-top:0px;
     padding-bottom:0px;
   }
   body {
     background-color: #D0D0FF;
     margin: 0;
     padding: 0;
   }
   h1 {
     text-align: center;
     border-top-style: solid;
     border-bottom-style: solid;
     border-color: #0000FF;
     background-color: #A0A0FF;
     margin: 0;
     padding: 10px;
     margin-bottom: 15px;
   }
   h2 {
     text-align: center;
     border-top-style: solid;
     border-bottom-style: solid;
     border-color: #0000FF;
     background-color: #A0A0FF;
     margin: 0;
     padding: 9px;
     margin-bottom: 13px;
   }
   h3 {
     text-align: center;
     background-color: #8080FF;
     margin: 0;
     padding: 7px;
     margin-bottom: 12px;
   }
   .onglet_h3 {
     text-align: center;
     background-color: #8080FF;
     margin: 0;
     padding: 7px;
   }
   .onglet_content {
     margin: 12px;
   }
   h5 {
     text-align: center;
     background-color: #A0A0FF;
     margin: 0;
     padding: 4px;
     margin-bottom: 4px;
   }
   |css}

(** Javascript *)
let scripts =
  let exists_login = href "exists_login" in
  let exists_classe = href "exists_classe" in
  {|
    function request(url) {
      console.log(url);
      var request = new XMLHttpRequest();
      request.open('GET', url, false);
      request.send();
      var str=request.responseText;
      console.log(str);
      return str.substring(0, 4) == "true";
   }
    function request_post(url,data) {
      console.log(url);
      var request = new XMLHttpRequest();
      request.open('POST', url, false);
      request.send(data);
      var str=request.responseText;
      console.log(str);
      return str.substring(0, 4) == "true";
   }
   function validate_new_user() {
     var f = document.forms["new_user"];
     console.log(f["login"]);
     if (f["login"].value == "" || f["name"].value == ""
         || f["firstname"].value == "" || f["classe"].value == ""
         || f["password"].value == "" || f["password2"].value == "") {
        alert("Il faut remplir tous les champs");
        return(false);
     }
     if (f["password"].value != f["password2"].value) {
        alert("Les deux mots de passe ne sont pas identique");
        return(false);
     }
     if (f["password"].value.length < 4) {
        alert("le mot de passe doit comporter au moins 4 caractère");
        return(false);
     }
     if (request("|}^exists_login^{|?login=" + f["login"].value)) {
        alert("ce login est déjà utilisé");
        return(false);
     }
     return(true);
   }
   function validate_login() {
     var f = document.forms["login"];
     console.log(f["login"]);
     if (f["login"].value == ""
         || f["password"].value == "" ) {
        alert("Il faut remplir tous les champs");
        return(false);
     }
     return(true);
   }
   function validate_creation_classe() {
     var f = document.forms["creation_classe"];
     console.log(f["login"]);
     if (f["name"].value == ""
         || f["password"].value == "" || f["password2"].value == "") {
        alert("Il faut remplir tous les champs");
        return(false);
     }
     if (f["password"].value != f["password2"].value) {
        alert("Les deux mots de passe ne sont pas identique");
        return(false);
     }
     if (f["password"].value.length < 4) {
        alert("le mot de passe doit comporter au moins 4 caractère");
        return(false);
     }
     if (request("|}^exists_classe^{|?name=" + f["name"].value)) {
        alert("cette classe existe déjà");
        return(false);
     }
     return(true);
   }
   function change_user_classe(form_name) {
     var f = document.forms[form_name];
     var classid = f["classid"].value;
     var userid = f["userid"].value;
     var checked = f["is_in"].checked;
     if (!request("change_user_classe?userid="+userid+
                                    "&classid="+classid+
                                    "&checked="+checked))
       alert("Le changement n'a pas été propagé sur le site. Rechargez la page");
   }
   function change_exo_classe(form_name) {
     var f = document.forms[form_name];
     var classid = f["classid"].value;
     var exoid = f["exoid"].value;
     var checked = f["is_in"].checked;
     if (!request("change_exo_classe?exoid="+exoid+
                                    "&classid="+classid+
                                    "&checked="+checked))
       alert("Le changement n'a pas été propagé sur le site. Rechargez la page");
   }
   function change_exo_visibility(form_name) {
     var f = document.forms[form_name];
     var visible = f["visible"].value;
     var exoid = f["exoid"].value;
     if (visible != "remove" ||
         window.confirm("Êtes vous sur de vouloir supprimer cet exercice?")) {
       if (!request("change_exo_visibility?exoid="+exoid+
                                      "&visible="+visible))
         {
           if (visible == "remove")
             alert("L'exercice n'a pu être supprimé, il y a des solutions attachées à cet exercice");
           else
             alert("Le changement n'a pas été propagé sur le site. Rechargez la page");
         }
     }
   }
   var exo_name_timeout=null;
   function change_exo_name(form_name) {
     clearTimeout(exo_name_timeout);
     exo_name_timeout = setTimeout(function () {
       var f = document.forms[form_name];
       var exoid = f["exoid"].value;
       var name = f["name"].value;
       if (!request("change_exo_name?exoid="+exoid+
                                      "&name="+name))
         alert("Le changement n'a pas été propagé sur le site. Rechargez la page");
     }, 1000);
   }
   function click_onglet(id,id2,id3,hide) {
     console.log(id,id2,id3,hide);
     var el1 = document.getElementById(id);
     el1.style.display=hide?"none":"block";
     var el2 = document.getElementById(id2);
     el2.style.display="none";
     var el3 = document.getElementById(id3);
     el3.style.display="block";
   }
   var user_name_timeout=null;
   function change_user_name(form_name) {
     clearTimeout(user_name_timeout);
     user_name_timeout = setTimeout(function () {
       var f = document.forms[form_name];
       var userid = f["userid"].value;
       var name = f["name"].value;
       if (!request("change_user_name?userid="+userid+
                                      "&name="+name))
         alert("Le changement n'a pas été propagé sur le site. Rechargez la page");
     }, 1000);
   }
   var user_firstname_timeout=null;
   function change_user_firstname(form_name) {
     clearTimeout(user_firstname_timeout);
     user_firstname_timeout = setTimeout(function () {
       var f = document.forms[form_name];
       var userid = f["userid"].value;
       var firstname = f["firstname"].value;
       if (!request("change_user_firstname?userid="+userid+
                                      "&firstname="+firstname))
         alert("Le changement n'a pas été propagé sur le site. Rechargez la page");
     }, 1000);
   }
   function change_user_password(form_name) {
     var f = document.forms[form_name];
     var userid = f["userid"].value;
     var password1 = f["password1"].value;
     var password2 = f["password2"].value;
     if (password1 != password2) {
        alert("Les deux mots de passe ne sont pas identique");
        return(false);
     }
     if (password1.length < 4) {
      alert("le mot de passe doit comporter au moins 4 caractère");
      return(false);
     }
     var data = 'userid='+userid+'&password='+password1;
     if (!request_post("change_user_password",data))
       alert("Le changement n'a pas été propagé sur le site. Rechargez la page");
   }
|}


(** Users *)

let create_user is_admin =
  let classe_menu =
    let classes = Db.all_classes () in
    let fn (name,classid) =
      [%html {|<option value="|}(string_of_int classid){|">|}
          (Html.txt name){|</option>|}]
    in
    let options = List.map fn classes in
    [%html
      {|<tr><td>
        <label for="classe">Classe:</label>
        </td><td>
        <select name="classe" id="classe">
        <option value="" selected>Choisis ta classe</option>
        |}options{|
        </select></td></tr>|}]
  in
  let role =
    if is_admin then [[%html {|
       <tr><td>
       <label for="role">Role:</label>
       </td><td>
         <select name="role" id="role">
           <option value="student">Élève</option>
           <option value="prof">Professeur</option>
           <option value="admin">Administrateur</option>
         </select>
      </td></tr>|}]]
    else []
  in
  onglet ~visible:(not is_admin) "Inscription au site"
    [[%html {|
      <div class="center">
      <form name="new_user" method="Post"
            onsubmit="return validate_new_user();"
            action="|}(href "new_user"){|">
      <table class="fill"><tr><td>
       <label for="login">Identifiant:</label>
       </td><td>
         <input type="text" name="login" id="login"/><br/>
       </td></tr><tr><td>
       <label for="name">Nom</label>
       </td><td>
         <input type="text" name="name" id="name"/><br/>
       </td></tr><tr><td>
       <label for="firstname">Prénom:</label>
       </td><td>
         <input type="text" name="firstname" id="firstname"/><br/>
       </td></tr><tr><td>
       <label for="password">Mot de passe:</label>
       </td><td>
         <input type="password" name="password" id="password"/><br/>
       </td></tr><tr><td>
       <label for="password2">Répèter le mot de passe:</label>
       </td><td>
          <input type="password" name="password2" id="password2"/><br/>
       </td></tr>|}
       (classe_menu :: role) {|
       <tr><td></td><td>
       <input type="submit" value="Valider"/><br/>
       </td></tr></table></form></div>|}]]

let manage_users admin =
  let users = Db.get_users admin in
  let fn (userid, login, name, firstname) =
    let suserid = string_of_int userid in
    let form_name = "name_" ^ suserid in
    let form_firstname = "firstname_" ^ suserid in
    let form_password = "password_" ^ suserid in
    let oninput_name = "change_user_name('"^form_name^"');" in
    let oninput_firstname = "change_user_firstname('"^form_firstname^"');" in
    let onclick_pwd = "change_user_password('"^form_password^"');" in

    [%html {|
       <tr>
          <td style="text-align:right;">Login:</td>
          <td style="text-align:left;">|}[Html.txt login]{|</td>
          <td style="text-align:right;">
          <label for="name">Nom:</label></td>
          <td style="text-align:left;">
             <form id="|}form_name{|">
               <input type="hidden" name="userid" value="|}suserid{|"/>
               <input type="text" name="name" id="name" value="|}name{|"
                      oninput="|}oninput_name{|"/>
             </form>
          </td>
       </tr><tr>
          <td style="text-align:right;">Userid:</td>
          <td style="text-align:left;">|}[Html.txt suserid]{|</td>
          <td style="text-align:right;">
          <label for="firstname">Prénom:</label></td>
          <td>
             <form id="|}form_firstname{|">
                <input type="hidden" name="userid" value="|}suserid{|"/>
                <input type="text" name="firstname" id="firstname" value="|}firstname{|"
                      oninput="|}oninput_firstname{|"/>
             </form>
          </td>
       </tr><tr>
       <td style="text-align:right;">Changement de mot de passe:</td>
       <td style="text-align:left; padding-right:0;"><input size="10" form="|}form_password{|" type="password" name="password1" autocomplete="off" readonly
onfocus="this.removeAttribute('readonly');"/></td>
       <td style="text-align:right; padding-left:0;"><input size="10" form="|}form_password{|" type="password" name="password2" autocomplete="off" readonly
onfocus="this.removeAttribute('readonly');"/></td>
       <td style="text-align:left;">
          <form id="|}form_password{|">
            <input type="hidden" name="userid" value="|}suserid{|"/>
            <input type="button" name="change_password" value="Appliquer"
                      onclick="|}onclick_pwd{|"/>
          </form>
       </td>
       </tr>
   |}]
  in
  let users_form = List.flatten (List.map fn users) in
  onglet ~visible:false "Gestion des utilisateurs"
      [[%html {|<table class="fill"><tbody>|}users_form{|</tbody></table>|}]]

(** Classes *)
let manage_classes _id =
  onglet ~visible:false "Gestion des classes"
    []

let creation_classe () =
  onglet ~visible:false "Ajout d'une classe"
  [[%html {|
      <form name="creation_classe" method="Post"
            onsubmit="return validate_creation_classe();"
            action="|}(href "creation_classe"){|">
       <table class="fill">
        <tr>
         <td style="text-align:right;">
          <label for="name">Nom de la classe</label>
         </td>
         <td style="text-align:left;">
          <input type="text" name="name" id="name"/>
         </td>
        </tr><tr>
         <td style="text-align:right;">
          <label for="password">Mot de passe</label>
         </td>
         <td style="text-align:left;">
          <input type="password" name="password" id="password"/>
         </td>
        </tr><tr>
         <td style="text-align:right;">
          <label for="password2">Répèter le mot de passe</label>
         </td>
         <td style="text-align:left;">
          <input type="password" name="password2" id="password2"/>
         </td>
         <td style="text-align:left;">
          <input type="submit" value="Ajoute la classe"/>
         </td>
        </tr>
       </table>
      </form>|}]]

let user_classes userid =
  let classes = Db.all_classes_user userid in
  let fn (name, classid, is_in) =
    let form_name = "change_" ^ string_of_int classid in
    let onclick = "change_user_classe('"^form_name^"');" in
    let checked =
      if is_in then
        [[%html {|<input type="checkbox" onclick="|}onclick{|"
                         name="is_in" checked="checked"/>|}]]
      else
        [[%html {|<input type="checkbox" onclick="|}onclick{|"
                         name="is_in"/>|}]]

    in
    [%html {|
      <form name="|}form_name{|" style="display: inline;">
       <input type="hidden" name="classid" value="|}(string_of_int classid){|"/>
       <input type="hidden" name="userid" value="|}(string_of_int userid){|"/>
       <span class="cl-item"><label for="is_in">|}[Html.txt name]{|</label>
       |}checked{|</span>
      </form>|}]
  in
  onglet ~visible:false "Mes classes"
  [[%html {|<div class="cl">|}(List.map fn classes){|</div>|}]]

(** Exercices *)

let creation_exo () =
  onglet ~visible:false "Ajouter un exercice"
  [[%html {|
       <form enctype="multipart/form-data" method="Post" action="|}
         (href "new_exo"){|">
       <table class="fill">
        <tr>
         <td style="text-align:right;">
           <label for="name">Nom de l'exercice:</label>
         </td>
         <td style="text-align:left;">
          <input type="text" name="name" id="name"/>
         </td>
         <td style="text-align:left;">
          <label for="visible">Visible:</label>
          <input type="checkbox" name="visible" id="visible"/>
         </td>
        </tr><tr>
         <td style="text-align:right;">
          <label for="description">Description:</label>
         </td>
         <td colspan=2 style="text-align:left;">
          <textarea name="description" id="description" cols="60" rows="5">
          </textarea>
         </td>
        </tr><tr>
         <td style="text-align:right;">
          <label for="tpy">Télécharger le template python:</label>
         </td>
         <td style="text-align:left;" colspan=2>
          <input type="file" name="tpy" id="tpy"/>
         </td>
        </tr><tr>
         <td style="text-align:right;">
          <label for="stdin">stdin pour ce test(optionnel):</label>
         </td>
         <td style="text-align:left;" colspan=2>
          <input type="file" name="stdin" id="stdin"/>
         </td>
        </tr><tr>
         <td></td><td></td>
         <td style="text-align:left;">
          <input type="submit" value="Envoyer le ou les fichiers"/>
         </td>
        </tr>
        </table>
        </form>|}]]

let exo_classes id exoid =
  let classes = Db.all_classes_exo id in
  let fn (cname, classid, is_in) =
    let form_name = "exo_classe_" ^ exoid ^ "_" ^ string_of_int classid in
    let onclick = "change_exo_classe('"^form_name^"');" in
    let checked =
      if is_in then
        [[%html {|<input type="checkbox" id="is_in" onclick="|}onclick{|"
                         name="is_in" checked="checked"/>|}]]
      else
        [[%html {|<input type="checkbox" id="is_in" onclick="|}onclick{|"
                         name="is_in" />|}]]
    in
    [%html {|
     <div class="cl-item">
     <form name="|}form_name{|">
       <input type="hidden" name="classid" value="|}(string_of_int classid){|"/>
       <input type="hidden" name="exoid" value="|}(exoid){|"/>
         <label for="is_in">|}[Html.txt cname]{|</label>|}checked{|
     </form>
     </div>|}]
  in
  [%html {|<div class="cl">|}(List.map fn classes){|</div>|}]

let search_exos _id =
  onglet ~visible:false "Recherche un exercice dans la base"
  []

let manage_exos id =
  let exos = Db.get_exos_by_id id in
  let fn (id,time,name,fname,visible) =
    let exoid = string_of_int id in
    let time = Util.string_of_time time in
    let py_url = href ("download?exoid="^exoid^"&type=question_py") in
    let ipynb_url = href ("download?exoid="^exoid^"&type=question_ipynb") in
    let cpy_url = href ("download?exoid="^exoid^"&type=corrige_py") in
    let cipynb_url = href ("download?exoid="^exoid^"&type=corrige_ipynb") in
    let tpy_url = href ("download?exoid="^exoid^"&type=test_py") in
    let tpl_url = href ("download?exoid="^exoid^"&type=template") in
    let basthon_url = Options.self_url ^ "download?exoid="^exoid^"&type=question_ipynb" in
    let basthon_url = Tiny_httpd_util.percent_encode basthon_url in
    let basthon_url = Options.jurl ^ basthon_url in
    let visibility_form_name = "visibility_form_" ^ exoid in
    let onclick = "change_exo_visibility('"^visibility_form_name^"');" in
    let html1 =
      if visible then
        [%html {|
            <input type="radio" name="visible" id="visible" value="visible" checked="checked" onclick="|}onclick{|"/>
          <label for="visible">visible</label>
            <input type="radio" name="visible" id="hidden" value="hidden" onclick="|}onclick{|"/>|}]
      else
        [%html {|
            <input type="radio" name="visible" id="visible" value="visible" onclick="|}onclick{|"/>
          <label for="visible">visible</label>
            <input type="radio" name="visible" id="hidden" value="hidden" checked="checked" onclick="|}onclick{|"/>|}]
    in
    let show_notes =
      if Db.has_solution id then
        begin
          let show_url = href (Printf.sprintf "show_notes?exoid=%s&exoname=%s"
                           exoid (Tiny_httpd_util.percent_encode name))
          in
          [[%html {|<a href="|}show_url{|">voir les notes</a>|}]]
        end
      else
        []
    in
    let form_name = "name" ^ "_" ^ exoid in
    let oninput = "change_exo_name('"^form_name^"');" in
    [%html {|<tr><td>
            <form name="|}form_name{|">
              <input type="hidden" name="exoid" value="|}exoid{|"/>
              <input type="text" name="name" value="|}name{|"
                     oninput="|}oninput{|"/>
            </form>
            </td><td>(|}[Html.txt time]{|)</td><td>
              |}show_notes{|</td>
           <td><a target="_blank" href="|}basthon_url{|">tester sur basthon</a></td></tr><tr>
              <td><a href="|}py_url{|" download="|}(Some (fname^".py")){|">python code</a></td>
              <td><a href="|}ipynb_url{|" download="|}(Some (fname^".ipynb")){|">python notebook</a></td>
              <td><a href="|}cpy_url{|" download="|}(Some (fname^".py")){|">python corrigé</a></td>
              <td><a href="|}cipynb_url{|" download="|}(Some (fname^".ipynb")){|">corrigé notebook</a></td></tr><tr>
              <td><a href="|}tpy_url{|" download="|}(Some (fname^".py")){|">python test</a></td>
              <td><a href="|}tpl_url{|" download="|}(Some (fname^".tpy")){|">template</a></td><td colspan=2>
              <form name="|}visibility_form_name{|">|}html1{|
              <label for="hidden">caché</label>
               <input type="radio" name="visible" id="remove" value="remove"  onclick="|}onclick{|"/>
              <label for="remove">supprimer</label>
              <input type="hidden" name="exoid" value="|}exoid{|"/></form>
              </td></tr><tr><td colspan=4>|}
              [exo_classes id exoid]{|</td></tr>|}]
  in
  let ender =
    [%html {|
            <tr><td colspan=2></td><td colspan=3 style="text-align:right;">
            </td></tr>|}]
  in
  let exos_form = List.flatten (List.map fn exos) @ [ender] in
  onglet ~visible:false "Visibilité et suppression des exercices"
  [[%html {|<table class="fill"><tbody>|}exos_form{|</tbody></table>|}]]

let show_notes login ?debut ?fin ?classid userid exoid exoname =
  let (_,_,fname,_) = Db.get_exo_by_id exoid in
  let notes = Db.get_notes ?debut ?fin ?classid userid exoid in
  let headers = ref [] in
  let exoid = string_of_int exoid in
  let fn (name,firstname,classe,result,time,userid) =
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
    let suffix = "&exoid="^exoid^"&time="^time^"&userid="^string_of_int userid in
    let py_url = Options.self_url ^ "basthon?type=question_py" ^ suffix in
    let ipynb_url = Options.self_url ^ "basthon?type=question_ipynb"  ^ suffix in
    let basthon_url = Tiny_httpd_util.percent_encode ipynb_url in
    let basthon_url = Options.jurl ^ basthon_url in
    let u = [%html
      {|<td>
         <a href="|}py_url{|" download="|}(Some (fname^".py")){|">P</a>
         <a href="|}ipynb_url{|" download="|}(Some (fname^".ipynb")){|">I</a>
         <a href="|}basthon_url{|" target="_blank">B</a>
        </td>|}]
    in
    [%html {|<tr><td>|}[Html.txt classe]{|</td><td>|}[Html.txt name]{|</td>
             <td>|}[Html.txt firstname]{|</td>
             <td>|}[Html.txt time]{|</td>|}(l @ [u]){|</tr>|}]
  in
  let notes = List.map fn notes in
  let headers = "Classe" :: "Nom" :: "Prénom" :: "Date/Time" ::  !headers @ ["Link"] in
  let headers = List.map (fun s -> [%html {|<th>|}[Html.txt s]{|</th>|}]) headers in
  let classe_menu =
    let classes = Db.all_classes () in
    let fn (name,cid) =
      if classid = Some cid then
        [%html {|<option value="|}(string_of_int cid){|" selected>|}
            (Html.txt name){|</option>|}]
      else
        [%html {|<option value="|}(string_of_int cid){|">|}
            (Html.txt name){|</option>|}]
    in
    let options = List.map fn classes in
    [%html
      {|<label for="classid">Classe:</label>
        <select name="classid" id="classid">
        <option value="">Toutes</option>
        |}options{|
        </select>|}]
  in
  let debut_html = match debut with
    | None -> [%html{|<input type="date" name="debut" id="debut"/>|}]
    | Some d -> [%html{|<input type="date" name="debut" value="|}d{|" id="debut"/>|}]
  in
  let fin_html = match fin with
    | None -> [%html{|<input type="date" name="fin" id="fin"/>|}]
    | Some d -> [%html{|<input type="date" name="fin" value="|}d{|" id="fin"/>|}]
  in
  let html =
    [%html
       (header login)
        {|<h3>Notes de l'exercice «|}[Html.txt exoname]{|»</h3>
          <div class="center">
          <form method="Get" action="|}(href "show_notes"){|">
            <input type="hidden" name="exoid" value="|}exoid{|"/>
            <input type="hidden" name="exoname" value="|}exoname{|"/>
            <label for="debut">début:</label>
              |}[debut_html]{|
            <label for="fin">fin:</label>
              |}(fin_html ::
            classe_menu){|
            <input type="submit" name="submit" value="filtrer"/>
          </form>
          </div>
          <div class="center">
          <table class="table"><tr>|}headers{|</tr>|}notes{|</table></div>|}]
  in
  Ok (html_wrapper html)

let form_exo userid exoid ?name time fname =
  let note = Db.get_note exoid userid in
  let exoid = string_of_int exoid in
  let time = Util.string_of_time time in
  let dt = match name with
    | None -> []
    | Some name -> [Html.txt (name ^ " (" ^ time ^ "):")]
  in
  let py_url = href ("download?exoid="^exoid^"&type=question_py") in
  let ipynb_url = href ("download?exoid="^exoid^"&type=question_ipynb") in
  let basthon_url = Options.self_url ^ "basthon?exoid="^exoid^"&type=question_ipynb" in
  let basthon_url = Tiny_httpd_util.percent_encode basthon_url in
  let basthon_url = Options.jurl ^ basthon_url in
  let show_url = href (Printf.sprintf "show_solution?exoid=%s" exoid) in
  let note = match note with
    | None ->
       [%html {|<a href="|}show_url{|">Pas encore de note (cliquer pour le détail)</a>|}]
    | Some n ->
       let n = [Html.txt (Printf.sprintf "%.1f" n)] in
       [%html {|<a href="|}show_url{|">Note: |}n{| (cliquer pour le détail)</a>|}]
  in
    [%html
      {|<h5>|}dt{|</h5><div style="margin-bottom:7px">
        <form enctype="multipart/form-data" method="Post" action="|}(href "submit_solution"){|">
                                                                                             <table><tr><td>
           <a target="_blank" href="|}basthon_url{|">Aller travailler sur basthon</a></td><td>|}[note]{|</td></tr><tr><td>
           <a href="|}py_url{|" download="|}(Some (fname^".py")){|">Télécharger le code python</a></td><td>
           <a href="|}ipynb_url{|" download="|}(Some (fname^".ipynb")){|">Télécharger le notebook jupyter/python</a></td></tr><tr><td>
           <input type="hidden" name="exoid" value="|}exoid{|"/>
           <label for="file">Sélectionner un fichier solution (.py ou .ipynb)</label>
              </td><td><input type="file" id="file" name="solution"/></td><td>
           <input type="submit" name="submit" value="et envoyer le"/></td></tr></table>
        </form></div>|}]

let exos_student userid =
  let exos = Db.get_visible_exos userid in
  let fn (exoid,time,name,fname) =
    form_exo userid exoid ~name time fname
  in
  [%html {|<h3>Exercices disponibles</h3>|}] ::
    (List.flatten (List.map fn exos))


let show_solution login userid exoid =
  let (time,name,fname,_visible) = Db.get_exo_by_id exoid in
  let (best,bt) = Db.best_solution userid exoid in
  let (latest,lt) = Db.latest_solution userid exoid in
  let html = header login @ [%html {|
    <h3>|}[Html.txt name]{|</h3>
    <div class="center"><table class="table">
    <tr><th>Meilleure</th><th>Dernière</th></tr>
    <tr><td>|}[Html.txt bt]{|</td><td>|}[Html.txt lt]{|</td></tr>
    <tr><td><pre>|}[Html.txt best]{|</pre></td>
        <td><pre>|}[Html.txt latest]{|</pre></td>
        </tr></table></div><br/>|}
    (form_exo userid exoid time fname)]
  in
  html_wrapper html

let add_exo ?stdin name description visible fname content =
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
    let (_out,_err,status) = run_cmd "py2nb" [fname] in
    if status <> Unix.WEXITED 0 then failwith "bad python template";
    let question_ipynb_fname = rname ^ ".ipynb" in
    let corrige_ipynb_fname = rname ^ "_corrige.ipynb" in
    let test_ipynb_fname = rname ^ "_test.ipynb" in
    let ipynbs = [question_ipynb_fname; corrige_ipynb_fname; test_ipynb_fname] in
    let (_out,_err,status) = run_cmd "nb2py" ipynbs in
    if status <> Unix.WEXITED 0 then failwith "bad python template";
    let question_py_fname = rname ^ ".py" in
    let corrige_py_fname = rname ^ "_corrige.py" in
    let test_py_fname = rname ^ "_test.py" in
    let last = if stdin_name = "" then [] else [stdin_name] in
    let (out1,_err,status) = run_cmd ~jail:dirname "python3"
                               ("/usr/local/lib/pynbtools/evaluation.py"::
                                test_py_fname::question_py_fname::last) in
    if status <> Unix.WEXITED 0 then failwith "bad python template";
    let (out2,_err,status) = run_cmd ~jail:dirname "python3"
                               ("/usr/local/lib/pynbtools/evaluation.py"::
                                test_py_fname::corrige_py_fname::last) in
    if status <> Unix.WEXITED 0 then failwith "bad python template";
    let visible = if visible then "true" else "false" in
    let html = html_wrapper [%html {|
      Évaluation de l'exercice: |}[Html.txt name]{|<br/>
      <table class="table">
       <tr><th>Correction</th><th>Question</th></tr>                                         <tr><td><pre>|}[Html.txt out2]{|</pre></td>
           <td><pre>|}[Html.txt out1]{|</pre></td></tr>
      </table>
      <form method="Post" action="|}(href "record_exo"){|">
        <input type="hidden" name="name" value="|}name{|"/>
        <input type="hidden" name="description" value="|}description{|"/>
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
        <input type="submit" name="submit" value="Enregistrer"/>
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

let home_page () =
  let contents = ref [] in
  let add_contents x = contents := !contents @ x @ [[%html {|<br/>|}]] in

  add_contents [%html
     {|<h1>Bienvenue sur le site d'auto-évaluation Basthon/Python</h1>
       <h3>Connexion</h3>
       <form method="Post" name="login" onsubmit="return validate_login();"
             action="|}(href "login"){|">
         <table class="fill"><tr><td style="text-align:left;">
          <label for="login">Identifiant:</label>
           <input type="text" name="login" id="login"/>
         </td><td style="text-align:center;">
          <label for="password">Mot de passe:</label>
           <input type="password" name="password" id="password"/>
         </td><td style="text-align:right;">
          <input type="submit" value="Entrer"/>
         </td></tr></table>
      </form>|}];

  add_contents [create_user false];

  html_wrapper !contents


let home_page_logged login id role =
  let contents = ref [] in
  let add_contents ?(br=false) x =
    contents := !contents @ x @ (if br then [[%html {|<br/>|}]] else [])
  in
  add_contents ~br:false (header ~is_home:true login);
  add_contents ~br:false
    [[%html {|<h2>Éval Basthon/Python</h2>|}]];
  begin
    let open Db.Role in
    match role with
    | Student ->
       add_contents ~br:true (exos_student id);
    | Prof | Admin ->

       add_contents [search_exos id];
       add_contents [manage_exos id];
       add_contents [creation_exo ()];

       add_contents [user_classes id];
       add_contents [manage_classes id];
       add_contents [creation_classe ()];

       add_contents [create_user true];
       add_contents [manage_users (role=Admin)];

  end;

  html_wrapper !contents
