(** SVG *)
let home_svg =
  {html|
   <svg width="30px" height="30px" viewBox="0 0 50 40">
     <polygon points="10,40 40,40 40,15 50,15 25,0 0,15 10,15 10,40"
              style="fill:#3030FF;stroke-width:0"/>
   </svg>|html}

let hide_svg =
  {html|
   <svg width="30px" height="30px" viewBox="0 0 50 40">
     <polygon points="0,0 10,15 0,30 0,0" style="fill:#3030FF;stroke-width:0"/>
   </svg>|html}

let show_svg =
  {html|
   <svg width="30px" height="30px" viewBox="0 0 50 40">
     <polygon points="0,0 15,10 30,0 0,0" style="fill:#3030FF;stroke-width:0"/>
   </svg>|html}

(** General *)
let href s =
  let prefix = Options.prefix () in
  if prefix = "" then s else
    if String.length s > 0 && s.[0] = '/' then
      "/" ^ prefix ^ s
    else
      "/" ^ prefix ^ "/" ^ s

let html_wrapper body =
  let style = href "style.css" in
  let scripts = href "scripts.js" in
  "<!DOCTYPE html>"^{html|<html>
   <head>
     <title>Pynbtools</title>
     <meta charset="utf-8"/>
     <link rel="stylesheet" href=<?=style?>>
     <script src=<?=scripts?>></script>
   </head>
   <body>
     <?= body ?>
   </body></html>|html}

let chaml_wrapper body =
  let open Simple_httpd in
  let style = href "style.css" in
  let scripts = href "scripts.js" in
      {chaml|<!DOCTYPE html><html>
         <head>
           <title>Pynbtools</title>
           <meta charset="utf-8"/>
           <link rel="stylesheet" href=<?=style?>>
           <script src=<?=scripts?>></script>
         </head>
         <body>
           <?= body ?>
         </body>
       </html>|chaml}

let header ?(is_home=false) login =
  let lhref = "location.href='" ^ href "" ^ "';" in
  let home_button =
    if is_home then {html||html} else
      {html|<div style="position:absolute; top:1px; left:5px;" onclick=<?=lhref?>>
           <?= home_svg?></div>|html}
  in
  {html|<?= home_button?>
     <div style="position:absolute; top:5px; right:5px;">
       <form method="Get" action=<?= href "logout" ?>>
         <label for="submit">Connecter comme: <?= login?></label>
          <input style="margin: 5px;" oninput="ignore_enter"
                 type="submit" id="submit" value="Déconnexion"/><br/>
       </form></div>|html}

let onglet ?(small=false) ?(visible=true) title content =
  let id = Digest.(to_hex (string title)) in
  let id_show = id ^ "_show" in
  let id_hide = id ^ "_hide" in
  let click_onglet = "click_onglet('"^id^"','"^id_show^"','"^id_hide^"');" in
  let style = if visible then "display:block;" else "display:none;" in
  let style_show = "position:absolute; top:5px; left:5px; " ^ style in
  let style_hide =
    let s = if visible then "display:none;" else "display:block;" in
    "position:absolute; top:5px; left:5px; " ^ s
  in
  let h3 = {html|<h3 class="onglet_h3"><?= title?></h3>|html} in
  let h4 = {html|<h4 class="onglet_h4"><?= title?></h4>|html} in
  let onload = "onload_" ^ id in
  {html|
   <div style="position:relative; padding:0px; margin:0px;" onload=<?=onload?>>
      <div onclick=<?=click_onglet?>>
        <div style=<?=style_hide?> id=<?=id_hide?>>
          <?=hide_svg?>
        </div>
        <div style=<?=style_show?> id=<?=id_show?>>
          <?= show_svg?>
        </div>
        <?= if small then h4 else h3 ?>
      </div>
      <div id=<?=id?> class="onglet_content" style=<?=style?>>
        <?=content?>
      </div>
    </div>
    <script>
        var hide = getCookie('Ong<?=id?>');
        if (hide == "false") {
          document.getElementById('<?=id?>').style.display = "block";
          document.getElementById('<?=id_show?>').style.display = "block";
          document.getElementById('<?=id_hide?>').style.display = "none";
        } else if (hide == "true") {
          document.getElementById('<?=id?>').style.display = "none";
          document.getElementById('<?=id_show?>').style.display = "none";
          document.getElementById('<?=id_hide?>').style.display = "block";
        }
    </script>|html}

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
   .attention {
     margin-left:10%;
     margin-right:10%;
     text-align:justify;
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
   .onglet_h4 {
     text-align: center;
     background-color: #B0B0FF;
     margin: 0;
     padding: 3px;
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
let scripts () =
  let prefix = Options.prefix () in
  Printf.sprintf {js|
    function setCookie(cname, cvalue, max_age) {
      document.cookie = cname + "=" + cvalue + ";Max-Age=" + max_age.toString() + ";Path=/;SameSite=Strict";
    }
    function getCookie(cname) {
      let name = cname + "=";
      let decodedCookie = decodeURIComponent(document.cookie);
      let ca = decodedCookie.split(';');
      for(let i = 0; i <ca.length; i++) {
        let c = ca[i];
          while (c.charAt(0) == ' ') {
            c = c.substring(1);
          }
          if (c.indexOf(name) == 0) {
          return c.substring(name.length, c.length);
        }
      }
      return "";
    }
    function request(url,ok,success,failure) {
	var request = new XMLHttpRequest();
	request.open('GET', '/%s/' + url, true);
	request.onload = function () {
	    var str=request.responseText;
	    if (str.length >= ok.length && str.substring(0, ok.length) == ok) {
		success();
	    }
	    else {
		failure();
	    }
	};
	request.onerror = function () {
	    failure();
	}
	request.send();
    }
    function request_alert(msg,url,form) {
	var data = new FormData(form);
	var request = new XMLHttpRequest();
	request.open('POST', '/%s/' + url, true);
	request.onload = function () {
            var str=request.responseText;
	  if (str.substring(0, 4) == "true") {
              alert(msg + " a réussi");
	  } else {
              alert(msg + " a échoué");
	  }
	};
	request.onerror = function () {
            alert(msg + " a échoué");
	}
	request.send(data);
    }
    function request_reload(url,form) {
	var data = new FormData(form);
	var request = new XMLHttpRequest();
	var url = '/%s/' + url;
	request.open('POST', url, true);
	request.onload = function () {
            var str=request.responseText;
            if (str.substring(0, 4) == "true") {
		location.reload();
            } else {
		alert("Le changement n'a pas été propagé sur le site");
            }
	};
	request.onerror = function () {
            alert("Le changement n'a pas été propagé sur le site");
	};
	request.send(data);
    }
    function do_new_user(is_admin) {
	var f = document.forms["new_user"];
	if (f["login"].value == "" || f["name"].value == ""
            || f["firstname"].value == ""
	    || (f["classe"].value == "" && f["role"] == "student")
            || f["classpass"].value == ""
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
	request("exists_login?login=" + encodeURIComponent(f["login"].value), "false"
		, (function () {
		    request_alert("L'inscription", is_admin?"new_user_logged":"new_user", f);
		})
		, (function () {alert("ce login est déjà utilisé")}));
	return false;
    }
    function validate_login() {
	var f = document.forms["login"];
	if (f["login"].value == ""
            || f["password"].value == "" ) {
            alert("Il faut remplir tous les champs");
            return(false);
	}
	return(true);
    }
    function do_creation_classe() {
	var f = document.forms["creation_classe"];
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
	request("exists_classe?name=" + encodeURIComponent(f["name"].value), "false"
		, (function () {
		    request_alert("La création de la classe", "creation_classe", f);
		}), (function () {
		    alert("cette classe existe déjà");
		}));
	return false;
   }
   function do_creation_course() {
       var f = document.forms["creation_course"];
       if (f["name"].value == "") {
           alert("Il faut remplir tous les champs");
           return(false);
       }
       request("exists_course?name=" + encodeURIComponent(f["name"].value), "false"
	       , (function () {
		   request_alert("La création du cours", "creation_course", f);
	       }), (function () {
		   alert("ce cours existe déjà")
	       }));
       return false;
   }
   function change_user_classe(form_name) {
       var f = document.forms[form_name];
       request_reload("change_user_classe",f);
   }
   function change_courses_classes(form_name) {
       var f = document.forms[form_name];
       request_reload("change_courses_classes", f);
   }
   function change_prof_course(form_name) {
       var f = document.forms[form_name];
       request_reload("change_prof_course", f);
   }
   function change_exo_course(form_name) {
       var f = document.forms[form_name];
       request_reload("change_exo_course", f);
   }
   function change_exo_visibility(form_name) {
       var f = document.forms[form_name];
       request_reload("change_exo_visibility", f);
   }
   function remove_exo(exoid,force) {
       var action = force?"forced_remove":"remove";
       if (window.confirm("Êtes vous sur de vouloir supprimer cet exercice?")) {
	   request("change_exo_visibility?exoid="+exoid+
                   "&visible="+action, "true"
		   , (function () { location.reload(); }),
		   (function () {
		       alert("L'exercice n'a pu être supprimé, il y a sans doute des solutions attachées à cet exercice");}))
       }
   }
   function change_exo_name(form_name) {
       var f = document.forms[form_name];
       request_reload("change_exo_name",f);
   }
   function click_onglet(id,id2,id3) {
       setCookie("Ong"+id,hide?"true":"false",3600);
       var el1 = document.getElementById(id);
       var el2 = document.getElementById(id2);
       var el3 = document.getElementById(id3);
       if (el1.style.display == "none") {
	   el1.style.display="block";
	   el2.style.display="block";
	   el3.style.display="none";
       } else {
	   el1.style.display="none";
	   el2.style.display="none";
	   el3.style.display="block";
       }
   }
   function change_user_name(form_name) {
     var f = document.forms[form_name];
     request_reload("change_user_name", f);
   }
   function change_user_firstname(form_name) {
     var f = document.forms[form_name];
     request_reload("change_user_firstname", f);
   }
   function change_course_name(form_name) {
     var f = document.forms[form_name];
     request_reload("change_course_name",f);
   }
   function change_user_password(form_name) {
       var f = document.forms[form_name];
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
       request_alert("Le changement", "change_user_password", f);
       return false;
   }
   function ignore_enter(event) {
     var x = event.which;
     if (x === 13) {
       event.preventDefault();
     }
   }
   |js} prefix prefix prefix


(** Users *)

let create_user is_admin req =
  let classe_menu =
    let classes = Db.all_classes () req in
    let fn (name,classid) =
      {html|<option value=<?=string_of_int classid?>>
          <?=name?></option>|html}
    in
    let options = String.concat " " (List.map fn classes) in
    {html|
      <tr><td>
        <label for="classe">Classe:</label>
        </td><td>
        <select name="classe" id="classe">
        <option value="" selected>Choisis ta classe</option>
          <?=options?>
        </select></td></tr>|html}
  in
  let role =
    if is_admin then
      {html|
        <tr><td>
        <label for="role">Role:</label>
        </td><td>
          <select name="role" id="role">
            <option value="student">Élève</option>
            <option value="prof">Professeur</option>
            <option value="admin">Administrateur</option>
          </select>
         </td></tr>
      |html}
    else
      {html|<input type="hidden" name="role" value="student"/>|html}
  in
  let classe_pass = if is_admin then {html||html}
                  else
       {html|<tr><td>
       <label for="classpass">Mot de passe de la classe:</label>
       </td><td>
         <input type="password" name="classpass" id="classpass"/><br/>
       </td></tr>|html}
  in
  let onsubmit =
      "return do_new_user("^(if is_admin then "true" else "false")^");"
  in
  onglet ~visible:(not is_admin) "Inscription au site"
    {html|
      <div class="center">
      <form name="new_user" action="javascript:void(0);"
            onsubmit=<?=onsubmit?>>
      <table class="fill"><tr><td>
       <label for="login">Identifiant:</label>
       </td><td>
         <input type="text" name="login" id="login"/><br/>
       </td></tr><tr><td>
       <label for="password">Mot de passe:</label>
       </td><td>
         <input type="password" name="password" id="password"/><br/>
       </td></tr><tr><td>
       <label for="password2">Répèter le mot de passe:</label>
       </td><td>
          <input type="password" name="password2" id="password2"/><br/>
       </td></tr><tr><td>
       <label for="name">Nom</label>
       </td><td>
         <input type="text" name="name" id="name"/><br/>
       </td></tr><tr><td>
       <label for="firstname">Prénom:</label>
       </td><td>
         <input type="text" name="firstname" id="firstname"/><br/>
       </td></tr>
       <?= classe_menu ?>
       <?= classe_pass ?>
       <?=role?>
       <tr><td></td><td>
       <input type="submit" value="Valider"/><br/>
       </td></tr></table></form></div>|html}

let user_classes userid req =
  let classes = Db.all_classes_user userid req in
  let fn (name, classid, is_in) =
    let form_name = "change_" ^ string_of_int classid ^ "_" ^ string_of_int userid in
    let onclick = "change_user_classe('"^form_name^"');" in
    let checked =
      if is_in then
        {html|<input type="checkbox" onchange=<?=onclick?>
                         name="is_in" checked="checked"/>|html}
      else
        {html|<input type="checkbox" onchange=<?=onclick?>
                         name="is_in"/>|html}

    in
    {html|
      <form name=<?=form_name?> style="display: inline;" method="Post">
       <input type="hidden" name="classid" value=<?=string_of_int classid?>/>
       <input type="hidden" name="userid" value=<?=string_of_int userid?>/>
       <span class="cl-item"><label for="is_in"><?=name?></label>
       <?=checked?></span>
      </form>|html}
  in
  {html|<div class="cl"><?=String.concat " " (List.map fn classes)?></div>|html}

let courses_classes courseid req =
  (let classes = Db.all_classes_course courseid req in
  let fn (name, classid, is_in) =
    let form_name = "cchange_" ^ string_of_int classid ^ "_" ^ string_of_int courseid in
    let onclick = "change_courses_classes('"^form_name^"');" in
    let checked =
      if is_in then
        {html|<input type="checkbox" onchange=<?=onclick?>
                         name="is_in" checked="checked"/>|html}
      else
        {html|<input type="checkbox" onchange=<?=onclick?>
                         name="is_in"/>|html}

    in
    {html|
      <form name=<?=form_name?> style="display: inline;" method="Post">
       <input type="hidden" name="classid" value=<?=string_of_int classid?>/>
       <input type="hidden" name="courseid" value=<?=string_of_int courseid?>/>
       <span class="cl-item"><label for="is_in"><?=name?></label>
       <?=checked?></span>
      </form>|html}
  in
  {html|<div class="cl"><?=String.concat " " (List.map fn classes)?></div>|html})

let manage_users admin req =
  let users = Db.get_users admin req in
  let fn (userid, login, name, firstname) =
    let suserid = string_of_int userid in
    let form_name = "name_" ^ suserid in
    let form_firstname = "firstname_" ^ suserid in
    let form_password = "password_" ^ suserid in
    let onchange_name = "change_user_name('"^form_name^"');" in
    let onchange_firstname = "change_user_firstname('"^form_firstname^"');" in
    let onsubmit_pwd = "change_user_password('"^form_password^"');" in

    {html|
       <tr>
          <td style="text-align:right;">Login:</td>
          <td style="text-align:left;"><?=login?></td>
          <td style="text-align:right;">
          <label for="name">Nom:</label></td>
          <td style="text-align:left;">
             <form id=<?=form_name?> method="Post">
               <input type="hidden" name="userid" value=<?=suserid?>/>
               <input type="text" name="name" id="name" value=<?=name?>
                      onchange=<?=onchange_name?>/>
             </form>
          </td>
       </tr><tr>
          <td style="text-align:right;">Userid:</td>
          <td style="text-align:left;"><?=suserid?></td>
          <td style="text-align:right;">
          <label for="firstname">Prénom:</label></td>
          <td>
             <form id=<?=form_firstname?> method="Post">
                <input type="hidden" name="userid" value=<?=suserid?>/>
                <input type="text" name="firstname" id="firstname" value=<?=firstname?>
                      onchange=<?=onchange_firstname?>/>
             </form>
          </td>
       </tr><tr>
       <td style="text-align:right;">Changement de mot de passe:</td>
       <td style="text-align:left; padding-right:0;"><input size="10" form=<?=form_password?> type="password" name="password1" autocomplete="off" readonly
onfocus="this.removeAttribute('readonly');"/></td>
       <td style="text-align:right; padding-left:0;"><input size="10" form=<?=form_password?> type="password" name="password2" autocomplete="off" readonly
onfocus="this.removeAttribute('readonly');"/></td>
       <td style="text-align:left;">
     <form id=<?=form_password?> action="javascript:void(0);"
                 onsubmit=<?=onsubmit_pwd?>>
            <input type="hidden" name="userid" value=<?=suserid?>/>
            <input type="submit" oninput="ignore_enter"
                      name="change_password" value="Appliquer"/>
          </form>
       </td>
     </tr>
     <tr><td colspan="4"><?=user_classes userid req?></td></tr>
   |html}
  in
  let users_form = String.concat " " (List.map fn users) in
  onglet ~visible:false "Gestion des utilisateurs"
      {html|<table class="fill"><tbody><?=users_form?></tbody></table>|html}

(** Classes *)
let manage_classes _id =
  onglet ~visible:false "Gestion des classes" "Bientôt disponible"

let manage_courses _id req =
  let courses= Db.all_courses () req in
  let fn (name, courseid) =
    let scourseid = string_of_int courseid in
    let form_name = "cname_" ^ scourseid in
    let onchange_name = "change_course_name('"^form_name^"');" in

    {html|
     <tr><td>Nom:</td><td>
       <form id=<?=form_name?> method="Post">
         <input type="hidden" name="courseid" value=<?=scourseid?>/>
         <input type="text"   name="name" value=<?=name?> onchange=<?=onchange_name?>/>
       </form></td>
       <td>Classes:</td><td><?=courses_classes courseid req?></td>
     </tr>
    |html}
  in
  let courses_form = String.concat " " (List.map fn courses) in
  onglet ~visible:false "Gestion des cours"
    {html|<table class="fill"><tbody><?=courses_form?></tbody></table>|html}


let creation_classe () =
  onglet ~visible:false "Ajout d'une classe"
  {html|
      <form name="creation_classe"
            onsubmit="return do_creation_classe();"
            action="javascript:void(0);">
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
      </form>|html}

let creation_course () =
  onglet ~visible:false "Ajout d'un cours"
  {html|
      <form name="creation_course"
            onsubmit="return do_creation_course();"
            action="javascript:void(0);">
       <table class="fill">
        <tr>
         <td style="text-align:right;">
          <label for="name">Nom du cours</label>
         </td>
         <td style="text-align:left;">
          <input type="text" name="name" id="name"/>
         </td>
         <td style="text-align:left;">
          <input type="submit" value="Ajoute le cours"/>
         </td>
        </tr>
       </table>
      </form>|html}

let prof_courses userid req =
  let courses = Db.all_courses_prof userid req in
  let fn (name, courseid, is_in) =
    let form_name = "pchange_" ^ string_of_int courseid ^ "_" ^ string_of_int userid in
    let onclick = "change_prof_course('"^form_name^"');" in
    let checked =
      if is_in then
        {html|<input type="checkbox" onchange=<?=onclick?>
                         name="is_in" checked="checked"/>|html}
      else
        {html|<input type="checkbox" onchange=<?=onclick?>
                         name="is_in"/>|html}

    in
    {html|
      <form name=<?=form_name?> style="display: inline;" method="Post">
       <input type="hidden" name="courseid" value=<?=string_of_int courseid?>/>
       <input type="hidden" name="userid" value=<?=string_of_int userid?>/>
       <span class="cl-item"><label for="is_in"><?=name?></label>
       <?=checked?></span>
      </form>|html}
  in
  onglet ~visible:false "Mes cours"
  {html|<div class="cl"><?=String.concat " " (List.map fn courses)?></div>|html}

(** Exercices *)

let creation_exo () =
  onglet ~visible:false "Ajouter un exercice"
  {html|
       <form enctype="multipart/form-data" method="Post" action=<?=
         (href "new_exo")?>>
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
        </form>|html}

let exo_courses id exoid req =
  let courses = Db.all_courses_exo id req in
  let fn (cname, courseid, is_in) =
    let form_name = "exo_classe_" ^ exoid ^ "_" ^ string_of_int courseid in
    let onclick = "change_exo_course('"^form_name^"');" in
    let checked =
      if is_in then
        {html|<input type="checkbox" id="is_in" onchange=<?=onclick?>
                         name="is_in" checked="checked"/>|html}
      else
        {html|<input type="checkbox" id="is_in" onchange=<?=onclick?>
                         name="is_in" />|html}
    in
    {html|
     <div class="cl-item">
     <form name=<?=form_name?> method="Post">
       <input type="hidden" name="courseid" value=<?=(string_of_int courseid)?>/>
       <input type="hidden" name="exoid" value=<?=(exoid)?>/>
         <label for="is_in"><?=cname?></label><?=checked?>
     </form>
     </div>|html}
  in
  {html|<div class="cl"><?=String.concat " " (List.map fn courses)?></div>|html}

let manage_exos id req =
  let exos = Db.get_exos_by_id id req in
  let fn (id,time,name,fname,visible) =
    let exoid = string_of_int id in
    let time = MyUtil.string_of_time time in
    let py_url = href ("download?exoid="^exoid^"&type=question_py") in
    let ipynb_url = href ("download?exoid="^exoid^"&type=question_ipynb") in
    let cpy_url = href ("download?exoid="^exoid^"&type=corrige_py") in
    let cipynb_url = href ("download?exoid="^exoid^"&type=corrige_ipynb") in
    let tpy_url = href ("download?exoid="^exoid^"&type=test_py") in
    let tpl_url = href ("download?exoid="^exoid^"&type=template") in
    let basthon_url = "/" ^ Options.prefix () ^
                        "/download?exoid="^exoid^"&type=question_ipynb" in
    let basthon_url = Simple_httpd.Util.percent_encode basthon_url in
    let basthon_url = Options.jurl () ^ basthon_url in
    let visibility_form_name = "visibility_form_" ^ exoid in
    let onclick = "change_exo_visibility('"^visibility_form_name^"');" in
    let onclick_rm force =
      let force = if force then "true" else "false" in
      "remove_exo('"^exoid^"',"^force^");"
    in
    let html1 =
      if visible then
        {html|
            <input type="radio" name="visible" id="visible" value="visible" checked="checked" onclick=<?=onclick?>/>
          <label for="visible">visible</label>
            <input type="radio" name="visible" id="hidden" value="hidden" onclick=<?=onclick?>/>|html}
      else
        {html|
            <input type="radio" name="visible" id="visible" value="visible" onclick=<?=onclick?>/>
          <label for="visible">visible</label>
            <input type="radio" name="visible" id="hidden" value="hidden" checked="checked" onclick=<?=onclick?>/>|html}
    in
    let has_solution = Db.has_solution id req in
    let show_notes =
      if has_solution then
        begin
          let show_url = href (Printf.sprintf "show_notes?exoid=%s&exoname=%s"
                           exoid (Simple_httpd.Util.percent_encode name))
          in
          {html|<a href=<?=show_url?>>voir les notes</a>|html}
        end
      else
        ""
    in
    let rm : string =
      if has_solution then
        {html|
         <input type="button" name="visible" id="remove" value="Supprimer avec notes" onclick=<?=onclick_rm true?>/>|html}
      else
        {html|<input type="button" name="visible" id="remove" value="Supprimer" onclick=<?=onclick_rm false?>/>|html}
    in
    let form_name = "ename" ^ "_" ^ exoid in
    let onchange = "change_exo_name('"^form_name^"');" in
    {html|<tr><td>
            <form name=<?=form_name?> method="Post">
              <input type="hidden" name="exoid" value=<?=exoid?>/>
              <input type="text" name="name" value=<?=name?>
                     onchange=<?=onchange?>/>
            </form>
            </td><td><?=fname?> (<?=time?>)</td><td>
              <?=show_notes?></td>
           <td><a target="_blank" href=<?=basthon_url?>>tester sur basthon</a></td></tr><tr>
              <td><a href=<?=py_url?> download=<?=fname^".py"?>>python code</a></td>
              <td><a href=<?=ipynb_url?> download=<?=fname^".ipynb"?>>python notebook</a></td>
              <td><a href=<?=cpy_url?> download=<?=fname^".py"?>>python corrigé</a></td>
              <td><a href=<?=cipynb_url?> download=<?=fname^".ipynb"?>>corrigé notebook</a></td></tr><tr>
              <td><a href=<?=tpy_url?> download=<?=fname^".py"?>>python test</a></td>
              <td><a href=<?=tpl_url?> download=<?=fname^".tpy"?>>template</a></td><td colspan=2>
              <form name=<?=visibility_form_name?> method="Post">
                <?=html1?>
                <label for="hidden">caché</label>
                <input type="hidden" name="exoid" value=<?=exoid?>/>
                <?=(rm:string)?>
              </form>
              </td></tr><tr><td colspan=4>
              <?=exo_courses id exoid req?></td></tr>|html}
  in
  let ender =
    {html|
            <tr><td colspan=2></td><td colspan=3 style="text-align:right;">
            </td></tr>|html}
  in
  let exos_form = List.map fn exos @ [ender] in
  onglet ~visible:false "Visibilité et suppression des exercices"
  {html|<table class="fill"><tbody><?=String.concat " " exos_form?></tbody></table>|html}

let show_notes login ?debut ?fin ?classid ?detailid profid exoid exoname req =
  let (_,_,fname,_) = Db.get_exo_by_id exoid req in
  let notes = Db.get_notes ?debut ?fin ?classid profid exoid req in
  let headers = ref [] in
  let exoid_str = string_of_int exoid in
  let exo_html userid result time =
    let l = String.split_on_char '\n' result in
    let gn l = match String.split_on_char ':' l with
      | h::n::_ -> Some (h, float_of_string n)
      | _      -> None
    in
    let l = List.filter_map gn l in
    let (h,l) = List.split l in
    headers := h;
    let l = List.map (fun f -> let f = Printf.sprintf "%.1f" f in
                               {html|<td><?=f?></td>|html}) l
    in
    let suffix = "&exoid="^exoid_str^"&time="^time^"&userid="^string_of_int userid in
    let py_url = "/" ^ Options.prefix () ^ "/basthon_exo?type=question_py" ^ suffix in
    let ipynb_url = "/" ^ Options.prefix () ^ "/basthon_exo?type=question_ipynb"  ^ suffix in
    let basthon_url = Simple_httpd.Util.percent_encode ipynb_url in
    let debut_val = match debut with
      | None -> ""
      | Some d -> "&debut=" ^ d
    in
    let fin_val = match fin with
      | None -> ""
      | Some d -> "&fin=" ^ d
    in
    let classe_val = match classid with
      | None -> ""
      | Some c -> "&classid=" ^ string_of_int c
    in
    let (detail_val, detail_char) =
      if detailid = Some userid then
        ("", "H")
      else
        ("&detailid=" ^ string_of_int userid, "D")
    in
    let detail_url = "/" ^ Options.prefix () ^ "/show_notes?exoname=" ^ exoname ^
                       "&exoid=" ^ exoid_str ^ "&userid=" ^ string_of_int profid ^
                       detail_val ^ debut_val ^ fin_val ^ classe_val
    in
    let basthon_url = Options.jurl () ^ basthon_url in
    let u = {html|
      <td>
         <a href=<?=py_url?> download=<?=fname^".py"?>>P</a>
         <a href=<?=ipynb_url?> download=<?=fname^".ipynb"?>>I</a>
         <a href=<?=basthon_url?> target="_blank">B</a>
         <a href=<?=detail_url?>><?=detail_char?></a>
        </td>|html}
    in
    (l, u)
  in
  let fn (name,firstname,classe,result,time,userid) =
    let (l, u) = exo_html userid result time in
    let line = {html|<tr><td><?=classe?></td><td><?=name?></td>
             <td><?=firstname?></td>
                <td><?=time?></td><?=String.concat " " (l @ [u])?></tr>|html}
    in
    let details = match detailid with
      | Some id when id = userid ->
         let details = Db.get_notes_detail ?debut ?fin profid userid exoid req in
         let hn (result, time) =
           let (l, u) = exo_html userid result time in
           {html|<tr><td></td><td></td>
                       <td></td>
                       <td><?=time?></td><?=String.concat " " (l @ [u])?></tr>|html}
         in
         String.concat " " (List.map hn details)
      | _ -> {html||html}
    in
    line ^ " " ^ details
  in
  let notes = List.map fn notes in
  let headers = "Classe" :: "Nom" :: "Prénom" :: "Date/Time" ::  !headers @ ["Link"] in
  let headers = List.map (fun s -> {html|<th><?=s?></th>|html}) headers in
  let classe_menu =
    let classes = Db.all_classes () req in
    let fn (name,cid) =
      if classid = Some cid then
        {html|<option value=<?=(string_of_int cid)?> selected>
            <?=name?></option>|html}
      else
        {html|<option value=<?=(string_of_int cid)?>>
            <?=name?></option>|html}
    in
    let options = List.map fn classes in
    {html|<label for="classid">Classe:</label>
        <select name="classid" id="classid">
        <option value="">Toutes</option>
         <?=String.concat " " options?>
        </select>|html}
  in
  let debut_html = match debut with
    | None -> {html|<input type="date" name="debut" id="debut"/>|html}
    | Some d -> {html|<input type="date" name="debut" value=<?=d?> id="debut"/>|html}
  in
  let fin_html = match fin with
    | None -> {html|<input type="date" name="fin" id="fin"/>|html}
    | Some d -> {html|<input type="date" name="fin" value=<?=d?> id="fin"/>|html}
  in
  let html =
    {html|
       <?=header login?>
        <h3>Notes de l'exercice «<?=exoname?>»</h3>
          <div class="center">
          <form method="Post" action=<?=(href "show_notes")?>>
            <input type="hidden" name="exoid" value=<?=exoid_str?>/>
            <input type="hidden" name="exoname" value=<?=exoname?>/>
            <label for="debut">début:</label>
              <?=debut_html?>
            <label for="fin">fin:</label>
              <?= fin_html?><?=classe_menu?>
            <input type="submit" name="submit" value="filtrer"/>
          </form>
          </div>
          <div class="center">
     <table class="table"><tr><?=String.concat " " headers?></tr>
       <?=String.concat " " notes?></table></div>|html}
  in
  html_wrapper html

let form_exo userid exoid ?name visible time _fname req =
  let note = Db.get_note exoid userid req in
  let exoid = string_of_int exoid in
  let time = MyUtil.string_of_time time in
  let dt = match name with
    | None -> ""
    | Some name -> name ^ " (" ^ time ^ ") "
  in
  let basthon_url = "/" ^ Options.prefix () ^
                      "/basthon_exo?exoid="^exoid^"&type=question_ipynb" in
  let basthon_url = Simple_httpd.Util.percent_encode basthon_url in
  let basthon_url = Options.jurl () ^ basthon_url in
  let show_url = href (Printf.sprintf "show_solution?exoid=%s" exoid) in
  let note, snote = match note with
    | None ->
       {html|<a href=<?=show_url?>>Pas encore de note (cliquer pour le détail)</a>|html},
       {html||html}
    | Some n ->
       let n = Printf.sprintf "%.1f" n in
       {html|<a href=<?=show_url?>>Note: <?=n?> (cliquer pour le détail)</a>|html},
       {html|<span>Note: <?=n?></span>|html}

  in
  onglet ~small:true ~visible (dt ^ snote)
    {html|<form enctype="multipart/form-data" method="Post" action=<?=(href "submit_solution")?>>
         <table class="fill"><tr><td>
           <a target="_blank" href=<?=basthon_url?>>Aller travailler sur basthon</a></td><td><?=note?></td></tr></table>
        </form>|html}

let exos_student userid req =
  let courses = Db.all_courses_student userid req in
  let gn (name, courseid, _) =
    let exos = Db.get_visible_exos userid courseid req in
    let visible = ref true in
    let fn (exoid,time,name,fname) =
      let r = form_exo userid exoid ~name !visible time fname req in
      visible := false;
      r
    in
    onglet ~visible:false name (String.concat " " (List.map fn exos))
  in
  {html|
    <h3>Exercices disponibles</h3>
    <p class="attention">ATTENTION: les notes sont indicatives, les tests automatiques peuvent être insuffisants pour détecter certaines erreurs et ils ne prennent pas en compte la qualité du code ou la présence de vos propres tests.</p>
    <?= String.concat " " (List.map gn courses)?>
   |html}


let show_solution login userid exoid req =
  let (time,name,fname,_visible) = Db.get_exo_by_id exoid req in
  let (best,bt) = Db.best_solution userid exoid req in
  let (latest,lt) = Db.latest_solution userid exoid req in
  let html = {html|<?= header login?>
    <h3><?=name?></h3>
    <p class="attention">ATTENTION: les notes sont indicatives, les tests automatiques peuvent être insuffisants pour détecter certaines erreurs et ils ne prennent pas en compte la qualité du code ou la présence de vos propres tests.</p>
    <div class="center"><table class="table">
    <tr><th>Meilleure</th><th>Dernière</th></tr>
    <tr><td><?=bt?></td><td><?=lt?></td></tr>
    <tr><td><pre><?=best?></pre></td>
        <td><pre><?=latest?></pre></td>
        </tr></table></div><br/>
    <?=form_exo userid exoid true time fname req?>|html}
  in
  html_wrapper html

let add_exo ?stdin name description visible fname content =
  let open MyUtil in
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
    let (out,status) = run_cmd "py2nb" [fname] in
    if status <> Simple_httpd.Process.Exited 0 then
      failwith ("bad python template(py2nb): " ^ out);
    let question_ipynb_fname = rname ^ ".ipynb" in
    let corrige_ipynb_fname = rname ^ "_corrige.ipynb" in
    let test_ipynb_fname = rname ^ "_test.ipynb" in
    let ipynbs = [question_ipynb_fname; corrige_ipynb_fname; test_ipynb_fname] in
    let (_out,status) = run_cmd "nb2py" ipynbs in
    if status <> Simple_httpd.Process.Exited 0 then
      failwith ("bad python template(nb2py): " ^ out);
    let question_py_fname = rname ^ ".py" in
    let corrige_py_fname = rname ^ "_corrige.py" in
    let test_py_fname = rname ^ "_test.py" in
    let last = if stdin_name = "" then [] else [stdin_name] in
    let (out1,status) = run_cmd ~jail:dirname "python3"
                               ("/usr/local/lib/pynbtools/evaluation.py"::
                                test_py_fname::question_py_fname::last) in
    if status <> Simple_httpd.Process.Exited 0 then
      failwith ("bad python template (check question): " ^ out1);
    let (out2,status) = run_cmd ~jail:dirname "python3"
                               ("/usr/local/lib/pynbtools/evaluation.py"::
                                test_py_fname::corrige_py_fname::last) in
    if status <> Simple_httpd.Process.Exited 0 then
      failwith ("bad python template (chack solution): " ^ out2);
    let visible = if visible then "true" else "false" in
    let html = html_wrapper {html|
      Évaluation de l'exercice: <?=name?><br/>
      <table class="table">
       <tr><th>Correction</th><th>Question</th></tr>                                         <tr><td><pre><?=out2?></pre></td>
           <td><pre><?=out1?></pre></td></tr>
      </table>
      <form method="Post" action=<?=(href "record_exo")?>>
        <input type="hidden" name="name" value=<?=name?>/>
        <input type="hidden" name="description" value=<?=description?>/>
        <input type="hidden" name="fname" value=<?=bname?>/>
        <input type="hidden" name="visible" value=<?=visible?>/>
        <input type="hidden" name="dirname" value=<?=dirname?>/>
        <input type="hidden" name="tpy" value=<?=fname?>/>
        <input type="hidden" name="question_ipynb" value=<?=question_ipynb_fname?>/>
        <input type="hidden" name="corrige_ipynb" value=<?=corrige_ipynb_fname?>/>
        <input type="hidden" name="question_py" value=<?=question_py_fname?>/>
        <input type="hidden" name="corrige_py" value=<?=corrige_py_fname?>/>
        <input type="hidden" name="test_py" value=<?=test_py_fname?>/>
        <input type="hidden" name="stdin" value=<?=stdin_name?>/>
        <input type="submit" name="submit" value="Enregistrer"/>
        <input type="submit" name="submit" value="Annuler"/>
      </form>|html}
    in
    (* TODO: remove dirname if this form is not submitted after some time ?
       If user leaves that page? Both?
    *)
    html
  with
    e ->
    let _ = Sys.command ("rm -rf "^dirname) in
    raise e

let home_page () req =
  let prefix = Options.prefix ()in
  let contents = ref "" in
  let add_contents x = contents :=
                         {html|<?=!contents?><?=x?><br/>|html} in
  let dest = "/" ^ prefix in

  add_contents
     {html|<h1>Bienvenue sur le site d'auto-évaluation Basthon/Python</h1>
       <h3>Connexion</h3>
         <form method="Post" name="login" onsubmit="validate_login"
             action=<?=href "login"?>>
         <table class="fill"><tr><td style="text-align:left;">
          <label for="login">Identifiant:</label>
           <input type="text" name="login" id="login"/>
         </td><td style="text-align:center;">
          <label for="password">Mot de passe:</label>
           <input type="password" name="password" id="password"/>
         </td><td style="text-align:right;">
          <input type="submit" value="Entrer"/>
          <input type="hidden" name="dest" value=<?=dest?> on/>
         </td></tr></table>
      </form>|html};

  add_contents (create_user false req);

  (html_wrapper !contents, chaml_wrapper !contents)


let home_page_logged login id role req =
  let contents = ref "" in
  let add_contents ?(br=false) x =
    contents := {html|<?=!contents?><?=x?><?=if br then {html|<br/>|html} else ""?>|html}
  in
  add_contents ~br:false (header ~is_home:true login);
  add_contents ~br:false
    {html|<h2>Éval Basthon/Python</h2>|html};
  begin
    let open Db.Role in
    match role with
    | Student ->
       add_contents ~br:true (exos_student id req);
    | Prof | Admin ->

       add_contents (manage_exos id req);
       add_contents (creation_exo ());

       add_contents (prof_courses id req);
       if role = Admin || role = Prof then
         begin
           add_contents (creation_course ());
           add_contents (manage_courses id req);

           add_contents (creation_classe ());
           add_contents (manage_classes id);

           add_contents (create_user (role = Admin) req);
           add_contents (manage_users (role = Admin) req);
         end

  end;

  html_wrapper !contents
