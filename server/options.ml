let prefix = ref ""
let tz = ref 0
let port = ref 8080
let jurl = ref "/basthon/?from="
let self_url = ref None
let docker_image= ref None

let spec =
  Arg.[
      ("-p", Set_string prefix,
       "fix the prefix of the url (no \"/\" allowed)");
      ("-z", Set_int tz, "fix the timezone insecond");
      ("-P", Set_int port, "the port to listen to");
      ("-j", Set_string jurl, "jupyter/basthon url");
      ("-i", String (fun s -> docker_image := Some s),
       "user given docker image");
      ("-d", Unit (fun () -> Tiny_httpd._enable_debug true),
       "enable debug of httpd server");
      ("-s", String (fun s -> self_url := Some s), "self url");
  ]

let usage = Printf.sprintf "usage: %s [-]" Sys.argv.(0)

let anon_fun s =
  Printf.eprintf "Dot not know what to do with %s\n%!" s;
  exit 1

let () = Arg.parse spec anon_fun usage

let prefix = !prefix
let tz = !tz
let port = !port
let jurl = !jurl
let self_url = match !self_url with
  | None -> prefix
  | Some s -> s
let docker_image = !docker_image
