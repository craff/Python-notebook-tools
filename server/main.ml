open Simple_httpd
module Options = Pynbtools.Options

let port = ref 8080
let ssl_port = ref 8443
let ssl_cert = ref ""
let ssl_priv = ref ""

let args, parameters = Simple_httpd.Server.args ()

let spec =
  Arg.[
      ("-P", Set_int port, "set the port");
      ("--ssl-port", Arg.Set_int ssl_port, " set ssl port");
      ("--ssl", Tuple[Set_string ssl_cert; Set_string ssl_priv], " give ssl certificate and private key");
      ("-p", String Options.set_prefix,
       "fix the prefix of the url (no \"/\" allowed)");
      ("-z", Int Options.set_tz, "fix the timezone insecond");
      ("-i", String (fun s -> Options.set_docker_image  s),
       "user given docker image");
  ] @ args

let usage = Printf.sprintf "usage: %s [-]" Sys.argv.(0)

let anon_fun s =
  Printf.eprintf "Dot not know what to do with %s\n%!" s;
  exit 1

let () = Arg.parse spec anon_fun usage

let ssl =
  if !ssl_cert <> "" then
    Some Simple_httpd.Address.{ cert = !ssl_cert; priv = !ssl_priv; protocol = Ssl.TLSv1_2 }
  else None
let port = !port
let ssl_port = !ssl_port

module Site = struct
  let addresses = match ssl with
    | None     -> [Address.make ~addr:"0.0.0.0" ~port:port ()]
    | Some ssl -> [Address.make ~addr:"0.0.0.0" ~port:ssl_port ~ssl ()]

  module Init = Pynbtools.Make
end


(** Start the server *)
let _ = Host.start_server parameters [(module Site)]
