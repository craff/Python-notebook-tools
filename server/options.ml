let prefix = ref ""
let tz = ref 0

let spec =
  Arg.[
      ("-p", Set_string prefix, "fix the prefix of the url (no \"/\" allowed)");
      ("-z", Set_int tz, "fix the timezone insecond")
  ]

let usage = Printf.sprintf "usage: %s [-]" Sys.argv.(0)

let anon_fun s =
  Printf.eprintf "Dot not know what to do with %s\n%!" s;
  exit 1

let () = Arg.parse spec anon_fun usage

let prefix = !prefix
let tz = !tz
