
let (tz, set_tz) =
  let tz = ref 0 in
  (fun () -> !tz),
  (fun x -> tz := x)

let (docker_image, set_docker_image) =
  let docker_image= ref None in
  (fun () -> !docker_image),
  (fun (i:string) -> docker_image := Some i)

let (docker_options, set_docker_options) =
  let docker_options= ref [||] in
  (fun () -> !docker_options),
  (fun (opts:string array) -> docker_options := opts)

let (top_dir, set_top_dir) =
  let top_dir = ref "./basthon_store" in
  (fun () -> !top_dir),
  (fun (i:string) -> top_dir := i)

let (prefix, set_prefix) =
  let prefix = ref "pynbtools" in
  (fun () -> !prefix),
  (fun p -> prefix := p)

let jurl () = Printf.sprintf "/%s/basthon/index.html?from=" (prefix ())
