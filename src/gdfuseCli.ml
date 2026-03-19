type parsed = {
  show_version : bool;
  mount_requested : bool;
  params : GdfuseCommon.application_params;
  fuse_args : string list;
}

let print_version () =
  Printf.printf
    "google-drive-ocamlfuse, version %s\n\
     Copyright (C) 2012-2026 Alessandro Strada\n\
     License MIT\n"
    Config.version

let parse_argv argv =
  let argv = if Array.length argv = 0 then [| "gdfuse" |] else argv in
  let fs_label = ref GdfuseCommon.default_fs_label in
  let mountpoint = ref "" in
  let fuse_args = ref [ "-f"; "-obig_writes" ] in
  let show_version = ref false in
  let debug = ref false in
  let client_id = ref "" in
  let client_secret = ref "" in
  let clear_cache = ref false in
  let headless = ref false in
  let skip_trash = ref false in
  let base_dir = ref "" in
  let multi_threading = ref false in
  let config_path = ref "" in
  let xdg_base_directory = ref false in
  let browser = ref "" in
  let docs_mode = ref "" in
  let service_account_credentials_path = ref "" in
  let service_account_user_to_impersonate = ref "" in
  let log_to = ref "" in
  let scope = ref "" in
  let redirect_uri = ref "" in
  let device = ref false in
  let port = ref 8080 in
  let program = Filename.basename argv.(0) in
  let usage = Printf.sprintf "Usage: %s [options] [mountpoint]" program in
  let parse_mount_options opt_string =
    let opts = Str.split (Str.regexp " *, *") opt_string in
    let base_dir_opt =
      List.filter (fun o -> ExtString.String.starts_with o "gdfroot") opts
    in
    List.iter
      (fun o ->
        try
          let _, bd = ExtString.String.split o "=" in
          base_dir := bd
        with ExtString.Invalid_string ->
          failwith "Invalid mount option gdfroot")
      base_dir_opt;
    let fuse_mount_opts =
      List.filter (fun o -> not (ExtString.String.starts_with o "gdfroot")) opts
    in
    let fuse_mount_opt_string = String.concat "," fuse_mount_opts in
    fuse_args := ("-o" ^ fuse_mount_opt_string) :: !fuse_args
  in
  let arg_specs =
    Arg.align
      [
        ("-version", Arg.Set show_version, " show version and exit.");
        ( "-verbose",
          Arg.Set Utils.verbose,
          " enable verbose logging on stdout. Default is false." );
        ( "-debug",
          Arg.Unit
            (fun () ->
              debug := true;
              Utils.verbose := true),
          " enable debug mode (implies -verbose). Default is false." );
        ( "-label",
          Arg.Set_string fs_label,
          " use a specific label to identify the filesystem. Default is \
           \"default\"." );
        ("-id", Arg.Set_string client_id, " provide OAuth2 client ID.");
        ( "-secret",
          Arg.Set_string client_secret,
          " provide OAuth2 client secret." );
        ( "-d",
          Arg.Unit (fun _ -> fuse_args := "-d" :: !fuse_args),
          " enable FUSE debug output." );
        ( "-m",
          Arg.Unit (fun _ -> multi_threading := true),
          " run in multi-threaded mode (default)." );
        ( "-s",
          Arg.Unit
            (fun _ ->
              fuse_args := "-s" :: !fuse_args;
              multi_threading := false),
          " run in single-threaded mode." );
        ("-o", Arg.String parse_mount_options, " specify FUSE mount options.");
        ("-cc", Arg.Set clear_cache, " clear cache");
        ( "-headless",
          Arg.Set headless,
          " enable headless mode. Default is false." );
        ( "-skiptrash",
          Arg.Set skip_trash,
          " enable permanent deletion mode. Default is false. Activate at your \
           own risk. Files deleted with this option *cannot* be restored." );
        ( "-config",
          Arg.Set_string config_path,
          " use a custom config file path. Default is \
           \"~/.gdfuse/[label]/config\"." );
        ( "-xdgbd",
          Arg.Set xdg_base_directory,
          " enable XDG Base Directory support. Default is false." );
        ( "-browser",
          Arg.Set_string browser,
          " starts a specific browser to access authorization page. Default is \
           xdg-open, firefox, google-chrome, chromium-browser, open." );
        ( "-docsmode",
          Arg.Set_string docs_mode,
          " sets the specified mode for Google Docs (implies -cc, if changed). \
           Supported values are: libreoffice, msoffice, desktop, off. Default \
           is desktop." );
        ( "-serviceaccountpath",
          Arg.Set_string service_account_credentials_path,
          " sets the path of the JSON file that contains service account \
           credentials." );
        ( "-serviceaccountuser",
          Arg.Set_string service_account_user_to_impersonate,
          " sets the email of the G Suite user to impersonate." );
        ( "-log_to",
          Arg.Set_string log_to,
          " logs to 'stdout' ('-'), 'stderr', or an absolute path." );
        ("-scope", Arg.Set_string scope, " sets a custom Drive API scope.");
        ( "-redirect_uri",
          Arg.Set_string redirect_uri,
          " sets a custom Drive API redirect URI." );
        ("-device", Arg.Set device, " use OAuth2 for devices. Default is false.");
        ("-port", Arg.Set_int port, " set loopback port. Default is 8080.");
      ]
  in
  ignore (Arg.parse_argv ~current:(ref 0) argv arg_specs (fun s -> mountpoint := s) usage);
  let params =
    {
      GdfuseCommon.debug = !debug;
      filesystem_label = !fs_label;
      client_id = !client_id;
      client_secret = !client_secret;
      mountpoint = !mountpoint;
      clear_cache = !clear_cache;
      headless = !headless;
      skip_trash = !skip_trash;
      base_dir = !base_dir;
      multi_threading = !multi_threading;
      config_path = !config_path;
      xdg_base_directory = !xdg_base_directory;
      browser = !browser;
      docs_mode = !docs_mode;
      service_account_credentials_path = !service_account_credentials_path;
      service_account_user_to_impersonate = !service_account_user_to_impersonate;
      log_to = !log_to;
      scope = !scope;
      redirect_uri = !redirect_uri;
      device = !device;
      port = !port;
    }
  in
  {
    show_version = !show_version;
    mount_requested = !mountpoint <> "";
    params;
    fuse_args = !fuse_args;
  }

let parse () = parse_argv Sys.argv
