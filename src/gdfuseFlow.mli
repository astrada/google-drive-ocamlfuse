module type DEPS = sig
  module System : sig
    val open_log_out_ch : string -> string -> out_channel
    val start_browser : string -> string -> unit
    val register_exit : (unit -> unit) -> unit
    val curl_init : unit -> [ `Initialized ] GapiCurl.t
    val curl_cleanup : [ `Initialized ] GapiCurl.t -> unit
    val exit : int -> 'a
  end

  module Auth : sig
    val gae_start_server_polling : unit -> unit
    val gae_refresh_access_token : unit -> string
    val get_access_token : bool -> bool -> string -> unit
    val refresh_access_token : unit -> string
  end

  module Cache : sig
    val create_cache : AppDir.t -> Config.t -> CacheData.t
    val clean_up_cache : CacheData.t -> unit
    val setup_db : CacheData.t -> unit
    val check_clean_shutdown : CacheData.t -> bool
    val reset_clean_shutdown : CacheData.t -> unit
    val set_clean_shutdown : CacheData.t -> unit
    val flush : CacheData.t -> unit
  end

  module Fuse : sig
    val start_filesystem : string -> string list -> unit
  end
end

module Make (D : DEPS) : sig
  val setup_application : GdfuseCommon.application_params -> unit
  val run_bootstrap_only : GdfuseCommon.application_params -> unit
  val run_mount_mode : GdfuseCommon.application_params -> string list -> unit
  val shutdown : unit -> unit
end
