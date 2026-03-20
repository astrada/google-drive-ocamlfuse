module ConfigFileStore = ConfigStore

module StateFileStore : sig
  type data = State.t
  type t = { path : string; data : data }

  val path : (t, string) GapiLens.t
  val data : (t, data) GapiLens.t
  val load : string -> t
  val save : t -> unit
end

type t = {
  app_dir : AppDir.t;
  config_store : ConfigFileStore.t;
  state_store : StateFileStore.t;
  gapi_config : GapiConfig.t;
  cache : CacheData.t;
  curl_state : [ `Initialized ] GapiCurl.t;
  mountpoint_path : string;
  mountpoint_stats : Unix.LargeFile.stats;
  metadata : CacheData.Metadata.t option;
  metadata_lock : Mutex.t;
  skip_trash : bool;
  memory_buffers : Buffering.MemoryBuffers.t;
  file_locks : (string, Mutex.t) Hashtbl.t;
  buffer_eviction_thread : Thread.t option;
  root_folder_id : string option;
  flush_db_thread : Thread.t option;
  async_upload_thread : Thread.t option;
  folder_fetching_thread : Thread.t option;
  verification_code : string;
}

val app_dir : (t, AppDir.t) GapiLens.t
val config_store : (t, ConfigFileStore.t) GapiLens.t
val state_store : (t, StateFileStore.t) GapiLens.t
val gapi_config : (t, GapiConfig.t) GapiLens.t
val cache : (t, CacheData.t) GapiLens.t
val curl_state : (t, [ `Initialized ] GapiCurl.t) GapiLens.t
val mountpoint_path : (t, string) GapiLens.t
val mountpoint_stats : (t, Unix.LargeFile.stats) GapiLens.t
val metadata : (t, CacheData.Metadata.t option) GapiLens.t
val metadata_lock : (t, Mutex.t) GapiLens.t
val skip_trash : (t, bool) GapiLens.t
val memory_buffers : (t, Buffering.MemoryBuffers.t) GapiLens.t
val file_locks : (t, (string, Mutex.t) Hashtbl.t) GapiLens.t
val buffer_eviction_thread : (t, Thread.t option) GapiLens.t
val root_folder_id : (t, string option) GapiLens.t
val flush_db_thread : (t, Thread.t option) GapiLens.t
val async_upload_thread : (t, Thread.t option) GapiLens.t
val folder_fetching_thread : (t, Thread.t option) GapiLens.t
val verification_code : (t, string) GapiLens.t
val config_lens : (t, Config.t) GapiLens.t
val state_lens : (t, StateFileStore.data) GapiLens.t
val request_id_lens : (t, string) GapiLens.t
val refresh_token_lens : (t, string) GapiLens.t
val saved_version_lens : (StateFileStore.t, string) GapiLens.t
val metadata_lens : (t, CacheData.Metadata.t) GapiLens.t
val metadata_last_update_lens : (t, float) GapiLens.t

module ConcurrentContext : sig
  type nonrec t = t

  val global : t Global.t
  val mutex : Mutex.t
  val with_lock : (unit -> 'a) -> 'a
  val get_no_lock : unit -> t
  val set_no_lock : t -> unit
  val get : unit -> t
  val set : t -> unit
  val clear : unit -> unit
  val update : (t -> t) -> unit
end

val get_ctx : unit -> ConcurrentContext.t
val set_ctx : ConcurrentContext.t -> unit
val clear_ctx : unit -> unit
val update_ctx : (ConcurrentContext.t -> ConcurrentContext.t) -> unit
val with_ctx_lock : (unit -> 'a) -> 'a
val save_state_store : StateFileStore.t -> unit
val save_state_from_context : ConcurrentContext.t -> unit
val save_config_store : ConfigFileStore.t -> unit
val get_cache : unit -> CacheData.t
