module Resource :
sig
  val insert_resource : CacheData.t -> CacheData.Resource.t -> CacheData.Resource.t
  val update_resource : CacheData.t -> CacheData.Resource.t -> unit
  val update_resource_state : CacheData.t -> CacheData.Resource.State.t -> int64 -> unit
  val update_resource_state_and_size :
    CacheData.t -> CacheData.Resource.State.t -> int64 -> int64 -> unit
  val delete_resource : CacheData.t -> CacheData.Resource.t -> unit
  val delete_not_found_resource_with_path : CacheData.t -> string -> unit
  val delete_resources : CacheData.t -> CacheData.Resource.t list -> unit
  val insert_resources : CacheData.t -> CacheData.Resource.t list -> string -> bool -> CacheData.Resource.t list
  val invalidate_resources : CacheData.t -> int64 list -> unit
  val invalidate_path : CacheData.t -> string -> unit
  val invalidate_all : CacheData.t -> unit
  val invalidate_trash_bin : CacheData.t -> unit
  val trash_resources : CacheData.t -> CacheData.Resource.t list -> unit
  val delete_all_with_parent_path : CacheData.t -> string -> bool -> unit
  val trash_all_with_parent_path : CacheData.t -> string -> unit
  val update_all_timestamps : CacheData.t -> float -> unit
  val select_resource_with_path : CacheData.t -> string -> bool -> CacheData.Resource.t option
  val select_first_resource_with_remote_id : CacheData.t -> string -> CacheData.Resource.t option
  val select_resources_with_remote_id : CacheData.t -> string -> CacheData.Resource.t list
  val select_resources_with_parent_path : CacheData.t -> string -> bool -> CacheData.Resource.t list
  val select_resources_order_by_last_update : CacheData.t -> CacheData.Resource.t list
  val select_resource_with_id : CacheData.t -> int64 -> CacheData.Resource.t option
  val select_next_folder_to_fetch : CacheData.t -> CacheData.Resource.t option

end

module Metadata :
sig
  val insert_metadata : CacheData.t -> CacheData.Metadata.t -> unit
  val select_metadata : CacheData.t -> CacheData.Metadata.t option
  val update_cache_size : CacheData.t -> int64 -> unit
end

module UploadQueue :
sig
  val insert_upload_entry :
    CacheData.t -> CacheData.UploadEntry.t -> CacheData.UploadEntry.t
  val select_next_resource : CacheData.t -> CacheData.UploadEntry.t option
  val select_with_resource_id :
    CacheData.t -> int64 -> CacheData.UploadEntry.t option
  val delete_upload_entry : CacheData.t -> CacheData.UploadEntry.t -> unit
  val update_entry_state :
    CacheData.t -> CacheData.UploadEntry.State.t -> int64 -> unit
  val count_entries : CacheData.t -> int

end

val setup : CacheData.t -> unit
val flush_db : CacheData.t -> unit

val stop_flush_db_thread : unit -> unit
val start_flush_db_thread : CacheData.t -> unit

