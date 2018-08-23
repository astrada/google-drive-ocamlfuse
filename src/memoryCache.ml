module Resource =
struct
  (* Queries *)
  let insert_resource cache resource =
    resource

  let update_resource cache resource =
    ()

  let update_resource_state cache state id =
    ()

  let update_resource_state_and_size cache state size id =
    ()

  let delete_resource cache resource =
    ()

  let delete_not_found_resource_with_path cache path =
    ()

  let delete_resources cache resources =
    ()

  let insert_resources cache resources parent_path trashed =
    []

  let invalidate_resources cache ids =
    ()

  let invalidate_path cache path =
    ()

  let invalidate_all cache =
    ()

  let invalidate_trash_bin cache =
    ()

  let trash_resources cache resources =
    ()

  let delete_all_with_parent_path cache parent_path trashed =
    ()

  let trash_all_with_parent_path cache parent_path =
    ()

  let update_all_timestamps cache last_update =
    ()

  let select_resource_with_path cache path trashed =
    None

  let select_resource_with_remote_id cache remote_id =
    None

  let select_resources_with_parent_path cache parent_path trashed =
    []

  let select_resources_order_by_last_update cache =
    []

end

module Metadata =
struct
  (* Queries *)
  let insert_metadata cache metadata =
    ()

  let select_metadata cache =
    None

  let update_cache_size cache delta =
    ()
end

let setup cache =
  ()

