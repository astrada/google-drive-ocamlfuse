let resource_keys_header_name = "X-Goog-Drive-Resource-Keys"

let build_resource_keys_header ids_and_resource_keys =
  let ids_with_valid_resource_keys =
    List.filter
      (fun (_, resource_key) ->
        match resource_key with None -> false | Some "" -> false | _ -> true)
      ids_and_resource_keys
  in
  let id_resource_key_pairs =
    List.map
      (fun id_resource_key ->
        match id_resource_key with
        | Some id, Some resource_key -> id ^ "/" ^ resource_key
        | _ -> "")
      ids_with_valid_resource_keys
  in
  let valid_id_resource_key_pairs =
    List.filter
      (fun p -> match p with "" -> false | _ -> true)
      id_resource_key_pairs
  in
  match valid_id_resource_key_pairs with
  | [] -> []
  | _ ->
      let header_value = String.concat "," valid_id_resource_key_pairs in
      let custom_header =
        GapiCore.Header.KeyValueHeader (resource_keys_header_name, header_value)
      in
      [ custom_header ]

let build_resource_keys_header_from_resource resource =
  let ids_and_resource_keys =
    [
      ( resource.CacheData.Resource.remote_id,
        resource.CacheData.Resource.resource_key );
    ]
  in
  build_resource_keys_header ids_and_resource_keys

let build_resource_keys_header_from_resources resources =
  let ids_and_resource_keys =
    List.map
      (fun resource ->
        ( resource.CacheData.Resource.remote_id,
          resource.CacheData.Resource.resource_key ))
      resources
  in
  build_resource_keys_header ids_and_resource_keys
