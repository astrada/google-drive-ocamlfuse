val resource_keys_header_name : string

val build_resource_keys_header :
  (string option * string option) list -> GapiCore.Header.t list

val build_resource_keys_header_from_resource :
  CacheData.Resource.t -> GapiCore.Header.t list

val build_resource_keys_header_from_resources :
  CacheData.Resource.t list -> GapiCore.Header.t list
