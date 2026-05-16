let f_bsize = 4096L
let f_namemax = 256L
let unlimited_quota_limit = Int64.max_int

type runtime = { metadata : CacheData.Metadata.t; config : Config.t }

let quota_limit runtime =
  if
    runtime.metadata.CacheData.Metadata.storage_quota_limit = 0L
    || runtime.config.Config.team_drive_id <> ""
  then unlimited_quota_limit
  else runtime.metadata.CacheData.Metadata.storage_quota_limit

let statfs runtime =
  let limit = quota_limit runtime in
  let f_blocks = Int64.div limit f_bsize in
  let free_bytes =
    Int64.sub limit runtime.metadata.CacheData.Metadata.storage_quota_usage
  in
  let f_bfree = Int64.div free_bytes f_bsize in
  {
    Fuse.Unix_util.f_bsize;
    f_blocks;
    f_bfree;
    f_bavail = f_bfree;
    f_files = f_blocks;
    f_ffree = f_bfree;
    f_namemax;
    (* ignored *)
    f_frsize = 0L;
    f_favail = 0L;
    f_fsid = 0L;
    f_flag = 0L;
  }
