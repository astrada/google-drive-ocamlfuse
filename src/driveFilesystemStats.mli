val f_bsize : int64
val f_namemax : int64
val unlimited_quota_limit : int64

type runtime = { metadata : CacheData.Metadata.t; config : Config.t }

val quota_limit : runtime -> int64
val statfs : runtime -> Fuse.Unix_util.statvfs
