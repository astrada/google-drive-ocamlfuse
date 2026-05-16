open OUnit
module Stats = DriveFilesystemStats

let make_metadata ?(storage_quota_limit = 40960L) ?(storage_quota_usage = 8192L)
    () =
  {
    CacheData.Metadata.display_name = "metadata";
    storage_quota_limit;
    storage_quota_usage;
    start_page_token = "token";
    cache_size = 0L;
    last_update = 0.;
    clean_shutdown = false;
  }

let make_config ?(team_drive_id = "") () = { Config.default with team_drive_id }

let runtime ?(metadata = make_metadata ()) ?(config = make_config ()) () =
  { Stats.metadata; config }

let assert_int64 expected actual =
  assert_equal ~printer:Int64.to_string expected actual

let assert_fixed_fields statfs =
  assert_int64 Stats.f_bsize statfs.Fuse.Unix_util.f_bsize;
  assert_int64 Stats.f_namemax statfs.Fuse.Unix_util.f_namemax;
  assert_int64 0L statfs.Fuse.Unix_util.f_frsize;
  assert_int64 0L statfs.Fuse.Unix_util.f_favail;
  assert_int64 0L statfs.Fuse.Unix_util.f_fsid;
  assert_int64 0L statfs.Fuse.Unix_util.f_flag

let test_quota_limit_uses_metadata_limit () =
  let metadata = make_metadata ~storage_quota_limit:12345L () in
  assert_int64 12345L (Stats.quota_limit (runtime ~metadata ()))

let test_quota_limit_zero_reports_unlimited () =
  let metadata = make_metadata ~storage_quota_limit:0L () in
  assert_int64 Stats.unlimited_quota_limit
    (Stats.quota_limit (runtime ~metadata ()))

let test_quota_limit_team_drive_reports_unlimited () =
  let config = make_config ~team_drive_id:"team-drive" () in
  assert_int64 Stats.unlimited_quota_limit
    (Stats.quota_limit (runtime ~config ()))

let test_statfs_normal_quota_block_math () =
  let metadata =
    make_metadata ~storage_quota_limit:40960L ~storage_quota_usage:8192L ()
  in
  let statfs = Stats.statfs (runtime ~metadata ()) in
  assert_int64 10L statfs.Fuse.Unix_util.f_blocks;
  assert_int64 8L statfs.Fuse.Unix_util.f_bfree;
  assert_int64 statfs.Fuse.Unix_util.f_bfree statfs.Fuse.Unix_util.f_bavail;
  assert_int64 statfs.Fuse.Unix_util.f_blocks statfs.Fuse.Unix_util.f_files;
  assert_int64 statfs.Fuse.Unix_util.f_bfree statfs.Fuse.Unix_util.f_ffree;
  assert_fixed_fields statfs

let test_statfs_zero_limit_uses_unlimited_blocks () =
  let metadata =
    make_metadata ~storage_quota_limit:0L ~storage_quota_usage:4096L ()
  in
  let statfs = Stats.statfs (runtime ~metadata ()) in
  let expected_blocks = Int64.div Stats.unlimited_quota_limit Stats.f_bsize in
  let expected_free =
    Int64.div (Int64.sub Stats.unlimited_quota_limit 4096L) Stats.f_bsize
  in
  assert_int64 expected_blocks statfs.Fuse.Unix_util.f_blocks;
  assert_int64 expected_free statfs.Fuse.Unix_util.f_bfree

let test_statfs_team_drive_uses_unlimited_blocks () =
  let metadata =
    make_metadata ~storage_quota_limit:40960L ~storage_quota_usage:8192L ()
  in
  let config = make_config ~team_drive_id:"team-drive" () in
  let statfs = Stats.statfs (runtime ~metadata ~config ()) in
  let expected_blocks = Int64.div Stats.unlimited_quota_limit Stats.f_bsize in
  let expected_free =
    Int64.div (Int64.sub Stats.unlimited_quota_limit 8192L) Stats.f_bsize
  in
  assert_int64 expected_blocks statfs.Fuse.Unix_util.f_blocks;
  assert_int64 expected_free statfs.Fuse.Unix_util.f_bfree

let test_statfs_preserves_negative_free_blocks () =
  let metadata =
    make_metadata ~storage_quota_limit:4096L ~storage_quota_usage:8192L ()
  in
  let statfs = Stats.statfs (runtime ~metadata ()) in
  assert_int64 1L statfs.Fuse.Unix_util.f_blocks;
  assert_int64 (-1L) statfs.Fuse.Unix_util.f_bfree;
  assert_int64 (-1L) statfs.Fuse.Unix_util.f_bavail;
  assert_int64 (-1L) statfs.Fuse.Unix_util.f_ffree

let suite =
  "DriveFilesystemStats tests"
  >::: [
         "test_quota_limit_uses_metadata_limit"
         >:: test_quota_limit_uses_metadata_limit;
         "test_quota_limit_zero_reports_unlimited"
         >:: test_quota_limit_zero_reports_unlimited;
         "test_quota_limit_team_drive_reports_unlimited"
         >:: test_quota_limit_team_drive_reports_unlimited;
         "test_statfs_normal_quota_block_math"
         >:: test_statfs_normal_quota_block_math;
         "test_statfs_zero_limit_uses_unlimited_blocks"
         >:: test_statfs_zero_limit_uses_unlimited_blocks;
         "test_statfs_team_drive_uses_unlimited_blocks"
         >:: test_statfs_team_drive_uses_unlimited_blocks;
         "test_statfs_preserves_negative_free_blocks"
         >:: test_statfs_preserves_negative_free_blocks;
       ]
