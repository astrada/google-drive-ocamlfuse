open OUnit

let string_of_int64_option = function
  | None -> "None"
  | Some value -> "Some " ^ Int64.to_string value

let assert_unix_error expected_error f =
  try
    f ();
    assert_failure "Expected Unix.Unix_error"
  with
  | Unix.Unix_error (actual_error, _, _) ->
      assert_equal expected_error actual_error
  | e -> raise e

let file_info ?(file_handle = 0L) ?(flags = []) () =
  {
    Fuse.fi_flags = flags;
    fi_flags_raw = 0;
    fi_fh = file_handle;
    fi_writepage = false;
    fi_direct_io = false;
    fi_keep_cache = false;
    fi_flush = false;
    fi_nonseekable = false;
    fi_flock_release = false;
    fi_cache_readdir = false;
    fi_noflush = false;
    fi_lock_owner = 0L;
    fi_poll_events = 0l;
  }

let rename_flags raw =
  {
    Fuse.rename_noreplace = false;
    rename_exchange = false;
    rename_whiteout = false;
    rename_flags_raw = raw;
  }

let test_dir_entry_of_name () =
  let entry = GdfuseFuseNative.dir_entry_of_name "file.txt" in
  assert_equal "file.txt" entry.Fuse.entry_name;
  assert_equal None entry.Fuse.entry_stats;
  assert_equal None entry.Fuse.entry_offset;
  assert_equal false entry.Fuse.entry_flags.Fuse.fill_dir_plus

let test_file_info_update_of_handle () =
  let no_handle = GdfuseFuseNative.file_info_update_of_handle None in
  let handle = GdfuseFuseNative.file_info_update_of_handle (Some 42) in
  assert_equal ~printer:string_of_int64_option None no_handle.Fuse.fi_update_fh;
  assert_equal ~printer:string_of_int64_option (Some 42L)
    handle.Fuse.fi_update_fh

let test_file_handle_conversion () =
  assert_equal ~printer:string_of_int 42
    (GdfuseFuseNative.int_of_file_handle 42L);
  assert_equal ~printer:string_of_int 7
    (GdfuseFuseNative.file_handle_as_int (file_info ~file_handle:7L ()))

let test_file_handle_overflow () =
  let too_large = Int64.add (Int64.of_int max_int) 1L in
  assert_unix_error Unix.EOVERFLOW (fun () ->
      ignore (GdfuseFuseNative.int_of_file_handle too_large))

let test_flags_of_file_info () =
  let flags = [ Unix.O_RDONLY; Unix.O_TRUNC ] in
  assert_equal flags (GdfuseFuseNative.flags_of_file_info (file_info ~flags ()))

let test_float_of_timestamp () =
  let timestamp = Fuse.Time { Fuse.tv_sec = 42L; tv_nsec = 500_000_000 } in
  assert_equal ~printer:string_of_float 42.5
    (GdfuseFuseNative.float_of_timestamp "utimens" "/file.txt" timestamp)

let test_float_of_timestamp_rejects_sentinels () =
  assert_unix_error Unix.EINVAL (fun () ->
      ignore
        (GdfuseFuseNative.float_of_timestamp "utimens" "/file.txt" Fuse.Now));
  assert_unix_error Unix.EINVAL (fun () ->
      ignore
        (GdfuseFuseNative.float_of_timestamp "utimens" "/file.txt" Fuse.Omit))

let test_reject_unsupported_rename_flags () =
  GdfuseFuseNative.reject_unsupported_rename_flags "/old.txt" (rename_flags 0);
  assert_unix_error Unix.EINVAL (fun () ->
      GdfuseFuseNative.reject_unsupported_rename_flags "/old.txt"
        (rename_flags 1))

let suite =
  "GdfuseFuseNative test"
  >::: [
         "test_dir_entry_of_name" >:: test_dir_entry_of_name;
         "test_file_info_update_of_handle" >:: test_file_info_update_of_handle;
         "test_file_handle_conversion" >:: test_file_handle_conversion;
         "test_file_handle_overflow" >:: test_file_handle_overflow;
         "test_flags_of_file_info" >:: test_flags_of_file_info;
         "test_float_of_timestamp" >:: test_float_of_timestamp;
         "test_float_of_timestamp_rejects_sentinels"
         >:: test_float_of_timestamp_rejects_sentinels;
         "test_reject_unsupported_rename_flags"
         >:: test_reject_unsupported_rename_flags;
       ]
