let stop_buffer_eviction_thread context =
  match context.Context.buffer_eviction_thread with
  | None -> ()
  | Some buffer_eviction_thread ->
      Utils.log_with_header "Stopping buffer eviction thread (TID=%d)...%!"
        (Thread.id buffer_eviction_thread);
      Buffering.MemoryBuffers.stop_eviction_thread context.Context.memory_buffers;
      Thread.join buffer_eviction_thread;
      Utils.log_message "done\n%!"

let stop_flush_db_thread context =
  match context.Context.flush_db_thread with
  | None -> ()
  | Some flush_db_thread ->
      Utils.log_with_header "Stopping flush DB thread (TID=%d)...%!"
        (Thread.id flush_db_thread);
      MemoryCache.stop_flush_db_thread ();
      Thread.join flush_db_thread;
      Utils.log_message "done\n%!"

let stop_async_upload_thread context =
  match context.Context.async_upload_thread with
  | None -> ()
  | Some async_upload_thread ->
      Utils.log_with_header "Stopping async upload thread (TID=%d)\n%!"
        (Thread.id async_upload_thread);
      UploadQueue.stop_async_upload_thread ();
      Thread.join async_upload_thread

let stop_folder_fetching_thread context =
  match context.Context.folder_fetching_thread with
  | None -> ()
  | Some folder_fetching_thread ->
      Utils.log_with_header
        "Stopping background folder fetching thread (TID=%d)...%!"
        (Thread.id folder_fetching_thread);
      BackgroundFolderFetching.stop_folder_fetching_thread ();
      Thread.join folder_fetching_thread;
      Utils.log_message "done\n%!"

let stop_background_threads context =
  stop_buffer_eviction_thread context;
  stop_flush_db_thread context;
  stop_async_upload_thread context;
  stop_folder_fetching_thread context

let flush_and_mark_clean_shutdown context =
  Utils.log_with_header "Flushing cache...\n%!";
  Cache.flush context.Context.cache;
  Utils.log_with_header "Storing clean shutdown flag...%!";
  DbCache.set_clean_shutdown context.Context.cache;
  Utils.log_message "done\n%!"

let cleanup_curl_and_context context =
  Utils.log_with_header "CURL cleanup...%!";
  ignore (GapiCurl.global_cleanup context.Context.curl_state);
  Utils.log_message "done\n%!";
  Utils.log_with_header "Clearing context...%!";
  Context.clear_ctx ();
  Utils.log_message "done\n%!"

let shutdown () =
  Utils.log_with_header "Exiting.\n%!";
  let context = Context.get_ctx () in
  stop_background_threads context;
  flush_and_mark_clean_shutdown context;
  cleanup_curl_and_context context
