open OUnit

let _ =
  let suite =
    "google-drive-ocamlfuse test suite"
    >::: [
           TestConfigRuntime.suite;
           TestConfigStore.suite;
           TestGdfuseCli.suite;
           TestGdfuseApp.suite;
           TestGdfuseFlow.suite;
           TestDriveUploadDispatch.suite;
           TestDriveFileMutations.suite;
           TestDriveMetadataMutations.suite;
           TestDriveOpens.suite;
           TestDriveReads.suite;
           TestDriveXattrs.suite;
           TestDriveMutations.suite;
           TestDriveViews.suite;
           TestDriveDirectoryReads.suite;
           TestBuffering.suite;
           TestThreadPool.suite;
           TestBufferPool.suite;
           TestUtils.suite;
         ]
  in
  OUnit.run_test_tt_main suite
