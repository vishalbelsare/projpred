# A customized version of vdiffr::expect_doppelganger() that is needed because
# in 'testthat' versions >= 3.3.0, testthat::expect_snapshot_file() (upon which
# vdiffr::expect_doppelganger() builds) errors if the snapshot file name has
# already been used.
expect_doppelganger_cust <- function(title,
                                     fig,
                                     test_filename = "methods_vsel",
                                     ...) {
  snap_nm_old <- testthat::test_path(
    "_snaps", test_filename, paste0(tolower(gsub("\\.|_", "-", title)), ".svg")
  )
  if (packageVersion("testthat") >= "3.3.0" && file.exists(snap_nm_old)) {
    variant_tmp <- paste0("tmp_doppelganger__",
                          format(Sys.time(), format = "%Y-%m-%d_%H-%M-%S"))
    vdiffr::expect_doppelganger(title, fig, variant = variant_tmp, ...)
    snap_nm_new <- sub("_snaps", file.path("_snaps", variant_tmp), snap_nm_old)
    expect_true(compare_file_text(old = snap_nm_old,
                                  new = snap_nm_new),
                info = title)
    file.remove(snap_nm_new)
    file.remove(testthat::test_path("_snaps", variant_tmp, test_filename))
    file.remove(testthat::test_path("_snaps", variant_tmp))
  } else {
    vdiffr::expect_doppelganger(title = title, fig = fig, ...)
  }
  return(invisible(TRUE))
}
