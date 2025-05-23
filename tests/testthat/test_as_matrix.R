context("as.matrix.projection()")

test_that("as.matrix.projection() works", {
  skip_if_not(run_prj)
  for (tstsetup in names(prjs)) {
    if (args_prj[[tstsetup]]$mod_nm == "gam") {
      # Skipping GAMs because of issue #150 and issue #151. Note that for GAMs,
      # the current expectations in `test_as_matrix.R` refer to a mixture of
      # brms's and rstanarm's naming scheme; as soon as issue #152 is solved,
      # these expectations need to be adapted.
      # TODO (GAMs): Fix this.
      next
    }
    if (args_prj[[tstsetup]]$mod_nm == "gamm") {
      # Skipping GAMMs because of issue #131.
      # TODO (GAMMs): Fix this.
      next
    }
    tstsetup_ref <- args_prj[[tstsetup]]$tstsetup_ref
    mod_crr <- args_prj[[tstsetup]]$mod_nm
    fam_crr <- args_prj[[tstsetup]]$fam_nm
    pkg_crr <- args_prj[[tstsetup]]$pkg_nm
    prj_crr <- args_prj[[tstsetup]]$prj_nm
    prd_trms <- args_prj[[tstsetup]]$predictor_terms
    ndr_ncl <- ndr_ncl_dtls(args_prj[[tstsetup]])

    m <- as.matrix(prjs[[tstsetup]],
                   allow_nonconst_wdraws_prj = ndr_ncl$clust_used_gt1)
    if (!has_const_wdr_prj(prjs[[tstsetup]])) {
      wdr_crr <- prjs[[tstsetup]][["wdraws_prj"]]
    } else {
      wdr_crr <- NULL
    }
    expect_identical(attr(m, "wdraws_prj"), wdr_crr, info = tstsetup)

    if (fam_crr == "gauss" || prj_crr == "latent") {
      npars_fam <- "sigma"
    } else {
      npars_fam <- character()
    }

    if (fam_crr == "cumul" && prj_crr == "augdat") {
      # Note: Here, we could also derive `icpt_nm` from
      # `prjs[[tstsetup]]$refmodel$family$cats`.
      if (pkg_crr == "rstanarm") {
        icpt_nm <- paste(head(levels(prjs[[tstsetup]]$refmodel$y), -1),
                         tail(levels(prjs[[tstsetup]]$refmodel$y), -1),
                         sep = "|")
      } else if (pkg_crr == "brms") {
        icpt_nm <- paste0("Intercept[",
                          seq_len(nlevels(prjs[[tstsetup]]$refmodel$y) - 1L),
                          "]")
      }
    } else {
      icpt_nm <- "Intercept"
      if (pkg_crr == "rstanarm") {
        icpt_nm <- paste0("(", icpt_nm, ")")
      }
    }
    colnms_prjmat_expect <- c(
      icpt_nm,
      grep("\\|", grep("x(co|ca)\\.[[:digit:]]", prd_trms, value = TRUE),
           value = TRUE, invert = TRUE)
    )
    xca_idxs <- as.integer(
      sub("^xca\\.", "", grep("^xca\\.", colnms_prjmat_expect, value = TRUE))
    )
    for (xca_idx in xca_idxs) {
      colnms_prjmat_expect <- grep(paste0("^xca\\.", xca_idx, "$"),
                                   colnms_prjmat_expect,
                                   value = TRUE, invert = TRUE)
      colnms_prjmat_expect <- c(
        colnms_prjmat_expect,
        paste0("xca.", xca_idx, "lvl", seq_len(nlvl_fix[xca_idx])[-1])
      )
    }
    I_logic_trms <- grep("I\\(.*as\\.logical\\(.*\\)\\)", colnms_prjmat_expect,
                         value = TRUE)
    if (length(I_logic_trms)) {
      colnms_prjmat_expect <- c(
        setdiff(colnms_prjmat_expect, I_logic_trms),
        unlist(lapply(I_logic_trms, function(I_logic_trms_i) {
          paste0(I_logic_trms_i, "TRUE")
        }))
      )
    }
    colnms_prjmat_expect <- expand_poly(colnms_prjmat_expect,
                                        info_str = tstsetup)
    if (pkg_crr == "brms") {
      if (fam_crr == "categ") {
        # Note: Here, we could also derive `yunq_norefcat` from
        # `prjs[[tstsetup]]$refmodel$family$cats`.
        yunq_norefcat <- tail(levels(prjs[[tstsetup]]$refmodel$y), -1)
        colnms_prjmat_expect <- unlist(lapply(
          colnms_prjmat_expect,
          function(colnms_prjmat_expect_i) {
            paste0("mu", yunq_norefcat, "_",
                   colnms_prjmat_expect_i)
          }
        ))
      }
      colnms_prjmat_expect <- paste0("b_", colnms_prjmat_expect)
    }
    if (any(c("(1 | z.1)", "(xco.1 | z.1)") %in% prd_trms)) {
      if (pkg_crr == "brms") {
        mlvl_icpt_str <- "Intercept"
        if (fam_crr == "categ") {
          mlvl_icpt_str <- paste0("mu", yunq_norefcat, "_", mlvl_icpt_str)
        }
        colnms_prjmat_expect <- c(colnms_prjmat_expect,
                                  paste0("sd_z.1__", mlvl_icpt_str))
        if (!getOption("projpred.mlvl_pred_new", FALSE)) {
          if (fam_crr == "categ") {
            mlvl_r_str <- paste0("__mu", yunq_norefcat)
          } else {
            mlvl_r_str <- ""
          }
          colnms_prjmat_expect <- c(
            colnms_prjmat_expect,
            unlist(lapply(mlvl_r_str, function(mlvl_r_str_i) {
              paste0("r_z.1", mlvl_r_str_i,
                     "[lvl", seq_len(nlvl_ran[1]), ",Intercept]")
            }))
          )
        }
      } else if (pkg_crr == "rstanarm") {
        colnms_prjmat_expect <- c(colnms_prjmat_expect,
                                  "Sigma[z.1:(Intercept),(Intercept)]")
        if (!getOption("projpred.mlvl_pred_new", FALSE)) {
          colnms_prjmat_expect <- c(
            colnms_prjmat_expect,
            paste0("b[(Intercept) z.1:lvl", seq_len(nlvl_ran[1]), "]")
          )
        }
      }
    }
    if ("(xco.1 | z.1)" %in% prd_trms) {
      if (pkg_crr == "brms") {
        mlvl_xco_str <- "xco.1"
        if (fam_crr == "categ") {
          mlvl_xco_str <- paste0("mu", yunq_norefcat, "_", mlvl_xco_str)
        }
        colnms_prjmat_expect <- c(colnms_prjmat_expect,
                                  paste0("sd_z.1__", mlvl_xco_str))
        if (!getOption("projpred.mlvl_pred_new", FALSE)) {
          colnms_prjmat_expect <- c(
            colnms_prjmat_expect,
            unlist(lapply(mlvl_r_str, function(mlvl_r_str_i) {
              paste0("r_z.1", mlvl_r_str_i,
                     "[lvl", seq_len(nlvl_ran[1]), ",xco.1]")
            }))
          )
        }
      } else if (pkg_crr == "rstanarm") {
        colnms_prjmat_expect <- c(colnms_prjmat_expect,
                                  "Sigma[z.1:xco.1,xco.1]")
        if (!getOption("projpred.mlvl_pred_new", FALSE)) {
          colnms_prjmat_expect <- c(
            colnms_prjmat_expect,
            paste0("b[xco.1 z.1:lvl", seq_len(nlvl_ran[1]), "]")
          )
        }
      }
      # Correlation:
      if (pkg_crr == "brms") {
        mlvl_icpt_xco_str <- combn(c(mlvl_icpt_str, mlvl_xco_str), m = 2,
                                   FUN = paste, collapse = "__")
        colnms_prjmat_expect <- c(colnms_prjmat_expect,
                                  paste0("cor_z.1__", mlvl_icpt_xco_str))
      } else if (pkg_crr == "rstanarm") {
        colnms_prjmat_expect <- c(colnms_prjmat_expect,
                                  "Sigma[z.1:xco.1,(Intercept)]")
      }
    }
    s_nms <- sub("\\)$", "",
                 sub("^s\\(", "",
                     grep("^s\\(.*\\)$", prd_trms, value = TRUE)))
    if (length(s_nms)) {
      stopifnot(inherits(refmods[[tstsetup_ref]]$fit, "stanreg"))
      # Get the number of basis coefficients:
      s_info <- refmods[[tstsetup_ref]]$fit$jam$smooth
      s_terms <- sapply(s_info, "[[", "term")
      s_dfs <- setNames(sapply(s_info, "[[", "df"), s_terms)
      ### Alternative:
      # par_nms_orig <- colnames(
      #   as.matrix(refmods[[tstsetup_ref]]$fit)
      # )
      # s_dfs <- sapply(s_nms, function(s_nm) {
      #   sum(grepl(paste0("^s\\(", s_nm, "\\)"), par_nms_orig))
      # })
      ###
      # Construct the expected column names for the basis coefficients:
      for (s_nm in s_nms) {
        colnms_prjmat_expect <- c(
          colnms_prjmat_expect,
          paste0("b_s(", s_nm, ").", seq_len(s_dfs[s_nm]))
        )
      }
      # Needed for the names of the `smooth_sd` parameters:
      s_nsds <- setNames(
        lapply(lapply(s_info, "[[", "sp"), names),
        s_terms
      )
      # Construct the expected column names for the SDs of the smoothing
      # terms:
      for (s_nm in s_nms) {
        colnms_prjmat_expect <- c(
          colnms_prjmat_expect,
          paste0("smooth_sd[", s_nsds[[s_nm]], "]")
        )
      }
    }
    colnms_prjmat_expect <- c(colnms_prjmat_expect, npars_fam)

    expect_identical(dim(m), c(ndr_ncl$nprjdraws, length(colnms_prjmat_expect)),
                     info = tstsetup)
    ### expect_setequal() does not have argument `info`:
    # expect_setequal(colnames(m), colnms_prjmat_expect)
    expect_true(setequal(colnames(m), colnms_prjmat_expect),
                info = tstsetup)
    ###
    if (run_snaps) {
      if (testthat_ed_max2) local_edition(3)
      width_orig <- options(width = 145)
      expect_snapshot({
        print(tstsetup)
        print(rlang::hash(m)) # message(m)
      })
      options(width_orig)
      if (testthat_ed_max2) local_edition(2)
    }
  }
})

test_that("`allow_nonconst_wdraws_prj = FALSE` causes an error", {
  skip_if_not(run_prj)
  for (tstsetup in grep("\\.clust", names(prjs), value = TRUE)) {
    if (grepl("\\.clust1", tstsetup)) {
      err_expected <- NA
    } else {
      err_expected <- "different .* weights"
    }
    expect_error(as.matrix(prjs[[tstsetup]]), err_expected, info = tstsetup)
  }
})

test_that(paste(
  "as_draws_matrix.projection() is a conversion of as.matrix.projection(),",
  "with different weights of the projected draws causing the application of",
  "posterior::weight_draws()"
), {
  skip_if_not(run_prj)
  skip_if_not_installed("posterior")
  for (tstsetup in names(prjs)) {
    if (args_prj[[tstsetup]]$mod_nm == "gamm") {
      # Skipping GAMMs because of issue #131.
      # TODO (GAMMs): Fix this.
      next
    }
    m <- as.matrix(prjs[[tstsetup]],
                   allow_nonconst_wdraws_prj = ndr_ncl_dtls(
                     args_prj[[tstsetup]]
                   )$clust_used_gt1)
    m_dr <- posterior::as_draws_matrix(prjs[[tstsetup]])
    m_dr_repl <- posterior::as_draws_matrix(m)
    if (!has_const_wdr_prj(prjs[[tstsetup]])) {
      m_dr_repl <- posterior::weight_draws(m_dr_repl,
                                           weights = attr(m, "wdraws_prj"))
    }
    expect_identical(m_dr, m_dr_repl, info = tstsetup)
  }
})

test_that(paste(
  "as_draws_matrix.projection() passes argument `nm_scheme` to",
  "as.matrix.projection()"
), {
  skip_if_not(run_prj)
  skip_if_not_installed("posterior")
  for (tstsetup in grep("rstanarm\\.glm\\.", names(prjs), value = TRUE)) {
    m <- as.matrix(prjs[[tstsetup]], nm_scheme = "brms",
                   allow_nonconst_wdraws_prj = ndr_ncl_dtls(
                     args_prj[[tstsetup]]
                   )$clust_used_gt1)
    m_dr <- posterior::as_draws_matrix(prjs[[tstsetup]], nm_scheme = "brms")
    m_dr_repl <- posterior::as_draws_matrix(m)
    if (!has_const_wdr_prj(prjs[[tstsetup]])) {
      m_dr_repl <- posterior::weight_draws(m_dr_repl,
                                           weights = attr(m, "wdraws_prj"))
    }
    expect_identical(m_dr, m_dr_repl, info = tstsetup)
  }
})

test_that(paste(
  "as_draws.projection() is a wrapper for as_draws_matrix.projection()"
), {
  skip_if_not(run_prj)
  skip_if_not_installed("posterior")
  for (tstsetup in grep("rstanarm\\.glm\\.", names(prjs), value = TRUE)) {
    m_dr <- posterior::as_draws_matrix(prjs[[tstsetup]], nm_scheme = "brms")
    m_dr_unspec <- posterior::as_draws(prjs[[tstsetup]], nm_scheme = "brms")
    expect_identical(m_dr, m_dr_unspec, info = tstsetup)
  }
})

if (run_snaps) {
  if (testthat_ed_max2) local_edition(3)
  width_orig <- options(width = 145)

  test_that(paste(
    "as.matrix.projection() works for projections based on varsel() output"
  ), {
    skip_if_not(run_vs)
    for (tstsetup in names(prjs_vs)) {
      if (args_prj_vs[[tstsetup]]$mod_nm == "gam") {
        # Skipping GAMs because of issue #150 and issue #151. Note that for
        # GAMs, the current expectations in `test_as_matrix.R` refer to a
        # mixture of brms's and rstanarm's naming scheme; as soon as issue #152
        # is solved, these expectations need to be adapted.
        # TODO (GAMs): Fix this.
        next
      }
      if (args_prj_vs[[tstsetup]]$mod_nm == "gamm") {
        # Skipping GAMMs because of issue #131.
        # TODO (GAMMs): Fix this.
        next
      }
      nterms_crr <- args_prj_vs[[tstsetup]]$nterms

      prjs_vs_l <- prjs_vs[[tstsetup]]
      if (length(nterms_crr) <= 1) {
        prjs_vs_l <- list(prjs_vs_l)
      }
      res_vs <- lapply(prjs_vs_l, function(prjs_vs_i) {
        m <- as.matrix(prjs_vs_i,
                       allow_nonconst_wdraws_prj = ndr_ncl_dtls(
                         args_prj_vs[[tstsetup]]
                       )$clust_used_gt1)
        expect_snapshot({
          print(tstsetup)
          print(prjs_vs_i$predictor_terms)
          print(rlang::hash(m)) # message(m)
        })
        return(invisible(TRUE))
      })
    }
  })

  test_that(paste(
    "as.matrix.projection() works for projections based on cv_varsel() output"
  ), {
    skip_if_not(run_cvvs)
    for (tstsetup in names(prjs_cvvs)) {
      if (args_prj_cvvs[[tstsetup]]$mod_nm == "gam") {
        # Skipping GAMs because of issue #150 and issue #151. Note that for
        # GAMs, the current expectations in `test_as_matrix.R` refer to a
        # mixture of brms's and rstanarm's naming scheme; as soon as issue #152
        # is solved, these expectations need to be adapted.
        # TODO (GAMs): Fix this.
        next
      }
      if (args_prj_cvvs[[tstsetup]]$mod_nm == "gamm") {
        # Skipping GAMMs because of issue #131.
        # TODO (GAMMs): Fix this.
        next
      }
      nterms_crr <- args_prj_cvvs[[tstsetup]]$nterms

      prjs_cvvs_l <- prjs_cvvs[[tstsetup]]
      if (length(nterms_crr) <= 1) {
        prjs_cvvs_l <- list(prjs_cvvs_l)
      }
      res_cvvs <- lapply(prjs_cvvs_l, function(prjs_cvvs_i) {
        m <- as.matrix(prjs_cvvs_i,
                       allow_nonconst_wdraws_prj = ndr_ncl_dtls(
                         args_prj_cvvs[[tstsetup]]
                       )$clust_used_gt1)
        expect_snapshot({
          print(tstsetup)
          print(prjs_cvvs_i$predictor_terms)
          print(rlang::hash(m)) # message(m)
        })
        return(invisible(TRUE))
      })
    }
  })

  options(width_orig)
  if (testthat_ed_max2) local_edition(2)
}
