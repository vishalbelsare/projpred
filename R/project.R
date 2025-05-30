#' Projection onto submodel(s)
#'
#' Project the posterior of the reference model onto the parameter space of a
#' single submodel consisting of a specific combination of predictor terms or
#' (after variable selection) onto the parameter space of a single or multiple
#' submodels of specific sizes.
#'
#' @param object An object which can be used as input to [get_refmodel()] (in
#'   particular, objects of class `refmodel`).
#' @param nterms Only relevant if `object` is of class `vsel` (returned by
#'   [varsel()] or [cv_varsel()]). Ignored if `!is.null(predictor_terms)`.
#'   Number of terms for the submodel (the corresponding combination of
#'   predictor terms is taken from `object`). If a numeric vector, then the
#'   projection is performed for each element of this vector. If `NULL` (and
#'   `is.null(predictor_terms)`), then the value suggested by [suggest_size()]
#'   is taken (with default arguments for [suggest_size()], implying that this
#'   suggested size is based on the ELPD). Note that `nterms` does not count the
#'   intercept, so use `nterms = 0` for the intercept-only model.
#' @param solution_terms Deprecated. Please use argument `predictor_terms`
#'   instead.
#' @param predictor_terms If not `NULL`, then this needs to be a character
#'   vector of predictor terms for the submodel onto which the projection will
#'   be performed. Argument `nterms` is ignored in that case. For an `object`
#'   which is not of class `vsel`, `predictor_terms` must not be `NULL`.
#' @param refit_prj A single logical value indicating whether to fit the
#'   submodels (again) (`TRUE`) or---if `object` is of class `vsel`---to re-use
#'   the submodel fits from the full-data search that was run when creating
#'   `object` (`FALSE`). For an `object` which is not of class `vsel`,
#'   `refit_prj` must be `TRUE`. See also section "Details" below.
#' @param ndraws Only relevant if `refit_prj` is `TRUE`. Number of posterior
#'   draws to be projected. Ignored if `nclusters` is not `NULL` or if the
#'   reference model is of class `datafit` (in which case one cluster is used).
#'   If both (`nclusters` and `ndraws`) are `NULL`, the number of posterior
#'   draws from the reference model is used for `ndraws`. See also section
#'   "Details" below.
#' @param nclusters Only relevant if `refit_prj` is `TRUE`. Number of clusters
#'   of posterior draws to be projected. Ignored if the reference model is of
#'   class `datafit` (in which case one cluster is used). For the meaning of
#'   `NULL`, see argument `ndraws`. See also section "Details" below.
#' @param seed Pseudorandom number generation (PRNG) seed by which the same
#'   results can be obtained again if needed. Passed to argument `seed` of
#'   [set.seed()], but can also be `NA` to not call [set.seed()] at all. If not
#'   `NA`, then the PRNG state is reset (to the state before calling
#'   [project()]) upon exiting [project()]. Here, `seed` is used for clustering
#'   the reference model's posterior draws (if `!is.null(nclusters)`) and for
#'   drawing new group-level effects when predicting from a multilevel submodel
#'   (however, not yet in case of a GAMM) and having global option
#'   `projpred.mlvl_pred_new` set to `TRUE`. (Such a prediction takes place when
#'   calculating output elements `dis` and `ce`.)
#' @param verbose A single integer value from the set \eqn{\{0, 1, 2\}}{{0, 1,
#'   2}} (if `!is.null(predictor_terms)`, \eqn{1} and \eqn{2} have the same
#'   effect), indicating how much information (if any) to print out during the
#'   computations. Higher values indicate that more information should be
#'   printed, `0` deactivates the verbose mode. Internally, argument `verbose`
#'   is coerced to integer via `as.integer()`, so technically, a single logical
#'   value or a single numeric value work as well.
#' @param ... Arguments passed to [get_refmodel()] (if [get_refmodel()] is
#'   actually used; see argument `object`) as well as to the divergence
#'   minimizer (if `refit_prj` is `TRUE`).
#'
#' @details Arguments `ndraws` and `nclusters` are automatically truncated at
#'   the number of posterior draws in the reference model (which is `1` for
#'   `datafit`s). Using less draws or clusters in `ndraws` or `nclusters` than
#'   posterior draws in the reference model may result in slightly inaccurate
#'   projection performance. Increasing these arguments affects the computation
#'   time linearly.
#'
#'   If `refit_prj = FALSE` (which is only possible if `object` is of class
#'   `vsel`), [project()] retrieves the submodel fits from the full-data search
#'   that was run when creating `object`. Usually, the search relies on a rather
#'   coarse clustering or thinning of the reference model's posterior draws (by
#'   default, [varsel()] and [cv_varsel()] use `nclusters = 20`). Consequently,
#'   [project()] with `refit_prj = FALSE` then inherits this coarse clustering
#'   or thinning.
#'
#' @return If the projection is performed onto a single submodel (i.e.,
#'   `length(nterms) == 1 || !is.null(predictor_terms)`), an object of class
#'   `projection` which is a `list` containing the following elements:
#'   \describe{
#'     \item{`dis`}{Projected draws for the dispersion parameter.}
#'     \item{`ce`}{The cross-entropy part of the Kullback-Leibler (KL)
#'     divergence from the reference model to the submodel. For some families,
#'     this is not the actual cross-entropy, but a reduced one where terms which
#'     would cancel out when calculating the KL divergence have been dropped. In
#'     case of the Gaussian family, that reduced cross-entropy is further
#'     modified, yielding merely a proxy.}
#'     \item{`wdraws_prj`}{Weights for the projected draws.}
#'     \item{`predictor_terms`}{A character vector of the submodel's predictor
#'     terms.}
#'     \item{`outdmin`}{A `list` containing the submodel fits (one fit per
#'     projected draw). This is the same as the return value of the
#'     `div_minimizer` function (see [init_refmodel()]), except if [project()]
#'     was used with an `object` of class `vsel` based on an L1 search as well
#'     as with `refit_prj = FALSE`, in which case this is the output from an
#'     internal *L1-penalized* divergence minimizer.}
#'     \item{`cl_ref`}{A numeric vector of length equal to the number of
#'     posterior draws in the reference model, containing the cluster indices of
#'     these draws.}
#'     \item{`wdraws_ref`}{A numeric vector of length equal to the number of
#'     posterior draws in the reference model, giving the weights of these
#'     draws. These weights should be treated as not being normalized (i.e.,
#'     they don't necessarily sum to `1`).}
#'     \item{`const_wdraws_prj`}{A single logical value indicating whether the
#'     projected draws have constant weights (`TRUE`) or not (`FALSE`).}
#'     \item{`refmodel`}{The reference model object.}
#'   }
#'   If the projection is performed onto more than one submodel, the output from
#'   above is returned for each submodel, giving a `list` with one element for
#'   each submodel.
#'
#'   The elements of an object of class `projection` are not meant to be
#'   accessed directly but instead via helper functions (see the main vignette
#'   and [projpred-package]; see also [as_draws_matrix.projection()], argument
#'   `return_draws_matrix` of [proj_linpred()], and argument
#'   `nresample_clusters` of [proj_predict()] for the intended use of the
#'   weights stored in element `wdraws_prj`).
#'
#' @examplesIf requireNamespace("rstanarm", quietly = TRUE)
#' # Data:
#' dat_gauss <- data.frame(y = df_gaussian$y, df_gaussian$x)
#'
#' # The `stanreg` fit which will be used as the reference model (with small
#' # values for `chains` and `iter`, but only for technical reasons in this
#' # example; this is not recommended in general):
#' fit <- rstanarm::stan_glm(
#'   y ~ X1 + X2 + X3 + X4 + X5, family = gaussian(), data = dat_gauss,
#'   QR = TRUE, chains = 2, iter = 500, refresh = 0, seed = 9876
#' )
#'
#' # Run varsel() (here without cross-validation, with L1 search, and with small
#' # values for `nterms_max` and `nclusters_pred`, but only for the sake of
#' # speed in this example; this is not recommended in general):
#' vs <- varsel(fit, method = "L1", nterms_max = 3, nclusters_pred = 10,
#'              seed = 5555)
#'
#' # Projection onto the best submodel with 2 predictor terms (with a small
#' # value for `nclusters`, but only for the sake of speed in this example;
#' # this is not recommended in general):
#' prj_from_vs <- project(vs, nterms = 2, nclusters = 10, seed = 9182)
#'
#' # Projection onto an arbitrary combination of predictor terms (with a small
#' # value for `nclusters`, but only for the sake of speed in this example;
#' # this is not recommended in general):
#' prj <- project(fit, predictor_terms = c("X1", "X3", "X5"), nclusters = 10,
#'                seed = 9182)
#'
#' @export
project <- function(
    object,
    nterms = NULL,
    solution_terms = predictor_terms,
    predictor_terms = NULL,
    refit_prj = TRUE,
    ndraws = 400,
    nclusters = NULL,
    seed = NA,
    verbose = getOption("projpred.verbose", as.integer(interactive())),
    ...
) {
  # Parse input -------------------------------------------------------------

  if (!missing(solution_terms)) {
    warning("Argument `solution_terms` is deprecated. Please use argument ",
            "`predictor_terms` instead.")
    predictor_terms <- solution_terms
  }

  verbose <- verbose_from_deprecated_options(verbose, with_cv = FALSE,
                                             proj_only = TRUE)
  verbose <- as.integer(verbose)

  ## `object` ---------------------------------------------------------------

  if (inherits(object, "datafit")) {
    stop("project() does not support an `object` of class `datafit`.")
  }
  if (!inherits(object, "vsel") && is.null(predictor_terms)) {
    stop("Please provide an `object` of class `vsel` or use argument ",
         "`predictor_terms`.")
  }
  if (!inherits(object, "vsel") && !refit_prj) {
    stop("Please provide an `object` of class `vsel` or use ",
         "`refit_prj = TRUE`.")
  }

  refmodel <- get_refmodel(object, ...)

  ## `seed` -----------------------------------------------------------------

  if (exists(".Random.seed", envir = .GlobalEnv)) {
    rng_state_old <- get(".Random.seed", envir = .GlobalEnv)
  }
  if (!is.na(seed)) {
    # Set seed, but ensure the old RNG state is restored on exit:
    if (exists(".Random.seed", envir = .GlobalEnv)) {
      on.exit(assign(".Random.seed", rng_state_old, envir = .GlobalEnv))
    }
    set.seed(seed)
  }

  ## `refit_prj` ------------------------------------------------------------

  if (refit_prj && inherits(refmodel, "datafit")) {
    warning("Automatically setting `refit_prj` to `FALSE` since the reference ",
            "model is of class `datafit`.")
    refit_prj <- FALSE
  }

  stopifnot(is.null(predictor_terms) || is.vector(predictor_terms, "character"))
  if (!refit_prj &&
      !is.null(predictor_terms) &&
      any(
        object$predictor_ranking[seq_along(predictor_terms)] != predictor_terms
      )) {
    warning("The given `predictor_terms` are not part of the predictor ",
            "ranking (from `object`), so `refit_prj` is automatically set to ",
            "`TRUE`.")
    refit_prj <- TRUE
  }

  ## `predictor_terms` and `nterms` -----------------------------------------

  if (!is.null(predictor_terms)) {
    # In this case, `predictor_terms` is given, so `nterms` is ignored.

    if (verbose == 1L) {
      verbose <- verbose + 1L
    }

    # The table of possible predictor terms:
    if (!is.null(object$predictor_ranking)) {
      vars <- object$predictor_ranking
    } else {
      vars <- split_formula(refmodel$formula, data = refmodel$fetch_data(),
                            add_main_effects = FALSE)
      vars <- setdiff(vars, "1")
    }

    # Reduce `predictor_terms` to those predictor terms that can be found in the
    # table of possible predictor terms:
    if (!all(predictor_terms %in% vars)) {
      warning(
        "The following element(s) of `predictor_terms` could not be found in ",
        "the table of possible predictor terms: `c(\"",
        paste(setdiff(predictor_terms, vars), collapse = "\", \""), "\")`. ",
        "These elements are ignored. (The table of predictor terms is either ",
        "`object$predictor_ranking` or the vector of terms in the reference ",
        "model, depending on whether `object$predictor_ranking` is `NULL` or ",
        "not. Here, the table of predictor terms is: `c(\"",
        paste(vars, collapse = "\", \""), "\")`.)"
      )
    }
    predictor_terms <- intersect(predictor_terms, vars)

    nterms <- length(predictor_terms)
  } else {
    # In this case, `predictor_terms` is not given, so it is fetched from
    # `object$predictor_ranking` and `nterms` becomes relevant.

    predictor_terms <- object$predictor_ranking

    if (is.null(nterms)) {
      # In this case, `nterms` is not given, so we infer it via suggest_size().
      sgg_size <- try(suggest_size(object, warnings = FALSE), silent = TRUE)
      if (!inherits(sgg_size, "try-error") && !is.null(sgg_size) &&
          !is.na(sgg_size)) {
        nterms <- min(sgg_size, length(predictor_terms))
      } else {
        stop("Could not suggest a submodel size automatically; please specify ",
             "`nterms` or `predictor_terms`.")
      }
    } else {
      if (!is.numeric(nterms) || any(nterms < 0)) {
        stop("Argument `nterms` must contain non-negative values.")
      }
      if (max(nterms) > length(predictor_terms)) {
        stop(paste(
          "Cannot perform the projection with", max(nterms), "variables,",
          "because variable selection was run only up to",
          length(predictor_terms), "variables."
        ))
      }
    }
  }

  ## `nclusters` ------------------------------------------------------------

  if (inherits(refmodel, "datafit")) {
    nclusters <- 1
  }

  ## Warnings ---------------------------------------------------------------

  nterms_max <- max(nterms)
  nterms_all <- count_terms_in_formula(refmodel$formula) - 1L
  if (nterms_max == nterms_all &&
      formula_contains_group_terms(refmodel$formula) &&
      getOption("projpred.warn_instable_projections", TRUE) &&
      (refmodel$family$family == "gaussian" || refmodel$family$for_latent)) {
    warning(
      "In case of the Gaussian family (also in case of the latent projection) ",
      "and multilevel terms, the projection onto the full model can be ",
      "instable and even lead to an error, see GitHub issue #323."
    )
  }

  # Projection --------------------------------------------------------------

  submodls <- perf_eval(
    search_path = list(predictor_ranking = predictor_terms,
                       p_sel = object$search_path$p_sel,
                       outdmins = object$search_path$outdmins),
    nterms = nterms, refmodel = refmodel, refit_prj = refit_prj,
    ndraws = ndraws, nclusters = nclusters, return_submodls = TRUE,
    verbose = verbose + (1L * as.logical(verbose)), ...
  )

  # Output ------------------------------------------------------------------

  projs <- lapply(submodls, function(submodl) {
    proj_k <- submodl
    proj_k$refmodel <- refmodel
    class(proj_k) <- "projection"
    return(proj_k)
  })
  # If there is only a single submodel size, just return the `projection` object
  # instead of returning it in a list of length 1:
  return(unlist_proj(projs))
}

#' Print information about [project()] output
#'
#' This is the [print()] method for objects of class `projection`. This method
#' mainly exists to avoid cluttering the console when printing such objects
#' accidentally.
#'
#' @param x An object of class `projection` (returned by [project()], possibly
#'   as elements of a `list`).
#' @param ... Currently ignored.
#'
#' @return The input object `x` (invisible).
#'
#' @export
print.projection <- function(x, ...) {
  cat_cls(x)
  # Print information about `x` (only information that is unique to `x`; for the
  # rest, print.refmodel() can be used).
  if (x$clust_used) {
    clust_pretty <- " (from clustered projection)"
  } else {
    clust_pretty <- ""
  }
  cat("Number of projected draws: ", x$nprjdraws, clust_pretty, "\n", sep = "")
  cat("Predictor terms: ",
      paste(paste0("\"", predictor_terms(x), "\""), collapse = ", "),
      "\n", sep = "")
  cat("\n")
  cat("More information can be printed via `print(get_refmodel(<x>))`, where ",
      "`<x>` denotes the object that is currently printed.", "\n", sep = "")
  return(invisible(x))
}
