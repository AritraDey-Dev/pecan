# Functions for processing output

#' Burn-in and thinning of MCMC samples
#' 
#' @param samples Matrix of MCMC samples
#' @param target Target number of samples (default = 5000). Only applicable if 
#' auto=TRUE.
#' @param burnin.ratio Fraction of samples to burn-in; i.e. 2 means to remove 
#' first 1/2 of samples, 3 means 1/3, etc. (default = 2). Only applicable if 
#' auto=TRUE.
#' @param auto Whether or not to perform automatic burnin and thin based on 
#' target number of samples.
#' @param burnin Number of samples to discard as burnin (auto must be FALSE)
#' @param thin Thinning interval (auto must be FALSE)
#' @export
burnin.thin <- function(samples, target = 5000, burnin.ratio = 2, 
                        auto = TRUE, burnin = NULL, thin = NULL){
  ngibbs <- nrow(samples)
  if(auto) {
    burnin <- floor(ngibbs / burnin.ratio)
    thin <- floor((ngibbs - burnin) / target)
    if(thin < 1){
      message("Fewer than target samples after burnin. No thinning applied.")
      thin <- 1
    }
  }
  bt <- seq(burnin, ngibbs, by = thin)
  samples.bt <- samples[bt, ]
  return(samples.bt)
}

#' Load object from an RData file
#' 
#' @param filename Full name (without path!) of RData file
#' @param filepath Path of RData file (default='.')
#' @export
load.from.name <- function(filename, filepath = ".") {
  f.path <- file.path(filepath, filename)
  load(f.path)
  f.name <- gsub("(.*)[.]RData", "\\1", filename)
  f.get <- get(f.name)
  return(f.get)
}

#' Multivariate normal fit
#' 
#' Fit multivariate normal to samples. Return means and covariance matrix as a 
#' long list (for easy construction of data.tables)
#' @param samples Matrix of MCMC samples.
#' @export
summary_mvnorm <- function(samples) {
  testForPackage("mclust")
  stopifnot(colnames(samples) != NULL)
  parnames <- colnames(samples)
  sigmanames <- sprintf("%s.%s.sigma", rep(parnames, 1, each=6),
                        rep(parnames, 6))
  fit <- mclust::mvn("XXX", samples)
  mu <- as.numeric(fit$parameters$mean)
  sigma <- c(fit$parameters$variance$Sigma)
  names(mu) <- sprintf("%s.mu", parnames)
  names(sigma) <- sigmanames
  out.list <- c(as.list(mu), as.list(sigma))
  return(out.list)
}

#' Simple summary statistics on MCMC samples
#' 
#' Calculate simple univariate summary statistics and return as named list
#' @param samples Matrix of MCMC samples
#' @export
summary_simple <- function(samples) {
  stopifnot(colnames(samples) != NULL)
  parnames <- colnames(samples)
  mu <- colMeans(samples, na.rm = TRUE)
  names(mu) <- sprintf("%s.mu", parnames)
  sigma <- apply(samples, 2, sd, na.rm = TRUE)
  names(sigma) <- sprintf("%s.sigma", parnames)
  q25 <- apply(samples, 2, quantile, 0.025, na.rm = TRUE)
  names(q25) <- sprintf("%s.q25", parnames)
  med <- apply(samples, 2, median, na.rm = TRUE)
  names(med) <- sprintf("%s.med", parnames)
  q975 <- apply(samples, 2, quantile, 0.975, na.rm = TRUE)
  names(q975) <- sprintf("%s.q975", parnames)
  out.list <- c(as.list(mu), as.list(sigma), as.list(q25),
                as.list(med), as.list(q975))
  return(out.list)
}

#' Process output of inversion
#'
#' @param output.list List of output from inversion
#' @param prev_out Previous output (default = NULL)
#' @param iter_conv_check Number of iterations for convergence check
#' @param save.samples Filename to save samples (default = NULL)
#' @param threshold Threshold for convergence check
#' @param calculate.burnin Whether to calculate burnin (default = TRUE)
#' @return List of processed output
#' @export
process_output <- function(output.list,
                           prev_out = NULL,
                           iter_conv_check,
                           save.samples,
                           threshold,
                           calculate.burnin) {

  samples.current <- lapply(output.list, "[[", "results")
  deviance_list.current <- lapply(output.list, "[[", "deviance")
  n_eff_list.current <- lapply(output.list, "[[", "n_eff")
  rm(output.list)

  out <- list()

  if (is.null(prev_out)) {
    out$samples <- PEcAn.assim.batch::makeMCMCList(samples.current)
    out$deviance_list <- deviance_list.current
    out$n_eff_list <- n_eff_list.current
  } else {
    out$samples <- combineChains(prev_out$samples, samples.current)
    out$deviance_list <- mapply(c, prev_out$deviance_list,
                                deviance_list.current, SIMPLIFY = F)
    out$n_eff_list <- mapply(c, prev_out$n_eff_list, n_eff_list.current,
                             SIMPLIFY = F)
  }
  rm(prev_out)

  if (!is.null(save.samples)) {
    saveRDS(out, file = save.samples)
  }

  out$nsamp <- coda::niter(out$samples)
  nburn <- min(floor(out$nsamp/2), iter_conv_check)
  burned_samples <- window(out$samples, start = nburn)
  check_initial <- check.convergence(burned_samples,
                                     threshold = threshold,
                                     autoburnin = FALSE)
  if (check_initial$error) {
    warning("Could not calculate Gelman diag. Assuming no convergence.")
    out$finished <- FALSE
    return(out)
  }
  if (!check_initial$converged) {
    message("Convergence was not achieved. Continuing sampling.")
    out$finished <- FALSE
    return(out)
  } else {
    message("Passed initial convergence check.")
  }
  if (calculate.burnin) {
    burn <- PEcAn.assim.batch::autoburnin(out$samples, return.burnin = TRUE, method = 'gelman.plot')
    out$burnin <- burn$burnin
    if (out$burnin == 1) {
      message("Robust convergence check in autoburnin failed. ",
              "Resuming sampling.")
      out$finished <- FALSE
      return(out)
    } else {
      message("Converged after ", out$nsamp, "iterations.")
      out$results <- summary_simple(do.call(rbind, burn$samples))
    }
  } else {
    message("Skipping robust convergece check (autoburnin) because ",
            "calculate.burnin == FALSE.")
    out$burnin <- nburn
    out$results <- summary_simple(do.call(rbind, burned_samples))
  }
  message("Burnin = ", out$burnin)
  out$finished <- TRUE
  return(out)
}
