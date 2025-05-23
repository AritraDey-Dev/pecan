
#' allom.BayesFit
#'
#' Module to fit a common power-law allometric model
#' to a mixture of raw data and allometric equations
#' in a Heirarchical Bayes framework with multiple imputation
#' of the allometric data
#'
#' dependencies: requires MCMCpack and mvtnorm
#'
#' note: runs 1 chain, but multiple chains can be simulated by
#'       multiple function calls
#'
#' @param allom - object (usually generated by query.allom.data) which
#'                  needs to be a list with two entries:
#'            'field' - contains a list, each entry for which is
#'                      a data frame with 'x' and 'y'. Can be NULL
#'            'parm' -  a single data frame with the following components:
#'            \describe{
#'                   \item{n}{sample size}
#'                   \item{a}{eqn coefficient}
#'                   \item{b}{eqn coefficient}
#'                   \item{c}{eqn coefficient}
#'                   \item{d}{eqn coefficient}
#'                   \item{e}{eqn coefficient}
#'                   \item{se}{standard error}
#'                   \item{eqn}{sample size}
#'                   \item{Xmin}{smallest tree sampled (cm)}
#'                   \item{Xmax}{largest tree sampled (cm)}
#'                   \item{Xcor}{units correction on X}
#'                   \item{Ycor}{units correction on Y}
#'                   \item{Xtype}{type of measurement on the X}
#'                   \item{spp}{ - USFS species code}
#'          }
#' @param nrep - number of MCMC replicates
#'
#' @param form   functional form of the allometry: 'power' vs 'exp'

#' @param dmin   minimum dbh of interest
#' @param dmax   maximum dbh of interest
#'
#' @return returns MCMC chain and ONE instance of 'data'
#' note: in many cases the estimates are multiply imputed
#'
#' @author Michael Dietze
#'
allom.BayesFit <- function(allom, nrep = 10000, form = "power", dmin = 0.1, dmax = 500) {
  
  ## check for valid inputs
  if (!(form %in% ("power"))) {
    print(c("allom.BayesFit: Requested functional form", form, "not currently supported"))
    return(NULL)
  }
  if (is.null(allom)) {
    print("allom.BayesFit: no data recieved, allom is NULL")
    return(NULL)
  }
  if (!is.list(allom)) {
    print("allom.BayesFit: arguement allom must be a list")
    return(NULL)
  }
  if (!is.numeric(nrep) | nrep <= 0) {
    print(c("allom.BayesFit: invalid nrep", nrep))
    return(NULL)
  }
  
  
  ## grab required variables from allom$parm
  n     <- nu(allom[["parm"]]$n)
  a     <- nu(allom[["parm"]]$a)
  b     <- nu(allom[["parm"]]$b)
  c     <- nu(allom[["parm"]]$c)
  d     <- nu(allom[["parm"]]$d)
  e     <- nu(allom[["parm"]]$e)
  se    <- nu(allom[["parm"]]$se)
  Xcor  <- nu(allom[["parm"]]$Xcor)
  Ycor  <- nu(allom[["parm"]]$Ycor)
  Xtype <- as.character(allom[["parm"]]$Xtype)
  eqn   <- nu(allom[["parm"]]$eqn)
  rng   <- cbind(nu(allom$parm$Xmin), nu(allom$parm$Xmax))
  spp   <- nu(allom[["parm"]]$spp)
  
  ## declare constants
  
  ## Drop equations outside of DBH range of interest & modifying the pseduodata as necessary We need
  ## the max of the allometry EQ to be >= min of interest and the min to be <= ntally =
  ## nrow(allom[['parm']]); if(is.null(ntally)) ntally = 0;
  rng.mod <- cbind(ifelse(nu(allom$parm$Xmin) > dmin, nu(allom$parm$Xmin), dmin),
                   ifelse(nu(allom$parm$Xmax) < dmax, nu(allom$parm$Xmax), dmax))
  
  n.mod <- n
  for (i in seq_along(n)) {
    tmp.seq <- seq(rng[i, 1], rng[i, 2], length.out = 100)
    n.mod[i] <- round(n[i] * length(tmp.seq[tmp.seq > dmin & tmp.seq < dmax]) /
                        length(tmp.seq), digits = 0)
  }
  
  ntally <- which(nu(allom[["parm"]][, "Xmax"]) >= dmin &
                    nu(allom[["parm"]][, "Xmin"]) <= dmax &
                    n.mod > 0)
  if (is.null(ntally)) {
    ntally <- 0
  }
  print(c("Dropping allom rows: ", which(!(1:nrow(allom[["parm"]]) %in% ntally))))
  
  nfield <- length(allom[["field"]])
  nsite <- length(ntally[ntally > 0]) + nfield
  my.spp <- unique(spp)
  nspp <- length(my.spp)
  
  if (nsite == 0) {
    print(c("allomBayesFit no data"))
    return(NULL)
  }
  
  ## define priors
  s1     <- s2 <- 0.1  # IG prior on the within-study variance
  mu0    <- c(0.2, 8 / 3)  # normal prior mean on global mean
  V0     <- matrix(c(100, 0, 0, 100), 2, 2)  # normal prior variance on global mean
  V0I    <- solve(V0)
  m0V0   <- t(mu0) %*% V0I %*% mu0
  V0Imu0 <- V0I %*% mu0
  v      <- 0.1  ## wishart prior on across-study variance
  S      <- diag(0.1, 2)
  
  ## declare storage
  b0GIBBS  <- matrix(0, nrep, nsite)
  b1GIBBS  <- matrix(0, nrep, nsite)
  muGIBBS  <- matrix(0, nrep, 2)
  sigGIBBS <- rep(NA, nrep)
  tauGIBBS <- matrix(0, nrep, 3)
  DGIBBS   <- rep(NA, nrep)
  BgGIBBS  <- matrix(0, nrep, 2)
  SgGIBBS  <- rep(NA, nrep)
  DgGIBBS  <- rep(NA, nrep)
  
  ## initialization
  mu    <- mu0
  b0    <- rep(mu[1], nsite)
  b1    <- rep(mu[2], nsite)
  tau   <- diag(c(1, 1))
  tauI  <- solve(tau)
  sigma <- 0.3
  sinv  <- 1 / sigma
  data  <- allom[["field"]]
  for (i in seq_along(ntally)) {
    data[[i + nfield]] <- list(x = rep(0, n.mod[ntally[i]]), y = rep(0, n.mod[ntally[i]]))
  }
  
  x   <- y <- NULL
  Sg  <- 1
  Bg  <- mu0
  SgI <- 1 / Sg
  D   <- Dg <- 0
  
  ## MCMC LOOP
  pb <- utils::txtProgressBar(min = 0, max = nrep, style = 3)
  for (g in seq_len(nrep)) {
    
    ## For tabulated equations, impute X,Y data --------------------------------------
    if (ntally[1] > 0)
    {
      for (j in ntally) {
        x0 <- stats::runif(n.mod[j], rng.mod[j, 1], rng.mod[j, 2])
        if (!is.na(Xcor[j])) {
          x <- Xcor[j] * x0
        } else {
          if (Xtype[j] == "d.b.h.^2") {
            ## convert to sq inches
            x <- x0 * x0 / (2.54 * 2.54)
          } else {
            x <- x0 * x0 * pi / 4  ## convert to cm Basal Area
          }
        }
        y <- NA
        if (eqn[j] == 1) {
          y <- a[j] + b[j] * c[j] * log10(x)
          y <- 10^stats::rnorm(n.mod[j], y, se[j])
        } else if (eqn[j] == 2) {
          y <- a[j] + b[j] * x + c[j] * d[j] * log(x)
          y <- exp(stats::rnorm(n.mod[j], y, se[j]))
        } else if (eqn[j] == 3) {
          y <- a[j] + b[j] * log(x) + c[j] * (d[j] + (e[j] * log(x)))
          y <- exp(stats::rnorm(n.mod[j], y, se[j]))
        } else if (eqn[j] == 4) {
          y <- a[j] + b[j] * x + c[j] * x^d[j]
          y <- stats::rnorm(n.mod[j], y, se[j])
        } else if (eqn[j] == 5) {
          y <- a[j] + b[j] * x + c[j] * x^2 + d[j] * x^3
          y <- stats::rnorm(n.mod[j], y, se[j])
        } else if (eqn[j] == 6) {
          y <- a[j] * (exp(b[j] + (c[j] * log(x)) + d[j] * x))
          y <- stats::rnorm(n.mod[j], y, se[j])
        } else if (eqn[j] == 7) {
          y <- a[j] + ((b[j] * (x^c[j]))/((x^c[j]) + d[j]))
          y <- stats::rnorm(n.mod[j], y, se[j])
        } else if (eqn[j] == 8) {
          y <- a[j] + b[j] * log10(x)
          y <- 100^stats::rnorm(n.mod[j], y, se[j])
        } else if (eqn[j] == 9) {
          y <- log(a[j]) + b[j] * log(x)
          y <- exp(stats::rnorm(n.mod[j], y, se[j]))
        } else if (eqn[j] == 10) {
          y <- a[j] + b[j] * log(x)
          y <- exp(stats::rnorm(n.mod[j], y, se[j]))
        } else if (eqn[j] == 11) {
          y <- a[j] * x^(b[j])
          y <- stats::rnorm(n.mod[j], y, se[j])
        }
        y[y <= 0] <- NA
        y         <- y * Ycor[j]
        s2        <- which(!is.na(y))
        data[[nfield + which(ntally == j)]]$x <- x0[s2]  ## store the std units, not the transformed
        data[[nfield + which(ntally == j)]]$y <- y[s2]  ## store y transformed to std units
      }  ## end loop over tally entries
    }  ## end check for ntally > 0
    
    if (FALSE) {
      # diagnostics
      grDevices::pdf("DvBscatter.pdf")
      plot(1, 1, type = "n", log = "xy", xlim = c(0.1, 1000), ylim = c(1e-04, 1e+05))
      BETA <- matrix(NA, nsite, 2)
      for (i in seq_len(nsite)) {
        graphics::points(data[[i]]$x, data[[i]]$y, col = i)
        BETA[i, ] <- coef(lm(log10(data[[i]]$y) ~ log10(data[[i]]$x)))
      }
      hist(BETA[, 1], breaks = 20)
      hist(BETA[, 2], breaks = 20)
      plot(BETA)
      grDevices::dev.off()
    }
    
    if (nsite > 1)
    {
      # Hierarchical Bayes Fit Model
      
      tauImu <- tauI %*% mu
      u1     <- s1
      u2     <- s2
      for (j in seq_len(nsite)) {
        
        ## Update study-level regression parameters
        X       <- cbind(rep(1, length(data[[j]]$x)), log(data[[j]]$x))
        Y       <- log(data[[j]]$y)
        bigV    <- solve(sinv * t(X) %*% X + tauI)
        littlev <- sinv * t(X) %*% Y + tauImu
        beta    <- t(mvtnorm::rmvnorm(1, bigV %*% littlev, bigV))
        b0[j]   <- beta[1]
        b1[j]   <- beta[2]
        
        ## Update study-level error
        u1 <- u1 + nrow(X) / 2
        u2 <- u2 + as.numeric(0.5 * crossprod(Y - X %*% beta))
        
        ## Calculate Deviance
        D[j] <- -2 * sum(stats::dnorm(Y, X %*% beta, sqrt(sigma), log = TRUE))
      }
      sinv <- stats::rgamma(1, u1, u2)  ## precision
      sigma <- 1 / sinv  ## variance
      
      ## Update across-study means
      B <- cbind(b0, b1)
      bigV <- solve(nrow(B) * tauI + V0I)
      littlev <- V0Imu0
      for (i in seq_len(nrow(B))) {
        littlev <- littlev + tauI %*% B[i, ]
      }
      mu <- t(mvtnorm::rmvnorm(1, bigV %*% littlev, bigV))
      
      ## Update across-study variance
      u1   <- v + nrow(B)
      u2   <- S + crossprod(B - t(matrix(mu, nrow = 2, ncol = nrow(B))))
      tau  <- MCMCpack::riwish(u1, u2)
      tauI <- solve(tau)
      
      ## Store Parameter estimates
      b0GIBBS[g, ]  <- b0
      b1GIBBS[g, ]  <- b1
      muGIBBS[g, ]  <- mu
      sigGIBBS[g]   <- sigma
      tauGIBBS[g, ] <- MCMCpack::vech(tau)
      DGIBBS[g]     <- sum(D)
    }  ## end (if nsite > 1)
    
    ## Fit 'random species' hierarchical model -------------------------------------
    if (nspp > 1) {
      
    }
    
    ## Fit alternative non-heirarchical model
    X <- Y <- NULL
    for (i in seq_len(nsite)) {
      X <- c(X, data[[i]]$x)
      Y <- c(Y, data[[i]]$y)
    }
    Y       <- log(Y)
    X       <- cbind(rep(1, length(X)), log(X))
    bigV    <- solve(SgI * t(X) %*% X + V0I)
    littlev <- SgI * t(X) %*% Y + V0Imu0
    Bg      <- t(mvtnorm::rmvnorm(1, bigV %*% littlev, bigV))
    u1      <- s1 + nrow(X) / 2
    u2      <- s2 + as.numeric(0.5 * crossprod(Y - X %*% Bg))
    SgI     <- stats::rgamma(1, u1, u2)  ## precision
    Sg      <- 1 / SgI  ## variance
    Dg      <- -2 * sum(stats::dnorm(Y, X %*% Bg, sqrt(Sg), log = TRUE))
    
    BgGIBBS[g, ] <- Bg
    SgGIBBS[g]   <- Sg
    DgGIBBS[g]   <- Dg
    
    if(interactive()) {
      utils::setTxtProgressBar(pb, g)
    }
  }  ## END MCMC LOOP
  close(pb)
  
  if (nsite <= 1) {
    b0GIBBS[1:nrep, ]  <- 0
    b1GIBBS[1:nrep, ]  <- 0
    muGIBBS[1:nrep, ]  <- 0
    sigGIBBS[1:nrep]   <- 0
    tauGIBBS[1:nrep, ] <- 0
    DGIBBS[1:nrep]     <- 0
  }
  
  out <- cbind(b0GIBBS, b1GIBBS, muGIBBS, sigGIBBS, tauGIBBS, DGIBBS, BgGIBBS, SgGIBBS, DgGIBBS)
  colnames(out) <- c(paste("b0", 1:nsite, sep = "."),
                     paste("b1", 1:nsite, sep = "."),
                     "mu0", "mu1",
                     "sigma",
                     "tau11", "tau12", "tau22",
                     "D", "Bg0", "Bg1", "Sg", "Dg")
  return(list(mc = coda::as.mcmc(out), obs = data))
  
}  # allom.BayesFit
