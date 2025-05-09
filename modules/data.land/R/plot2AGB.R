#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##' convert composite ring & census data into AGB
##' 
##' @name  plot2AGB
##' @title plot2AGB
##' 
##' @param combined   data frame merging plot inventory and tree ring data
##' @param out        MCMC samples for diameter (sample x tree)
##' @param outfolder  output folder for graphs & data
##' @param allom.stats Allometry statistics computed by `AllomAve`
##' @param unit.conv  area conversion from sum(kg/tree) to kg/area
##' 
##' @author Mike Dietze \email{dietze@@bu.edu}
##' @export
plot2AGB <- function(combined, out, outfolder, allom.stats, unit.conv = 0.02) {
  
  ## Jenkins: hemlock (kg) b0 <- -2.5384 b1 <- 2.4814
  
  ## Allometric statistics
  b0   <- allom.stats[[1]][[6]]$statistics["Bg0", "Mean"]
  b1   <- allom.stats[[1]][[6]]$statistics["Bg1", "Mean"]
  B    <- allom.stats[[1]][[6]]$statistics[c("Bg0", "Bg1"), "Mean"]
  Bcov <- allom.stats[[1]][[6]]$cov[c("Bg0", "Bg1"), c("Bg0", "Bg1")]
  Bsd  <- sqrt(allom.stats[[1]][[6]]$statistics["Sg", "Mean"])
  
  ## prep data
  out[out < 0.1] <- 0.1
  nrep    <- nrow(out)
  ntree   <- nrow(combined)
  nt      <- ncol(out) / ntree
  mplot   <- 1  ## later need to generalize to splitting up plots
  ijindex <- matrix(1, ntree, 1)
  yrvec   <- as.numeric(colnames(combined))
  yrvec   <- yrvec[!is.na(yrvec)]
  
  ## set up storage
  NPP <- array(NA, c(mplot, nrep, nt - 1))
  AGB <- array(NA, c(mplot, nrep, nt))
  biomass_tsca  <- array(NA, c(mplot, nrep, nt))
  biomass_acsa3 <- array(NA, c(mplot, nrep, nt))
  biomass_beal2 <- array(NA, c(mplot, nrep, nt))
  biomass_thoc2 <- array(NA, c(mplot, nrep, nt))
  
  ## sample over tree chronologies
  pb <- utils::txtProgressBar(min = 0, max = nrep, style = 3)
  for (g in seq_len(nrep)) {
    
    ## Draw allometries
    b <- mvtnorm::rmvnorm(1, B, Bcov)
    
    ## convert tree diameter to biomass
    biomass <- matrix(exp(b[1] + b[2] * log(out[g, ])), ntree, nt)
    
    for (j in seq_len(mplot)) {
      
      ## aggregate to stand AGB
      AGB[j, g, ] <- apply(biomass, 2, sum, na.rm = TRUE) * unit.conv
      # AGB[j,g,] <- apply(biomass[ijindex[,1]==j,],2,sum,na.rm=TRUE)*unit.conv
      
      biomass_tsca[j, g, ] <- apply(biomass[combined$SPP == "TSCA", ], 2, sum, na.rm = TRUE) * unit.conv
      biomass_acsa3[j, g, ] <- apply(biomass[combined$SPP == "ACSA3", ], 2, sum, na.rm = TRUE) * unit.conv
      biomass_beal2[j, g, ] <- apply(biomass[combined$SPP == "BEAL2", ], 2, sum, na.rm = TRUE) * unit.conv
      biomass_thoc2[j, g, ] <- apply(biomass[combined$SPP == "THOC2", ], 2, sum, na.rm = TRUE) * unit.conv
      
      ## diff to get NPP
      NPP[j, g, ] <- diff(AGB[j, g, ])
    }
    utils::setTxtProgressBar(pb, g)
  }
  
  mAGB <- sAGB <- matrix(NA, mplot, nt)
  mNPP <- sNPP <- matrix(NA, mplot, nt - 1)
  
  mbiomass_tsca  <- sbiomass_tsca <- matrix(NA, mplot, nt)
  mbiomass_acsa3 <- sbiomass_acsa3 <- matrix(NA, mplot, nt)
  mbiomass_beal2 <- sbiomass_beal2 <- matrix(NA, mplot, nt)
  mbiomass_thoc2 <- sbiomass_thoc2 <- matrix(NA, mplot, nt)
  
  for (i in seq_len(mplot)) {
    mNPP[i, ] <- apply(NPP[i, , ], 2, mean, na.rm = TRUE)
    sNPP[i, ] <- apply(NPP[i, , ], 2, stats::sd, na.rm = TRUE)
    mAGB[i, ] <- apply(AGB[i, , ], 2, mean, na.rm = TRUE)
    sAGB[i, ] <- apply(AGB[i, , ], 2, stats::sd, na.rm = TRUE)
    
    mbiomass_tsca[i, ]  <- apply(biomass_tsca[i, , ], 2, mean, na.rm = TRUE)
    sbiomass_tsca[i, ]  <- apply(biomass_tsca[i, , ], 2, stats::sd, na.rm = TRUE)
    mbiomass_acsa3[i, ] <- apply(biomass_acsa3[i, , ], 2, mean, na.rm = TRUE)
    sbiomass_acsa3[i, ] <- apply(biomass_acsa3[i, , ], 2, stats::sd, na.rm = TRUE)
    mbiomass_beal2[i, ] <- apply(biomass_beal2[i, , ], 2, mean, na.rm = TRUE)
    sbiomass_beal2[i, ] <- apply(biomass_beal2[i, , ], 2, stats::sd, na.rm = TRUE)
    mbiomass_thoc2[i, ] <- apply(biomass_thoc2[i, , ], 2, mean, na.rm = TRUE)
    sbiomass_thoc2[i, ] <- apply(biomass_thoc2[i, , ], 2, stats::sd, na.rm = TRUE)
  }
  
  grDevices::pdf(file.path(outfolder, "plot2AGB.pdf"))
  graphics::par(mfrow = c(2, 1))
  for (i in seq_len(mplot)) {
    up  <- mNPP[i, ] + sNPP[i, ] * 1.96
    low <- mNPP[i, ] - sNPP[i, ] * 1.96
    graphics::plot(yrvec[-1], mNPP[i, ], ylim = range(c(up, low)), ylab = "Mg/ha/yr", xlab = "year")
    graphics::lines(yrvec[-1], up)
    graphics::lines(yrvec[-1], low)
    upA  <- mAGB[i, ] + sAGB[i, ] * 1.96
    lowA <- mAGB[i, ] - sAGB[i, ] * 1.96
    graphics::plot(yrvec, mAGB[i, ], ylim = range(c(upA, lowA)), ylab = "Mg/ha", xlab = "year")
    graphics::lines(yrvec, upA)
    graphics::lines(yrvec, lowA)
  }
  grDevices::dev.off()
  
  save(AGB, NPP, mNPP, sNPP, mAGB, sAGB, yrvec, 
       mbiomass_tsca, sbiomass_tsca, mbiomass_acsa3, sbiomass_acsa3, 
       mbiomass_beal2, sbiomass_beal2, mbiomass_thoc2, sbiomass_thoc2, 
       file = file.path(outfolder, "plot2AGB.Rdata"))
  return(list(AGB = AGB, NPP = NPP, 
              biomass_tsca = biomass_tsca, biomass_acsa3 = biomass_acsa3, 
              biomass_beal2 = biomass_beal2, biomass_thoc2 = biomass_thoc2))
} # plot2AGB
