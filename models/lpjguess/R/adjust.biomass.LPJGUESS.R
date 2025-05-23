
##' Adjust LPJ-GUESS individual's biomass
##' 
##' This function adjusts an LPJ-GUESS individual by calling the LPJ-GUESS allocation function (compiled C++)
##' with a given biomass change.  It updates the individual biomass pools directly, and also returns, in a list further 
##' adjustments to the litter pools.
##' 
##' @param individual A nested list which encapsulates an LPJ-GUESS 'Individual' as read from a binary state file
##' @param biomass.inc A numeric value specifying the biomass increment to be applied.
##' @param sla The SLA (specific leaf area) (per PFT parameter)
##' @param wooddens Wood density (kgC/m^2) (per PFT parameter)
##' @param lifeform An integer code for the lifeform of this individual (cohort): 1 = Tree, 2 = Grass
##' @param k_latosa The leaf area to sapwood area ratio (per PFT parameter)
##' @param k_allom2,k_allom3, Allometry coefficients (per PFT parameters)
##' @param trace Logical; if TRUE, prints details of biomass adjustment process.
##' 
##' The changes in C pools are determined by the allocation.  The changes in the N pools are designed to 
##' maintain the pre-exisiing C:N ratios, so N is just scaled using the updated C with the initial C:N ratio.
##' The N storage pools (nstore_longterm and nstore_labile) don't have pre-existing C:N ratios, so they are 
##' just scaled by the overall biomass change (the 'rel.change' argument to the function).
##' 
##' Note that after this function is called the function \code{allometry} should be used to update the individual
##' and to check that the newly updated individual has a 'valid' allometry. The litter pools should also be updated.
##' This is implemented in the \code{update.state.LPJGUESS} function following the call to this \code{adjust.biomass.LPJGUESS} function. 
##' 
##' 
##' @keywords internal
##' @return the scaled 'individual' (the initial nested list with update values)
##' @author Matthew Forrest
adjust.biomass.LPJGUESS <- function(individual, biomass.inc, sla, wooddens, lifeform, k_latosa, k_allom2, k_allom3, trace = TRUE){
  
  # dummy input values to the allocation function below
  # note that they are not actually updated by the function, the updated values are in the returned list
  cmass_leaf_inc <- 0
  cmass_root_inc <- 0
  cmass_sap_inc <- 0 
  cmass_debt_inc <- 0
  cmass_heart_inc <- 0
  litter_leaf_inc <- 0
  litter_root_inc <- 0
  exceeds_cmass <- 0
  
  # calculate the total biomass and the absolute change based on this
  biomass.total <- TotalCarbon(individual)
  rel.change <- (biomass.total + biomass.inc) / biomass.total
  if(trace) {
    print(paste(" ------- DURING BIOMASS ADJUSTMENT -------"))
    print(paste(" ***** Total Biomass Increment =", biomass.inc))
  }
  
  updated.pools <- allocation(
    # vegetation state
    bminc = as.numeric(biomass.inc/individual$densindiv),
    cmass_leaf = as.numeric(individual$cmass_leaf/individual$densindiv),              
    cmass_root = as.numeric(individual$cmass_root/individual$densindiv),
    cmass_sap = as.numeric(individual$cmass_sap/individual$densindiv),
    cmass_debt  = as.numeric(individual$cmass_debt/individual$densindiv),
    cmass_heart = as.numeric(individual$cmass_heart/individual$densindiv),
    ltor = as.numeric(individual$ltor),
    height = as.numeric(individual$height),
    # PFT parameters
    sla = as.numeric(sla),
    wooddens = as.numeric(wooddens),
    lifeform = as.integer(lifeform),
    k_latosa = as.numeric(k_latosa),
    k_allom2 = as.numeric(k_allom2),
    k_allom3 = as.numeric(k_allom3),
    # calculated increments (not actually used, the updated values returned in the updated.pools list)
    cmass_leaf_inc = as.numeric(cmass_leaf_inc),
    cmass_root_inc = as.numeric(cmass_root_inc),
    cmass_sap_inc = as.numeric(cmass_sap_inc),
    cmass_debt_inc = as.numeric(cmass_debt_inc),
    cmass_heart_inc = as.numeric(cmass_heart_inc),
    litter_leaf_inc = as.numeric(litter_leaf_inc),
    litter_root_inc = as.numeric(litter_root_inc),
    exceeds_cmass = as.numeric(exceeds_cmass))
  
  
  # adjust the various associated C (and N) pools based on the results of the allocation call
  
  # leaf
  original.cmass_leaf <- individual$cmass_leaf
  new.cmass_leaf <- individual$cmass_leaf + (unname(updated.pools[["cmass_leaf_inc"]] * individual$densindiv))
  leaf.scaling <- new.cmass_leaf / original.cmass_leaf
  individual$cmass_leaf <- new.cmass_leaf
  individual$nmass_leaf <- individual$nmass_leaf * leaf.scaling
  
  # root
  original.cmass_root <- individual$cmass_root
  new.cmass_root <- individual$cmass_root + (unname(updated.pools[["cmass_root_inc"]] * individual$densindiv))
  root.scaling <- new.cmass_root / original.cmass_root
  individual$cmass_root <- new.cmass_root
  individual$nmass_root <- individual$nmass_root * root.scaling
  
  
  # sap, heart and debt only for trees
  if(lifeform == 1) {
    
    # sap
    original.cmass_sap <- individual$cmass_sap
    new.cmass_sap <- individual$cmass_sap + (unname(updated.pools[["cmass_sap_inc"]] * individual$densindiv))
    sap.scaling <- new.cmass_sap / original.cmass_sap
    individual$cmass_sap <- new.cmass_sap
    individual$nmass_sap <- individual$nmass_sap * sap.scaling
    
    
    # heart
    original.cmass_heart <- individual$cmass_heart
    new.cmass_heart <- individual$cmass_heart + (unname(updated.pools[["cmass_heart_inc"]] * individual$densindiv))
    heart.scaling <- new.cmass_heart / original.cmass_heart
    individual$cmass_heart <- new.cmass_heart
    individual$nmass_heart <- individual$nmass_heart * heart.scaling
    
    # debt - note no equivalant N debt
    original.cmass_debt <- individual$cmass_debt
    new.cmass_debt <- individual$cmass_debt + (unname(updated.pools[["cmass_debt_inc"]] * individual$densindiv))
    individual$cmass_debt <- new.cmass_debt
    
  }
 
  
  # N labile and long term storage - note no equivalant C pools and they are not determined by allocation upgrade,
  # so simply scale by the overall biomass change
  individual$nstore_labile <- unname(individual$nstore_labile * rel.change)
  individual$nstore_longterm <- unname(individual$nstore_longterm * rel.change)
  
  
  # TODO (potentially): MF - for simulations involving managed forestry and harvest the variable 'cmass_wood_inc_5'
  # should also be updated. This is a vector, and I am not sure if the increment should go at the start or the end of it.
  # But also, how to deal simultaneously with harvesting and nudging will probably require some thought, and maybe
  # it is not necessary or appropriate to update this variable
  #print(individual$cmass_wood_inc_5)
  
  return(list(individual = individual,
              litter_leaf_inc = updated.pools[["litter_leaf_inc"]],
              litter_root_inc = updated.pools[["litter_root_inc"]],
              exceeds_cmass = updated.pools[["exceeds_cmass"]]
  ))
  
}
