##############################
# Survival Outcome function v.0.1 
###############################
# statistical theory found in Survial SupplementaryFunctions.R

########################
# supporting functions 
#########################
# functions required for this to work

# found in SupplementaryFunctions.R


# test data
# numberOfTreatments =2
# MCsims = 100
# survivalDist <- "exponential" # other option = "weibull"
# scaleParameter_t1 = 20
# shapeParameter_t1 = 1
# mu_t2=0
# variance_t2=1
# dist_t2="norm"
# direction_t2= NA
# mu_t3=NA
# variance_t3=NA
# dist_t3=NA
# direction_t3=NA
# mu_t4=NA
# variance_t4=NA
# dist_t4=NA
# direction_t4=NA
# nameOf_t1="1"
# nameOf_t2="2"
# nameOf_t3=NA
# nameOf_t4=NA
# typeOfOutcome="benefit"
# incidence=1000
# timeInformation=15
# discountRate=3.5
# durationOfResearch= 4
# costResearchFunder=1000000
# MCD_t2=0
# MCD_t3=NA
# MCD_t4=NA
# utilisation_t1=100
# utilisation_t2=0
# utilisation_t3=NA
# utilisation_t4=NA



SurvivalOutcomeFunction.v.0.1 <- function(numberOfTreatments, MCsims, 
                                            survivalDist,scaleParameter_t1,shapeParameter_t1,
                                            mu_t2, variance_t2, dist_t2, direction_t2,
                                            mu_t3, variance_t3, dist_t3, direction_t3,
                                            mu_t4, variance_t4, dist_t4, direction_t4,
                                            nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                                            typeOfOutcome, incidence,timeInformation,
                                            discountRate ,durationOfResearch,costResearchFunder,
                                            MCD_t2, MCD_t3, MCD_t4,
                                            utilisation_t1, utilisation_t2,
                                            utilisation_t3, utilisation_t4, currencySymbol){
  

  set.seed(5)
  # no uncertainty in baseline for now
  ExpectedSurvival_t1 <- if(survivalDist == "exponential"){
    rep(scaleParameter_t1, MCsims)
  } else { # if weibull
    rep(scaleParameter_t1^(1/shapeParameter_t1)*gamma(1/shapeParameter_t1 + 1), MCsims)
  }
    
  
  # Based on baseline outcome above simulate expected survival each treatment
  #########################
  
  ExpectedSurvival_t <- simDurationMatrixSurvival(numberOfTreatments, ExpectedSurvival_t1,survivalDist,
                                                  scaleParameter_t1, shapeParameter_t1, MCsims,
                                               mu_t2, variance_t2, dist_t2, direction_t2,
                                               mu_t3, variance_t3, dist_t3, direction_t3,
                                               mu_t4, variance_t4, dist_t4, direction_t4)
  
  # create economic model from deltas
  #########################
  
  INB_Event <- ifelse(typeOfOutcome== "benefit", 1, -1)
  
  NB_t  <- ExpectedSurvival_t*INB_Event # multiply every element by INB_Event (1st step in converting to NB)
  
  addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the vector to convert to net benefit
  NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
  
  # each column now represents simulations of the NB of each treatment
  
  # Calculate outputs from NB matrix
  ######################################
  
  VOIoutputs <- NBtoEVPIResults(NB_t,
                                nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                                typeOfOutcome, incidence,timeInformation,
                                discountRate ,durationOfResearch,costResearchFunder,
                                MCD_t2, MCD_t3, MCD_t4,
                                utilisation_t1, utilisation_t2,
                                utilisation_t3, utilisation_t4,
                                costHealthSystem = NA, k = NA, currencySymbol = "£")
  
  # return the list of results
  ###############################
  
  return(VOIoutputs)
  
}

# assign this model to the generic name of the function
SurvivalOutcomeFunction <- SurvivalOutcomeFunction.v.0.1

# test function
# resultsholder <- SurvivalOutcomeFunction(numberOfTreatments =2 , MCsims = 1000,
#                                                  survivalDist = "exponential",
#                                                  scaleParameter_t1 = 10,shapeParameter_t1 = 1,
#                                                  mu_t2=0, variance_t2=1, dist_t2="norm", direction_t2= NA,
#                                                  mu_t3=NA, variance_t3=NA, dist_t3=NA, direction_t3=NA,
#                                                  mu_t4=NA, variance_t4=NA, dist_t4=NA, direction_t4=NA,
#                                                  nameOf_t1="1",nameOf_t2="2", nameOf_t3=NA, nameOf_t4=NA,
#                                                  typeOfOutcome="benefit", incidence=1000,timeInformation=15,
#                                                  discountRate=3.5 ,durationOfResearch= 4,costResearchFunder=1000000,
#                                                  MCD_t2=0, MCD_t3=NA, MCD_t4=NA,
#                                                  utilisation_t1=100, utilisation_t2=0,
#                                                  utilisation_t3=NA, utilisation_t4=NA, currencySymbol = "£")








SurvivalOutcomeFunctionFeas.v.0.1 <- function(numberOfTreatments, MCsims, 
                                          survivalDist,scaleParameter_t1,shapeParameter_t1,
                                          mu_t2, variance_t2, dist_t2, direction_t2,
                                          mu_t3, variance_t3, dist_t3, direction_t3,
                                          mu_t4, variance_t4, dist_t4, direction_t4,
                                          nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                                          typeOfOutcome, incidence,timeInformation,
                                          discountRate ,
                                          MCD_t2, MCD_t3, MCD_t4,
                                          utilisation_t1, utilisation_t2,
                                          utilisation_t3, utilisation_t4, 
                                          durationOfResearchDefinitive, durationOfResearchFeas,
                                          costResearchFunderFeas,costResearchFunderDefinitive,
                                          probabilityOfDefinitiveResearch, currencySymbol){
  
  
  # no uncertainty in baseline for now
  ExpectedSurvival_t1 <- if(survivalDist == "exponential"){
    rep(scaleParameter_t1, MCsims)
  } else { # if weibull
    rep(scaleParameter_t1^(1/shapeParameter_t1)*gamma(1/shapeParameter_t1 + 1), MCsims)
  }
  
  
  # Based on baseline outcome above simulate expected survival each treatment
  #########################
  
  ExpectedSurvival_t <- simDurationMatrixSurvival(numberOfTreatments, ExpectedSurvival_t1,survivalDist,
                                                  scaleParameter_t1, shapeParameter_t1, MCsims,
                                                  mu_t2, variance_t2, dist_t2, direction_t2,
                                                  mu_t3, variance_t3, dist_t3, direction_t3,
                                                  mu_t4, variance_t4, dist_t4, direction_t4)
  
  # create economic model from deltas
  #########################
  
  INB_Event <- ifelse(typeOfOutcome== "benefit", 1, -1)
  
  NB_t  <- ExpectedSurvival_t*INB_Event # multiply every element by INB_Event (1st step in converting to NB)
  
  addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the vector to convert to net benefit
  NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
  
  # each column now represents simulations of the NB of each treatment
  
  # Calculate outputs from NB matrix
  ######################################
  
  VOIoutputs <- NBtoEVPIResultsFeas(NB_t,
                                    nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                                    typeOfOutcome, incidence,timeInformation,
                                    discountRate ,durationOfResearchDefinitive,
                                    durationOfResearchFeas,costResearchFunderFeas,
                                    costResearchFunderDefinitive,
                                    MCD_t2, MCD_t3, MCD_t4,
                                    utilisation_t1, utilisation_t2,
                                    utilisation_t3, utilisation_t4,
                                    probabilityOfDefinitiveResearch,
                                    costHealthSystemFeas = NA,costHealthSystemDefinitive =NA, k = NA,
                                    currencySymbol)
  
  # return the list of results
  ###############################
  
  return(VOIoutputs)
  
}

# assign this model to the generic name of the function
SurvivalOutcomeFunctionFeas <- SurvivalOutcomeFunctionFeas.v.0.1

# test function
# resultsholder <- SurvivalOutcomeFunctionFeas(numberOfTreatments =2 , MCsims = 1000,
#                                                  survivalDist = "exponential",
#                                                  scaleParameter_t1 = 10,shapeParameter_t1 = 1,
#                                                  mu_t2=0, variance_t2=1, dist_t2="norm", direction_t2= NA,
#                                                  mu_t3=NA, variance_t3=NA, dist_t3=NA, direction_t3=NA,
#                                                  mu_t4=NA, variance_t4=NA, dist_t4=NA, direction_t4=NA,
#                                                  nameOf_t1="1",nameOf_t2="2", nameOf_t3=NA, nameOf_t4=NA,
#                                                  typeOfOutcome="benefit", incidence=1000,timeInformation=15,
#                                                  discountRate=3.5,
#                                                  MCD_t2=0, MCD_t3=NA, MCD_t4=NA,
#                                                  utilisation_t1=100, utilisation_t2=0,
#                                                  utilisation_t3=NA, utilisation_t4=NA, 
#                                                  durationOfResearchDefinitive = 6, durationOfResearchFeas = 2,
#                                                  costResearchFunderFeas = 100000,costResearchFunderDefinitive= 2000000,
#                                                  probabilityOfDefinitiveResearch = 0.5,
#                                                  currencySymbol="£")









