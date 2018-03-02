##############################
# Binary Outcome function v.0.1 
###############################
# calculates P_t (matrix) 
# allow for 4 treatments
# allow MCD
# only EVPI
# does not (and cannot) take account of NHS costs of research - need a different model for this



# Note*** test for this bug!
# good chance there is a bug in $Probability_t_is_max when the primary outcome is a harm


########################
# supporting functions 
#########################
# functions required for this to work

# found in SupplementaryFunctions.R
# required to simulate probabilities of outcome
# must be loaded in this order
# simProbOfOutcomeNormBinary
# simProbOfOutcomeHalfNormBinary
# simProbOfOutcomeMatrixBinary
# basic population function - calculates the population numbers required in the model
# verybasicPop


# functions not used here (code exists in NETSCC codes: GenericBinaryOutcome.R)
##
# allow for utilisation based on stat significance
# UtilMatrix.fn 
# allow implementation to depend on an observed MCD
# UtilMatrixMCD.fn 
# Implementation adjusted EVTPI 
# EVTPIadjImp.fn 


# mock data
#MCsims <- 10
#numberOfTreatments <- 2
#P_t1 <- 0.9
#mu_t2 <- 0
#variance_t2 <- 0.1
#dist_t2 <- "norm" 
#direction_t2 <- "alwaysPositive"

#mu_t3 <- 100
#variance_t3 <- 0.001
#dist_t3 <- "norm" 
#direction_t3 <- "alwaysNegative"
#mu_t4 <- 1
#variance_t4 <- 100
#dist_t4 <- "halfNorm" 
#direction_t4 <- "alwaysNegative" 

#nameOf_t1 <- "late PTP"
#nameOf_t2 <- "early PTP"
#nameOf_t3 <- "treatment 3"
#nameOf_t4 <- "treatment 4"

#typeOfOutcome <- "benefit" # "harm" "netHealth" # was Benefit==1 or 0 for benefit or harm
#incidence = 8000 # was Incidence
#timeInformation  = 15 # Time_info  = 15
#discountRate = 3.5  #D_rate = 0.035 ***NB need to divide by 100
#costResearchFunder = 882177 #Cost_research_funder =  882177
#durationOfResearch = 3  # Time_research = 3

#MCD_t2 = 0   # MCD_t <- c(0) # remember MCD is a relative input - if two treatmetns => just one MCD
#MCD_t3 = 0
#MCD_t4 = NA

#utilisation_t1 = 0.5 # check these sum to 1. 
#utilisation_t2 = 0.5
#utilisation_t3 = 0
#utilisation_t4 = NA

####################################


# numberOfTreatments =2
# MCsims = 100
# P_t1 =0.5
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
# currencySymbol = "£"

BinaryOutcomeFunction.v.0.1 <- function(numberOfTreatments, MCsims, P_t1,
                                        mu_t2, variance_t2, dist_t2, direction_t2,
                                        mu_t3, variance_t3, dist_t3, direction_t3,
                                        mu_t4, variance_t4, dist_t4, direction_t4,
                                        nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                                        typeOfOutcome, incidence,timeInformation,
                                        discountRate ,durationOfResearch,costResearchFunder,
                                        MCD_t2, MCD_t3, MCD_t4,
                                        utilisation_t1, utilisation_t2,
                                        utilisation_t3, utilisation_t4,
                                        currencySymbol
                                        ){
  
  # simulate probabilities of event
  #########################
  set.seed(5)
  # simulate probabilities of event with baseline treatment
  P_t1 <- rep(P_t1, MCsims)
  
  # simulate probabilities of the event for other treatments
  P_t <- simProbOfOutcomeMatrixBinary(numberOfTreatments, P_t1,
                               mu_t2, variance_t2, dist_t2, direction_t2,
                               mu_t3, variance_t3, dist_t3, direction_t3,
                               mu_t4, variance_t4, dist_t4, direction_t4)
  
  # create Binary economic model from probability of event
  #########################
  
  INB_Event <- ifelse(typeOfOutcome== "benefit", 1, -1)
  
  NB_t  <- P_t*INB_Event # multiply every element by INB_Event (1st step in converting to NB)
  
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
                                  costHealthSystem = NA, k = NA, currencySymbol)
  
  # return the list of results
  ###############################
  
  return(VOIoutputs)
  
}

BinaryOutcomeFunction <- BinaryOutcomeFunction.v.0.1

# test function
# resultsholder <- BinaryOutcomeFunction(numberOfTreatments =2 , MCsims = 100000, P_t1 =0.1,
#                                        mu_t2=0, variance_t2=1, dist_t2="norm", direction_t2= NA,
#                                        mu_t3=NA, variance_t3=NA, dist_t3=NA, direction_t3=NA,
#                                        mu_t4=NA, variance_t4=NA, dist_t4=NA, direction_t4=NA,
#                                        nameOf_t1="1",nameOf_t2="2", nameOf_t3=NA, nameOf_t4=NA,
#                                        typeOfOutcome="benefit", incidence=1000,timeInformation=15,
#                                        discountRate=3.5 ,durationOfResearch= 4,costResearchFunder=1000000,
#                                        MCD_t2=0, MCD_t3=NA, MCD_t4=NA,
#                                        utilisation_t1=100, utilisation_t2=0,
#                                        utilisation_t3=NA, utilisation_t4=NA,
#                                        currencySymbol="£")
# resultsholder







############## FEASIBILITY BINARY OUTCOME FUNCTION

# numberOfTreatments =2
# MCsims = 100
# P_t1 =0.5
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
# MCD_t2=0
# MCD_t3=NA
# MCD_t4=NA
# utilisation_t1=100
# utilisation_t2=0
# utilisation_t3=NA
# utilisation_t4=NA
# costResearchFunderFeas = 100000
# costResearchFunderDefinitive = 882177 #Cost_research_funder =  882177
# durationOfResearchDefinitive = 3 #durationOfResearch = 3  # Time_research = 3
# durationOfResearchFeas = 1
# probabilityOfDefinitiveResearch = 0.5
# currencySymbol = "£"

BinaryOutcomeFunctionFeas.v.0.1 <- function(numberOfTreatments, MCsims, P_t1,
                                        mu_t2, variance_t2, dist_t2, direction_t2,
                                        mu_t3, variance_t3, dist_t3, direction_t3,
                                        mu_t4, variance_t4, dist_t4, direction_t4,
                                        nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                                        typeOfOutcome, incidence,timeInformation,
                                        discountRate,
                                        MCD_t2, MCD_t3, MCD_t4,
                                        utilisation_t1, utilisation_t2,
                                        utilisation_t3, utilisation_t4,
                                        durationOfResearchDefinitive, durationOfResearchFeas,
                                        costResearchFunderFeas,costResearchFunderDefinitive,
                                        probabilityOfDefinitiveResearch, currencySymbol
){
  
  # simulate probabilities of event
  #########################
  
  # simulate probabilities of event with baseline treatment
  P_t1 <- rep(P_t1, MCsims)
  
  # simulate probabilities of the event for other treatments
  P_t <- simProbOfOutcomeMatrixBinary(numberOfTreatments, P_t1,
                                      mu_t2, variance_t2, dist_t2, direction_t2,
                                      mu_t3, variance_t3, dist_t3, direction_t3,
                                      mu_t4, variance_t4, dist_t4, direction_t4)
  
  # create Binary economic model from probability of event
  #########################
  
  INB_Event <- ifelse(typeOfOutcome== "benefit", 1, -1)
  
  NB_t  <- P_t*INB_Event # multiply every element by INB_Event (1st step in converting to NB)
  
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

BinaryOutcomeFunctionFeas <- BinaryOutcomeFunctionFeas.v.0.1



# test function 
# resultsholder <- BinaryOutcomeFunctionFeas(numberOfTreatments =2 , MCsims = 1000, P_t1 =0.5,
#                                        mu_t2=0, variance_t2=1, dist_t2="norm", direction_t2= NA,
#                                        mu_t3=NA, variance_t3=NA, dist_t3=NA, direction_t3=NA,
#                                        mu_t4=NA, variance_t4=NA, dist_t4=NA, direction_t4=NA,
#                                        nameOf_t1="1",nameOf_t2="2", nameOf_t3=NA, nameOf_t4=NA,
#                                        typeOfOutcome="benefit", incidence=1000,timeInformation=15,
#                                        discountRate=3.5,
#                                        MCD_t2=0, MCD_t3=NA, MCD_t4=NA,
#                                        utilisation_t1=100, utilisation_t2=0,
#                                        utilisation_t3=NA, utilisation_t4=NA,
#                                        durationOfResearchDefinitive = 6, durationOfResearchFeas = 2,
#                                        costResearchFunderFeas = 100000,costResearchFunderDefinitive= 2000000,
#                                        probabilityOfDefinitiveResearch = 0.5,
#                                        currencySymbol="£")

