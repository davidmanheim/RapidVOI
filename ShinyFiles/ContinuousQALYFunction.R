##############################
# Continuous QALY function v.0.1 
###############################

#ContinuousQALYFunction.R
########################
# supporting functions 
#########################
# functions required for this to work

# found in SupplementaryFunctions.R


# required to simulate probabilities of outcome
# must be loaded in this order (actually i am not sure if order matters!)
#simDeltaOfOutcomeMatrixContinuous
#simDeltaOfOutcomeNormContinuous
#simDeltaOfOutcomeHalfNormContinuous
# basic population function - calculates the population numbers required in the model
# verybasicPop


# test data
# numberOfTreatments =2
# MCsims = 100
# INBContinEvent = 0.05
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
# costHealthSystem = 1000000
# k = 13000
# cost_t1 = 30
# cost_t2 = 2000
# cost_t3 = NA
# cost_t4 = NA

ContinuousQALYFunction.v.0.1 <- function(numberOfTreatments, MCsims, INBContinEvent,
                                            mu_t2, variance_t2, dist_t2, direction_t2,
                                            mu_t3, variance_t3, dist_t3, direction_t3,
                                            mu_t4, variance_t4, dist_t4, direction_t4,
                                            nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                                            cost_t1, cost_t2, cost_t3, cost_t4,
                                            typeOfOutcome, incidence,timeInformation,
                                            discountRate ,durationOfResearch,costResearchFunder,
                                            MCD_t2, MCD_t3, MCD_t4,
                                            utilisation_t1, utilisation_t2,
                                            utilisation_t3, utilisation_t4,
                                            costHealthSystem, k, currencySymbol){
  
  # simulate plausible changes in continuous outcome for each treatment relative to t1 (aka deltas)
  #########################
  set.seed(5)
  Delta_t <- simDeltaOfOutcomeMatrixContinuous(numberOfTreatments, MCsims,
                                               mu_t2, variance_t2, dist_t2, direction_t2,
                                               mu_t3, variance_t3, dist_t3, direction_t3,
                                               mu_t4, variance_t4, dist_t4, direction_t4)
  
  # create economic model from deltas
  #########################
  
  NB_t  <- Delta_t*INBContinEvent # multiply every element by net benefit (1st step in converting to NB)
  
  addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the vector to convert to net benefit
  NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
  
  addCost_t <- c(-cost_t1/k ,-cost_t2/k, -cost_t3/k, -cost_t4/k) # subtract the health effects of costs
  NB_t  <- NB_t  + rep(addCost_t, each = MCsims)
  
  # each column now represents simulations of the NB of each treatment
  
  # generate and format costs table (assuming treatment costs do not depend on continuous event)
  cost_t <- c(cost_t1, cost_t2, cost_t3, cost_t4)
  Cost_per_individual <- paste0(currencySymbol,formatC(cost_t, big.mark = ',', format = 'd'))
  Yearly_costs <- paste0(currencySymbol,formatC(cost_t*incidence, big.mark = ',', format = 'd'))
  Additional_cost_per_year <- paste0(currencySymbol,formatC((cost_t - cost_t1)*incidence, big.mark = ',', format = 'd'))
  popTotal <- (incidence/-discountRate) * (exp(-discountRate*timeInformation) - exp(-discountRate*0))
  Total_Costs <- paste0(currencySymbol,formatC(cost_t*popTotal, big.mark = ',', format = 'd'))
  Treatment_name <- c(nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4)
  tableTreatmentCostsDF <- as.data.frame(cbind(Treatment_name, Cost_per_individual, Yearly_costs, Additional_cost_per_year, Total_Costs))
  tableTreatmentCostsDF <- tableTreatmentCostsDF[1:numberOfTreatments,]
  
  
  # Calculate outputs from NB matrix
  ######################################
  
  VOIoutputs <- NBtoEVPIResults(NB_t,
                                nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                                typeOfOutcome, incidence,timeInformation,
                                discountRate ,durationOfResearch,costResearchFunder,
                                MCD_t2, MCD_t3, MCD_t4,
                                utilisation_t1, utilisation_t2,
                                utilisation_t3, utilisation_t4,
                                costHealthSystem, k, currencySymbol)
  VOIoutputs$tableTreatmentCostsDF <- tableTreatmentCostsDF # add the expected cost table to the input list
  
  # return the list of results
  ###############################
  
  return(VOIoutputs)
  
}


ContinuousQALYFunction <- ContinuousQALYFunction.v.0.1
# test function
# resultsholder <- ContinuousQALYFunction(numberOfTreatments =2 , MCsims = 1000, INBContinEvent = 0.1,
#                                                  mu_t2=0, variance_t2=1, dist_t2="norm", direction_t2= NA,
#                                                  mu_t3=NA, variance_t3=NA, dist_t3=NA, direction_t3=NA,
#                                                  mu_t4=NA, variance_t4=NA, dist_t4=NA, direction_t4=NA,
#                                                  nameOf_t1="1",nameOf_t2="2", nameOf_t3=NA, nameOf_t4=NA,
#                                                  cost_t1= 1000, cost_t2= 100, cost_t3= NA, cost_t4=0,
#                                                  typeOfOutcome="benefit", incidence=1000,timeInformation=15,
#                                                  discountRate=3.5 ,durationOfResearch= 4,costResearchFunder=1000000,
#                                                  MCD_t2=0, MCD_t3=NA, MCD_t4=NA,
#                                                  utilisation_t1=100, utilisation_t2=0,
#                                                  utilisation_t3=NA, utilisation_t4=NA,
#                                                  costHealthSystem = 1000000, k = 13000, currencySymbol ="£")





##################### CONTINUOUS FEASIBILITY QALY function

# test data
# numberOfTreatments =2
# MCsims = 100
# INBContinEvent = 0.05
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
# costHealthSystem = 1000000
# k = 13000
# cost_t1 = 30
# cost_t2 = 2000
# cost_t3 = NA
# cost_t4 = NA

ContinuousQALYFunctionFeas.v.0.1 <- function(numberOfTreatments, MCsims, INBContinEvent,
                                         mu_t2, variance_t2, dist_t2, direction_t2,
                                         mu_t3, variance_t3, dist_t3, direction_t3,
                                         mu_t4, variance_t4, dist_t4, direction_t4,
                                         nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                                         cost_t1, cost_t2, cost_t3, cost_t4,
                                         typeOfOutcome, incidence,timeInformation,
                                         discountRate ,
                                         MCD_t2, MCD_t3, MCD_t4,
                                         utilisation_t1, utilisation_t2,
                                         utilisation_t3, utilisation_t4,
                                         costHealthSystem, k, currencySymbol,
                                         probabilityOfDefinitiveResearch,durationOfResearchDefinitive,
                                         durationOfResearchFeas,costResearchFunderFeas,
                                         costResearchFunderDefinitive,
                                         costHealthSystemFeas,costHealthSystemDefinitive){
  
  # simulate plausible changes in continuous outcome for each treatment relative to t1 (aka deltas)
  #########################
  
  Delta_t <- simDeltaOfOutcomeMatrixContinuous(numberOfTreatments, MCsims,
                                               mu_t2, variance_t2, dist_t2, direction_t2,
                                               mu_t3, variance_t3, dist_t3, direction_t3,
                                               mu_t4, variance_t4, dist_t4, direction_t4)
  
  # create economic model from deltas
  #########################
  
  NB_t  <- Delta_t*INBContinEvent # multiply every element by net benefit (1st step in converting to NB)
  
  addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the vector to convert to net benefit
  NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
  
  addCost_t <- c(-cost_t1/k ,-cost_t2/k, -cost_t3/k, -cost_t4/k) # subtract the health effects of costs
  NB_t  <- NB_t  + rep(addCost_t, each = MCsims)
  
  # each column now represents simulations of the NB of each treatment
  
  # generate and format costs table (assuming treatment costs do not depend on continuous event)
  cost_t <- c(cost_t1, cost_t2, cost_t3, cost_t4)
  Cost_per_individual <- paste0(currencySymbol,formatC(cost_t, big.mark = ',', format = 'd'))
  Yearly_costs <- paste0(currencySymbol,formatC(cost_t*incidence, big.mark = ',', format = 'd'))
  Additional_cost_per_year <- paste0(currencySymbol,formatC((cost_t - cost_t1)*incidence, big.mark = ',', format = 'd'))
  popTotal <- (incidence/-discountRate) * (exp(-discountRate*timeInformation) - exp(-discountRate*0))
  Total_Costs <- paste0(currencySymbol,formatC(cost_t*popTotal, big.mark = ',', format = 'd'))
  Treatment_name <- c(nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4)
  tableTreatmentCostsDF <- as.data.frame(cbind(Treatment_name, Cost_per_individual, Yearly_costs, Additional_cost_per_year, Total_Costs))
  tableTreatmentCostsDF <- tableTreatmentCostsDF[1:numberOfTreatments,]
  
  
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
                                    costHealthSystemFeas,costHealthSystemDefinitive, k,
                                    currencySymbol)
  VOIoutputs$tableTreatmentCostsDF <- tableTreatmentCostsDF # add the expected cost table to the input list
  
  # return the list of results
  ###############################
  
  return(VOIoutputs)
  
}


ContinuousQALYFunctionFeas <- ContinuousQALYFunctionFeas.v.0.1
# test function
# resultsholder <- ContinuousQALYFunctionFeas(numberOfTreatments =2 , MCsims = 1000, INBContinEvent = 0.1,
#                                                  mu_t2=0, variance_t2=1, dist_t2="norm", direction_t2= NA,
#                                                  mu_t3=NA, variance_t3=NA, dist_t3=NA, direction_t3=NA,
#                                                  mu_t4=NA, variance_t4=NA, dist_t4=NA, direction_t4=NA,
#                                                  nameOf_t1="1",nameOf_t2="2", nameOf_t3=NA, nameOf_t4=NA,
#                                                  cost_t1= 1000, cost_t2= 100, cost_t3= NA, cost_t4=0,
#                                                  typeOfOutcome="benefit", incidence=1000,timeInformation=15,
#                                                  discountRate=3.5 ,
#                                                  MCD_t2=0, MCD_t3=NA, MCD_t4=NA,
#                                                  utilisation_t1=100, utilisation_t2=0,
#                                                  utilisation_t3=NA, utilisation_t4=NA,
#                                                  costHealthSystem = 1000000, k = 13000, currencySymbol ="£",
#                                             probabilityOfDefinitiveResearch,durationOfResearchDefinitive = 0.5,
#                                             durationOfResearchFeas = 2,costResearchFunderFeas = 300000,
#                                             costResearchFunderDefinitive = 2000000,
#                                             costHealthSystemFeas = 100000,costHealthSystemDefinitive = 1500000)

