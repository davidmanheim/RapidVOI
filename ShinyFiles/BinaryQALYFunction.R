##############################
# Binary QALY function v.0.1 
###############################

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

#typeOfOutcome <- "netHealth" # "harm" "netHealth" # was Benefit==1 or 0 for benefit or harm
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

# test data
# numberOfTreatments =2
# MCsims = 100
# P_t1 =0.5
# INBBinaryEvent = 2
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
# cost_t1 = 100
# cost_t2 = 2000
# cost_t3 = NA
# cost_t4 = NA
# costEvent_t1 = 300
# costEvent_t2 = 200
# costEvent_t3 = NA
# costEvent_t4 = NA
# costNotEvent_t1 = 0
# costNotEvent_t2 = 400
# costNotEvent_t3 = NA
# costNotEvent_t4 =NA
# tCostsDependOnEvent = "No"


# test data (the same as) P6
# numberOfTreatments =2 
# MCsims = 10000
# P_t1 =0.95
# INBBinaryEvent = 9.5
# mu_t2=0
# variance_t2=0.25
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
# nameOf_t1="continual treatment"
# nameOf_t2="withdrawal"
# nameOf_t3=NA
# nameOf_t4=NA
# tCostsDependOnEvent = "Yes"
# cost_t1 = 1000
# cost_t2 = 91000
# cost_t3 = NA
# cost_t4 = NA
# costEvent_t1 = 3100000
# costEvent_t2 = 3100000
# costEvent_t3 = NA
# costEvent_t4 = NA
# costNotEvent_t1 = 7300000
# costNotEvent_t2= 4000000
# costNotEvent_t3=NA
# costNotEvent_t4 = NA
# typeOfOutcome="benefit"
# incidence=26.26
# timeInformation=10
# discountRate=3.5 
# durationOfResearch= 4
# costResearchFunder=855403
# MCD_t2=0
# MCD_t3=NA
# MCD_t4=NA
# utilisation_t1=100
# utilisation_t2=0
# utilisation_t3=NA
# utilisation_t4=NA
# costHealthSystem = 9899380
# k = 13000
# currencySymbol = "£"

BinaryQALYFunction.v.0.1 <- function(numberOfTreatments, MCsims, P_t1, INBBinaryEvent,
                                    mu_t2, variance_t2, dist_t2, direction_t2,
                                    mu_t3, variance_t3, dist_t3, direction_t3,
                                    mu_t4, variance_t4, dist_t4, direction_t4,
                                    nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                                    tCostsDependOnEvent, 
                                    cost_t1, cost_t2, cost_t3, cost_t4,
                                    costEvent_t1,costEvent_t2,costEvent_t3,costEvent_t4,
                                    costNotEvent_t1,costNotEvent_t2,costNotEvent_t3,costNotEvent_t4,
                                    typeOfOutcome, incidence,timeInformation,
                                    discountRate ,durationOfResearch,costResearchFunder,
                                    MCD_t2, MCD_t3, MCD_t4,
                                    utilisation_t1, utilisation_t2,
                                    utilisation_t3, utilisation_t4, 
                                    costHealthSystem, k, currencySymbol){
  
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
  
  # create Binary QALY economic model from probability of event
  #########################
  # creates a matrix NB_t which holds simulations of NB for each treatment

  if(tCostsDependOnEvent == "No"){ # if treatment costs do not depend on whether the event occurs or not
  
    NB_t  <- P_t*INBBinaryEvent # multiply every element by INBBinaryEvent (1st step in converting to NB)
    addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the vector to convert to net benefit
    NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
    # subtract the costs from each column in the vector.
    addCost_t <- c(-cost_t1/k ,-cost_t2/k, -cost_t3/k, -cost_t4/k) 
    NB_t  <- NB_t  + rep(addCost_t, each = MCsims) # each column now represents simulations of the NB of each treatment
  
    # generate and format costs table (assuming treatment costs do not depend on event)
    cost_t <- c(cost_t1, cost_t2, cost_t3, cost_t4)
    Cost_per_individual <- paste0(currencySymbol,formatC(cost_t, big.mark = ',', format = 'd'))
    Yearly_costs <- paste0(currencySymbol,formatC(cost_t*incidence, big.mark = ',', format = 'd'))
    Additional_cost_per_year <- paste0(currencySymbol,formatC((cost_t - cost_t1)*incidence, big.mark = ',', format = 'd'))
    popTotal <- (incidence/-discountRate) * (exp(-discountRate*timeInformation) - exp(-discountRate*0))
    Total_Costs <- paste0(currencySymbol,formatC(cost_t*popTotal, big.mark = ',', format = 'd'))
    Treatment_name <- c(nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4)
    tableTreatmentCostsDF <- as.data.frame(cbind(Treatment_name, Cost_per_individual, Yearly_costs, Additional_cost_per_year, Total_Costs))
    tableTreatmentCostsDF <- tableTreatmentCostsDF[1:numberOfTreatments,]
    
  } else { # if treatment costs DO depend on whether the event occurs or not
    
    costEvent_t <- c(costEvent_t1,costEvent_t2,costEvent_t3,costEvent_t4)
    costNotEvent_t <- c(costNotEvent_t1,costNotEvent_t2,costNotEvent_t3,costNotEvent_t4)
    # for each row of P_t calculate the QALY benefit of the event occuring minus the costs associated with the event/not event
    outputNB <- apply(P_t, 1, function(x)
      x*INBBinaryEvent + (x*-costEvent_t/k) + ((1-x)*-costNotEvent_t/k))
    NB_t <- t(outputNB) # need to transpose output to get in correct NB_t matrix form
    addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the matrix
    NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
    
   
    # generate and format costs table (assuming treatment costs DO depend on event)
    EP_t <- apply(P_t, 2, mean) # expected probability of outcomes
    expectedCost_t <- EP_t*costEvent_t + (1 - EP_t)*costNotEvent_t # expected cost per person for each treatment
    Cost_per_individual <- paste0(currencySymbol,formatC(expectedCost_t, big.mark = ',', format = 'd'))
    Yearly_costs <- paste0(currencySymbol,formatC(expectedCost_t*incidence, big.mark = ',', format = 'd'))
    Additional_cost_per_year <- paste0(currencySymbol,formatC((expectedCost_t - expectedCost_t[1])*incidence, big.mark = ',', format = 'd'))
    popTotal <- (incidence/-discountRate) * (exp(-discountRate*timeInformation) - exp(-discountRate*0))
    Total_Costs <- paste0(currencySymbol,formatC(expectedCost_t*popTotal, big.mark = ',', format = 'd'))
    Treatment_name <- c(nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4)
    tableTreatmentCostsDF <- as.data.frame(cbind(Treatment_name, Cost_per_individual, Yearly_costs, Additional_cost_per_year, Total_Costs))
    tableTreatmentCostsDF <- tableTreatmentCostsDF[1:numberOfTreatments,]
    
  }
  
  # Calculate outputs from NB matrix
  ######################################
  
  VOIoutputs <- NBtoEVPIResults(NB_t,
                                nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                                typeOfOutcome, incidence,timeInformation,
                                discountRate ,durationOfResearch,costResearchFunder,
                                MCD_t2, MCD_t3, MCD_t4,
                                utilisation_t1, utilisation_t2,
                                utilisation_t3, utilisation_t4,
                                costHealthSystem, k)
  VOIoutputs$tableTreatmentCostsDF <- tableTreatmentCostsDF # add the expected cost table to the input list
  
  # return the list of results
  ###############################
  
  return(VOIoutputs)
  
}


BinaryQALYFunction <- BinaryQALYFunction.v.0.1
#test function
# resultsholder <- BinaryQALYFunction(numberOfTreatments =2 , MCsims = 10000, P_t1 =0.95, INBBinaryEvent = 9.5,
#                                     mu_t2=0, variance_t2=0.25, dist_t2="norm", direction_t2= NA,
#                                     mu_t3=NA, variance_t3=NA, dist_t3=NA, direction_t3=NA,
#                                     mu_t4=NA, variance_t4=NA, dist_t4=NA, direction_t4=NA,
#                                     nameOf_t1="continual treatment",nameOf_t2="withdrawal", nameOf_t3=NA, nameOf_t4=NA,
#                                     tCostsDependOnEvent = "Yes",
#                                     cost_t1 = 100, cost_t2 = 1000, cost_t3 = NA, cost_t4 = NA,
#                                     costEvent_t1 = 3100000,costEvent_t2 = 3100000,costEvent_t3 = NA,costEvent_t4 = NA,
#                                     costNotEvent_t1 = 7300000,costNotEvent_t2= 4000000,costNotEvent_t3=NA,costNotEvent_t4 = NA,
#                                     typeOfOutcome="benefit", incidence=26.26,timeInformation=10,
#                                     discountRate=3.5 ,durationOfResearch= 4,costResearchFunder=855403,
#                                     MCD_t2=0, MCD_t3=NA, MCD_t4=NA,
#                                     utilisation_t1=100, utilisation_t2=0,
#                                     utilisation_t3=NA, utilisation_t4=NA,
#                                     costHealthSystem = 9899380, k = 13000, currencySymbol = "£")





########### BINARY QALY FEASIBILITY

BinaryQALYFunctionFeas.v.0.1 <- function(numberOfTreatments, MCsims, P_t1, INBBinaryEvent,
                                     mu_t2, variance_t2, dist_t2, direction_t2,
                                     mu_t3, variance_t3, dist_t3, direction_t3,
                                     mu_t4, variance_t4, dist_t4, direction_t4,
                                     nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4,
                                     tCostsDependOnEvent, 
                                     cost_t1, cost_t2, cost_t3, cost_t4,
                                     costEvent_t1,costEvent_t2,costEvent_t3,costEvent_t4,
                                     costNotEvent_t1,costNotEvent_t2,costNotEvent_t3,costNotEvent_t4,
                                     typeOfOutcome, incidence,timeInformation,
                                     discountRate ,
                                     MCD_t2, MCD_t3, MCD_t4,
                                     utilisation_t1, utilisation_t2,
                                     utilisation_t3, utilisation_t4, 
                                     k, currencySymbol,
                                     probabilityOfDefinitiveResearch,durationOfResearchDefinitive,
                                     durationOfResearchFeas,costResearchFunderFeas,
                                     costResearchFunderDefinitive,
                                     costHealthSystemFeas,costHealthSystemDefinitive ){
  
  # simulate probabilities of event
  #########################
  
  # simulate probabilities of event with baseline treatment
  P_t1 <- rep(P_t1, MCsims)
  
  # simulate probabilities of the event for other treatments
  P_t <- simProbOfOutcomeMatrixBinary(numberOfTreatments, P_t1,
                                      mu_t2, variance_t2, dist_t2, direction_t2,
                                      mu_t3, variance_t3, dist_t3, direction_t3,
                                      mu_t4, variance_t4, dist_t4, direction_t4)
  
  # create Binary QALY economic model from probability of event
  #########################
  # creates a matrix NB_t which holds simulations of NB for each treatment
  
  if(tCostsDependOnEvent == "No"){ # if treatment costs do not depend on whether the event occurs or not
    
    NB_t  <- P_t*INBBinaryEvent # multiply every element by INBBinaryEvent (1st step in converting to NB)
    addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the vector to convert to net benefit
    NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
    # subtract the costs from each column in the vector.
    addCost_t <- c(-cost_t1/k ,-cost_t2/k, -cost_t3/k, -cost_t4/k) 
    NB_t  <- NB_t  + rep(addCost_t, each = MCsims) # each column now represents simulations of the NB of each treatment
    
    # generate and format costs table (assuming treatment costs do not depend on event)
    cost_t <- c(cost_t1, cost_t2, cost_t3, cost_t4)
    Cost_per_individual <- paste0(currencySymbol,formatC(cost_t, big.mark = ',', format = 'd'))
    Yearly_costs <- paste0(currencySymbol,formatC(cost_t*incidence, big.mark = ',', format = 'd'))
    Additional_cost_per_year <- paste0(currencySymbol,formatC((cost_t - cost_t1)*incidence, big.mark = ',', format = 'd'))
    popTotal <- (incidence/-discountRate) * (exp(-discountRate*timeInformation) - exp(-discountRate*0))
    Total_Costs <- paste0(currencySymbol,formatC(cost_t*popTotal, big.mark = ',', format = 'd'))
    Treatment_name <- c(nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4)
    tableTreatmentCostsDF <- as.data.frame(cbind(Treatment_name, Cost_per_individual, Yearly_costs, Additional_cost_per_year, Total_Costs))
    tableTreatmentCostsDF <- tableTreatmentCostsDF[1:numberOfTreatments,]
    
  } else { # if treatment costs DO depend on whether the event occurs or not
    
    costEvent_t <- c(costEvent_t1,costEvent_t2,costEvent_t3,costEvent_t4)
    costNotEvent_t <- c(costNotEvent_t1,costNotEvent_t2,costNotEvent_t3,costNotEvent_t4)
    # for each row of P_t calculate the QALY benefit of the event occuring minus the costs associated with the event/not event
    outputNB <- apply(P_t, 1, function(x)
      x*INBBinaryEvent + (x*-costEvent_t/k) + ((1-x)*-costNotEvent_t/k))
    NB_t <- t(outputNB) # need to transpose output to get in correct NB_t matrix form
    addMCD_t <- c(0 ,MCD_t2, MCD_t3, MCD_t4)   # add the MCD to each column in the matrix
    NB_t  <- NB_t  + rep(addMCD_t, each = MCsims)
    
    
    # generate and format costs table (assuming treatment costs DO depend on event)
    EP_t <- apply(P_t, 2, mean) # expected probability of outcomes
    expectedCost_t <- EP_t*costEvent_t + (1 - EP_t)*costNotEvent_t # expected cost per person for each treatment
    Cost_per_individual <- paste0(currencySymbol,formatC(expectedCost_t, big.mark = ',', format = 'd'))
    Yearly_costs <- paste0(currencySymbol,formatC(expectedCost_t*incidence, big.mark = ',', format = 'd'))
    Additional_cost_per_year <- paste0(currencySymbol,formatC((expectedCost_t - expectedCost_t[1])*incidence, big.mark = ',', format = 'd'))
    popTotal <- (incidence/-discountRate) * (exp(-discountRate*timeInformation) - exp(-discountRate*0))
    Total_Costs <- paste0(currencySymbol,formatC(expectedCost_t*popTotal, big.mark = ',', format = 'd'))
    Treatment_name <- c(nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4)
    tableTreatmentCostsDF <- as.data.frame(cbind(Treatment_name, Cost_per_individual, Yearly_costs, Additional_cost_per_year, Total_Costs))
    tableTreatmentCostsDF <- tableTreatmentCostsDF[1:numberOfTreatments,]
    
  }
  
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


BinaryQALYFunctionFeas <- BinaryQALYFunctionFeas.v.0.1
#test function
# resultsholder <- BinaryQALYFunctionFeas(numberOfTreatments =2 , MCsims = 10000, P_t1 =0.95, INBBinaryEvent = 9.5,
#                                     mu_t2=0, variance_t2=0.25, dist_t2="norm", direction_t2= NA,
#                                     mu_t3=NA, variance_t3=NA, dist_t3=NA, direction_t3=NA,
#                                     mu_t4=NA, variance_t4=NA, dist_t4=NA, direction_t4=NA,
#                                     nameOf_t1="continual treatment",nameOf_t2="withdrawal", nameOf_t3=NA, nameOf_t4=NA,
#                                     tCostsDependOnEvent = "Yes",
#                                     cost_t1 = 100, cost_t2 = 1000, cost_t3 = NA, cost_t4 = NA,
#                                     costEvent_t1 = 3100000,costEvent_t2 = 3100000,costEvent_t3 = NA,costEvent_t4 = NA,
#                                     costNotEvent_t1 = 7300000,costNotEvent_t2= 4000000,costNotEvent_t3=NA,costNotEvent_t4 = NA,
#                                     typeOfOutcome="benefit", incidence=26.26,timeInformation=10,
#                                     discountRate=3.5 ,
#                                     MCD_t2=0, MCD_t3=NA, MCD_t4=NA,
#                                     utilisation_t1=100, utilisation_t2=0,
#                                     utilisation_t3=NA, utilisation_t4=NA,
#                                     k = 13000, currencySymbol = "£",
#                                     probabilityOfDefinitiveResearch,durationOfResearchDefinitive = 0.5,
#                                     durationOfResearchFeas = 2,costResearchFunderFeas = 300000,
#                                     costResearchFunderDefinitive = 2000000,
#                                     costHealthSystemFeas = 100000,costHealthSystemDefinitive = 1500000)
