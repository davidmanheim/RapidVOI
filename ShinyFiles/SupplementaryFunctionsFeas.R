#####################
# Supplementary functions for Feasibility studies
######################


options(scipen = 999) # turn off scientific notation


############################
# feasibility population function - calculates the population numbers required for feasibility analysis
############################
# works well but tiny leftover when result should be zero : -4.656613e-10
# note discount rate converted 

feasibilityPop <- function(incidence, discountRate, durationOfResearchDefinitive, timeInformation, durationOfResearchFeas){
  
  discountRate <- discountRate/100 # convert from 3.5 to 0.035
  
  #                                        time end                time start  
  popTotal <- (incidence/-discountRate) * (exp(-discountRate*timeInformation) - exp(-discountRate*0))
  
  durationOfResearchFeas_adj <- ifelse(durationOfResearchFeas < timeInformation, durationOfResearchFeas, timeInformation )  # need to make sure the delay is not longer than timeInformation
  popDuringFeasResearch <- (incidence/-discountRate) * (exp(-discountRate*durationOfResearchFeas_adj) - exp(-discountRate*0))
  
  durationOfResearchDefinitive_end_adj <- ifelse(durationOfResearchFeas_adj + durationOfResearchDefinitive > timeInformation, 
                                  timeInformation,
                                  durationOfResearchFeas_adj + durationOfResearchDefinitive)
  
  popDuringDefinitiveResearch <-  ((incidence) /-discountRate) * (exp(-discountRate*durationOfResearchDefinitive_end_adj) - exp(-discountRate*durationOfResearchFeas_adj))
  
  
  popAfterDefinitiveResearch <-  ((incidence) /-discountRate) * (exp(-discountRate*timeInformation) - exp(-discountRate*durationOfResearchDefinitive_end_adj))
  
  output <- list(popTotal = popTotal,
                 popDuringFeasResearch = popDuringFeasResearch,
                 popDuringDefinitiveResearch = popDuringDefinitiveResearch,
                 popAfterDefinitiveResearch = popAfterDefinitiveResearch)
  
  return(output)
}

# test feasibilityPop funtion
# pops <- feasibilityPop(incidence =  355000*0.73 ,
#             timeInformation =  5   ,
#             discountRate =  0.035,
#             durationOfResearchDefinitive =  3,
#             durationOfResearchFeas = 6)
# pops$popTotal - pops$popDuringFeasResearch - pops$popDuringDefinitiveResearch - pops$popAfterDefinitiveResearch





# # test data for NBtoEVPIResultsFeas
# nameOf_t1 <- "late PTP"
# nameOf_t2 <- "early PTP"
# nameOf_t3 <- "treatment 3"
# nameOf_t4 <- "treatment 4"
# typeOfOutcome <- "benefit" # "harm" "netHealth" # was Benefit==1 or 0 for benefit or harm
# incidence = 8000 # was Incidence
# timeInformation  = 15 # Time_info  = 15
# discountRate = 3.5  #D_rate = 0.035 ***NB need to divide by 100
# costResearchFunderFeas = 100000
# costResearchFunderDefinitive = 882177 #Cost_research_funder =  882177
# durationOfResearchDefinitive = 3 #durationOfResearch = 3  # Time_research = 3
# durationOfResearchFeas = 1
# utilisation_t1 = 50 # check these sum to 100***.
# utilisation_t2 = 50
# utilisation_t3 = 0
# utilisation_t4 = NA
# NB_t <- simProbOfOutcomeMatrixBinary (numberOfTreatments = 3, P_t1 = rep(0.1, 10),
#                           mu_t2 = 0, variance_t2 = 0.1, dist_t2 = "norm",  direction_t2 = "alwaysPositive",
#                           mu_t3 = 0.2, variance_t3 = 0.1, dist_t3 = "halfNorm", direction_t3 = "alwaysPositive",
#                           mu_t4 = NA, variance_t4 = NA, dist_t4 = "halfNorm", direction_t4 = NA
#                           )
# probabilityOfDefinitiveResearch = 0.5
# costHealthSystemFeas = 100000 # Cost_research_pilot_NHS
# costHealthSystemDefinitive = 2000000
# k = 13000
# currencySymbol = "£"

# takes in a matrix of net benefits and outputs all relevant EVPI metrics
# Requires: feasibilityPop
# Consider: adding convergence check! make sure current implementation outputs calculating properly 
# ---- should i have NA's in the default inputs???

NBtoEVPIResultsFeas <- function(NB_t,
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
                            currencySymbol= "£"){
  
  # define variables required
  MCsims <- nrow(NB_t) # impled number of simulations
  Utilisation_t <- c(utilisation_t1/100, utilisation_t2/100, utilisation_t3/100, utilisation_t4/100)
  numberOfTreatments <- sum(!is.na(NB_t[1,]))
  
  # expected outcome with each treatment (uninformed prior)
  ENB_t  <- apply(NB_t , 2, mean)
  
  # Best outcome with current information
  NB_EVTCI  = max(ENB_t , na.rm = TRUE)
  
  # optimalTreatment: tells you which treatment is best given current information
  optimalTreatment <- c(nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4)[which(ENB_t  == max(ENB_t , na.rm = TRUE))]
  expectedOutcomesPerYearoptimalTreatment <- NB_EVTCI*incidence
  
  # logical, TRUE if implementation value exists i.e is there potential value in changing implementation?
  # this is used to determine what text gets dispalyed in output
  implementationValueExists <- ifelse(sum(ENB_t*Utilisation_t, na.rm = TRUE) == NB_EVTCI, FALSE, TRUE)
  
  # table of events per year - needs to be outputted as a data frame
  Treatment_name <- c(nameOf_t1,nameOf_t2, nameOf_t3, nameOf_t4)
  Expected_outcomes_per_year <- formatC(c(ENB_t[1], ENB_t[2], ENB_t[3], ENB_t[4])*incidence, big.mark = ',', format = 'd')
  Current_utilisation <- paste0(round(Utilisation_t*100) , '%')
  tableEventsPerYearDF <- as.data.frame(cbind(Treatment_name, Expected_outcomes_per_year, Current_utilisation))
  tableEventsPerYearDF <- tableEventsPerYearDF[1:numberOfTreatments,]   # only output the number of rows = to the number of treatments considered
  
  # Expected value of treating with perfect information
  NB_VTPI  <- apply(NB_t , 1, max, na.rm = TRUE) #so I can check convergence - COULD ADD THIS CHECK
  NB_EVTPI  <- mean(NB_VTPI )
  NB_EVPI  <-  NB_EVTPI  - NB_EVTCI 
  
  # probability each treatment has highest NB - provides vector of probabilities
  # for the column of simulated NBs for each treatment (x)
  # take the sum of the number of times that that treatment is the maximum NB
  # divide by the number of sumulations to get the probability
  Probability_t_is_max <- apply(NB_t , 2, function(x) sum(x==NB_VTPI , na.rm = TRUE))/MCsims
  
  probTreatment1isMax <- Probability_t_is_max[1]
  probTreatment2isMax <- Probability_t_is_max[2]
  probTreatment3isMax <- Probability_t_is_max[3]
  probTreatment4isMax <- Probability_t_is_max[4]
  
  # what is the probability that the best treatment is optimal
  # first calculate it unformatted - for use to calculate its opposite
  probOptimalTisMaxUnFormat <- Probability_t_is_max[which(ENB_t  == max(ENB_t , na.rm = TRUE))]
  probOptimalTisMax <- paste0(round(probOptimalTisMaxUnFormat*100) , '%')
  probOptimalTisNotMax <- paste0(round((1 - probOptimalTisMaxUnFormat)*100) , '%')
  
  # table of probability each treatmentis best - needs to be outputted as a data frame
  Probability_of_being_best_treatment <- paste0(round(Probability_t_is_max*100) , '%')
  tableProbabilityMaxDF <- as.data.frame(cbind(Treatment_name, Probability_of_being_best_treatment))
  tableProbabilityMaxDF <- tableProbabilityMaxDF[1:numberOfTreatments,]
  
  # logical, is there any uncertaity in the current evidence?
  # for use structuring the outputs of the results
  # probability that the optimal treatment is 
  # if the prob of any treatment being the best is 1 then there is no uncertainty in evidence
  uncertaintyInCurrentEvidenceExists <- ifelse(sum(Probability_t_is_max == 1) == 1, FALSE, TRUE )
  
  #############################################
  # population 
  
  popOutputs <- feasibilityPop(incidence, discountRate, durationOfResearchDefinitive, 
                               timeInformation, durationOfResearchFeas)
    
  
  popTotal <-  popOutputs$popTotal
  popDuringFeasResearch <- popOutputs$popDuringFeasResearch
  popDuringDefinitiveResearch <- popOutputs$popDuringDefinitiveResearch
  popAfterDefinitiveResearch <- popOutputs$popAfterDefinitiveResearch
  
  
  ########### BASIC TRIAL ANALYSIS ################################################
  
  # YEARLY OUTCOMES #
  valueOfResearchPerYear <- NB_EVTPI *incidence - NB_EVTCI *incidence
  valueOfImplementationPerYear <- incidence*NB_EVTCI  - sum(ENB_t * Utilisation_t*incidence, na.rm = TRUE)
  
  # histogram of effects per year
  #               # NB per simulation with max(ENB_t ) - max NB per simulation
  #                 # best treament with current evidence - max NB per simulation
  # calculate loss from not having perfect information each year
  NB_loss_maxt <- NB_t[,which(ENB_t  == max(ENB_t , na.rm = TRUE))] - NB_VTPI 
  Hist_value_of_trial_per_year <- hist(-NB_loss_maxt*incidence)
  # convert to probability plot, not density
  Hist_value_of_trial_per_year$density = Hist_value_of_trial_per_year$counts/sum(Hist_value_of_trial_per_year$counts)*100
  #plot(Hist_value_of_trial_per_year,freq=FALSE,
  #     main = "Consequences of uncertainty (per year)",
  #     xlab = "Primary outcomes",
  #     ylab = "Probability (%)")
  
  # output the list which is required to produce the VOI histogram - the plot will be constructed with
  # this output so that it can be publised in shinyapps.io
  listForhistVOIYear <-  Hist_value_of_trial_per_year
  
  # this was a previous failed attempt, would not publish on shinyapps.io
  # base graphics draw directly on a device.
  #histVOIYear <- recordPlot() #record the histogram from the device
  #plot.new() ## clean up device
  
  # FULL TIME OUTCOMES #
  
  ## Cell_A : net benefit of current situation with current utilisation
  # take the weighted average of the expected NB for each treatment scaled up to full population
  Cell_A <- sum(ENB_t *Utilisation_t*popTotal, na.rm = TRUE)
  
  ## Cell_B: need to add this?
  
  ## Cell_C : maximum Net benfit of implemetation (Early access - approval)  
  Cell_C <- popTotal*NB_EVTCI 
  
  ## Cell_D : maximum Net benfit of information (delay access for information)
  # "instant trial with perfect information"
  # Pure definition of Cell D
  Cell_D <- NB_EVTPI *popTotal
  
  # assume perfect and instant implementation/information
  # and no costs of research imposed on health system
  maxvalueOfImplementation <- Cell_C - Cell_A # max value of early access
  maxvalueOfResearch <- Cell_D - Cell_C 
  
  
  # calculating the benefits of research (under differnt assumptions)
  ########################################
  # perfect info and perfect implementation (includes that it is instant)
  NB_instant_research_perfect_info_imp <- Cell_D
  
  # cu = current utilisation. NOT instant trial - while trial is running just keep whatever treatment 
  # utilisation is theere at the start of the trial
  # Below is the same as OIR if the use of the new treatment is restricted
  # --- return to this -----
  #NB_cu_perfect_info_imp <- sum(ENB_t*Utilisation_t*popDuringResearch, na.rm = TRUE) + popAfterResearch*NB_EVTPI  
  
  # maxt = use the best treatmet according to current NB. NOT instant trial - 
  # instantly and perfectly implement best treatment while trial is running 
  # below is the same as AWR if the new treatment is the best with current information
  # --- return to this -----
  #NB_maxt_perfect_info_imp <- popDuringResearch*NB_EVTCI  + popAfterResearch*NB_EVTPI 
  
  # gross net benefit with perfect implementation during the trial
  NB_E_maxt_trial <- 
    # if its a net benefit analysis the NHS cost of pilot is always subtracted
    ifelse(typeOfOutcome == "netHealth" ,- costHealthSystemFeas/k, 0) +
    # If definitive trial HAPPENS
    probabilityOfDefinitiveResearch*(
      popDuringFeasResearch*NB_EVTCI + popDuringDefinitiveResearch*NB_EVTCI +
        popAfterDefinitiveResearch*NB_EVTPI + ifelse(typeOfOutcome == "netHealth" ,- costHealthSystemDefinitive/k, 0)
    ) +
    # If definitive trial DOES NOT happen (everybody just gets best treatment)
    (1 - probabilityOfDefinitiveResearch)*(
      popTotal*NB_EVTCI 
    )
  
  # expected value of treating with current utilisation
  # 
  NB_EVTCU <-  sum(ENB_t*Utilisation_t, na.rm = TRUE)
  
  # gross net benefit with current implementation during the trial
  NB_E_cu_trial <- 
    # if its a net benefit analysis the NHS cost of pilot is always subtracted
    ifelse(typeOfOutcome == "netHealth" ,- costHealthSystemFeas/k, 0) +
    # If definitive trial HAPPENS
    probabilityOfDefinitiveResearch*(
      popDuringFeasResearch*NB_EVTCU + popDuringDefinitiveResearch*NB_EVTCU +
        popAfterDefinitiveResearch*NB_EVTPI + ifelse(typeOfOutcome == "netHealth" ,- costHealthSystemDefinitive/k, 0)
    ) +
    # If definitive trial DOES NOT happen (everybody just gets best treatment)
    (1 - probabilityOfDefinitiveResearch)*(
      popTotal*NB_EVTCU 
    )
  
  # this is the pure information value under different types of research and implementation assumptions
  # Cell_C : maximum Net benfit of implemetation (Early access - approval)  
  # the value here is the value of the feasibility (with imperfect implementation) 
  # compared to implementing the optimal treatment with 
  # current information
  valueOfResearchWithCurrentImplementation <- NB_E_cu_trial - Cell_C
  
  # same as above but for perfect implementation
  valueOfResearchWithPerfectImplementation <- NB_E_maxt_trial - Cell_C 
  
  # calculate the value of feasibility research if the definitivei trial was certain to occur
  # assumes that implementation will stay the same during the trial
  valueOfCertainResearchWithPerfectImplementation <- 
    # if its a net benefit analysis the NHS cost of pilot is always subtracted
    ifelse(typeOfOutcome == "netHealth" ,- costHealthSystemFeas/k, 0) +
    # definitive trial always HAPPENS
      popDuringFeasResearch*NB_EVTCU + popDuringDefinitiveResearch*NB_EVTCU +
      popAfterDefinitiveResearch*NB_EVTPI + ifelse(typeOfOutcome == "netHealth" ,- costHealthSystemDefinitive/k, 0) -
    # minus the alternative (fully implement best treatment with curren evidence)
    Cell_C

  
  # expected research funder costs
  # new output for pilot studies
  expectedCostResearchFunder <- 
    costResearchFunderFeas +  # always incur this cost
    # If definitive trial HAPPENS
    probabilityOfDefinitiveResearch*costResearchFunderDefinitive
  
    
  # ICER of research relative to early access (assumed to be costless to the agency)
  # all other costs assumed to be captured by the MCD
  ICER_ResearchWithCurrentImplementation <- expectedCostResearchFunder/valueOfResearchWithCurrentImplementation
  ICER_ResearchWithPerfectImplementation <- expectedCostResearchFunder/valueOfResearchWithPerfectImplementation
  
  valuePer15KResearchSpend <- (valueOfResearchWithPerfectImplementation/expectedCostResearchFunder)*15000
  valuePerOpCostResearchSpend <- (valueOfResearchWithPerfectImplementation/expectedCostResearchFunder)*k
  
  # expected costs to helath system 
  expectedCostHealthSystem <-   costHealthSystemFeas +  probabilityOfDefinitiveResearch*costHealthSystemDefinitive
  
  # expected op costs of the research
  healthOpportunityCostsOfResearch <- -expectedCostHealthSystem/k
  
  # absolute value of proposed research project 
  # the health outcomes you get from funding the research project
  # this scales the ICER of the project as a whole to reflect the fact that the research funder only funds the feasibility trial
  # => what is the expected aboslute benefit of funding the feasibility section of the project
  absoluteExpectedHealthOutcomesFromResearchProject <- costResearchFunderFeas*(valueOfResearchWithPerfectImplementation/expectedCostResearchFunder)

  # complete list of outputs
  ###########################
  NBtoEVPIResults <- list(
    optimalTreatment = optimalTreatment,
    expectedOutcomesPerYearoptimalTreatment = formatC(expectedOutcomesPerYearoptimalTreatment, big.mark = ',', format = 'd'),
    implementationValueExists = implementationValueExists,            # new output
    uncertaintyInCurrentEvidenceExists = uncertaintyInCurrentEvidenceExists, # new
    probTreatment1isMax = probTreatment1isMax, 
    probTreatment2isMax = probTreatment2isMax, 
    probTreatment3isMax = probTreatment3isMax, 
    probTreatment4isMax = probTreatment4isMax,
    probOptimalTisMax = probOptimalTisMax,                 # note: already formatted
    probOptimalTisNotMax = probOptimalTisNotMax,           # note: already formatted
    #popDuringResearch = popDuringResearch, # removed
    #popAfterResearch = popAfterResearch,   # removed
    popDuringFeasResearch = formatC(popDuringFeasResearch, big.mark = ',', format = 'd'),                      # new output
    popDuringDefinitiveResearch = formatC(popDuringDefinitiveResearch, big.mark = ',', format = 'd'),          #new output
    popAfterDefinitiveResearch = formatC(popAfterDefinitiveResearch, big.mark = ',', format = 'd'),            # new output
    popTotal = formatC(popTotal, big.mark = ',', format = 'd'), 
    listForhistVOIYear = listForhistVOIYear,
    valueOfResearchPerYear = formatC(valueOfResearchPerYear, big.mark = ',', format = 'd'),
    valueOfImplementationPerYear = formatC(valueOfImplementationPerYear, big.mark = ',', format = 'd'),
    tableEventsPerYearDF = tableEventsPerYearDF,                         # new
    tableProbabilityMaxDF = tableProbabilityMaxDF,                      # new
    Cell_A = Cell_A,
    Cell_C = Cell_C,
    Cell_D = Cell_D,
    maxvalueOfImplementation = formatC(maxvalueOfImplementation, big.mark = ',',format = 'd'),
    maxvalueOfResearch = formatC(maxvalueOfResearch, big.mark = ',',format = 'd'),
    expectedCostHealthSystem = paste0(currencySymbol, formatC(expectedCostHealthSystem, big.mark = ',',format = 'd')),                  # new output
    healthOpportunityCostsOfResearch = formatC(round(healthOpportunityCostsOfResearch, 2), big.mark = ','),
    valueOfResearchWithCurrentImplementation = formatC(valueOfResearchWithCurrentImplementation, big.mark = ',',format = 'd'),
    valueOfResearchWithPerfectImplementation = formatC(valueOfResearchWithPerfectImplementation, big.mark = ',',format = 'd'),
    valueOfCertainResearchWithPerfectImplementation = formatC(valueOfCertainResearchWithPerfectImplementation, big.mark = ',',format = 'd'),   # new output
    ICER_ResearchWithCurrentImplementation = paste0(currencySymbol, formatC(ICER_ResearchWithCurrentImplementation, big.mark = ',', format = 'd')),
    ICER_ResearchWithPerfectImplementation = paste0(currencySymbol, formatC(ICER_ResearchWithPerfectImplementation, big.mark = ',', format = 'd')),
    valuePer15KResearchSpend = round(valuePer15KResearchSpend, 2),
    valuePerOpCostResearchSpend = round(valuePerOpCostResearchSpend, 2),
    expectedCostResearchFunder = paste0(currencySymbol ,formatC(expectedCostResearchFunder, big.mark = ',',format = 'd')), # new output for pilot studies
    absoluteExpectedHealthOutcomesFromResearchProject = formatC(absoluteExpectedHealthOutcomesFromResearchProject, big.mark = ',', format = 'd')  # new output for pilot studies
  )
  
  # return this list from the function
  return(NBtoEVPIResults)
  
}


# test the function- inputs from P1 used
# # costruct input matrix
# NB_t <- simProbOfOutcomeMatrixBinary (numberOfTreatments = 3, P_t1 = rep(0.525, 10000),
#                                       mu_t2 = 0, variance_t2 = 0.25, dist_t2 = "norm",  direction_t2 = "alwaysPositive",
#                                       mu_t3 = 0, variance_t3 = 0.25, dist_t3 = "norm", direction_t3 = "alwaysPositive",
#                                       mu_t4 = NA, variance_t4 = NA, dist_t4 = "halfNorm", direction_t4 = NA
# )
# resultlist <- NBtoEVPIResultsFeas(NB_t = NB_t,
#                 nameOf_t1 = "PI",nameOf_t2 = "APs", nameOf_t3 = "PI + APs", nameOf_t4 = "4",
#                 typeOfOutcome = "benefit", incidence = 1563 ,timeInformation = 15,
#                 discountRate = 3.5 ,durationOfResearchDefinitive = 6,durationOfResearchFeas = 2,
#                 costResearchFunderFeas = 601480,costResearchFunderDefinitive = 2522710,
#                 MCD_t2 = 0, MCD_t3 = 0, MCD_t4 = 0,
#                 utilisation_t1 = 100, utilisation_t2 = 0,
#                 utilisation_t3 = 0, utilisation_t4 =0,
#                 probabilityOfDefinitiveResearch = 0.5,
#                 costHealthSystemFeas = 150000, costHealthSystemDefinitive = 490000,
#                 k = 15000, currencySymbol = "£")

