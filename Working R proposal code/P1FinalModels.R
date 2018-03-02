##############################
# P1 - tornado and all results
###########################





# Natural outcome function for pilot studies (only does 3 treatments) 
######################

PilotStudyBinaryOutcomeNat <- function(MCouter ,Benefit,P_t0_U ,mu_t1_U ,sigma_t1_U ,mu_t2_U ,sigma_t2_U ,MCD_t1 ,MCD_t2 , Incidence, 
                                       Time_info ,Utilisation_t0 ,Utilisation_t1 ,Utilisation_t2 ,D_rate ,Time_research_pilot ,Time_research_definitive ,
                                       Probability_of_definitive_research ,Cost_research_pilot_NETSCC ,Cost_research_pilot_NHS  ,
                                       Cost_research_definitive_NETSCC ,Cost_research_definitive_NHS){
  
  
  
  Odds_t0_U <- P_t0_U / (1 - P_t0_U)
  LO_t0_U <- log(Odds_t0_U)
  
  LOR_t1_U <- rnorm(MCouter, mu_t1_U, sigma_t1_U)
  LO_t1_U <- LO_t0_U + LOR_t1_U 
  Odds_t1_U <- exp(LO_t1_U)
  P_t1_U <- Odds_t1_U / (Odds_t1_U + 1) 
  
  LOR_t2_U <- rnorm(MCouter, mu_t2_U, sigma_t2_U)
  LO_t2_U <- LO_t0_U + LOR_t2_U 
  Odds_t2_U <- exp(LO_t2_U)
  P_t2_U <- Odds_t2_U / (Odds_t2_U + 1)
  
  
  Utilisation_t <- c(Utilisation_t0 ,Utilisation_t1 ,Utilisation_t2)
  
  # Put together mini economic model
  #########################################
  
  INB_Event <- ifelse(Benefit==1, 1, -1)
  P_t_U <- matrix(c(rep(P_t0_U,MCouter), P_t1_U, P_t2_U), ncol = 3)
  nt <- dim(P_t_U)[2] # the number of treatment implied by the input matrix
  nsim <- dim(P_t_U)[1]
  
  MCD_t <- c(MCD_t1 ,MCD_t2)
  NB_t_U <- P_t_U*INB_Event # multiply every element by INB_Event (1st step in converting to NB)
  addMCD_t <- c(0, MCD_t)   # add the MCD to each column in the vector to convert to net benefit
  NB_t_U <- NB_t_U + rep(addMCD_t, each = nsim)
  
  
  # trial inputs
  ########################
  
  # expected outcome with each treatment (uninformed prior)
  ENB_t_U <- apply(NB_t_U, 2, mean)
  
  E_outcomes_per_year_t_U <- ENB_t_U*Incidence
    
  # Best outcome with uninformed prior 
  NB_EVTCI_U = max(ENB_t_U)
  Optimal_t <- paste0("t_" ,which(ENB_t_U == max(ENB_t_U)) - 1) # tells you which treatment is best
  
  # EVTPI with uninformed prior and natural outcome
  #EVTPI_U  <- mean(apply(INB_t_U , 1, max))
  NB_VTPI_U <- apply(NB_t_U, 1, max) #so I can check convergence
  
  
  NB_EVTPI_U <- mean(NB_VTPI_U)
  NB_EVPI_U <-  NB_EVTPI_U - NB_EVTCI_U
  
  # probability each treatment has highest NB - provides vector of probabilities
  # for the column of simulated NBs for each treatment (x)
  # take the sum of the number of times that that treatment is the maximum NB
  # divide by the number of sumulations to get the probability
  Probability_t_is_max <- apply(NB_t_U, 2, function(x) sum(x==NB_VTPI_U))/nsim
  
  
  # Population calculations
  Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
  Pop_during_pilot_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research_pilot) - exp(-D_rate*0))
  Pop_during_definitive_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*(Time_research_pilot+Time_research_definitive)) - exp(-D_rate*Time_research_pilot))
  Pop_after_definitive_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*(Time_research_pilot+Time_research_definitive)))
  
  ########### BASIC TRIAL ANALYSIS ################################################
  # 
  
  # YEARLY OUTCOMES #
  Value_of_trial_per_year <- NB_EVTPI_U*Incidence - NB_EVTCI_U*Incidence
  Value_of_implementation_per_year <- Incidence*NB_EVTCI_U - sum(ENB_t_U*Utilisation_t*Incidence)
  
  # histogram of effects per year
  #               # NB per simulation with max(ENB_t_U) - max NB per simulation
  #                 # best treament with current evidence - max NB per simulation
  NB_loss_maxt <- NB_t_U[,which(ENB_t_U == max(ENB_t_U))] - NB_VTPI_U
  Hist_value_of_trial_per_year <- hist(-NB_loss_maxt*Incidence)
  # convert to probability plot, not density
  Hist_value_of_trial_per_year$density = Hist_value_of_trial_per_year$counts/sum(Hist_value_of_trial_per_year$counts)*100
  plot(Hist_value_of_trial_per_year,freq=FALSE,
       main = "Consequences of uncertainty (per year)",
       xlab = "Primary outcomes",
       ylab = "Probability (%)")
  
  
  # prob of being in a bin
  # to inform : There is a greater chance of more limited consequences (e.g., a 45% chance of consequences between zero and 300 additional relapses per year) and a smaller chance of larger consequences 
  Cumulative_prob_being_in_bin <- cumsum(Hist_value_of_trial_per_year$counts/sum(Hist_value_of_trial_per_year$counts) )
  Mid_point_of_bin <- Hist_value_of_trial_per_year$mids
  
  Prob_of_consequences <- rbind(Cumulative_prob_being_in_bin, Mid_point_of_bin)
  
  # FULL TIME OUTCOMES #
  
  ## Cell_A : net benefit of current situation with current utilisation
  # take the weighted average of the expected NB for each treatment scaled up to full population
  Cell_A <- sum(ENB_t_U*Utilisation_t*Pop_total)
  
  
  ## Cell_C : maximum Net benfit of implemetation (Early access - approval)  
  Cell_C <- Pop_total*NB_EVTCI_U
  NB_maxt_U <- Cell_C  # AKA nb_maxt_U
  
  ## Cell_D : maximum Net benfit of information (delay access for information)
  # "instant trial with perfect information"
  # Pure definition of Cell D
  Cell_D <- NB_EVTPI_U*Pop_total
  
  # assume perfect and instant implementation/information
  Max_value_of_implementation <- Cell_C - Cell_A # max value of early access
  Max_value_of_research <- Cell_D - Cell_C 
  
  
  
  
  
  Value_of_instant_research_perfect_info_imp <- Cell_D - Cell_C
  
  # Net benefit of maxt trial (AWR)
  # Pop during the trials: get treated with best treatment during trials 
  
  NB_E_maxt_trial_U <- 
    # If definitive trial HAPPENS
    Probability_of_definitive_research*(
      Pop_during_pilot_research*NB_EVTCI_U + Pop_during_definitive_research*NB_EVTCI_U +
        Pop_after_definitive_research*NB_EVTPI_U 
    ) +
    # If definitive trial DOES NOT happen (everybody just gets best treatment)
    (1 - Probability_of_definitive_research)*(
      Pop_total*NB_EVTCI_U 
    )
  
  #Cell_D <- NB_E_maxt_trial_U
  
  
  # value of trial
  # difference in effects (between trial and implementign best treatment)
  Value_of_maxt_perfect_info_imp <- NB_E_maxt_trial_U - NB_maxt_U
  
  # difference in costs (between trial and implementign best treatment)
  E_Cost_NETSCC  <- 
    Cost_research_pilot_NETSCC +  # always incur this cost
    # If definitive trial HAPPENS
    Probability_of_definitive_research*Cost_research_definitive_NETSCC
  
  # the ICER
  ICER_maxt_perfect_info_imp <- E_Cost_NETSCC/(NB_E_maxt_trial_U - NB_maxt_U)
  
  
  # expected *NHS* costs of research
  E_Cost_NHS <- 
    Cost_research_pilot_NHS  +  # always incur this cost
    # If definitive trial HAPPENS
    Probability_of_definitive_research*Cost_research_definitive_NHS 
  
  
  #Total_NB_research <- Cell_D -  Cell_A
  #Implementation_NB_research <- Cell_C -  Cell_A
  #Information_NB_research <- Cell_D -  Cell_C
  
  # % of total value which is implementation value
  #Perc_implementation_NB <- ( Cell_C -  Cell_A)/( Cell_D -  Cell_A)*100
  # % of total value which is pure information value
  #Perc_information_NB <- ( Cell_D -  Cell_C)/( Cell_D -  Cell_A)*100
  
  
  
  outputs <- list(ENB_t_U = ENB_t_U,
                  Optimal_t = Optimal_t,
                  Probability_t_is_max = Probability_t_is_max,
                  E_outcomes_per_year_t_U = E_outcomes_per_year_t_U,
                  Prob_of_consequences = Prob_of_consequences,
                  Value_of_trial_per_year = Value_of_trial_per_year,
                  Value_of_implementation_per_year = Value_of_implementation_per_year,
                  Max_value_of_implementation =Max_value_of_implementation,
                  Max_value_of_research =Max_value_of_research,
                  Value_of_maxt_perfect_info_imp = Value_of_maxt_perfect_info_imp,
                  Value_of_instant_research_perfect_info_imp =Value_of_instant_research_perfect_info_imp,
                  E_Cost_NETSCC = E_Cost_NETSCC,
                  ICER_maxt_perfect_info_imp = ICER_maxt_perfect_info_imp,
                  E_Cost_NHS = E_Cost_NHS
  )
  return(outputs)
  
}












# Comprehensive outcome function for pilot studies (only does 3 treatments) 
######################

PilotStudyBinaryQALY <- function(MCouter ,Benefit,INB_Event, C_t0, C_t1, C_t2, P_t0_U ,mu_t1_U ,sigma_t1_U ,mu_t2_U ,sigma_t2_U ,MCD_t1 ,MCD_t2 , Incidence, 
                                       Time_info ,Utilisation_t0 ,Utilisation_t1 ,Utilisation_t2 ,D_rate ,Time_research_pilot ,Time_research_definitive ,
                                       Probability_of_definitive_research ,Cost_research_pilot_NETSCC ,Cost_research_pilot_NHS  ,
                                       Cost_research_definitive_NETSCC ,Cost_research_definitive_NHS){
  
  
  
  Odds_t0_U <- P_t0_U / (1 - P_t0_U)
  LO_t0_U <- log(Odds_t0_U)
  
  LOR_t1_U <- rnorm(MCouter, mu_t1_U, sigma_t1_U)
  LO_t1_U <- LO_t0_U + LOR_t1_U 
  Odds_t1_U <- exp(LO_t1_U)
  P_t1_U <- Odds_t1_U / (Odds_t1_U + 1) 
  
  LOR_t2_U <- rnorm(MCouter, mu_t2_U, sigma_t2_U)
  LO_t2_U <- LO_t0_U + LOR_t2_U 
  Odds_t2_U <- exp(LO_t2_U)
  P_t2_U <- Odds_t2_U / (Odds_t2_U + 1)
  
  
  Utilisation_t <- c(Utilisation_t0 ,Utilisation_t1 ,Utilisation_t2)
  
  # Put together mini economic model
  #########################################
  
  P_t_U <- matrix(c(rep(P_t0_U,MCouter), P_t1_U, P_t2_U), ncol = 3)
  nt <- dim(P_t_U)[2] # the number of treatment implied by the input matrix
  nsim <- dim(P_t_U)[1]
  
  #MCD_t <- c(MCD_t1 ,MCD_t2)
  #NB_t_U <- P_t_U*INB_Event # multiply every element by INB_Event (1st step in converting to NB)
  #addMCD_t <- c(0, MCD_t)   # add the MCD to each column in the vector to convert to net benefit
  #NB_t_U <- NB_t_U + rep(addMCD_t, each = nsim)
  
  k <- 15000
  NB_t0_U <- P_t0_U*INB_Event - C_t0/k # 
  NB_t1_U <- (P_t1_U + MCD_t1)*INB_Event - C_t1/k # 
  NB_t2_U <- (P_t2_U + MCD_t2)*INB_Event - C_t2/k # 
  NB_t_U <- cbind(NB_t0_U, NB_t1_U, NB_t2_U)
  
  # trial inputs
  ########################
  
  # expected outcome with each treatment (uninformed prior)
  ENB_t_U <- apply(NB_t_U, 2, mean)
  
  E_outcomes_per_year_t_U <- ENB_t_U*Incidence
  
  # Best outcome with uninformed prior 
  NB_EVTCI_U = max(ENB_t_U)
  Optimal_t <- paste0("t_" ,which(ENB_t_U == max(ENB_t_U)) - 1) # tells you which treatment is best
  
  # EVTPI with uninformed prior and natural outcome
  #EVTPI_U  <- mean(apply(INB_t_U , 1, max))
  NB_VTPI_U <- apply(NB_t_U, 1, max) #so I can check convergence
  
  
  NB_EVTPI_U <- mean(NB_VTPI_U)
  NB_EVPI_U <-  NB_EVTPI_U - NB_EVTCI_U
  
  # probability each treatment has highest NB - provides vector of probabilities
  # for the column of simulated NBs for each treatment (x)
  # take the sum of the number of times that that treatment is the maximum NB
  # divide by the number of sumulations to get the probability
  Probability_t_is_max <- apply(NB_t_U, 2, function(x) sum(x==NB_VTPI_U))/nsim
  
  
  # Population calculations
  Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
  Pop_during_pilot_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research_pilot) - exp(-D_rate*0))
  Pop_during_definitive_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*(Time_research_pilot+Time_research_definitive)) - exp(-D_rate*Time_research_pilot))
  Pop_after_definitive_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*(Time_research_pilot+Time_research_definitive)))
  
  ########### BASIC TRIAL ANALYSIS ################################################
  # 
  
  # YEARLY OUTCOMES #
  Value_of_trial_per_year <- NB_EVTPI_U*Incidence - NB_EVTCI_U*Incidence
  Value_of_implementation_per_year <- Incidence*NB_EVTCI_U - sum(ENB_t_U*Utilisation_t*Incidence)
  
  # histogram of effects per year
  #               # NB per simulation with max(ENB_t_U) - max NB per simulation
  #                 # best treament with current evidence - max NB per simulation
  NB_loss_maxt <- NB_t_U[,which(ENB_t_U == max(ENB_t_U))] - NB_VTPI_U
  Hist_value_of_trial_per_year <- hist(-NB_loss_maxt*Incidence)
  # convert to probability plot, not density
  Hist_value_of_trial_per_year$density = Hist_value_of_trial_per_year$counts/sum(Hist_value_of_trial_per_year$counts)*100
  plot(Hist_value_of_trial_per_year,freq=FALSE,
       main = "Consequences of uncertainty (per year)",
       xlab = "Primary outcomes",
       ylab = "Probability (%)")
  
  
  # prob of being in a bin
  # to inform : There is a greater chance of more limited consequences (e.g., a 45% chance of consequences between zero and 300 additional relapses per year) and a smaller chance of larger consequences 
  Cumulative_prob_being_in_bin <- cumsum(Hist_value_of_trial_per_year$counts/sum(Hist_value_of_trial_per_year$counts) )
  Mid_point_of_bin <- Hist_value_of_trial_per_year$mids
  
  Prob_of_consequences <- rbind(Cumulative_prob_being_in_bin, Mid_point_of_bin)
  
  # FULL TIME OUTCOMES #
  
  ## Cell_A : net benefit of current situation with current utilisation
  # take the weighted average of the expected NB for each treatment scaled up to full population
  Cell_A <- sum(ENB_t_U*Utilisation_t*Pop_total)
  
  
  ## Cell_C : maximum Net benfit of implemetation (Early access - approval)  
  Cell_C <- Pop_total*NB_EVTCI_U
  NB_maxt_U <- Cell_C  # AKA nb_maxt_U
  
  ## Cell_D : maximum Net benfit of information (delay access for information)
  # "instant trial with perfect information"
  # Pure definition of Cell D
  Cell_D <- NB_EVTPI_U*Pop_total
  
  # assume perfect and instant implementation/information
  Max_value_of_implementation <- Cell_C - Cell_A # max value of early access
  Max_value_of_research <- Cell_D - Cell_C 
  
  
  
  
  
  Value_of_instant_research_perfect_info_imp <- Cell_D - Cell_C
  
  # Net benefit of maxt trial (AWR)
  # Pop during the trials: get treated with best treatment during trials 
  
  NB_E_maxt_trial_U <- 
    # always inclur NHS cost of pilot
    - Cost_research_pilot_NHS/k +
    # If definitive trial HAPPENS
    Probability_of_definitive_research*(
      Pop_during_pilot_research*NB_EVTCI_U + Pop_during_definitive_research*NB_EVTCI_U +
        Pop_after_definitive_research*NB_EVTPI_U - Cost_research_definitive_NHS/k
    ) +
    # If definitive trial DOES NOT happen (everybody just gets best treatment)
    (1 - Probability_of_definitive_research)*(
      Pop_total*NB_EVTCI_U 
    )
  
  #Cell_D <- NB_E_maxt_trial_U
  
  
  # value of trial
  # difference in effects (between trial and implementign best treatment)
  Value_of_maxt_perfect_info_imp <- NB_E_maxt_trial_U - NB_maxt_U
  
  # difference in costs (between trial and implementign best treatment)
  E_Cost_NETSCC  <- 
    Cost_research_pilot_NETSCC +  # always incur this cost
    # If definitive trial HAPPENS
    Probability_of_definitive_research*Cost_research_definitive_NETSCC
  
  # the ICER
  ICER_maxt_perfect_info_imp <- E_Cost_NETSCC/(NB_E_maxt_trial_U - NB_maxt_U)
  
  
  # expected *NHS* costs of research - already integrated into the NB of research
  E_Cost_NHS <- 
    Cost_research_pilot_NHS  +  # always incur this cost
    # If definitive trial HAPPENS
    Probability_of_definitive_research*Cost_research_definitive_NHS 
  
  
  #Total_NB_research <- Cell_D -  Cell_A
  #Implementation_NB_research <- Cell_C -  Cell_A
  #Information_NB_research <- Cell_D -  Cell_C
  
  # % of total value which is implementation value
  #Perc_implementation_NB <- ( Cell_C -  Cell_A)/( Cell_D -  Cell_A)*100
  # % of total value which is pure information value
  #Perc_information_NB <- ( Cell_D -  Cell_C)/( Cell_D -  Cell_A)*100
  
  
  
  outputs <- list(ENB_t_U = ENB_t_U,
                  Optimal_t = Optimal_t,
                  Probability_t_is_max = Probability_t_is_max,
                  E_outcomes_per_year_t_U = E_outcomes_per_year_t_U,
                  Prob_of_consequences = Prob_of_consequences,
                  Value_of_trial_per_year = Value_of_trial_per_year,
                  Value_of_implementation_per_year = Value_of_implementation_per_year,
                  Max_value_of_implementation =Max_value_of_implementation,
                  Max_value_of_research =Max_value_of_research,
                  Value_of_maxt_perfect_info_imp = Value_of_maxt_perfect_info_imp,
                  Value_of_instant_research_perfect_info_imp =Value_of_instant_research_perfect_info_imp,
                  E_Cost_NETSCC = E_Cost_NETSCC,
                  ICER_maxt_perfect_info_imp = ICER_maxt_perfect_info_imp,
                  E_Cost_NHS = E_Cost_NHS
  )
  return(outputs)
  
}


































##########################################################################################################
#                    old stuff
##################################################################################



NB_t0_U <- P_t0_U*INB_Event
NB_t1_U <- (P_t1_U + MCD_t1)*INB_Event 
NB_t2_U <- (P_t2_U + MCD_t2)*INB_Event 

# pre trial expected NB of outcomes
#########################

E_NB_t0_U <- mean(NB_t0_U) # 
E_NB_t1_U <- mean(NB_t1_U) # 
E_NB_t2_U <- mean(NB_t2_U) # 

# incremental benefit compared to standard care (APs) (is this required?)
E_INB_t0_U <- 0
E_INB_t1_U <- E_NB_t1_U - E_NB_t0_U
E_INB_t2_U <- E_NB_t2_U - E_NB_t0_U

# matrix for calculating VOI outputs- needs to use simulations
###########################################################

NB_t_U <- cbind(NB_t0_U, 
                NB_t1_U, 
                NB_t2_U)

# expected outcome with each treatment (uninformed prior)
ENB_t_U <- apply(NB_t_U, 2, mean)

Optimal_t <- paste0("t_" ,which(ENB_t_U == max(ENB_t_U)) - 1) # tells you which treatment is best

# trial inputs
########################

# Best outcome with uninformed prior 
NB_EVTCI_U = max(E_NB_t0_U, E_NB_t1_U, E_NB_t2_U) # 

# EVTPI with uninformed prior and natural outcome
#EVTPI_U  <- mean(apply(INB_t_U , 1, max))
NB_VTPI_U <- apply(NB_t_U, 1, max) #so I can check convergence
NB_EVTPI_U <- mean(NB_VTPI_U)
NB_EVPI_U <-  NB_EVTPI_U - NB_EVTCI_U


# probability each treatment has highest NB - provides vector of probabilities
# for the column of simulated NBs for each treatment (x)
# take the sum of the number of times that that treatment is the maximum NB
# divide by the number of sumulations to get the probability
Probability_t_is_max <- apply(NB_t_U, 2, function(x) sum(x==NB_VTPI_U))/length(NB_VTPI_U)





# yearly outputs
#################
# YEARLY OUTCOMES #
Value_of_trial_per_year <- NB_EVTPI_U*Incidence - NB_EVTCI_U*Incidence
Value_of_implementation_per_year <- Incidence*NB_EVTCI_U - sum(E_NB_t0_U*Utilisation_t0, 
                                                               E_NB_t1_U*Utilisation_t1,
                                                               E_NB_t2_U*Utilisation_t2)*Incidence


# plot yearly outcomes
NB_loss_maxt <- NB_t_U[,which(ENB_t_U == max(ENB_t_U))] - NB_VTPI_U
Hist_value_of_trial_per_year <- hist(-NB_loss_maxt*Incidence)
# convert to probability plot, not density
Hist_value_of_trial_per_year$density = Hist_value_of_trial_per_year$counts/sum(Hist_value_of_trial_per_year$counts)*100
plot(Hist_value_of_trial_per_year,freq=FALSE,
     main = "Consequences of uncertainty (per year)",
     xlab = "Primary outcomes",
     ylab = "Probability (%)")

########### TRIAL ANALYSIS (QALYs) ################################################
# 

## Cell_A : net benefit of current situation
# here assume everybody gets antipsychotics

Cell_A <- Pop_total*E_NB_t0_U*Utilisation_t0 +
  Pop_total*E_NB_t1_U*Utilisation_t1 +
  Pop_total*E_NB_t2_U*Utilisation_t2

## Cell_C : Net benfit of immediately implemeting best treatment (Approval)
NB_maxt_U <- Pop_total*NB_EVTCI_U
Cell_C <- NB_maxt_U

## Cell_D : Net benefit of having perfect information immediately
Cell_D <- Pop_total*NB_EVTPI_U






#INPUTS in order and comments on them
#,MCouter = 20000       # SKIP! i.e.do not vary in SA
# Benefit <- TRUE    # SKIP 
# ,P_t0_U <- 0.37    # bounded by 0 and 1 (should be done in program somewhere)
#,mu_t1_U <- 0          # 20% of zero = 0,  could change to +20% of ~N(0, 0.5) something like -0.1
# ,sigma_t1_U <- 0.5 # must be > 0
# ,mu_t2_U <- 0      # 20% of zero = 0,  could change to +20% of ~N(0, 0.5) something like -0.1
# ,sigma_t2_U <- 0.5 # must be > 0
#,MCD_t1 = 0.07         
#,MCD_t2 = 0            # 20% of zero = 0, just go +- 0.1
# , Incidence = 1563
# ,Time_info = 15
#,Utilisation_t0 = 1 # will not affect info value, MUST sum to 1! and cannot >1 <0
#,Utilisation_t1 = 0 # will not affect info value, MUST sum to 1! and cannot >1 <0
#,Utilisation_t2 = 0 # will not affect info value, MUST sum to 1! and cannot >1 <0
#,D_rate =  0.035 
# ,Time_research_pilot = 2 # must be >0 (multipliation will handle this)
# ,Time_research_definitive = 6  # must be >0 (multipliation will handle this)
# ,Probability_of_definitive_research = 0.5 # must be >0 and <1 (multipliation will handle the first)
# ,Cost_research_pilot_NETSCC =   601480 # will enter calculation of NHE in this model
# ,Cost_research_pilot_NHS  = 150000  # will enter calculation of NHE in this model
# ,Cost_research_definitive_NETSCC =  2500000 # will enter calculation of NHE in this model
# ,Cost_research_definitive_NHS  = 450000  # will enter calculation of NHE in this model





# create results vectors
################
baseInputs <- c(90000,TRUE,0.37 ,0,0.5,0,0.5,0.07,0, 1563,15,1 ,0 ,0 ,0.035 ,2, 6, 0.5, 601480 ,150000 , 2500000 ,450000)
resultsVectorUp <- rep(NA, length(baseInputs)) 
# PilotStudyVOI2 defined above

# +20%
for (i in 3:length(baseInputs)){   # start at the third input do not use first two in SA- they will be left as NAs
  arguments <- baseInputs
  arguments[i] <- baseInputs[i]*1.2
  resultsVectorUp[i] <- do.call(PilotStudyVOI2 , as.list(arguments))
}

# -20%
resultsVectorDown <- rep(NA, length(baseInputs)) 
for (i in 3:length(baseInputs)){   # start at the third input do not use first two in SA- they will be left as NAs
  arguments <- baseInputs
  arguments[i] <- baseInputs[i]*0.8
  resultsVectorDown[i] <- do.call(PilotStudyVOI2 , as.list(arguments))
}

# put together in a tornado
# example from: https://stackoverflow.com/questions/37059281/tornado-plot-in-r

############################################
# PERCENTAGE CHANGE from base case TORNADO
############################################


NBbase <- do.call(PilotStudyVOI2, as.list(baseInputs))


# turn into one row matrixies 
Up <- matrix(resultsVectorUp/NBbase -1, ncol = length(baseInputs))
Down <- matrix(resultsVectorDown/NBbase -1, ncol = length(baseInputs))

data <- rbind(Up, Down)


#1,MCouter = 20000       # SKIP! i.e.do not vary in SA
#2 Benefit <- TRUE    # SKIP 
#3 ,P_t0_U <- 0.37    # bounded by 0 and 1 (should be done in program somewhere)
#4,mu_t1_U <- 0          # 20% of zero = 0,  could change to +20% of ~N(0, 0.5) something like -0.1
#5 ,sigma_t1_U <- 0.5 # must be > 0
#6 ,mu_t2_U <- 0      # 20% of zero = 0,  could change to +20% of ~N(0, 0.5) something like -0.1
#7 ,sigma_t2_U <- 0.5 # must be > 0
#8,MCD_t1 = 0.07         
#9,MCD_t2 = 0            # 20% of zero = 0, just go +- 0.1
#10 , Incidence = 1563
#11 ,Time_info = 15
#12,Utilisation_t0 = 1 # will not affect info value, MUST sum to 1! and cannot >1 <0
#13,Utilisation_t1 = 0 # will not affect info value, MUST sum to 1! and cannot >1 <0
#14,Utilisation_t2 = 0 # will not affect info value, MUST sum to 1! and cannot >1 <0
#15,D_rate =  0.035 
#16 ,Time_research_pilot = 2 # must be >0 (multipliation will handle this)
#17 ,Time_research_definitive = 6  # must be >0 (multipliation will handle this)
#18 ,Probability_of_definitive_research = 0.5 # must be >0 and <1 (multipliation will handle the first)
#19 ,Cost_research_pilot_NETSCC =   601480 # will enter calculation of NHE in this model
#20 ,Cost_research_pilot_NHS  = 150000  # will enter calculation of NHE in this model
#21 ,Cost_research_definitive_NETSCC =  2500000 # will enter calculation of NHE in this model
#22 ,Cost_research_definitive_NHS  = 450000  # will enter calculation of NHE in this model

# MC error has entered even where there is no chance of an input changing the
# result

# asign

# asign zeros to costs of research (they do not enter into this analysis)
data[,19:22] <- NA


# example from: https://stackoverflow.com/questions/37059281/tornado-plot-in-r
#data <- matrix(c(-0.02,0.02,-0.01,0.01,-0.03,0.02,-0.01,0.04), ncol = 4)
rownames(data) <- c('+20%','-20%')                       # Amount of change in variables
colnames(data) <- c('' ,'','P_t0_U' ,'mu_t1_U' ,'sigma_t1_U' ,'mu_t2_U' ,'sigma_t2_U ','MCD_t1' ,'MCD_t2 ', 'Incidence', 
                    'Time_info' ,'Utilisation_t0' ,'Utilisation_t1' ,'Utilisation_t2' ,'D_rate' ,'Time_research_pilot' ,'Time_research_definitive' ,
                    'Probability_of_definitive_research' ,'' ,''  ,
                    '' ,'')

#colnames(data) <- c('V_bar', 'alpha', 'rho','xi')        # Names of variables
x <- seq(-0.4,0.4, length=10)                          # For plotting '%' on x-axis

#barplot(data, horiz = T, las=1, xlim = c(-0.04,0.04), xaxt='n', ylab = '',
#        beside=T, col=c('springgreen','indianred2'))
#axis(1, at=pretty(x),  lab=paste0(pretty(x) * 100," %"), las=TRUE)


# NB need a different coluour for increasing and decreasing as red and green will be 
# confused with cost effectiveness
barplot(data[1,], horiz = T, las=1, xlim = c(-0.4,0.4), xaxt='n', ylab = '',
        beside=T, col=c('springgreen'))
barplot(data[2,], horiz = T, las=1, xlim = c(-0.4,0.4), xaxt='n', ylab = '',
        beside=T, col=c('indianred2'), add = TRUE)
axis(1, at=pretty(x),  lab=paste0(pretty(x) * 100," %"), las=TRUE)





############################################
# ABSOLUTE CHANGE from base case TORNADO
############################################
# include ICERs on the graph? 





















######################
# tornado diagram for just binary outcome and MCD
##########################

# EXAMPLE #
###########
# example from: https://stackoverflow.com/questions/37059281/tornado-plot-in-r
data <- matrix(c(-0.02,0.02,-0.01,0.01,-0.03,0.02,-0.01,0.04), ncol = 4)
rownames(data) <- c('+20%','-20%')                       # Amount of change in variables
colnames(data) <- c('V_bar', 'alpha', 'rho','xi')        # Names of variables
x <- seq(-0.04,0.04, length=10)                          # For plotting '%' on x-axis

#barplot(data, horiz = T, las=1, xlim = c(-0.04,0.04), xaxt='n', ylab = '',
#        beside=T, col=c('springgreen','indianred2'))
#axis(1, at=pretty(x),  lab=paste0(pretty(x) * 100," %"), las=TRUE)

barplot(data[1,], horiz = T, las=1, xlim = c(-0.04,0.04), xaxt='n', ylab = '',
        beside=T, col=c('springgreen'))
barplot(data[2,], horiz = T, las=1, xlim = c(-0.04,0.04), xaxt='n', ylab = '',
        beside=T, col=c('indianred2'), add = TRUE)
axis(1, at=pretty(x),  lab=paste0(pretty(x) * 100," %"), las=TRUE)

# notes on example #
####################

# need to fill matrix with 
#    ncol=number of variables which are varied
#    for each variable calculate two entries: +20% and -20% and asign all of these results to a vector





# input variables:
#baseInputs # vector of the base case inputs to the sensitivity analysis 
# MUST contain inputs in the same order as the function needs them
#nInputs <- length(baseInputs) # number of inputs into the function (each will get +-20%)
# putput variables (used in the loop function)
#arguments # temporary variable which stores the inputs for one cycle of the loop where one variable
# only is increased/decreased by 20%
# output variable
#resultsVectorUp # vector containing the NHE of the scenario in which one of the variables was 
# increased by 20%
#resultsVectorDown # results where inputs reduced


# toy example
################
baseInputs <- c(0.1, 0.2)
resultsVectorUp <- rep(NA, length(baseInputs))
PilotStudyVOI2 <- function(V1, V2){
  V1 + V2 # very simple function
}

for (i in 1:length(baseInputs)){
  arguments <- baseInputs
  arguments[i] <- baseInputs[i]*1.1
  resultsVectorUp[i] <- do.call(PilotStudyVOI2 , as.list(arguments))
}



# function 2 :does basic MCD thing with pilot study
# takes 3 treatments
##############
#outputs: just NHE as information value

# note: this is for the basic cost per primary outcome model (for feasibility trial - normal trials are a special case of this)
# this is the list of arguments ot the model function in order.
# they are set at their basic values
