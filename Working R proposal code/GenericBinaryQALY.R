########################################################
##### GENERIC QALY outcome
#########################################################

UtilMatrix.fn <- function(M_t_U, Imp_cutoff, Utilisation_t){
  
  # M_t_U = the result on the outcome scale (log odds ratio) **NB needs to be a matrix!!
  # Imp_cutoff = the required result on the outcome scale
  # Utilisation_t = the current utilisation
  
  # decide when a new treatment is warrented given the implementation cutoffs 
  Nrows <- nrow(M_t_U)
  Ncols <- ncol(M_t_U)
  UtilMatrix <- matrix(rep(NA, Nrows*Ncols), nrow = Nrows, ncol = Ncols)
  for (i in 1:Nrows){
    # for each row assign a 1 to the new treatment which is above the cutoff and is the best of the new treatments
    UtilMatrix[i,] <- sapply(M_t_U[i,], function(y) ifelse(y>=Imp_cutoff[i] && y==max(M_t_U[i,]), 1, 0))
    
    if(max(M_t_U[i,])<Imp_cutoff[i]){ # if nothing is above the cutoff then just stick with current utilisation
      UtilMatrix[i,] <- Utilisation_t
    }
  }
  
  UtilMatrix # output
  
}

# Implementation adjusted EVTPI using the using 
##################################
# input: the implementation matrix for each simulation (from above)
# net benefit for each simulation

EVTPIadjImp.fn <- function(UtilMatrix, NB_t){
  EVTPIadjImp <- mean(apply(UtilMatrix*NB_t, 1, sum))
  EVTPIadjImp
}





# basic population function
#############################
# add delay!

PopWdelay.fn <- function(Incidence, D_rate, Time_research, Time_info, Time_delay = 0){
  
  #                                        time end                time start  
  Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
  
  Time_delay_adj <- ifelse(Time_delay < Time_info, Time_delay, Time_info )  # need to make sure the delay is not longer than Time_info
  Pop_delay <- (Incidence/-D_rate) * (exp(-D_rate*Time_delay_adj) - exp(-D_rate*0))
  
  Time_research_end_adj <- ifelse(Time_delay_adj + Time_research > Time_info, 
                                  Time_info,
                                  Time_delay_adj + Time_research)
  
  Pop_during_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research_end_adj) - exp(-D_rate*Time_delay_adj))
  
  
  Pop_after_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research_end_adj))
  
  output <- list(Pop_total = Pop_total,
                 Pop_delay = Pop_delay,
                 Pop_during_research = Pop_during_research,
                 Pop_after_research = Pop_after_research)
  
  return(output)
}

#pops <- PopWdelay.fn(Incidence =  355000*0.73 ,
#             Time_info =  15   ,
#             D_rate =  0.035,
#             Time_research =  3)
#
#pops$Pop_total - pops$Pop_delay - pops$Pop_during_research - pops$Pop_after_research


######################
# P4 QALY inputs
#####################
MCouter <- 10 # NOTE not bad convergence of EVTPI at MCouter = 40000
library(fdrtool)
P_t0_U <- 0.30
Odds_t0_U <- P_t0_U / (1 - P_t0_U)
LO_t0_U <- log(Odds_t0_U)
# halfnormal simulations on LOR 
sigma_t1_U <- 0.5
LOR_t1_U <- rhalfnorm(MCouter, theta=sd2theta(sigma_t1_U))
#plot(density(LOR_t1_U))
LO_t1_U <- LO_t0_U + LOR_t1_U 
Odds_t1_U <- exp(LO_t1_U)
P_t1_U <- Odds_t1_U / (Odds_t1_U + 1) 
P_t_U <- matrix(c(rep(P_t0_U,MCouter), P_t1_U), ncol = 2)

INB_Event = (3000 - 2107.5)/15000 # INB event always assumes a k value
C_t0 <- 0
booklet_cost <- 4104/453
C_t1 <- booklet_cost
C_t <- c(C_t0, C_t1)  # required input! vector of costs for each treatment
Utilisation_t <- c(1,0)
Incidence =  355000*0.73 
Time_info =  15   
D_rate =  0.035 
k = 15000 
Time_research =  3
Cost_research_funder =  882177
Cost_research_system = 4104
MCD_t <- c(0)


GenericBinaryQALY.v1(P_t_U ,INB_Event,k,C_t, MCD_t , Incidence, 
                     Time_info ,Utilisation_t,D_rate ,Time_research ,
                     Cost_research_funder,Cost_research_system, UtilMatrix = NA, Time_delay = 0)


GenericBinaryQALY.v1 <- function(P_t_U ,INB_Event,k,C_t, MCD_t , Incidence, 
                                    Time_info ,Utilisation_t,D_rate ,Time_research ,
                                    Cost_research_funder,Cost_research_system, UtilMatrix = NA, Time_delay = 0){
  
  
  # Put together mini economic model
  #########################################
  
  nt <- dim(P_t_U)[2] # the number of treatment implied by the input matrix
  nsim <- dim(P_t_U)[1]
  
  NB_t_U <- P_t_U*INB_Event # multiply every element by INB_Event (1st step in converting to NB)
  NB_t_U <- NB_t_U - rep(C_t/k, each = nsim) # subtract the QALY loss from increased costs
  
  addMCD_t <- c(0, MCD_t*INB_Event)   # add the MCD to each column in the vector to convert to net benefit
  NB_t_U <- NB_t_U + rep(addMCD_t, each = nsim)
  
  # trial inputs
  ########################
  
  # expected outcome with each treatment (uninformed prior)
  ENB_t_U <- apply(NB_t_U, 2, mean)
  
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
  
  #############################################
  # population 
  Popoutputs <- PopWdelay.fn(Incidence, D_rate, Time_research, Time_info, Time_delay)
  
  Pop_delay <- Popoutputs$Pop_delay
  Pop_during_research <- Popoutputs$Pop_during_research 
  Pop_after_research <- Popoutputs$Pop_after_research 
  Pop_total <- Popoutputs$Pop_total
  
  
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
  
  
  ######## ADJUSTING TRIAL ANALYSIS FOR IMPLEMENTATION ###################
  # uses implementation matrix which encodes clinican behavour in response to particular trial results
  
  EVTPIadjImp <- ifelse(is.na(UtilMatrix), NA, EVTPIadjImp.fn(UtilMatrix, NB_t_U))  
  
  # calculating the benefits of research (under differnt assumptions)
  ########################################
  
  # cu = current utilisation. NOT instant trial - while trial is running just keep whatever treatment 
  # utilisation is theere at the start of the trial
  # Below is the same as OIR if the use of the new treatment is restricted
  NB_cu_perfect_info_imp <- sum(ENB_t_U*Utilisation_t*Pop_during_research) + 
                            Pop_after_research*NB_EVTPI_U -
                            Cost_research_system/k
  
  NB_cu_perfect_info_stat_sig <- sum(ENB_t_U*Utilisation_t*Pop_during_research) + 
                                 Pop_after_research*EVTPIadjImp -
                                 Cost_research_system/k
  
  # maxt = use the best treatmet according to current NB. NOT instant trial - 
  # instantly and perfectly implement best treatment while trial is running 
  # below is the same as AWR if the new treatment is the best with current information
  NB_maxt_perfect_info_imp <- Pop_during_research*NB_EVTCI_U + 
                              Pop_after_research*NB_EVTPI_U -
                              Cost_research_system/k
  
  NB_maxt_perfect_info_stat_sig <-Pop_during_research*NB_EVTCI_U + 
                                  Pop_after_research*EVTPIadjImp - 
                                  Cost_research_system/k
  
  # this is the pure information value under different types of research and implementation assumptions
  Value_of_cu_perfect_info_imp <- NB_cu_perfect_info_imp - Cell_C
  Value_of_cu_perfect_info_stat_sig <- NB_cu_perfect_info_stat_sig - Cell_C
  Value_of_maxt_perfect_info_imp <- NB_maxt_perfect_info_imp - Cell_C
  Value_of_maxt_perfect_info_stat_sig <- NB_maxt_perfect_info_stat_sig - Cell_C
  
  
  # ICER of research relative to early access (assumed to be costless to the agency)
  # all other costs assumed to be captured by the MCD
  ICER_instant_research_perfect_info_imp <- Cost_research_funder/(Max_value_of_research)
 
  ICER_cu_perfect_info_imp <- Cost_research_funder/Value_of_cu_perfect_info_imp
  ICER_cu_perfect_info_stat_sig <- Cost_research_funder/Value_of_cu_perfect_info_stat_sig
  ICER_maxt_perfect_info_imp <- Cost_research_funder/Value_of_maxt_perfect_info_imp
  ICER_maxt_perfect_info_stat_sig <- Cost_research_funder/Value_of_maxt_perfect_info_stat_sig
  
  # ratio of research vs general health system QALYs

  QALY_research_vs_system_spend_maxt_perfect_info_imp <- Value_of_maxt_perfect_info_imp*k/Cost_research_funder
  
  VOIoutputs <- list(
    Optimal_t = Optimal_t,
    Value_of_trial_per_year = Value_of_trial_per_year,
    Value_of_implementation_per_year = Value_of_implementation_per_year,
    Probability_t_is_max = Probability_t_is_max,
    Cell_A = Cell_A,
    Cell_C = Cell_C,
    Cell_D = Cell_D,
    Max_value_of_implementation = Max_value_of_implementation,
    Max_value_of_research = Max_value_of_research,
    
    NB_cu_perfect_info_imp =NB_cu_perfect_info_imp,
    NB_cu_perfect_info_stat_sig =NB_cu_perfect_info_stat_sig,
    
    NB_maxt_perfect_info_imp =NB_maxt_perfect_info_imp,
    NB_maxt_perfect_info_stat_sig =NB_maxt_perfect_info_stat_sig,
    
    Value_of_cu_perfect_info_imp =Value_of_cu_perfect_info_imp,
    Value_of_cu_perfect_info_stat_sig =Value_of_cu_perfect_info_stat_sig,
    Value_of_maxt_perfect_info_imp =Value_of_maxt_perfect_info_imp,
    Value_of_maxt_perfect_info_stat_sig =Value_of_maxt_perfect_info_stat_sig,
    
    ICER_instant_research_perfect_info_imp =ICER_instant_research_perfect_info_imp,
    ICER_cu_perfect_info_imp =ICER_cu_perfect_info_imp,
    ICER_cu_perfect_info_stat_sig =ICER_cu_perfect_info_stat_sig,
    ICER_maxt_perfect_info_imp =ICER_maxt_perfect_info_imp,
    ICER_maxt_perfect_info_stat_sig =ICER_maxt_perfect_info_stat_sig,
    
    QALY_research_vs_system_spend_maxt_perfect_info_imp = QALY_research_vs_system_spend_maxt_perfect_info_imp
    
    
  )
  return(VOIoutputs)
  
}



















