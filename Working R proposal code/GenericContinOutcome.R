##############################
# GENERIC continuous outcome model 
###############################

#########################
# *************** results in GenericBinaryResults.R **** NB ****



# takes Delta_t_U (matrix) as an input 
# do NOT allow for pilot study yet
# allow for multiple treatments
# allow MCD
# only EVPI
# does not (and cannot) take account of NHS costs of research - need a different model for this

# PROs and CONs
# PRO: can take the output of evidence synthesis with all correlations etc
# CON: cannot easily extend to EVSI


# OUTPUTS
# NB Implementation value (benefit of early adoption)
# NB Information value (benefit of research)
# ICER total value (NETSCC cost per primary outcome)
# ICER information value (NETSCC cost per primary outcome)
# 

#INPUTS in order and comments on them
#
# Delta_t_U (matrix) - number of rows = number of sims , number of columns = number of treatmetns
# Benefit <- TRUE    #  
#
# MCD_t <- vector of MCDs for each treatment that is not the baseline
# MCD_t <- c(0.07, 0)
# 
# Incidence = 1563 must be greater than zero
# Time_info = 15 must be greater than zero 
# 
# Utilisation_t <- c(1, 0, 0) # utilisation for each treatment must all sum to 1 and none >1 <0
# 
# D_rate =  0.035 
# 
# Time_research # must be greater than zero, if less than zero then information value = zero
# Cost_research_funder =  2500000 # must be greater than zero

# relevant?
# base case inputs in order, for 
# remember do not vary the first two
# baseInputs <- c(20000,TRUE,0.37 ,0,0.5,0,0.5,0.07,0, 1563,15,1 ,0 ,0 ,0.035 ,2, 6, 0.5, 601480 ,150000 , 2500000 ,450000)

# inputs for alzheimers proposal

#########################
# health system model inputs (some may not be needed)
Utilisation_t <- c(1, 0, 0, 0)
Incidence =  100000 # from proposal (not referenced!)
# number diagnosed with AD each year
Time_info =  20 # assuming the area moves "very slowly" 
D_rate =  0.035 
k = 15000 
Time_info = 20
Time_research = 6 
Cost_research_funder =  3310883


# epi inputs
P_t0_U <- 0 # by definition
P_t1_U <- 1 / (200 +2)
P_t2_U <- 1 / (200 +2)
P_t3_U <-  1 / (200 +2)

# change in MMSE decline
# model change in delcine (not absolute level)


# choosing the correct sigma
################################

# distibution of reduction in delcline (benefit)
# mean is zero (no reduction in delcine - same as baseline)
# choose sigma s.t. P_t1_U % of distribution lies above 1.4
# this is equivalent to 
1 - P_t1_U # below 1.4

# find sigma t
findsigma = function(sigma_star){
  #% of dist below 1.4 with sigma_star - the required % of dist below 1.4
  return(abs(pnorm(1.4, 0, sigma_star) - 0.9950495))
}

result = optimize(findsigma, interval = c(0, 5))
print(result$minimum)


# check result
sigma_star <- 0.5427811
pnorm(1.4, 0, sigma_star)

# looks correct
plot(density(rnorm(999999, 0, sigma_star)), main="Probability of MMSE decline", xlab="Decline in MMSE")
abline(v = 1.4)
text(2, 0.05, "0.5%")



#**could generalise to allow for sigma_t0?

MCouter <- 10 # NOTE not bad convergence of EVTPI at MCouter = 40000
sigma_t1 <- sigma_star
sigma_t2 <- sigma_star
sigma_t3 <- sigma_star


Delta_t1 <- rnorm(MCouter, 0, sigma_t1)
Delta_t2 <- rnorm(MCouter, 0, sigma_t2)
Delta_t3 <- rnorm(MCouter, 0, sigma_t3)

# equivalent to Delta_t_U matrix
Delta_t_U <- matrix(c(rep(0,MCouter),Delta_t1, Delta_t2, Delta_t3), ncol = 4)


# MCD inputs
# could potentially incorporate this as 1.4
MCD_t <- c(0, 0, 0) # remember MCD is a relative input - if two treatmetns => just one MCD

Benefit = TRUE


#* include implementation functions
UtilMatrix <- NA

########################
# supporting functions 
#########################



#* extend to allow for delay in funding

# basic population function
#############################

verybasicPop.fn <- function(Incidence, D_rate, Time_research, Time_info){
  
  #                                        time end                time start  
  Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
  Pop_during_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
  Pop_after_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))
  
  output <- list(Pop_total = Pop_total,
                 Pop_during_research = Pop_during_research,
                 Pop_after_research = Pop_after_research)
  
  return(output)
}



# mock data
#Delta_t_U <- matrix(c(rep(0.3, 10), rnorm(10, 0.3, 0.05), rnorm(10, 0.3, 0.05)), ncol = 3)
#MCD_t <- c(0.3, 0.4)
#Benefit = TRUE

#n = 670 # proper value
#Imp_cutoff <- 0.32
# Incidence = 1000

# MAIN function   
# good chance there is a bug in $Probability_t_is_max when the primary outcome is a harm


GenericContinOutcome.v1 <- function(Delta_t_U ,Benefit,MCD_t , Incidence, 
                                    Time_info ,Utilisation_t,D_rate ,Time_research ,
                                    Cost_research_funder,UtilMatrix){
  
  
  # Put together mini economic model
  #########################################
  
  INB_Event <- ifelse(Benefit==1, 1, -1)
  
  nt <- dim(Delta_t_U)[2] # the number of treatment implied by the input matrix
  nsim <- dim(Delta_t_U)[1]
  
  # convert Deltas to "net benefits" with MCD and benefit/harm
  NB_t_U <- Delta_t_U*INB_Event # multiply every element by INB_Event (1st step in converting to NB)
  
  addMCD_t <- c(0, MCD_t)   # add the MCD to each column in the vector to convert to net benefit
  NB_t_U <- NB_t_U + rep(addMCD_t, each = nsim)
  
  # each column now represents simulations of the NB of each treatment
  
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
  
  Popoutputs <- verybasicPop.fn(Incidence, D_rate, Time_research, Time_info)
  
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
       xlab = "Units of outcome",
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
  
  EVTPIadjImp <- NA #   EVTPIadjImp.fn(UtilMatrix, NB_t_U)
  
  # calculating the benefits of research (under differnt assumptions)
  ########################################
  # perfect info and perfect implementation (includes that it is instant)
  NB_instant_research_perfect_info_imp <- Cell_D
  # only implement the results of the research if they are statistically significant (not considering costs!)
  NB_instant_research_perfect_info_stat_sig <- Pop_total*EVTPIadjImp 
  
  # cu = current utilisation. NOT instant trial - while trial is running just keep whatever treatment 
  # utilisation is theere at the start of the trial
  # Below is the same as OIR if the use of the new treatment is restricted
  NB_cu_perfect_info_imp <- sum(ENB_t_U*Utilisation_t*Pop_during_research) + Pop_after_research*NB_EVTPI_U 
  NB_cu_perfect_info_stat_sig <- sum(ENB_t_U*Utilisation_t*Pop_during_research) + Pop_after_research*EVTPIadjImp
  
  # maxt = use the best treatmet according to current NB. NOT instant trial - 
  # instantly and perfectly implement best treatment while trial is running 
  # below is the same as AWR if the new treatment is the best with current information
  NB_maxt_perfect_info_imp <- Pop_during_research*NB_EVTCI_U + Pop_after_research*NB_EVTPI_U
  NB_maxt_perfect_info_stat_sig <-Pop_during_research*NB_EVTCI_U + Pop_after_research*EVTPIadjImp
  
  # this is the pure information value under different types of research and implementation assumptions
  Value_of_instant_research_perfect_info_imp <- NB_instant_research_perfect_info_imp - Cell_C
  Value_of_instant_research_perfect_info_stat_sig <- NB_instant_research_perfect_info_stat_sig - Cell_C
  Value_of_cu_perfect_info_imp <- NB_cu_perfect_info_imp - Cell_C
  Value_of_cu_perfect_info_stat_sig <- NB_cu_perfect_info_stat_sig - Cell_C
  Value_of_maxt_perfect_info_imp <- NB_maxt_perfect_info_imp - Cell_C
  Value_of_maxt_perfect_info_stat_sig <- NB_maxt_perfect_info_stat_sig - Cell_C
  
  
  # ICER of research relative to early access (assumed to be costless to the agency)
  # all other costs assumed to be captured by the MCD
  ICER_instant_research_perfect_info_imp <- Cost_research_funder/Value_of_instant_research_perfect_info_imp
  ICER_instant_research_perfect_info_stat_sig <- Cost_research_funder/Value_of_instant_research_perfect_info_stat_sig
  ICER_cu_perfect_info_imp <- Cost_research_funder/Value_of_cu_perfect_info_imp
  ICER_cu_perfect_info_stat_sig <- Cost_research_funder/Value_of_cu_perfect_info_stat_sig
  ICER_maxt_perfect_info_imp <- Cost_research_funder/Value_of_maxt_perfect_info_imp
  ICER_maxt_perfect_info_stat_sig <- Cost_research_funder/Value_of_maxt_perfect_info_stat_sig
  
  
  
  
  
  #Total_NB_research <- Cell_D -  Cell_A
  
  # % of total value which is implementation value
  #Perc_implementation_NB <- ( Cell_C -  Cell_A)/( Cell_D -  Cell_A)*100
  # % of total value which is pure information value
  #Perc_information_NB <- ( Cell_D -  Cell_C)/( Cell_D -  Cell_A)*100
  # max value of implementation (early access) : Cell_C - Cell_A
  # max value of research : Cell_D - Cell_A
  
  
  
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
    NB_instant_research_perfect_info_imp =NB_instant_research_perfect_info_imp,
    
    NB_instant_research_perfect_info_stat_sig =NB_instant_research_perfect_info_stat_sig,
    
    NB_cu_perfect_info_imp =NB_cu_perfect_info_imp,
    NB_cu_perfect_info_stat_sig =NB_cu_perfect_info_stat_sig,
    
    NB_maxt_perfect_info_imp =NB_maxt_perfect_info_imp,
    NB_maxt_perfect_info_stat_sig =NB_maxt_perfect_info_stat_sig,
    
    Value_of_instant_research_perfect_info_imp =Value_of_instant_research_perfect_info_imp,
    Value_of_instant_research_perfect_info_stat_sig =Value_of_instant_research_perfect_info_stat_sig,
    Value_of_cu_perfect_info_imp =Value_of_cu_perfect_info_imp,
    Value_of_cu_perfect_info_stat_sig =Value_of_cu_perfect_info_stat_sig,
    Value_of_maxt_perfect_info_imp =Value_of_maxt_perfect_info_imp,
    Value_of_maxt_perfect_info_stat_sig =Value_of_maxt_perfect_info_stat_sig,
    
    ICER_instant_research_perfect_info_imp =ICER_instant_research_perfect_info_imp,
    ICER_instant_research_perfect_info_stat_sig =ICER_instant_research_perfect_info_stat_sig,
    ICER_cu_perfect_info_imp =ICER_cu_perfect_info_imp,
    ICER_cu_perfect_info_stat_sig =ICER_cu_perfect_info_stat_sig,
    ICER_maxt_perfect_info_imp =ICER_maxt_perfect_info_imp,
    ICER_maxt_perfect_info_stat_sig =ICER_maxt_perfect_info_stat_sig
    
    
  )
  return(VOIoutputs)
  
}






















# implementation function
##########################
# stat sig decision rule


# inputs: M_t_U = matrix of inputs for each treatment
# Imp_cutoff = probability cutoff for utilisation
# Utilisation_t = current utiliasation

# output: an implementation matrix indicating the proportion of patients recieving the 
# treatment given a cutoff, observed results and behavioural assumptions about doctors

# ***include consideration of whether the outcome is a benefit or a harm!! ****
# ***function currently designed for outcome as a benefit!
# ***incorporate probability of new treatment being used : sum(P5_UtilMatrix[,2])/length(LOR_t1_U)
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

# mock data
#Delta_t_U <- matrix(c(rep(0.3, 10), rnorm(10, 0.3, 0.05), rnorm(10, 0.3, 0.05)), ncol = 3)
#MCD_t <- c(0.3, 0.4)
#Benefit = TRUE
#Utilisation_t <- c(1, 0, 0)
#n = 670 # proper value
# ##### hmmmm MCD_cutoff <- 0.32
# Incidence = 1000

# MCD decision rule
#####################
# current treatment probability known with certainty (possibly can relax this easy)

UtilMatrixMCD.fn <- function(Delta_t_U, MCD_cutoff, Utilisation_t){
  
  # Delta_t_U = the "true" result on the probability scale for base and new treatment **NB needs to be a matrix!!
  # MCD_cutoff = the required difference on the probability scale
  # Utilisation_t = the current utilisation
  
  # decide when a new treatment is warrented given the implementation cutoffs 
  Nrows <- nrow(Delta_t_U)
  Ncols <- ncol(Delta_t_U)
  UtilMatrix <- matrix(rep(NA, Nrows*Ncols), nrow = Nrows, ncol = Ncols)
  for (i in 1:Nrows){
    # for each row assign a 1 to the new treatment which is above the cutoff and is the best of the new treatments
    UtilMatrix[i,] <- sapply(Delta_t_U[i,], function(y) ifelse(y>=MCD_cutoff[i] && y==max(Delta_t_U[i,]), 1, 0))
    
    if(max(Delta_t_U[i,])<MCD_cutoff[i]){ # if nothing is above the cutoff then just stick with current utilisation
      UtilMatrix[i,] <- Utilisation_t
    }
  }
  
  UtilMatrix # output
  
}




# mock data for working on function (inputs above)
#M_t_U <- cbind(rep(0, length(LOR_t1_U)), LOR_t1_U)
#Imp_cutoff <- SE_U
#Utilisation_t <- c(1,0)


# test the implementation function
#utilisation_matrix <- UtilMatrix.fn(Delta_t_U, rep(0.32, MCouter), c(1,0))

# stat sig analysis
# utilisation matrix for P4 analysis
# 
#n = 670 # unbalanced trial 
#n_t0 <- 227
#n_t1 <- 453

#a <- P_t1_U*n_t1 # number of events in the treatment arm
#b <- (1 - P_t1_U)*n_t1 # number of non-events in the treatment arm
#c <- rep(P_t0_U*n_t0, MCouter) # number of events in the control arm
#d <- rep((1 - P_t0_U)*n_t0, MCouter) # number of non-events in the control arm

#SE_U <- sqrt(1/a + 1/b + 1/c + 1/d) # standard error for each simulation

# rule: implement (imp = 1) if LOR_t1 stat sig > zero  (else inp = 0) 
# i.e. stat sig increase the number of home deaths in a trial of this size
# i.e. implement if the result in the trial itself is stat sig (on its own) 
# REGARDLESS OF NET BENEFIT 
# note: use the 97.5% percentile to represent a 2.5% two sided test?

#Utilisation_t <- c(1, 0)

# 1st argument: inputs for decision rule - matrix of log odds ratios for each of the treatments
# 2nd arg: inputs for the decision rule - new treatment must be greater than this value for it to be implemented
# 3rd arg: starting utilisation (all using current treatment)
#UtilMatrix <- UtilMatrix.fn(cbind(rep(0, length(LOR_t1_U)), LOR_t1_U), SE_U*qnorm(0.975), Utilisation_t)

#summary(UtilMatrix)





# Implementation adjusted EVTPI using the using 
##################################
# input: the implementation matrix for each simulation (from above)
# net benefit for each simulation

EVTPIadjImp.fn <- function(UtilMatrix, NB_t){
  EVTPIadjImp <- mean(apply(UtilMatrix*NB_t, 1, sum))
  EVTPIadjImp
}

# test EVTPIadjImp.fn
#EVTPIadjImp.fn(utilisation_matrix, Delta_t_U)
# unadjusted EVTPI
#mean(apply(Delta_t_U, 1, max))



#########################################################################################



# simulation model thing
# output Delta_t_U matrix (input to the above model)

# P_t0_U <- 0.37    # bounded by 0 and 1 (should be done in program somewhere)
# vector of mu_t_U i.e. means of each non baseline treatment prior (on log odds scale)
# mu_t_U <- c(0, 0) in this case (as there are three treatments), two of these reqires two sigma_t_U
# vectorof sigma_t_U corresponding vector of sigmas for the treatments
# sigma_t_U <- c(0.5, 0.5) must be >0