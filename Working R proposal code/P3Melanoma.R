##############################
# Proposal 3: Melanoma
##############################

set.seed(20)
options(scipen=999) # trun off scientifc notation (for convergence axis)


######################################################################################################
##### INPUTS


###################
# simulation resoultion
MCouter <- 40000 # NOTE not bad convergence of EVTPI at MCouter = 40000
MCinner <- 4000 
# MCinner <- 10000 # m simulations for each inner loop
ptm <- proc.time() # start the clock



#########################
# health system model inputs (some may not be needed)
Utilisation =  0  
Incidence =  1137 #(NICE budget impact statements) 
#Prevalence =  # ignore for now!
Time_info =  10   
D_rate =  0.035 
k = 15000 
#Time_research =  6
#Time_delay_informprior = 0.5 # set later in code: do for two options: 6 months (0.5 ) and 2 years (2)
Cost_research_NETSCC =  2522710
#Cost_research_NHS = -62410967

P_t0_U <- 0.51

Time_research = 6


# NHS research costs allocated equally over 10 years
Cost_research_NHS_yr <- 62410967/10
Cost_research_NHS <- (Cost_research_NHS_yr/-D_rate) * (exp(-D_rate*10) - exp(-D_rate*0))
# QALY impact of research costs
Cost_research_NHS/15000
# NETSCC cost per QALY lost
Cost_research_NETSCC/(Cost_research_NHS/15000)



62410967/15000




#################################
# NATURAL OUTCOME - NOT INCLUDING SECONDARY OUTCOMES!!!
##################################



# events per year for each treatment
E_NB_t0_U_nat*Incidence
E_NB_t1_U_nat*Incidence




#################################
# Cost calculations
##################################

# difference in costs
# use markov to convert 2 year PFS to mean time to transition

# notes on function 
####################
# based on Toronto course markov model
#perc_in_state : % in the state you care about
#at_time : % in the state you care about at time X (in months)
#numb_cycles: how long to run the markov 
# model assumes constant risk over time! 
# alternative formula possible - analytic matrix solution:
# https://www.researchgate.net/profile/J_Beck3/publication/16527320_The_Markov_Process_in_Medical_Prognosis/links/53fb3ec40cf20a4549705c5b.pdf
# the initial part assumes constant risks over time (prob of transitioning from pre prog to death or post prog)
# implicity runs the markov to infinity

expected_time_in_state2 <- function(perc_in_state, at_time){
  
  # model as prob of leaving state
  #perc_in_state = 0.37
  prob_leave_state <- 1 - perc_in_state # probability of not being the the start state when time = at_time
  
  rate_leave <- -log(1-prob_leave_state )/at_time
  # one month probability of leaving starting state (transition from state S to state not S)
  p.S_notS <- 1 - exp(-rate_leave)
  p.S_notS
  
  # based on p431 of 
  # https://www.researchgate.net/profile/J_Beck3/publication/16527320_The_Markov_Process_in_Medical_Prognosis/links/53fb3ec40cf20a4549705c5b.pdf
  # The "matrix" Q you are interested in
  p.S_S <- 1- p.S_notS
  
  # fundamental matrix (N) = the inverse of (I - Q), where I is identity and Q is the matrix
  # you are interested in
  #
  solve(1 - p.S_S)
  # this tells you how many cycles you will spend in the state S 
  
}

#expected_time_in_state2(perc_in_state = )

mean_time_preProg_t0 <- expected_time_in_state2(perc_in_state = 0.51, at_time = 24)
mean_time_preProg_t0
# people spend 33.5 months on average for progression free
# (note: median around 25 months according to this)

# Intensive treatment - per person costs
Drug_cost_month <- 145000/24

# average cost per person
mean_time_preProg_t0*Drug_cost_month


# conservative treatmetn - per person costs
12*145000/24


# additinal months of treatment
(mean_time_preProg_t0 - 12) # *Drug_cost_month

# additional costs of intensive treatment 
mean_time_preProg_t0*Drug_cost_month  - 12*145000/24
# per year 
(mean_time_preProg_t0*Drug_cost_month  - 12*145000/24)*Incidence

# in general purpose file!
# CONTINUOUS ACCRUAL AND DISCOUNTING FORMULA
#####################################
# for costs and populations
# NB inputs in YEARS!!!
# if something builds up steadly over time (like population)
# put in yearly accrual (incidence)
# and this extends it for the required number of YEARS and
# discounts appropriately
contin_accrual_disc <- function(D_rate, accrual_per_year, years, start_year = 0){
  (accrual_per_year/-D_rate) * (exp(-D_rate*years) - exp(-D_rate*start_year))
}

# average cost per person (discounted)
contin_accrual_disc(D_rate, Drug_cost_month*12, mean_time_preProg_t0/12)


### Costs and consequences table
##########

# expected yearly additional costs of using control treatment
mean_time_preProg_t0*Drug_cost_month*Incidence


# 2 year PFS for current treatment 
P_t0_U*Incidence

# non-inferiority according to proposal
(0.51 - 0.079)*Incidence

# this implies a cost per additinal 2 year PFS of

mean_time_preProg_t0*Drug_cost_month*Incidence/((0.51 - 0.079)*Incidence)

# equivalent spent else where in health system

(mean_time_preProg_t0*Drug_cost_month*Incidence/((0.51 - 0.079)*Incidence))/k













#####################
# START MARKOV approach
#####################

# Both treatments: transition probability for death from natrual causes
########################################################
# http://mfne.org/learn-about-melanoma/facts-about-melanoma-and-skin-cancer/
start_age <- 53 # average age of diagnosis 52 then we assume they are started on drugs and 
# randomised at year 1
# from NICE proposal: "The model adopted a lifetime time horizon of 40 years"
# from life tables (averaged tps from CHE advanced course for men and women for everyone over 55)
p.Pre_Dead_yr <- 0.0681 
rate.Pre_Dead_m <- -log(1 - p.Pre_Dead_yr)/12 #montly rate of death
p.Pre_Dead <- 1 - exp(-rate.Pre_Dead_m) # monthly transition probability


# Both treatments: time from post progression to death (same for both treatments)
#################################
# from KEYNOTE-002 (Ribas 2015) and 
# follow up to KEYNOTE-002: http://oncologypro.esmo.org/Meeting-Resources/ESMO-2016/Final-overall-survival-for-KEYNOTE-002-pembrolizumab-pembro-versus-investigator-choice-chemotherapy-chemo-for-ipilimumab-ipi-refractory-melanoma
# 24m OS 37% (from follow up)
E_months_alive <- expected_time_in_state2(perc_in_state = 0.37, at_time=24)
E_months_alive
# 6m PF 36% (from KEYNOTE-002)
E_months_PF <- expected_time_in_state2(perc_in_state = 0.36, at_time=6)
E_months_PF
# time between progression and death
E_months_alive - E_months_PF

# transition probability associated with this time 
# major fudge! just keep trying values untill get correct answer!
# 
X = 0.258
perc_in_state <- X
at_time = 24

prob_leave_state <- 1 - perc_in_state # probability of not being the the start state when time = at_time
rate_leave <- -log(1-prob_leave_state )/at_time
# one month probability of leaving starting state (transition from state S to state not S)
p.S_notS <- 1 - exp(-rate_leave)
p.S_notS

expected_time_in_state2(perc_in_state = X, at_time = 24) 
# choose X (and p.S_notS) such that expected_time_in_state = E_months_alive - E_months_PF
# expected_time_in_state = 18.2 months
#p.S_notS is the answer I need
# answer p.S_notS = 0.05488609 => expected_time_in_state = 18.2 months


# t0 specific inputs
#####################

# t0: TP Pre -> Post (stable to progressed)
###################
# t0 monthly rate for transition out of pre progression
rate_m_t0 <- -(log(1- P_t0_U  ))/24  # 24 as 24 months in 2 years 
# monthly TP for transition out of pre progression
tp_m_t0 <- 1 - exp(- rate_m_t0)
# t0 yearly rate
#rate_yr_t0 <- -(log(1- P_t0_U  ))/2  #  2 years 
# yearly TP
#tp_yr_t0 <- 1 - exp(- rate_yr_t0)
# monthly probability of staying in pre progression 
1 - tp_m_t0



##########################################
# transition probabilities for t0 markov
##########################################
# all monthly
p.SS_t0 <- 1 - tp_m_t0
p.SD_t0 <- p.Pre_Dead # described above (prob from stable to dead), same for both treatments
p.SP_t0 <- 1 - p.SS_t0 - p.SD_t0 # defined above (probability from stable to progressed)
p.PD_t0 <- p.S_notS # same for both treatments



# work out inputs for t1 markov
####################################
# Uninformed prior on Log hazard ratio

library(fdrtool) # to give half normal distribution

sigma_t1_U = 0.5 #sd if the model was a normal gaussian distribution

#plot(density(-rhalfnorm(MCouter, theta=sd2theta(sigma_t1_U))))
#plot(density(rnorm(MCouter, 0, 0.5)))
# "very uncertain prior"
# additional chance of leaving pre progressed state
log_h_ratio_t1 <- rhalfnorm(MCouter, theta=sd2theta(sigma_t1_U))
h_ratio_t1 <- exp(log_h_ratio_t1) # to compare to Bennette paper 
log_rate_m_t0 <- log(rate_m_t0)

log_rate_m_t1 <- log_rate_m_t0 + log_h_ratio_t1
rate_m_t1 <- exp(log_rate_m_t1)

tp_m_t1 <- 1 - exp(- rate_m_t1)  # probabilties of levaing pre progression for t1

#plot(density(h_ratio_t1))
#plot(density(tp_m_t1))
#plot(density(rate_m_t1))
#abline(v = rate_m_t0)
summary(tp_m_t1)

# transition probs of staying in pre progressed 
1 - tp_m_t1

summary(1 - tp_m_t1)
plot(density(1 - tp_m_t1))
sd(1 - tp_m_t1)

##########################################
# transition probabilities for t1 markov
##########################################
# all monthly
p.SS_t1 <- 1 - tp_m_t1
p.SD_t1 <- p.Pre_Dead #  same for both treatments
p.SP_t1 <- 1 - p.SS_t1 - p.SD_t1 # defined above (probability from stable to progressed)
p.PD_t1 <- p.S_notS  # same for both treatments

summary(p.SS_t1)
summary(p.SP_t1)
########################
# Markov model
####################
# notes:
# 3 states: stable (pre progress), progressed, dead
# monthly cycles 
# stable = pre prog
# strictly speaking the markov structure here is wastefull in the t0 case as there is
# nothing uncertain in the t0 model!

###############################################################################################

n.t <- 40*12         # number of cycles: run for 40 years (like in NICE guidance)
v.n <- c("stable", "progressed", "dead")  # state names
n.s<- length(v.n)                            # number of states
nsim <- length(tp_m_t1) #the only uncertain quantity!

###############
# MARKOV old treatment: t0 (treat to progression)
###############

p.SD <- p.SD_t0 
p.SP <- p.SP_t0 
p.PD <- p.PD_t0 

# cost of being in stable states
c.S  <- 100         
c.P  <- 300 + 6000 # cost of care + cost of drug                
c.D  <- 0

u.S  <- 0.79065/12              # utility (per month) of being in stable state (pre prog) - from NICE
u.P  <- 0.71655/12              # utility (per month) of being in the progressed state - from NICE
u.D  <- 0

S <- P <- D <- matrix(0, ncol=n.t, nrow =  nsim,
                      dimnames = list(1:nsim, 1:n.t))  #initialize State matrices

S[,1] <- 1  # assign every proportion across the simulations to the stable state
for (t in 2:n.t)
{
  S[, t] <- S[ ,t - 1] * (1 - p.SD - p.SP)                       # calulate the prop of cohort in S
  P[, t] <- P[ ,t - 1] * (1 - p.PD) + S[ ,t -1] * p.SP           # calulate the prop of cohort in P
  D[, t] <- D[ ,t - 1] + S[ ,t - 1] * p.SD + P[ ,t - 1] * p.PD   # calulate the prop of cohort in D
}

TE_t0 <- rowSums(u.S * S + u.P * P + u.D * D)    # total QALYs for all simulations
TC_t0 <- rowSums(c.S * S  + c.P * P + c.D * D)    # total cost for all simulations

E_TE_t0 <- mean(TE_t0)
E_TC_t0 <- mean(TC_t0)




###############
# MARKOV new treatment: t1 (treat for 12 months)
###############

p.SD <- p.SD_t1 # described above (prob from stable to dead)
p.SP <- p.SP_t1 # defined above (probability from stable to progressed)
p.PD <- p.PD_t1 # see above 

# cost of being in stable states
c.S  <- 100         
c.P  <- 300  # cost of care + cost of drug                
c.D  <- 0

u.S  <- 0.79065/12              # utility (per month) of being in stable state (pre prog) - from NICE
u.P  <- 0.71655/12              # utility (per month) of being in the progressed state - from NICE
u.D  <- 0

S <- P <- D <- matrix(0, ncol=n.t, nrow =  nsim,
                      dimnames = list(1:nsim, 1:n.t))  #initialize State matrices

S[,1] <- 1  # assign every proportion across the simulations to the stable state
for (t in 2:n.t)
{
  S[, t] <- S[ ,t - 1] * (1 - p.SD - p.SP)                       # calulate the prop of cohort in S
  P[, t] <- P[ ,t - 1] * (1 - p.PD) + S[ ,t -1] * p.SP           # calulate the prop of cohort in P
  D[, t] <- D[ ,t - 1] + S[ ,t - 1] * p.SD + P[ ,t - 1] * p.PD   # calulate the prop of cohort in D
}

TE_t1 <- rowSums(u.S * S + u.P * P + u.D * D)    # total QALYs for all simulations
TC_t1 <- rowSums(c.S * S  + c.P * P + c.D * D)    # total cost for all simulations

E_TE_t1 <- mean(TE_t1)
E_TC_t1 <- mean(TC_t1)



#####################
#####################
# END MARKOV approach
#####################


NB_t0_U <- TE_t0 - TC_t0/k
NB_t1_U <- TE_t1 - TC_t1/k

# vector for calculating VOI outputs
NB_t_U <- cbind(NB_t0_U, NB_t1_U)



# expected NB per person for each treatment 
E_NB_t0_U <- mean(NB_t0_U)
E_NB_t1_U <- mean(NB_t1_U)


#  EVTCI_U 
NB_EVTCI_U <- max(E_NB_t0_U, E_NB_t1_U)

# EVTPI_U 
NB_EVTPI_U <- mean(apply(NB_t_U, 1, max))

# EVPI
NB_EVPI_U <- NB_EVTPI_U - NB_EVTCI_U


############# yearly outputs #########################
##############################################

# yearly implementation value
Incidence*NB_EVTPI_U - Incidence*E_NB_t0_U


# VOI per year
NB_EVTPI_U*Incidence - NB_EVTCI_U*Incidence


#############################################################
############ pop calculations ###################
###############################################




Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))


############# outputs ########################
##########################################

# not general
## Cell_A : net benefit of current situation with current utilisation
# take the weighted average of the expected NB for each treatment scaled up to full population
Cell_A <- sum(E_NB_t0_U *Pop_total)


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

# maxt = use the best treatmet according to current NB. NOT instant trial - 
# instantly and perfectly implement best treatment while trial is running 
# below is the same as AWR if the new treatment is the best with current information
NB_maxt_perfect_info_imp <- Pop_during_research*NB_EVTCI_U + 
  Pop_after_research*NB_EVTPI_U # do not count the cost savings of research - Cost_research_NHS/k


# this is the pure information value under different types of research and implementation assumptions
Value_of_maxt_perfect_info_imp <- NB_maxt_perfect_info_imp - Cell_C


# ICER of research relative to early access (assumed to be costless to the agency)
# all other costs assumed to be captured by the MCD
ICER_maxt_perfect_info_imp <- Cost_research_NETSCC/Value_of_maxt_perfect_info_imp






















































############################ old stuff ###########################################


#######################################
# base case 
#NOT JUST PURE INFORMATION VALUE!!
# INSTANT trial result (proposal says 6 years)
#
# Population calculations - no delay (ignore prevalence!)
##############################################

Time_info = 10
Time_research = 0 # instant
#Cost_research_NETSCC # already defined above
#Cost_research_NHS 

Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))

## NOT ALLOWED DO THIS! Net benfit of immediately implemeting best treatment (Approval)
#NB_maxt_U <- Pop_total*NB_EVTCI_U

NB_mandatedt_U <- E_NB_t0_U*Pop_total

################### NB from costlessly and instantly) Implementing best treatment
# and carrying out perfect trial (with perfect implementing of result)
NB_E_mandatedt_trial_U <- Pop_during_trial*E_NB_t0_U + Pop_after_trial*NB_EVTPI_U #-
  #Cost_research_NHS/k # trial reports instantly so nothing will be saved during the trial in treatment costs.

# ICER
# difference in effects (between trial and implementign best treatment)
NB_E_mandatedt_trial_U - NB_mandatedt_U
# difference in costs (between trial and implementign best treatment)
Cost_research_NETSCC
# the ICER
Cost_research_NETSCC/(NB_E_mandatedt_trial_U - NB_mandatedt_U)





#######################################
# NOT JUST PURE INFORMATION VALUE!!
# trial takes the same time as stated in proposal
#
# Population calculations - no delay (ignore prevalence!)
##############################################

Time_info = 10
Time_research = 6
#Cost_research_NETSCC # already defined above
#Cost_research_NHS 

Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))

## NOT ALLOWED DO THIS! Net benfit of immediately implemeting best treatment (Approval)
#NB_maxt_U <- Pop_total*NB_EVTCI_U

NB_mandatedt_U <- E_NB_t0_U*Pop_total

################### NB from costlessly and instantly) Implementing best treatment
# and carrying out perfect trial (with perfect implementing of result)
NB_E_mandatedt_trial_U <- Pop_during_trial*E_NB_t0_U + Pop_after_trial*NB_EVTPI_U -
   Cost_research_NHS/k # assume that the cost savings reported in the proposal accurately 
                       # reflect the savings during the 6 years to the NHS from giving
                       # X people in the trial the new (cheaper treatment)
                       # note: this is probably overestimating savings as the savings
                       # were reported for a 10 year overall trial time

# ICER
# difference in effects (between trial and implementign best treatment)
NB_E_mandatedt_trial_U - NB_mandatedt_U
# difference in costs (between trial and implementign best treatment)
Cost_research_NETSCC
# the ICER
Cost_research_NETSCC/(NB_E_mandatedt_trial_U - NB_mandatedt_U)








#######################################################################################
# SENSITIVITY ANALYSIS 
########################################################################

#######################################################################################
# Optimistic 
########################################################################

increase <- 1.2
decrease <- 0.8

# optimistic inputs
Time_info = 10*increase
Time_research = 6*decrease 
Cost_research_NETSCC =  2522710*decrease
Cost_research_NHS = -62410967*increase
#INB_Event = (0.85*Time_effect - ((687*0.871 + 2801*0.129)*12*Time_effect)/k)*increase
Incidence = 1137*increase


Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))

## NOT ALLOWED DO THIS! Net benfit of immediately implemeting best treatment (Approval)
#NB_maxt_U <- Pop_total*NB_EVTCI_U

NB_mandatedt_U <- E_NB_t0_U*Pop_total

################### NB from costlessly and instantly) Implementing best treatment
# and carrying out perfect trial (with perfect implementing of result)
NB_E_mandatedt_trial_U <- Pop_during_trial*E_NB_t0_U + Pop_after_trial*NB_EVTPI_U -
  Cost_research_NHS/k # assume that the cost savings reported in the proposal accurately 
# reflect the savings during the 6 years to the NHS from giving
# X people in the trial the new (cheaper treatment)
# note: this is probably overestimating savings as the savings
# were reported for a 10 year overall trial time

# ICER
# difference in effects (between trial and implementign best treatment)
NB_E_mandatedt_trial_U - NB_mandatedt_U
# difference in costs (between trial and implementign best treatment)
Cost_research_NETSCC
# the ICER
Cost_research_NETSCC/(NB_E_mandatedt_trial_U - NB_mandatedt_U)



#######################################################################################
# Pessimistic
########################################################################


# pessimistic inputs
Time_info = 10*decrease
Time_research = 6*increase 
Cost_research_NETSCC =  2522710*increase
Cost_research_NHS = -62410967*decrease
#INB_Event = (0.85*Time_effect - ((687*0.871 + 2801*0.129)*12*Time_effect)/k)*increase
Incidence = 1137*decrease


Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))

## NOT ALLOWED DO THIS! Net benfit of immediately implemeting best treatment (Approval)
#NB_maxt_U <- Pop_total*NB_EVTCI_U

NB_mandatedt_U <- E_NB_t0_U*Pop_total

################### NB from costlessly and instantly) Implementing best treatment
# and carrying out perfect trial (with perfect implementing of result)
NB_E_mandatedt_trial_U <- Pop_during_trial*E_NB_t0_U + Pop_after_trial*NB_EVTPI_U -
  Cost_research_NHS/k # assume that the cost savings reported in the proposal accurately 
# reflect the savings during the 6 years to the NHS from giving
# X people in the trial the new (cheaper treatment)
# note: this is probably overestimating savings as the savings
# were reported for a 10 year overall trial time

# ICER
# difference in effects (between trial and implementign best treatment)
NB_E_mandatedt_trial_U - NB_mandatedt_U
# difference in costs (between trial and implementign best treatment)
Cost_research_NETSCC
# the ICER
Cost_research_NETSCC/(NB_E_mandatedt_trial_U - NB_mandatedt_U)





