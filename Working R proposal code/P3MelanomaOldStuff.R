###########################
# P2 OLD STUFF
##########################

##############################
# Proposal 3: Melanoma
##############################

set.seed(20)
options(scipen=999) # trun off scientifc notation (for convergence axis)


######################################################################################################
##### INPUTS


###################
# simulation resoultion
MCouter <- 4000 # NOTE not bad convergence of EVTPI at MCouter = 40000
MCinner <- 4000 
# MCinner <- 10000 # m simulations for each inner loop
ptm <- proc.time() # start the clock


###################
# Epi parameters


# ignore below?
# early day 3 treatment
#mu_t1_U <- 0 # uninformed mean of treatment effect (log odds ratio scale)
#sigma_t1_U <- 0.5 # uninformed sd of treatment effect (log odds ratio scale) 
# UCI  log odds ratio
#0 + sigma_t1_U*1.96 # correct!
# UCI odds ratio
#exp(0 + sigma_t1_U*1.96)  # correct!
# LCI odds ratio
#exp(0 - sigma_t1_U*1.96)  # correct!
# informed priors 
#sigma_t0_I <- 0.
#sigma_t1_I <- 0.2

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
Cost_research_NHS = -62410967



# Uninformed prior on LOR 
###################

#ignore?
######################################################################################################
# [2] Specify a diffuse prior on the log odds ratio of functional recovery
#LO_t0_U <- rnorm(MCouter, mu_t0_U, sigma_t0_U)
#LOR_t1_U <- rnorm(MCouter, mu_t1_U, sigma_t1_U)
#plot(density(LOR_t1_U))
#priorplot.df <- as.data.frame(LO_t0_U)
#ggplot(priorplot.df, aes(x=LO_t0_U)) + geom_density(fill = "blue", alpha = 0.5) +
#  geom_hline(yintercept = 0, size = 1) + 
#  geom_vline(xintercept = 0, size = 1) +
#  annotate("text", x=4.5, y=0.17, label="~N(mean = 0, sd = 2)", size = 5) + 
#  ggtitle("Prior distribution of functional recovery for each treatment") + 
#  scale_x_continuous("Log odds of functional recovery") +
#  scale_y_continuous("Density") +
#  theme_igray()
#plot(density(LO_t1_U))
#########################################################################################
# [3] Now a range of (l) implied plausible values for the proportion of people 
# expected to have the event with both treatments 
library(fdrtool)

P_t0_U <- 0.51
Odds_t0_U <- P_t0_U / (1 - P_t0_U)
LO_t0_U <- log(Odds_t0_U)

# halfnormal simulations on LOR 
sigma_t1_U <- 0.5
LOR_t1_U <- -rhalfnorm(MCouter, theta=sd2theta(sigma_t1_U))
#plot(density(LOR_t1_U))

LO_t1_U <- LO_t0_U + LOR_t1_U 
Odds_t1_U <- exp(LO_t1_U)
P_t1_U <- Odds_t1_U / (Odds_t1_U + 1) 

#plot(density(P_t1_U)) # looks good - about 99% of density over 0.25 - maybe too much probability at 0.5
#Odds_t1_U <- exp(LO_t1_U)
#P_t1_U <- Odds_t1_U / (1 + Odds_t1_U)

#summary(Odds_t1_U)
#summary(LOR_t1_U)
#plot(density(Odds_t1_U))
#plot(density(Odds_t1_U))

#################################
# NATURAL OUTCOME - NOT INCLUDING SECONDARY OUTCOMES!!!
##################################
# current treatment will always be better! so no uncertainty

# construct the (natural outcome) INB_t1 function 
# primary outcome is a good natural outcome (units = progression free survival at 2 years) 
INB_Event_nat = 1
NB_t0_U_nat <- P_t0_U*INB_Event_nat
NB_t1_U_nat <- 1/sd2theta(sigma_t1_U)*INB_Event_nat #analytic mean
E_NB_t0_U_nat <- mean(NB_t0_U_nat)
E_NB_t1_U_nat <- mean(NB_t1_U_nat)
# events per year for each treatment
E_NB_t0_U_nat*Incidence
E_NB_t1_U_nat*Incidence




#################################
# NATURAL OUTCOME -  SECONDARY OUTCOMES INCLUDED
##################################

# difference in costs
# use markov to convert 2 year PFS to mean time to transition

# notes on function --- BETTER FORMULA BELOW!!! expected_time_in_state2
####################
# based on Toronto course markov model
#perc_in_state : % in the state you care about
#at_time : % in the state you care about at time X (in months)
#numb_cycles: how long to run the markov 
# model assumes constant risk over time! 

expected_time_in_state <- function(perc_in_state, at_time, numb_cycles){
  
  # model as prob of leaving state
  #perc_in_state = 0.37
  prob_leave_state <- 1 - perc_in_state # probability of not being the the start state when time = at_time
  
  rate_leave <- -log(1-prob_leave_state )/at_time
  # one month probability of leaving starting state (transition from state S to state not S)
  p.S_notS <- 1 - exp(-rate_leave)
  p.S_notS
  
  
  #p.S_S <- exp(--log(1-perc_in_state)/at_time) 
  
  n.t <- numb_cycles                                    # number of cycles
  v.n <- c("start state", "not start state")  # state names
  n.s<- length(v.n)                            # number of states
  m.P <- rbind(c(1-p.S_notS, p.S_notS), # Transition matrix
               c(0,            1))
  
  m.TR <- matrix(NA, nrow = n.t, ncol = n.s, 
                 dimnames = list(1:n.t, v.n))   # create Markov trace
  m.TR[1,] <- c(1,0)                         # initialize Markov trace
  for (t in 2:n.t)                             # throughout the number of cycles
  {
    m.TR[t, ] <-m.TR[t - 1,] %*% m.P           # estimate the Markov trace for cycle t 
  }
  
  # go from cumulative sum in start state to the differences in % after each month
  # i.e. 97% in state after 1 month => 3% only lived one month
  # 94% in state after 2 months => 3% more lived 2 months... etc 
  vec <- m.TR[,1] # create start state as cumulative sum vector
  startstate <- -c(vec[1],diff(vec))
  
  # get weighted average for months to progression
  
  # weighted average of % progressed in month i x month i 
  startstate %*% 0:(n.t - 1)
  
}



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



mean_time_preProg_t0 <- expected_time_in_state2(perc_in_state = 0.51, at_time = 24)
mean_time_preProg_t0
# people spend 33.5 months on average for progression free
# (note: median around 25 months according to this)

# per person costs
Drug_cost_month <- 6000

# average cost per person
mean_time_preProg_t0*Drug_cost_month

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





MCD_nat = 0.079 # this is the difference between equivalent effects for t1 and t0
# i.e. it reflects how much better the new treatment is on the secondary outcomes

INB_t1_U_nat <- P_t1_U - P_t0_U + MCD_nat
# i.e. if the probabiliyt of the primary event was the same in both treatments then t1 would be
# better as it has better secondary characteristics. This advantage is represented by the MCD

# old stuff
#INB_Event_nat = 1
#used analytic expectation of half normal = 1/theta
# events per year for each treatment
#E_NB_t0_U_nat*Incidence
#E_NB_t1_U_nat*Incidence
# old stuff

NB_t0_U_nat <- P_t0_U
NB_t1_U_nat <- P_t1_U + MCD_nat
E_NB_t0_U_nat <- mean(NB_t0_U_nat) #both analytic so no real need for this!
E_NB_t1_U_nat <- mean(NB_t1_U_nat) #both analytic so no real need for this!

# vector for calculating VOI outputs- needs to use simulations
NB_t_U_nat <- cbind(NB_t0_U_nat, NB_t1_U_nat)


# Population calculations - no delay (ignore prevalence!)
##############################################
#Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
#Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
#Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))


# Best outcome with uninformed prior 
#EVTCI_U_nat = max(0, E_INB_t1_U_nat) # in this case should always equal zero if no Monte carlo error
NB_EVTCI_U = max(E_NB_t0_U_nat, E_NB_t1_U_nat) # 

# EVTPI with uninformed prior and natural outcome
#EVTPI_U_nat <- mean(apply(INB_t_U_nat, 1, max))
NB_VTPI_U <- apply(NB_t_U_nat, 1, max) #so I can check convergence
NB_EVTPI_U <- mean(NB_VTPI_U)


NB_EVPI_U <-  NB_EVTPI_U - NB_EVTCI_U
#Running mean to check convergence
#EVTPI.run<-c(rep(0,150))
#for (i in 1:150){
#  EVTPI.run[i]<-mean(NB_VTPI_U_nat[1:(i*(MCouter/150))])
#}
#plot(seq(1,i*(MCouter/150), length.out = length(EVTPI.run)), EVTPI.run, type="l",lty=1, xlab="Simulation", ylab="EVTPI")
# does not really appear to be converged until 45K as far as we can tell
# need to check when converted to natrual outcomes


# annual outcomes
#####################
# ADDITIONAL PFS per year from doing study (compared to best treatment)
NB_EVTPI_U*Incidence - NB_EVTCI_U*Incidence


# full time horizon outcomes 
# full time horizon effect of implementing best treatment (both treatments the same here!)
#NB_maxt_U_nat <- Pop_total*NB_EVTCI_U_nat
# adjust to eliminate MC error
#E_imp_t0_U <- Pop_total*(Incidence/2) 

# vector of payoff vs time to researcdh report
#Time_research_vec <- c(0:15)
#Pop_total_vec <- rep(Pop_total, length(Time_research_vec))
#Pop_during_trial_vec <- ((Incidence) /-D_rate) * (exp(-D_rate*Time_research_vec) - exp(-D_rate*0))
#Pop_after_trial_vec <- ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research_vec))
#NB_E_maxt_trial_U_nat_vec <- Pop_during_trial_vec*NB_EVTCI_U_nat + Pop_after_trial_vec*NB_EVTPI_U_nat
#NB_maxt_U_nat_vec <- Pop_total*NB_EVTCI_U_nat
#natural_outcome_time_to_research_data <- data.frame(Time_research_vec = Time_research_vec,
#           )
#
#write.csv("natural_outcome_time_to_research_data")



#######################################
# INSTANT trial result (proposal says 6 years)
# IGNORE option to add extra trial arms (underestimates benefit)
# NATURAL OUTCOMES so completely ignores research costs to the NHS
# Population calculations - no delay (ignore prevalence!)
##############################################

Time_info = 10
Time_research = 0 # instant
Cost_research_NETSCC =  2522710
#Cost_research_NHS_budget = -62410967


Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))

## Net benfit of immediately implemeting best treatment (Approval)
NB_maxt_U <- Pop_total*NB_EVTCI_U

# EVPI
NB_EVTPI_U - NB_EVTCI_U
################### NB from costlessly and instantly) Implementing best treatment
# and carrying out perfect trial (with perfect implementing of result)
NB_E_maxt_trial_U <- Pop_during_trial*NB_EVTCI_U + Pop_after_trial*NB_EVTPI_U 

# ICER
# difference in effects (between trial and implementign best treatment)
NB_E_maxt_trial_U - NB_maxt_U
# difference in costs (between trial and implementign best treatment)
Cost_research_NETSCC
# the ICER
Cost_research_NETSCC/(NB_E_maxt_trial_U - NB_maxt_U)




#######################################
# Trial takes 6 years (same duration as proposal says)
# IGNORE option to add extra trial arms (underestimates benefit)
# NATURAL OUTCOMES so completely ignores research costs to the NHS
# Population calculations - no delay (ignore prevalence!)
##############################################

Time_info = 10
Time_research = 6 
Cost_research_NETSCC =  2522710
#Cost_research_NHS_budget = -62410967

Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))

## Net benfit of immediately implemeting best treatment (Approval)
NB_maxt_U <- Pop_total*NB_EVTCI_U


################### NB from costlessly and instantly) Implementing best treatment
# and carrying out perfect trial (with perfect implementing of result)
NB_E_maxt_trial_U <- Pop_during_trial*NB_EVTCI_U + Pop_after_trial*NB_EVTPI_U 

# ICER
# difference in effects (between trial and implementign best treatment)
NB_E_maxt_trial_U - NB_maxt_U
# difference in costs (between trial and implementign best treatment)
Cost_research_NETSCC
# the ICER
Cost_research_NETSCC/(NB_E_maxt_trial_U - NB_maxt_U)



#######################################
# Trial takes 10 years (proposal says 6 )
# IGNORE option to add extra trial arms (underestimates benefit)
# NATURAL OUTCOMES so completely ignores research costs to the NHS
# Population calculations - no delay (ignore prevalence!)
##############################################

Time_info = 10
Time_research = 10 
Cost_research_NETSCC =  2522710
#Cost_research_NHS_budget = -62410967

Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))

## Net benfit of immediately implemeting best treatment (Approval)
NB_maxt_U <- Pop_total*NB_EVTCI_U


################### NB from costlessly and instantly) Implementing best treatment
# and carrying out perfect trial (with perfect implementing of result)
NB_E_maxt_trial_U <- Pop_during_trial*NB_EVTCI_U + Pop_after_trial*NB_EVTPI_U 

# ICER
# difference in effects (between trial and implementign best treatment)
NB_E_maxt_trial_U - NB_maxt_U
# difference in costs (between trial and implementign best treatment)
Cost_research_NETSCC
# the ICER
Cost_research_NETSCC/(NB_E_maxt_trial_U - NB_maxt_U)


















#####################################################################################
# Economic inputs
# take account of QALYs and aquisition costs
# ignore any other secondary outcomes
######################################################################################

#####################
#####################
# START MARKOV approach
#####################

# transition probability for death from natrual causes
########################################################
# http://mfne.org/learn-about-melanoma/facts-about-melanoma-and-skin-cancer/
start_age <- 53 # average age of diagnosis 52 then we assume they are started on drugs and 
# randomised at year 1
# from NICE proposal: "The model adopted a lifetime time horizon of 40 years"
# from life tables (averaged tps from CHE advanced course for men and women for everyone over 55)
p.Pre_Dead_yr <- 0.0681 
rate.Pre_Dead_m <- -log(1 - p.Pre_Dead_yr)/12 #montly rate of death
p.Pre_Dead <- 1 - exp(-rate.Pre_Dead_m) # monthly transition probability

# TP Pre -> Post (stable to progressed)
###################
# t0 monthly rate
rate_m_t0 <- -(log(1- P_t0_U  ))/24  # 24 as 24 months in 2 years 
# monthly TP
tp_m_t0 <- 1 - exp(- rate_m_t0)
# t0 yearly rate
#rate_yr_t0 <- -(log(1- P_t0_U  ))/2  #  2 years 
# yearly TP
#tp_yr_t0 <- 1 - exp(- rate_yr_t0)



sigma_t1_U = 0.5

#plot(density(-rhalfnorm(MCouter, theta=sd2theta(sigma_t1_U))))
#plot(density(rnorm(MCouter, 0, 0.5)))
# "very uncertain prior"
log_h_ratio_t1 <- -rhalfnorm(MCouter, theta=sd2theta(sigma_t1_U))
h_ratio_t1 <- exp(log_h_ratio_t1) # to compare to Bennette paper 
log_rate_m_t0 <- log(rate_m_t0)

log_rate_m_t1 <- log_rate_m_t0 + log_h_ratio_t1
rate_m_t1 <- exp(log_rate_m_t1)

tp_m_t1 <- 1 - exp(- rate_m_t1)

plot(density(h_ratio_t1))
plot(density(tp_m_t1))
plot(density(rate_m_t1))
abline(v = rate_m_t0)

summary(tp_m_t1)

# t1 monthly rate
rate_m_t1 <- -(log(1- P_t1_U  ))/24  # 24 as 24 months in 2 years 
# monthly TP
tp_m_t1 <- 1 - exp(- rate_m_t1)
mean(tp_m_t1)
sd(tp_m_t1)
# t1 yearly rate (to compare to Benette approach)
#rate_yr_t1 <- -(log(1- P_t1_U  ))/2  # 2 as 2 years 
# monthly TP
#tp_yr_t1 <- 1 - exp(- rate_yr_t1)
#plot(density(tp_yr_t1))

# time from post progression to death (same for both treatments)
#################################
# from KEYNOTE-002 (Ribas 2015) and 
# follow up to KEYNOTE-002: http://oncologypro.esmo.org/Meeting-Resources/ESMO-2016/Final-overall-survival-for-KEYNOTE-002-pembrolizumab-pembro-versus-investigator-choice-chemotherapy-chemo-for-ipilimumab-ipi-refractory-melanoma
# 24m OS 37% (from follow up)
E_months_alive <- expected_time_in_state(perc_in_state = 0.37, at_time=24, numb_cycles=200)
E_months_alive
# 6m PF 36% (from KEYNOTE-002)
E_months_PF <- expected_time_in_state(perc_in_state = 0.36, at_time=6, numb_cycles=200)
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

expected_time_in_state(perc_in_state = X, at_time = 24, numb_cycles =200) 
# choose X (and p.S_notS) such that expected_time_in_state = E_months_alive - E_months_PF
# expected_time_in_state = 18.2 months
#p.S_notS is the answer I need
# answer p.S_notS = 0.05488609 => expected_time_in_state = 18.2 months


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

p.SD <- p.Pre_Dead # described above (prob from stable to dead)
p.SP <- tp_m_t0 # defined above (probability from stable to progressed)
p.PD <- 0.05488609 # see above 

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

p.SD <- p.Pre_Dead # described above (prob from stable to dead)
p.SP <- tp_m_t1 # defined above (probability from stable to progressed)
p.PD <- 0.05488609 # see above 

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
# also need to inlcude aquisition costs at the individual level
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






#######################################
# NOT JUST PURE INFORMATION VALUE!!
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

















