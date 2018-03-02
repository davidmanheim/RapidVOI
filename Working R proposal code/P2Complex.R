##############################
# Proposal 2: Alzheimers Complex
##############################

set.seed(20)
options(scipen=999) # trun off scientifc notation (for convergence axis)


######################################################################################################
##### INPUTS


###################
# simulation resoultion - not needed! analytic calcluation
#MCouter <- 400000 # 
#MCinner <- 4000 
# MCinner <- 10000 # m simulations for each inner loop
ptm <- proc.time() # start the clock



#########################
# health system model inputs (some may not be needed)
Utilisation =  0  # 
Incidence =  100000 # from proposal (not referenced!)
# number diagnosed with AD each year

Time_info =  20 # assuming the area moves "very slowly" 
D_rate =  0.035 
k = 15000 

#Time_research_pilot =  2
#Cost_research_pilot_NETSCC =  601480
#Cost_research_pilot_NHS_budget = 150000
# complicated to specify these - will be addressed later
#Time_research_definitive = 6
#Cost_research_definitive_NETSCC =  2500000
#Cost_research_definitive_NHS_budget = 450000

Time_info = 20
Time_research = 6 
Cost_research_NETSCC =  3310883
Cost_research_NHS = 1297789

# Information at time of decision making
###################

# primary outcome: significant slowing of decline in 2year MMSE 
# this is a decline of no more than 3.1 points (assuming no variability in 
# rate of decline on placebo) - from proposal

# different options for modelling
# 1) model mean changes to MMSE on quasi continuous scale
# 2) model probability of success for each treatment

# NOTES:
# choose option 2 for now (but option 1 potentially easier to integrate into
# EVSI)

# for two treatments and simple trial:

# probability of treatment "success"
# S_t1_U ~ Bin(MCouter, P_t1_U)
# S_t0_U ~ Bin(MCouter, 0)


#####################################################################################
# primary outcome analysis 
# 
# IGNORE ALL SECONDARY OUTCOMES (COSTS and SIDE EFFECTS)
######################################################################################

# assume that the new treatments cannot be implemented without a trial of some
# sort. even though if costs and side effects ignored then the new treatments 
# should be implemented
# if there is even a tiny chance of working (on expected net benefit)


####################################
# Epi parameters
#####################################

# "200 drugs have advanced to stage 2 and none demonstrated disease modification"
# need to take this into account!

#https://stats.stackexchange.com/questions/134380/how-to-tell-the-probability-of-failure-if-there-were-no-failures
# could use the above as justification for chance of success!
# interesting!
# http://jamanetwork.com/journals/jama/article-abstract/385438
# also in the JAMA - nice citation

# use Laplace rule of succession:
#https://en.wikipedia.org/wiki/Rule_of_succession

# probability of success = (S +1)/(N +2) 
# S = number of success so far (here S =0)
# N = number of trials

# 1 / (200 +2) = 0.004950495
# about 0.5%

# probability of success with each treatment (assumed independent)

P_t0_U <- 0 # by definition

P_t1_U <- 1 / (200 +2)
P_t2_U <- 1 / (200 +2)

# additional probability of combination working if the individual treatments do not
# could just assume the combination is independet of the others for 
# simplicity



#P_t1_t2_inter <- 0.01 # more complex specification
# probaiblity of working = 
# prob of either t1 or t2 working: (P_t1_U + P_t2_U) or
# 1% chance if neither work: prob neither works: (1 - P_t1_U)*(1 - P_t2_U)*0.01
#P_t3_U <- (P_t1_U + P_t2_U) + (1 - P_t1_U)*(1 - P_t2_U)*P_t1_t2_inter

P_t3_U <-  1 / (200 +2)

P_t1_U + P_t2_U + P_t3_U

####################################
# NB model
#####################################

INB_success = 1 # good outcome meaning "successful treatment of AD"

# expected payoffs given we think there is some prob they will work
E_NB_t0_U <- 0
E_NB_t1_U <- P_t1_U*INB_success 
E_NB_t2_U <- P_t2_U*INB_success 
E_NB_t3_U <- P_t3_U*INB_success 


##############################################
# analytic solution for binomial VOI (not fully general - just works in this case)
###########################################
# see rationale in mint pukka note book (also more detail below)

# old method in which p_t3 was not independent
#P_t1_and_t2 <- P_t2_U*P_t1_U 
#NB_EVTPI_U <- P_t1_U*(1 - P_t1_and_t2)*(INB_success - C_t1/k) + 
#  P_t2_U*(1 - P_t1_and_t2)*(INB_success - C_t2/k) +
#  P_t1_and_t2*max(INB_success - C_t1/k, INB_success - C_t2/k) +
#  P_t1_t2_inter*(INB_success - C_t3/k)


              # prob only t1 works                        x payoff when t1 works
NB_EVTPI_U <- P_t1_U*(1 - P_t1_U*P_t2_U)*(1- P_t1_U*P_t3_U)*(INB_success) + 
  # prob only t2 works                        x payoff when t2 works
  P_t2_U*(1 - P_t1_U*P_t2_U)*(1- P_t2_U*P_t3_U)*(INB_success) + 
  # prob only t3 works                        x payoff when t3 works
  P_t3_U*(1 - P_t1_U*P_t3_U)*(1- P_t2_U*P_t3_U)*(INB_success) + 
  # prob both t1 and t2 work x payoff when they both work (choose highest)
  P_t1_U*P_t2_U*(INB_success) +
  P_t1_U*P_t3_U*(INB_success) +
  P_t2_U*P_t3_U*(INB_success) +
  # prob t1, t2 and t3 all work x payoff when they all work (choose highest)
  P_t1_U*P_t2_U*P_t3_U*(INB_success)
  







########### TRIAL ANALYSIS ################################
########### TRIAL ANALYSIS ################################
########### TRIAL ANALYSIS ################################


# trial impact per year
########################

# even though if costs and side effects ignored then the new treatments 
# should be implemented
# if there is even a tiny chance of working (on expected net benefit)

NB_EVTCI_U = max(E_NB_t0_U, E_NB_t1_U, E_NB_t2_U, E_NB_t3_U) # 



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
NB_maxt_perfect_info_imp <- Pop_during_research*NB_EVTCI_U + Pop_after_research*NB_EVTPI_U

# this is the pure information value under different types of research and implementation assumptions
Value_of_maxt_perfect_info_imp <- NB_maxt_perfect_info_imp - Cell_C


# ICER of research relative to early access (assumed to be costless to the agency)
# all other costs assumed to be captured by the MCD
ICER_maxt_perfect_info_imp <- Cost_research_NETSCC/Value_of_maxt_perfect_info_imp

























#####################################################################################
# primary outcome analysis + secondary outcomes
# 
# ignore any other secondary outcomes
######################################################################################

#P_t3_U <-  1 / (200 +2)

####################################
# NB model
#####################################


########
# assume treatment effect of 2 years

# qol effect of treatment no effect on survival
# duration of effect = duration of treatment

Time_effect = 2
Time_treat = Time_effect # number of years patients are treated

# MCD to reflect increased costs:


# no treatment
C_t0 <- 0

# Exanatide
#https://www.medicinescomplete.com/mc/bnflegacy/64/PHP4187-bydureon.htm
# cost per treatment: 18.34 per week * 10 years (520 weeks)
# lifetime costs: 
C_t1 <- 18.34*Time_treat*52
C_t1

# Telmisartan
# https://www.medicinescomplete.com/mc/bnflegacy/64/PHP1240-micardis.htm
# cost per treatmetnt: 40 mg, 28-tab pack = £13.61, 
# cost per tablet (daily cost) 13.61/28
# days in 10 years 365*10

C_t2 <- (13.61/28)*(365*Time_treat)
C_t2

# combination
C_t3 <- C_t1 + C_t2
C_t3


C_t0
C_t1
C_t2
C_t3

# cost per year
C_t0*Incidence
C_t1*Incidence
C_t2*Incidence
C_t3*Incidence









#####################################################################################
# Economic inputs
# take account of QALYs and aquisition costs
# ignore any other secondary outcomes
######################################################################################

# t0 placebo
# t1 exenatide 2mg injection once weakely
# t2 telmisartan 40mg OD
# t3 combination 

# life expectancy with alzheimers: 8-12 years
# https://www.unforgettable.org/blog/what-is-the-life-expectancy-for-someone-with-dementia/
# assume 10 years (that includes the 2 in the trial)
#

#######################
# model
#######################





#########################
# Utilitiy
############################



#############################
# costs
#############################

#https://www.medicinescomplete.com/mc/bnflegacy/64/PHP4187-bydureon.htm
# cost per treatment: 18.34 per week * 52 weeks * years of treatment
# lifetime costs: 
#C_t1 <- Time_treat*52*18.34

# https://www.medicinescomplete.com/mc/bnflegacy/64/PHP1240-micardis.htm
# cost per treatmetnt: 40 mg, 28-tab pack = £13.61, 
# cost per tablet (daily cost) 13.61/28
# 

#C_t2 <- (13.61/28)*(365*Time_treat)

# combination
#C_t3 <- C_t1 + C_t2


######################################
# Survival
####################################

# if decline in MMSE slowed down - assume for simplicity no difference in survival
# all gains as qol



########################################
# utilities
#####################################

# INB_success
# difference in NB between average MMSE deline of 4.5 points at 2 years
# vs decline of 3.1 points at 2 years

# from Bond et al Utility = 0.01*MMSE
# "success" = 1.4 additional MMSE points relative to placebo
# Time_effect = how long the effect lasts

# just assume you survive for Time_effect extra years at the same morbidity

#NB_nosuccess <- 
#NB_success <- 

# monthly costs : £687 home care mild patient 
#                 £2801   institutional care mild patient

# 12.9% of mild patients in institutional care


# utility : of mild state 0.85



         #  survival qol benefit       # cost
INB_Event <- 0.85*Time_effect - ((687*0.871 + 2801*0.129)*12*Time_effect)/k 


####################################
# Epi parameters
#####################################

# probability of success with each treatment (assumed independent)
#P_t1_U <- 1/(200 + 2)
#P_t2_U <- 1/(200 + 2)

# additional probability of combination working if the individual treatments do not

#P_t1_t2_inter <- 

# probaiblity of working = 
# prob of either t1 or t2 working: (P_t1_U + P_t2_U) or
# 1% chance if neither work: prob neither works: (1 - P_t1_U)*(1 - P_t2_U)*0.01

#P_t3_U <- (P_t1_U + P_t2_U) + (1 - P_t1_U)*(1 - P_t2_U)*P_t1_t2_inter

####################################
# NB model
#####################################

# expected payoffs given we think there is some prob they will work
E_NB_t0_U <- 0
E_NB_t1_U <- P_t1_U*INB_Event - C_t1/k
E_NB_t2_U <- P_t2_U*INB_Event - C_t2/k
E_NB_t3_U <- P_t3_U*INB_Event - C_t3/k



###########################
# benefit of trial
#########################

# NB payoffs when the treatments work
NB_t0_payoff <- 0  
NB_t1_payoff <- INB_Event - C_t1/k
NB_t2_payoff <- INB_Event - C_t2/k
NB_t3_payoff <- INB_Event - C_t3/k




# prob only t1 works                                    x payoff when t1 works
NB_EVTPI_U <- P_t1_U*(1 - P_t1_U*P_t2_U)*(1- P_t1_U*P_t3_U)*max(0, NB_t1_payoff) + 
  # prob only t2 works                        x payoff when t2 works
  P_t2_U*(1 - P_t1_U*P_t2_U)*(1- P_t2_U*P_t3_U)*max(0, NB_t2_payoff) + 
  # prob only t3 works                        x payoff when t3 works
  P_t3_U*(1 - P_t1_U*P_t3_U)*(1- P_t2_U*P_t3_U)*max(0, NB_t3_payoff) + 
  # prob both t1 and t2 work x payoff when they both work (choose highest)
  P_t1_U*P_t2_U*max(0, NB_t1_payoff, NB_t2_payoff) +
  P_t1_U*P_t3_U*max(0, NB_t1_payoff, NB_t3_payoff) +
  P_t2_U*P_t3_U*max(0, NB_t2_payoff, NB_t3_payoff) +
  # prob t1, t2 and t3 all work x payoff when they all work (choose highest)
  P_t1_U*P_t2_U*P_t3_U*max(0, NB_t1_payoff, NB_t2_payoff, NB_t2_payoff)






########### TRIAL ANALYSIS ################################################
########## TRIAL ANALYSIS ################################
######### TRIAL ANALYSIS ################################

# trial impact per year
########################

# even though if costs and side effects ignored then the new treatments 
# should be implemented
# if there is even a tiny chance of working (on expected net benefit)

NB_EVTCI_U = max(E_NB_t0_U, E_NB_t1_U, E_NB_t2_U, E_NB_t3_U) # 



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
  Pop_after_research*NB_EVTPI_U - Cost_research_NHS/k


# this is the pure information value under different types of research and implementation assumptions
Value_of_maxt_perfect_info_imp <- NB_maxt_perfect_info_imp - Cell_C


# ICER of research relative to early access (assumed to be costless to the agency)
# all other costs assumed to be captured by the MCD
ICER_maxt_perfect_info_imp <- Cost_research_NETSCC/Value_of_maxt_perfect_info_imp





















































################################# old stuff #############################################





# trial impact per year
########################

# Best outcome with uninformed prior 
#EVTCI_U_nat = max(0, E_INB_t1_U_nat) # in this case should always equal zero if no Monte carlo error
NB_EVTCI_U = max(E_NB_t0_U, E_NB_t1_U, E_NB_t2_U, E_NB_t3_U) # 


# QALYs per year from doing the definitive trial vs implementing best treatment
P2QALYsPerYear <- NB_EVTPI_U*Incidence - NB_EVTCI_U*Incidence



#######################################
# INSTANT trial result (proposal says 6 years)
# IGNORE option to add extra trial arms (underestimates benefit)

# Population calculations - no delay (ignore prevalence!)
##############################################

Time_info = 20
Time_research = 0 # instant
Cost_research_NETSCC =  3310883
Cost_research_NHS = 1297789

Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))

## Net benfit of immediately implemeting best treatment (Approval)
NB_maxt_U <- Pop_total*NB_EVTCI_U


################### NB from costlessly and instantly) Implementing best treatment
# and carrying out perfect trial (with perfect implementing of result)
NB_E_maxt_trial_U <- Pop_during_trial*NB_EVTCI_U + Pop_after_trial*NB_EVTPI_U -
  Cost_research_NHS/k

# ICER
# difference in effects (between trial and implementign best treatment)
P2QALYsInstantFreeTrial <- NB_E_maxt_trial_U - NB_maxt_U
# difference in costs (between trial and implementign best treatment)
Cost_research_NETSCC
# the ICER
P2ICERInstantFreeTrialQALYs <- Cost_research_NETSCC/(NB_E_maxt_trial_U - NB_maxt_U)




# base case
#######################################
# Trial takes 6 years (same duration as proposal says)
# IGNORE option to add extra trial arms (underestimates benefit)

# Population calculations - no delay (ignore prevalence!)
##############################################

Time_info = 20
Time_research = 6 
Cost_research_NETSCC =  3310883
Cost_research_NHS = 1297789

Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))

## Net benfit of immediately implemeting best treatment (Approval)
NB_maxt_U <- Pop_total*NB_EVTCI_U


################### NB from costlessly and instantly) Implementing best treatment
# and carrying out perfect trial (with perfect implementing of result)
NB_E_maxt_trial_U <- Pop_during_trial*NB_EVTCI_U + Pop_after_trial*NB_EVTPI_U -
  Cost_research_NHS/k

# ICER
# difference in effects (between trial and implementign best treatment)
P2BaseCaseNHE <- NB_E_maxt_trial_U - NB_maxt_U
# difference in costs (between trial and implementign best treatment)
Cost_research_NETSCC
# the ICER
P2BaseCaseICER <- Cost_research_NETSCC/(NB_E_maxt_trial_U - NB_maxt_U)







#######################################################################################
# SENSITIVITY ANALYSIS 
########################################################################

#######################################################################################
# Optimistic 
########################################################################

increase <- 1.2
decrease <- 0.8

# optimistic inputs
Time_info = 20*increase
Time_research = 6*decrease 
Cost_research_NETSCC =  3310883*decrease
Cost_research_NHS = 1297789*decrease
INB_Event = (0.85*Time_effect - ((687*0.871 + 2801*0.129)*12*Time_effect)/k)*increase
Incidence = 100000*increase

###########################
# benefit of trial
#########################

# NB payoffs when the treatments work
NB_t0_payoff <- 0  
NB_t1_payoff <- INB_Event - C_t1/k
NB_t2_payoff <- INB_Event - C_t2/k
NB_t3_payoff <- INB_Event - C_t3/k

# prob only t1 works                                    x payoff when t1 works
NB_EVTPI_U <- P_t1_U*(1 - P_t1_U*P_t2_U)*(1- P_t1_U*P_t3_U)*max(0, NB_t1_payoff) + 
  # prob only t2 works                        x payoff when t2 works
  P_t2_U*(1 - P_t1_U*P_t2_U)*(1- P_t2_U*P_t3_U)*max(0, NB_t2_payoff) + 
  # prob only t3 works                        x payoff when t3 works
  P_t3_U*(1 - P_t1_U*P_t3_U)*(1- P_t2_U*P_t3_U)*max(0, NB_t3_payoff) + 
  # prob both t1 and t2 work x payoff when they both work (choose highest)
  P_t1_U*P_t2_U*max(0, NB_t1_payoff, NB_t2_payoff) +
  P_t1_U*P_t3_U*max(0, NB_t1_payoff, NB_t3_payoff) +
  P_t2_U*P_t3_U*max(0, NB_t2_payoff, NB_t3_payoff) +
  # prob t1, t2 and t3 all work x payoff when they all work (choose highest)
  P_t1_U*P_t2_U*P_t3_U*max(0, NB_t1_payoff, NB_t2_payoff, NB_t2_payoff)

# Best outcome with uninformed prior 
#EVTCI_U_nat = max(0, E_INB_t1_U_nat) # in this case should always equal zero if no Monte carlo error
NB_EVTCI_U = max(E_NB_t0_U, E_NB_t1_U, E_NB_t2_U, E_NB_t3_U) # 


# Optimistic
#######################################
# Trial takes 6 years (same duration as proposal says)
# IGNORE option to add extra trial arms (underestimates benefit)

# Population calculations - no delay (ignore prevalence!)
##############################################
Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))

## Net benfit of immediately implemeting best treatment (Approval)
NB_maxt_U <- Pop_total*NB_EVTCI_U


################### NB from costlessly and instantly) Implementing best treatment
# and carrying out perfect trial (with perfect implementing of result)
NB_E_maxt_trial_U <- Pop_during_trial*NB_EVTCI_U + Pop_after_trial*NB_EVTPI_U -
  Cost_research_NHS/k

# ICER
# difference in effects (between trial and implementign best treatment)
P2OptimisticNHE <- NB_E_maxt_trial_U - NB_maxt_U
# difference in costs (between trial and implementign best treatment)
Cost_research_NETSCC
# the ICER
P2OptimisticICER <- Cost_research_NETSCC/(NB_E_maxt_trial_U - NB_maxt_U)






#######################################################################################
       # pesimistic 
########################################################################


# pesimistic inputs
Time_info = 20*decrease
Time_research = 6*increase 
Cost_research_NETSCC =  3310883*increase
Cost_research_NHS = 1297789*increase
INB_Event = (0.85*Time_effect - ((687*0.871 + 2801*0.129)*12*Time_effect)/k)*decrease
Incidence = 100000*decrease

###########################
# benefit of trial
#########################

# NB payoffs when the treatments work
NB_t0_payoff <- 0  
NB_t1_payoff <- INB_Event - C_t1/k
NB_t2_payoff <- INB_Event - C_t2/k
NB_t3_payoff <- INB_Event - C_t3/k

# prob only t1 works                                    x payoff when t1 works
NB_EVTPI_U <- P_t1_U*(1 - P_t1_U*P_t2_U)*(1- P_t1_U*P_t3_U)*max(0, NB_t1_payoff) + 
  # prob only t2 works                        x payoff when t2 works
  P_t2_U*(1 - P_t1_U*P_t2_U)*(1- P_t2_U*P_t3_U)*max(0, NB_t2_payoff) + 
  # prob only t3 works                        x payoff when t3 works
  P_t3_U*(1 - P_t1_U*P_t3_U)*(1- P_t2_U*P_t3_U)*max(0, NB_t3_payoff) + 
  # prob both t1 and t2 work x payoff when they both work (choose highest)
  P_t1_U*P_t2_U*max(0, NB_t1_payoff, NB_t2_payoff) +
  P_t1_U*P_t3_U*max(0, NB_t1_payoff, NB_t3_payoff) +
  P_t2_U*P_t3_U*max(0, NB_t2_payoff, NB_t3_payoff) +
  # prob t1, t2 and t3 all work x payoff when they all work (choose highest)
  P_t1_U*P_t2_U*P_t3_U*max(0, NB_t1_payoff, NB_t2_payoff, NB_t2_payoff)

# Best outcome with uninformed prior 
#EVTCI_U_nat = max(0, E_INB_t1_U_nat) # in this case should always equal zero if no Monte carlo error
NB_EVTCI_U = max(E_NB_t0_U, E_NB_t1_U, E_NB_t2_U, E_NB_t3_U) # 





# Pessimistic
#######################################
# Trial takes 6 years (same duration as proposal says)
# IGNORE option to add extra trial arms (underestimates benefit)

# Population calculations - no delay (ignore prevalence!)
##############################################



Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))

## Net benfit of immediately implemeting best treatment (Approval)
NB_maxt_U <- Pop_total*NB_EVTCI_U


################### NB from costlessly and instantly) Implementing best treatment
# and carrying out perfect trial (with perfect implementing of result)
NB_E_maxt_trial_U <- Pop_during_trial*NB_EVTCI_U + Pop_after_trial*NB_EVTPI_U -
  Cost_research_NHS/k

# ICER
# difference in effects (between trial and implementign best treatment)
P2PessimisticNHE <- NB_E_maxt_trial_U - NB_maxt_U
# difference in costs (between trial and implementign best treatment)
Cost_research_NETSCC
# the ICER
P2PessimisticICER <- Cost_research_NETSCC/(NB_E_maxt_trial_U - NB_maxt_U)


























































###################################################################################
                   ###### old stuf - relevant anymore???

#########################################################################


###########################
# MCD questions
#############################

# cost table 

C_t0*Incidence # nothing 
C_t1*Incidence # exenatide
C_t2*Incidence # telmisartan
C_t3*Incidence # combination


# if would be WTP £30K per disease modification:

WTP_success <- 20000

C_t1*Incidence/WTP_success # exenatide
C_t2*Incidence/WTP_success # telmisartan
C_t3*Incidence/WTP_success # combination


##################
# constructing MCD values / NB function

INB_Event_nat <- 1 # means success

# expected events per year before taking account of additional costs
P_t0_U*INB_Event_nat*Incidence # 
P_t1_U*INB_Event_nat*Incidence 
P_t2_U*INB_Event_nat*Incidence  # 
P_t3_U*INB_Event_nat*Incidence  # 



stated_yrly_MCD_t1 <- C_t1*Incidence/WTP_success

stated_yrly_MCD_t2 <- C_t2*Incidence/WTP_success # telmisartan

stated_yrly_MCD_t3 <- C_t3*Incidence/WTP_success # combination

# finish derivation !!! 
##########################
# INB_t1 per year = NB_t1 - NB_t0
# INB_t1 = 0 when NB_t1 = NB_t0
# from MCD questions, NB_t1 = NB_t0 when 
# t1 results in 9,500 extra disease modifications (stated_yrly_MCD_t1) per year

# NB_t1 = NB_t0 when  P_t1*Incidence - P_t0*Incidence = 9500 (stated_yrly_MCD_t1)
# P_t0*Incidence = 0

# INB_t1 per year = NB_t1 - NB_t0

# P_t1*Incidence 

#                 = P_t1*Incidence
# finish derivation !!! 
##########################

MCD_nat_t1 <- -stated_yrly_MCD_t1/Incidence
MCD_nat_t2 <- -stated_yrly_MCD_t2/Incidence
MCD_nat_t3 <- -stated_yrly_MCD_t3/Incidence

NB_t0_U_nat <- P_t0_U*INB_Event_nat  # 
NB_t1_U_nat <- P_t1_U*INB_Event_nat + MCD_nat_t1 # 
NB_t2_U_nat <- P_t2_U*INB_Event_nat + MCD_nat_t2 # 
NB_t3_U_nat <- P_t3_U*INB_Event_nat + MCD_nat_t3 # 

NB_t1_U_nat
NB_t2_U_nat
NB_t3_U_nat
# very low probabiltiy of each treatment working! so all look really bad due to the 
# high costs associated with each one and the low probabiltiy of payoff

# redundant as they are always evaluated at expectation
E_NB_t0_U_nat <- mean(NB_t0_U_nat) # 
E_NB_t1_U_nat <- mean(NB_t1_U_nat) # 
E_NB_t2_U_nat <- mean(NB_t2_U_nat) # 
E_NB_t3_U_nat <- mean(NB_t3_U_nat) # 


# implcations for these MCD values
#####################################

# Expected Equivalent events per year for each treatment
# expected payoffs given we think there is some (very small!) prob they will work
E_NB_t0_U_nat*Incidence # negative as these outcomes are bad = least negative is best (t1)
E_NB_t1_U_nat*Incidence
E_NB_t2_U_nat*Incidence
E_NB_t3_U_nat*Incidence



# NB payoffs when the treatments work (per person)
NB_t0_payoff <- 0  
NB_t1_payoff <- INB_Event_nat + MCD_nat_t1
NB_t2_payoff <- INB_Event_nat + MCD_nat_t2
NB_t3_payoff <- INB_Event_nat + MCD_nat_t3


# NB payoffs when the treatments work (per year)
NB_t1_payoff*Incidence
NB_t2_payoff*Incidence
NB_t3_payoff*Incidence



# prob only t1 works                                    x payoff when t1 works
NB_EVTPI_U <- P_t1_U*(1 - P_t1_U*P_t2_U)*(1- P_t1_U*P_t3_U)*NB_t1_payoff + 
  # prob only t2 works                        x payoff when t2 works
  P_t2_U*(1 - P_t1_U*P_t2_U)*(1- P_t2_U*P_t3_U)*NB_t2_payoff + 
  # prob only t3 works                        x payoff when t3 works
  P_t3_U*(1 - P_t1_U*P_t3_U)*(1- P_t2_U*P_t3_U)*NB_t3_payoff + 
  # prob both t1 and t2 work x payoff when they both work (choose highest)
  P_t1_U*P_t2_U*max(NB_t1_payoff, NB_t2_payoff) +
  P_t1_U*P_t3_U*max(NB_t1_payoff, NB_t3_payoff) +
  P_t2_U*P_t3_U*max(NB_t2_payoff, NB_t3_payoff) +
  # prob t1, t2 and t3 all work x payoff when they all work (choose highest)
  P_t1_U*P_t2_U*P_t3_U*max(NB_t1_payoff, NB_t2_payoff, NB_t2_payoff)



########### TRIAL ANALYSIS ################################
########### TRIAL ANALYSIS ################################
########### TRIAL ANALYSIS ################################


# trial impact per year
########################

# Best outcome with uninformed prior 
# NOW DO NOT NEED TO ASSUME THIS 
# (assume that the new treatments cannot be implemented without a trial of some
# sort.) all of the treatments have NB very below zero!

NB_EVTCI_U = max(E_NB_t0_U, E_NB_t1_U, E_NB_t2_U, E_NB_t3_U) # 


# successes per year from doing the definitive trial vs implementing best treatment
NB_EVTPI_U*Incidence - NB_EVTCI_U*Incidence



#######################################
# INSTANT trial result (proposal says 6 years)
# IGNORE option to add extra trial arms (underestimates benefit)
# NATURAL OUTCOMES so completely ignores research costs to the NHS
# Population calculations - no delay (ignore prevalence!)
##############################################

Time_info = 20
Time_research = 0 # instant
Cost_research_NETSCC =  3310883
#Cost_research_NHS = 1297789

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

Time_info = 20
Time_research = 6 
Cost_research_NETSCC =  3310883
#Cost_research_NHS = 1297789

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

Time_info = 20
Time_research = 10 
Cost_research_NETSCC =  3310883
#Cost_research_NHS = 1297789

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
# Trial takes 10 years (runs over time)
# IGNORE option to add extra trial arms (underestimates benefit)

# Population calculations - no delay (ignore prevalence!)
##############################################

Time_info = 20
Time_research = 10 
Cost_research_NETSCC =  3310883
Cost_research_NHS = 1297789

Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))

## Net benfit of immediately implemeting best treatment (Approval)
NB_maxt_U <- Pop_total*NB_EVTCI_U


################### NB from costlessly and instantly) Implementing best treatment
# and carrying out perfect trial (with perfect implementing of result)
NB_E_maxt_trial_U <- Pop_during_trial*NB_EVTCI_U + Pop_after_trial*NB_EVTPI_U -
  Cost_research_NHS/k

# ICER
# difference in effects (between trial and implementign best treatment)
NB_E_maxt_trial_U - NB_maxt_U
# difference in costs (between trial and implementign best treatment)
Cost_research_NETSCC
# the ICER
Cost_research_NETSCC/(NB_E_maxt_trial_U - NB_maxt_U)












###################old stuff ##############################

#######################################
# INSTANT trial result (proposal says 6 years)
# IGNORE option to add extra trial arms (underestimates benefit)
# NATURAL OUTCOMES so completely ignores research costs to the NHS
# Population calculations - no delay (ignore prevalence!)
##############################################

Time_info = 20
Time_research = 0 # instant
Cost_research_NETSCC =  3310883
#Cost_research_NHS = 1297789

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
P2eventsInstantFreeTrialJustPrimary <- NB_E_maxt_trial_U - NB_maxt_U
# difference in costs (between trial and implementign best treatment)
Cost_research_NETSCC
# the ICER
P2ICERInstantFreeTrialJustPrimary <- Cost_research_NETSCC/(NB_E_maxt_trial_U - NB_maxt_U)




#######################################
# Trial takes 6 years (same duration as proposal says)
# IGNORE option to add extra trial arms (underestimates benefit)
# NATURAL OUTCOMES so completely ignores research costs to the NHS
# Population calculations - no delay (ignore prevalence!)
##############################################

Time_info = 20
Time_research = 6 
Cost_research_NETSCC =  3310883
#Cost_research_NHS = 1297789

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
P2eventsTrialJustPrimary <- NB_E_maxt_trial_U - NB_maxt_U
# difference in costs (between trial and implementign best treatment)
Cost_research_NETSCC
# the ICER
P2ICERTrialJustPrimary <- Cost_research_NETSCC/(NB_E_maxt_trial_U - NB_maxt_U)



#######################################
# Trial takes 10 years (proposal says 6 )
# IGNORE option to add extra trial arms (underestimates benefit)
# NATURAL OUTCOMES so completely ignores research costs to the NHS
# Population calculations - no delay (ignore prevalence!)
##############################################

Time_info = 20
Time_research = 10 
Cost_research_NETSCC =  3310883
#Cost_research_NHS = 1297789

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

















