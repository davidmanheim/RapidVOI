##############################
# Proposal 1: First episode psychosis
##############################

set.seed(20)
options(scipen=999) # trun off scientifc notation (for convergence axis)
library(scales) 

######################################################################################################
##### INPUTS


###################
# simulation resoultion
MCouter <- 400000 # NOTE not bad convergence of EVTPI at MCouter = 40000
MCinner <- 4000 
# MCinner <- 10000 # m simulations for each inner loop
ptm <- proc.time() # start the clock


#########################
# health system model inputs (some may not be needed)
Utilisation =  0  # current practice unclear from proposal - assume all using anti psychs
Incidence =  1563 # http://www.psymaptic.org/prediction/psychosis-incidence-national-summary/
# for 16-35 year olds 5939 (5785, 6109) 
# (taking central estimate and assuming constant rate over age range) 5939 cases over 19 years 
5939/19 # 312.5789 cases per year 
# need incidence for 14-18 year olds:
5*(5939/19) # 1563 FEP for 14-18 year olds


#Prevalence =  # ignore for now!
Time_info =  15 # assuming the area moves "slowly" 
D_rate =  0.035 
k = 15000 
#Time_research_pilot =  2
#Cost_research_pilot_NETSCC =  601480
#Cost_research_pilot_NHS  = 150000

# proposal 2 is complex design, 
# proposal 3 primary analysis takes 7 years £2.5M for NETSCC, - 62M for NHS
# proposal 4 brief trial of booklet - not v.relevant
# proposal 5 TBI 5 yrs and 2.9M for netscc and 490K for NHS, proposal 6 non randomised rare disease
# take average length proposal 3 and 5 = 6 years
# NETSCC costs probably closer to proposal 3 = £2.5M
# NHS definitive research costs costs propoably closer to proposal 5 = £490K

#Time_research_definitive = 6
#Cost_research_definitive_NETSCC =  2500000
#Cost_research_definitive_NHS  = 450000

###################
# Epi parameters

# Uninformed prior on LOR for talking and APs
###################
# previously tried to put prior on interaction term 

# primary outcome should be evaluated at the same time as the definitive trial - we do not know when
# this outcome will be evaluated. Assume evaluated at 1 year. 

## Baseline (t0): APs
#############
P_t0_U <- 0.37 # probability of relapse at 1 year on APs in report
# assume certain for now

Odds_t0_U <- P_t0_U / (1 - P_t0_U)
LO_t0_U <- log(Odds_t0_U)

eventsPerYearCurrent <- round(P_t0_U*Incidence,0) # relapses per year under standard care

## new treatment one (t1): CBT + FI 
###############################
# no information on the potential improvement of CBT
sigma_t1_U <- 0.5 # for t0 prob 50% this is 95% 0.25 - 0.75 probability. plus minus 25%
LOR_t1_U <- rnorm(MCouter, 0, sigma_t1_U)
#plot(density(LOR_t1_U))

LO_t1_U <- LO_t0_U + LOR_t1_U 
Odds_t1_U <- exp(LO_t1_U)
P_t1_U <- Odds_t1_U / (Odds_t1_U + 1) 

## combination (t2): CBT + FI + APs
#####################################
# Assume treatment effect is exchangable with t1 - no reason to think it will be better or worse
# more complex assumption possible but would require work.
# assume all 3 treatments exchangable - we dont know expect the combination to be better
# than the others : would require methods development to specify this properly, see:
# http://onlinelibrary.wiley.com/doi/10.1002/(SICI)1521-4036(199804)40:1%3C43::AID-BIMJ43%3E3.0.CO;2-J/abstract
# https://www.jstor.org/stable/2533949?seq=1#page_scan_tab_contents

# no information on the potential improvement of combination
sigma_t2_U <- 0.5 # for t0 prob 50% this is 95% 0.25 - 0.75 probability. plus minus 25%
LOR_t2_U <- rnorm(MCouter, 0, sigma_t2_U)
#plot(density(LOR_t1_U))

LO_t2_U <- LO_t0_U + LOR_t2_U 
Odds_t2_U <- exp(LO_t2_U)
P_t2_U <- Odds_t2_U / (Odds_t2_U + 1)




# first model: assume always better than other two with an interation effect
#P_t2_U_no_interaction <- apply(cbind(rep(P_t0_U,MCouter), P_t1_U), 1, min)
#Odds_t2_U_no_interaction <- P_t2_U_no_interaction/ (1 - P_t2_U_no_interaction)
#LO_t2_U_no_interaction <- log(Odds_t2_U_no_interaction)
#library(fdrtool) #load up half normal distribution package into R
#sigma_t2_interaction_U <- 0.1 # sd of interaction effect , defined to be always positive below
#LOR_t2_U_interaction <- - rhalfnorm(MCouter, theta=sd2theta(sigma_t2_interaction_U)) 
# note: could potentially inform this interaction effect empirically - informative prior.
#LO_t2_U <- LO_t2_U_no_interaction + LOR_t2_U_interaction 
#Odds_t2_U <- exp(LO_t2_U)
#P_t2_U <- Odds_t2_U/(Odds_t2_U + 1)

#plot(density(P_t2_U))
#plot(density(P_t1_U))
#plot(density(P_t2_U_no_interaction))



# a priori events (not taking into account MCD)
P_t0_U*Incidence
mean(P_t1_U)*Incidence
mean(P_t2_U)*Incidence


#################################
# NATURAL OUTCOME - NOT INCLUDING SECONDARY OUTCOMES!!!
##################################


####################################################
# VOI just using primary outcome
INB_Event_nat = -1 # the event is harmful


# matrix for calculating VOI outputs- needs to use simulations
NB_t_U  <- cbind(P_t0_U*INB_Event_nat, 
                    P_t1_U*INB_Event_nat , 
                    P_t2_U*INB_Event_nat )

# force each to have the same expected benefit as the primary outcome 
E_NB_t0_U  <- E_NB_t1_U  <- E_NB_t2_U  <-  mean(P_t0_U*INB_Event_nat )


# trial impact per year
########################

# Best outcome with uninformed prior 
#EVTCI_U  = max(0, E_INB_t1_U ) # in this case should always equal zero if no Monte carlo error
NB_EVTCI_U  = max(E_NB_t0_U , E_NB_t1_U , E_NB_t2_U ) # 

# EVTPI with uninformed prior and natural outcome
#EVTPI_U  <- mean(apply(INB_t_U , 1, max))
NB_VTPI_U  <- apply(NB_t_U , 1, max) #so I can check convergence
NB_EVTPI_U  <- mean(NB_VTPI_U )
NB_EVPI_U  <-  NB_EVTPI_U  - NB_EVTCI_U 
#Running mean to check convergence
#EVTPI.run<-c(rep(0,150))
#for (i in 1:150){
#  EVTPI.run[i]<-mean(NB_VTPI_U [1:(i*(MCouter/150))])
#}
#plot(seq(1,i*(MCouter/150), length.out = length(EVTPI.run)), EVTPI.run, type="l",lty=1, xlab="Simulation", ylab="EVTPI")
# does not really appear to be converged until 45K as far as we can tell
# need to check when converted to natrual outcomes

# relapses prevented per year from doing the definitive trial
eventsPerYearJustPrimary <- round(NB_EVTPI_U *Incidence - NB_EVTCI_U *Incidence,0)



########### TRIAL ANALYSIS (primary outcome only)
# note *this completely ignores the costs imposed on the NHS from running trial (only NETSCC costs)
# SKIP pilot trial and go straight to instant definitive trial!
# INPUTS
Time_research_pilot =  0
Time_research_definitive = 0
Probability_of_definitive_research = 1
Cost_research_pilot_NETSCC =  0 # 601480 (assume there is no pilot)
#Cost_research_pilot_NHS  = 150000 # ignored in this analysis
Cost_research_definitive_NETSCC =  2500000
#Cost_research_definitive_NHS  = 450000 # ignored in this analysis


Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
Pop_during_pilot_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research_pilot) - exp(-D_rate*0))
Pop_during_definitive_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*(Time_research_pilot+Time_research_definitive)) - exp(-D_rate*Time_research_pilot))
Pop_after_definitive_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*(Time_research_pilot+Time_research_definitive)))

## Net benfit of immediately implemeting best treatment (Approval)
NB_maxt_U  <- Pop_total*NB_EVTCI_U 

## Net benefit of maxt trial (AWR)
# get treated with best treatment during trials
NB_E_maxt_trial_U  <- Pop_during_pilot_research*NB_EVTCI_U  + Pop_during_definitive_research*NB_EVTCI_U  +
  # definitive trial happens
  Probability_of_definitive_research*Pop_after_definitive_research*NB_EVTPI_U  +
  # definitive trial does not happen (just get treated with the best treatment )
  (1-Probability_of_definitive_research)*Pop_after_definitive_research*NB_EVTCI_U 

# ICER
# difference in effects (between trial and implementign best treatment)
eventsInstantFreeTrialJustPrimary <- round(NB_E_maxt_trial_U  - NB_maxt_U ,0)
# difference in costs (between trial and implementign best treatment)
E_Cost_NETSCC  <- Cost_research_pilot_NETSCC + Probability_of_definitive_research*Cost_research_definitive_NETSCC
E_Cost_NETSCC
# the ICER
ICERInstantFreeTrialJustPrimary <-  dollar_format(prefix = "£", largest_with_cents = 0)(E_Cost_NETSCC/(NB_E_maxt_trial_U  - NB_maxt_U ))



########### TRIAL ANALYSIS (primary outcome only)
# note *this completely ignores the costs imposed on the NHS from running trial (only NETSCC costs)
# 2 year pilot with 50% chance of 6 year definitive trial
# INPUTS
Time_research_pilot =  2
Time_research_definitive = 6
Probability_of_definitive_research = 0.5
Cost_research_pilot_NETSCC =   601480 
# Cost_research_pilot_NHS  = 150000 # ignored in this analysis
Cost_research_definitive_NETSCC =  2500000
#Cost_research_definitive_NHS  = 450000 # ignored in this analysis


Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
Pop_during_pilot_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research_pilot) - exp(-D_rate*0))
Pop_during_definitive_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*(Time_research_pilot+Time_research_definitive)) - exp(-D_rate*Time_research_pilot))
Pop_after_definitive_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*(Time_research_pilot+Time_research_definitive)))

## Net benfit of immediately implemeting best treatment (Approval)
NB_maxt_U  <- Pop_total*NB_EVTCI_U 

## Net benefit of maxt trial (AWR)
# Pop during the trials: get treated with best treatment during trials 


NB_E_maxt_trial_U  <- 
  # If definitive trial HAPPENS
  Probability_of_definitive_research*(
    Pop_during_pilot_research*NB_EVTCI_U  + Pop_during_definitive_research*NB_EVTCI_U  +
      Pop_after_definitive_research*NB_EVTPI_U 
  ) +
  # If definitive trial DOES NOT happen (everybody just gets best treatment)
  (1 - Probability_of_definitive_research)*(
    Pop_total*NB_EVTCI_U 
  )




# ICER
# difference in effects (between trial and implementign best treatment)
eventsProperPilotTrialJustPrimary <- round(NB_E_maxt_trial_U  - NB_maxt_U ,0)
# difference in costs (between trial and implementign best treatment)
E_Cost_NETSCC  <- 
  Cost_research_pilot_NETSCC +  # always incur this cost
  # If definitive trial HAPPENS
  Probability_of_definitive_research*Cost_research_definitive_NETSCC

E_Cost_NETSCC
# the ICER
ICERProperPilotTrialJustPrimary <- dollar_format(prefix = "£", largest_with_cents = 0)(E_Cost_NETSCC/(NB_E_maxt_trial_U  - NB_maxt_U ))



# expected research costs imposed on NHS
Cost_research_pilot_NHS  = 150000 # ignored in this analysis
Cost_research_definitive_NHS  = 450000 # ignored in this analysis

E_Cost_NHS <- 
  Cost_research_pilot_NHS  +  # always incur this cost
  # If definitive trial HAPPENS
  Probability_of_definitive_research*Cost_research_definitive_NHS 
NHSCostsProperPilotTrialJustPrimary <- dollar_format(prefix = "£", largest_with_cents = 0)(E_Cost_NHS)


#########################################################################
# end of primary only analysis
######################################################

# take account of side effects and costs etch

#############################
# costs
#############################
# just one year so no need to discount

# Cost of new treatment:
# Up to 30 sessions will be delivered to CYP's over 6 months. FI involves an extra 6 sessions
# assume an average of half the sessions for each will be attended: 15 CBT and 3 FI

# http://www.pssru.ac.uk/project-pages/unit-costs/2016/
# page 77 £97 per CBT session
# 18 sessions expected (they will go to half)
C_t1 <- 97*18  
C_t1_format <- dollar_format(prefix = "£", largest_with_cents = 0)(C_t1)

# cost of antipyschotics (baseline treatment)
# https://www.nice.org.uk/guidance/cg178/evidence/full-guideline-pdf-490503565
# page 421 - 

# consider time horizon of 2 years: 1373.52 for drugs (57.23*24)
# assumes that use of any of the durgs in adult pyschosis NICE guideline equally likely
# two years of durgs

#57.23 #Amisulpride  £
#14.35 #Haloperidol
#85.13 #Olanzapine 
#108.89 #Aripiprazole  £
#156.34 # Paliperidone  £
#67.52 #Risperidone 
#63.03 #Zotepine  
#6.7 #Flupentixoldecanoate 
# average cost : £57.23 per month for drugs
C_t0 <- mean(57.23 ,14.35 ,85.13 ,108.89 ,156.34 ,67.52 ,63.03 ,6.7)*12 # for 1 year of durgs

# 2.5 vists to psychiatrist over 6 months p425 NICE guidance
# each visit costs £240
# can assume that every patient recieves these costs.

C_t0_format <- dollar_format(prefix = "£", largest_with_cents = 0)(C_t0)

# for combination:
C_t2 <- 97*18  + mean(57.23 ,14.35 ,85.13 ,108.89 ,156.34 ,67.52 ,63.03 ,6.7)*12
C_t2_format <- dollar_format(prefix = "£", largest_with_cents = 0)(C_t2)




###################
## defining MCD
####################

# secondary outcomes :

# t0 (APs) and t2 (combination) side efects: 
# weight gain                cost of treating £177 p437 NICE                   
# extraparymidial symptoms : cost of treating £177 p437 NICE


# costs: 
C_t0 # APs
C_t1 # talking
C_t2 # combination

######## cost table

C_t0_yrly <- C_t0*Incidence # APs
C_t1_yrly <- C_t1*Incidence # talking
C_t2_yrly <- C_t2*Incidence # combination


# incremental costs

C_t1*Incidence - C_t0*Incidence # inc cost talking
C_t2*Incidence - C_t0*Incidence # inc cost combination

# AP montherapy (to) cheapest but more (potentially expensive )side effects 
# than talking therapy

# talking therapy (t1) no side effect expected

# Combination (t2): most expensive and more side effects than talking therapy




########################################
# QALYs / utilities of relapse (event) vs remission (no event)
#####################################

# from Singapore study
# http://www.psy-journal.com/article/S0165-1781(13)00650-1/pdf 
# This is associated with utility of 0.926 for remission and 0.850 for non-remission. 
# Difference of 0.076 (0.009, 0.142)

# use NICE guideline - judged to be relevant
# from NICE guideline https://www.nice.org.uk/guidance/cg178/evidence/full-guideline-pdf-490503565
# p434
# remission: 0.799
# relapse: 0.670
# difference: 0.129

# assume relapse happens half way through time horizon
Model_time_horizon <- 1

Utility_post_event <- 0.670 # 
Utility_pre_event <- 0.799

QALY_event <- Utility_pre_event*Model_time_horizon/2 + Utility_post_event*Model_time_horizon/2
QALY_noEvent <- Utility_pre_event*Model_time_horizon


##################################
# Cost of relapse (event) vs remission (no event)
########################

# use NICE guideline - judged to be relevant
# from NICE guideline https://www.nice.org.uk/guidance/cg178/evidence/full-guideline-pdf-490503565
# p436
# remission: £12,726 per year
# relapse: £33,018 per year

# assume relapse happens half way through time horizon

Cost_pre_event <- 12726
Cost_post_event <- 33018

Cost_event <-  Cost_pre_event*Model_time_horizon/2 + Cost_post_event*Model_time_horizon/2
Cost_noEvent <- Cost_pre_event*Model_time_horizon

#############################
# INB of relapse (compared to staying relapse free)
##############################

# qalyS lost from relapse
QALY_noEvent - QALY_event

# costs inclurred from relapse
Cost_noEvent - Cost_event

INB_Event <- QALY_event - QALY_noEvent -(Cost_event - Cost_noEvent)/15000




# MCD : everything is compared to AP monotherapy

# the primary outcome is beneficial
# MCD_t1 : relative to t0 this has beneficial secondary outcomes: MCD_t1>0
# MCD_t2 : relative to t0 this has negative secondary outcomes : MCD_t2 <0


# Question for clinicians (everything quoted per year)
# comparing everything to the current standard of care: AP monotherapy (717 relapses per year)
# talking therapy is £600K more costly with lower/no side effects 
# APs are associated with 717 relapses per year,
# what number of relapses for talking therapy would make these treatments equivalent?
# say 730 relapses per year for talking therapy as more expensive but considered more
# acceptable to patients, on balance a similar effectiveness is required.
# stated_yrly_MCD_t1 <-  

# Note: a natural objection to this analysis is that it will be a matter of matching patient to the
# therapy (a subgroup / individualised care problem - could develop this framework. Note: the trial
# is not designed to understand this matching problem!) Ignore for now.

# comparing everything to the current standard of care: AP monotherapy
# Combination therapy is £2.8M more costly than monotherapy
# to make up for the extra costs of combination therapy 
# what number of relapses for combination thearpy would make these treatments equivalent? 
# say 600 relapses per year for combination therapy as much more expensive so a 
# substantial improvement in effectiveness is required
# stated_yrly_MCD_t2 <-  


# percentage change in relapses compared to APs
#MCD_t1 <- 0.07 # for talking therapy
#MCD_t1 # postive MCD as t1 better than t0 on secondary outcome
#MCD_t1 <- 0 # round down to zero in this case

#MCD_t2 <- (stated_yrly_MCD_t2 - P_t0_U*Incidence)/(P_t0_U*Incidence) # for combination therapy
 # negative MCD as t2 worse than t0 on secondary outcome
#MCD_t2 <- 0
#INB_Event_nat = -1 # the event is harmful

MCD_t1 <- 0.07 # for talking therapy
MCD_t2 <- 0

# number of additional relapses for talking therapy
MCDImpliedIncrease_t1 <- round(P_t0_U*Incidence + MCD_t1*P_t0_U*Incidence,0)




#####################################################################################
# Economic inputs
# take account of QALYs, SEs and aquisition costs
######################################################################################

###############################################
# Put together mini economic model

NB_t0_U <- P_t0_U*INB_Event - C_t0/k # 
NB_t1_U <- (P_t1_U + MCD_t1)*INB_Event - C_t1/k # 
NB_t2_U <- (P_t2_U + MCD_t2)*INB_Event - C_t2/k # 


############################
# pre trial expected NB of outcomes
#########################

E_NB_t0_U <- mean(NB_t0_U) # 
E_NB_t1_U <- mean(NB_t1_U) # 
E_NB_t2_U <- mean(NB_t2_U) # 

# incremental benefit compared to standard care (APs)
E_INB_t0_U <- 0
E_INB_t1_U <- E_NB_t1_U - E_NB_t0_U
E_INB_t2_U <- E_NB_t2_U - E_NB_t0_U




# matrix for calculating VOI outputs- needs to use simulations
###########################################################

NB_t_U <- cbind(NB_t0_U, 
                NB_t1_U, 
                NB_t2_U)


# trial impact per year
########################

# Best outcome with uninformed prior 
NB_EVTCI_U = max(E_NB_t0_U, E_NB_t1_U, E_NB_t2_U) # 

# EVTPI with uninformed prior and natural outcome
#EVTPI_U  <- mean(apply(INB_t_U , 1, max))
NB_VTPI_U <- apply(NB_t_U, 1, max) #so I can check convergence
NB_EVTPI_U <- mean(NB_VTPI_U)
NB_EVPI_U <-  NB_EVTPI_U - NB_EVTCI_U
#Running mean to check convergence
#EVTPI.run<-c(rep(0,150))
#for (i in 1:150){
#  EVTPI.run[i]<-mean(NB_VTPI_U [1:(i*(MCouter/150))])
#}
#plot(seq(1,i*(MCouter/150), length.out = length(EVTPI.run)), EVTPI.run, type="l",lty=1, xlab="Simulation", ylab="EVTPI")
# does not really appear to be converged until 45K as far as we can tell
# need to check when converted to natrual outcomes

# QALYs prevented per year from doing the definitive trial
QALYsPerYear <- NB_EVTPI_U*Incidence - NB_EVTCI_U*Incidence





########### TRIAL ANALYSIS (QALYs) ################################################
# SKIP pilot trial and go straight to instant definitive trial!
# INPUTS
Time_research_pilot =  0
Time_research_definitive = 0
Probability_of_definitive_research = 1
Cost_research_pilot_NETSCC =  0 # 601480 (assume there is no pilot)
Cost_research_pilot_NHS  = 0 #150000 # (assume there is no pilot)
Cost_research_definitive_NETSCC =  2500000
Cost_research_definitive_NHS  = 450000 # 


Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
Pop_during_pilot_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research_pilot) - exp(-D_rate*0))
Pop_during_definitive_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*(Time_research_pilot+Time_research_definitive)) - exp(-D_rate*Time_research_pilot))
Pop_after_definitive_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*(Time_research_pilot+Time_research_definitive)))

## Net benfit of immediately implemeting best treatment (Approval)
NB_maxt_U <- Pop_total*NB_EVTCI_U


# Net benefit of trial
NB_E_maxt_trial_U <- 
  # If definitive trial HAPPENS
  Probability_of_definitive_research*(
      Pop_during_pilot_research*NB_EVTCI_U + Pop_during_definitive_research*NB_EVTCI_U +
      Pop_after_definitive_research*NB_EVTPI_U - 
      Cost_research_pilot_NHS /k - # NHS cost of research (pilot cost = 0 in this case)
      Cost_research_definitive_NHS /k # NHS cost of research 
  ) +
  # If definitive trial DOES NOT happen (everybody just gets best treatment)
  (1 - Probability_of_definitive_research)*(
      Pop_total*NB_EVTCI_U -
      Cost_research_pilot_NHS /k # cost of the pilot (still have to pay it!)
  )




# ICER
# difference in effects (between trial and implementign best treatment)
QALYsInstantFreeTrial <- NB_E_maxt_trial_U - NB_maxt_U
# difference in costs (between trial and implementign best treatment)
E_Cost_NETSCC  <- 
  Cost_research_pilot_NETSCC +  # always incur this cost
  # If definitive trial HAPPENS
  Probability_of_definitive_research*Cost_research_definitive_NETSCC

E_Cost_NETSCC
# the ICER
ICERInstantFreeTrialQALYs <- E_Cost_NETSCC/(NB_E_maxt_trial_U - NB_maxt_U)





########### TRIAL ANALYSIS (QALYs) ################################################
# 
# 2 year pilot with 50% chance of 6 year definitive trial
# INPUTS
Time_research_pilot =  2
Time_research_definitive = 6
Probability_of_definitive_research = 0.5
Cost_research_pilot_NETSCC =   601480 
Cost_research_pilot_NHS  = 150000 
Cost_research_definitive_NETSCC =  2500000
Cost_research_definitive_NHS  = 450000 


Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
Pop_during_pilot_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research_pilot) - exp(-D_rate*0))
Pop_during_definitive_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*(Time_research_pilot+Time_research_definitive)) - exp(-D_rate*Time_research_pilot))
Pop_after_definitive_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*(Time_research_pilot+Time_research_definitive)))

## Net benfit of immediately implemeting best treatment (Approval)
NB_maxt_U  <- Pop_total*NB_EVTCI_U 

## Net benefit of maxt trial (AWR)
# Pop during the trials: get treated with best treatment during trials 

NB_E_maxt_trial_U <- 
  # If definitive trial HAPPENS
  Probability_of_definitive_research*(
    Pop_during_pilot_research*NB_EVTCI_U + Pop_during_definitive_research*NB_EVTCI_U +
      Pop_after_definitive_research*NB_EVTPI_U - 
      Cost_research_pilot_NHS /k - # NHS cost of research (pilot cost = 0 in this case)
      Cost_research_definitive_NHS /k # NHS cost of research 
  ) +
  # If definitive trial DOES NOT happen (everybody just gets best treatment)
  (1 - Probability_of_definitive_research)*(
    Pop_total*NB_EVTCI_U -
      Cost_research_pilot_NHS /k # cost of the pilot (still have to pay it!)
  )




# ICER
# difference in effects (between trial and implementign best treatment)
QALYsProperPilotTrial <- NB_E_maxt_trial_U - NB_maxt_U
# difference in costs (between trial and implementign best treatment)
E_Cost_NETSCC  <- 
  Cost_research_pilot_NETSCC +  # always incur this cost
  # If definitive trial HAPPENS
  Probability_of_definitive_research*Cost_research_definitive_NETSCC

E_Cost_NETSCC
# the ICER
ICERProperPilotTrialQALYs <- E_Cost_NETSCC/(NB_E_maxt_trial_U - NB_maxt_U)




###################################################################################################################
  # implementation value vs information value
###############################################################################################################
# just for proper analysis
# using Fenwick implementation framework
# Cell_A = net benefit of current situation
# Cell_C = net benefit of perfect implementation with current info
# Cell_D = net benefit of research with perfect implementation (AWR type scenario)

#Cell_D - Cell_A # total value of the research project
#Cell_C - Cell_A # implementation value 
#Cell_D - Cell_C # information value



# simultaneous analysis
######################
# 2 year pilot with 50% chance of 6 year definitive trial
# Base case INPUTS
Time_research_pilot =  2
Time_research_definitive = 6
Probability_of_definitive_research = 0.5
Cost_research_pilot_NETSCC =   601480 
Cost_research_pilot_NHS  = 150000 
Cost_research_definitive_NETSCC =  2500000
Cost_research_definitive_NHS  = 450000 


Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
Pop_during_pilot_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research_pilot) - exp(-D_rate*0))
Pop_during_definitive_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*(Time_research_pilot+Time_research_definitive)) - exp(-D_rate*Time_research_pilot))
Pop_after_definitive_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*(Time_research_pilot+Time_research_definitive)))


## Cell_A : net benefit of current situation
# here assume everybody gets antipsychotics
Utilisation_t0 <- 1
Utilisation_t1 <- 0
Utilisation_t2 <- 0
Cell_A <- Pop_total*E_NB_t0_U*Utilisation_t0 +
  Pop_total*E_NB_t1_U*Utilisation_t1 +
  Pop_total*E_NB_t2_U*Utilisation_t2

## Cell_C : Net benfit of immediately implemeting best treatment (Approval)
NB_maxt_U <- Pop_total*NB_EVTCI_U
Cell_C <- NB_maxt_U

## Cell_D : Net benefit of maxt trial (AWR)
# Pop during the trials: get treated with best treatment during trials 

NB_E_maxt_trial_U <- 
  # If definitive trial HAPPENS
  Probability_of_definitive_research*(
    Pop_during_pilot_research*NB_EVTCI_U + Pop_during_definitive_research*NB_EVTCI_U +
      Pop_after_definitive_research*NB_EVTPI_U - 
      Cost_research_pilot_NHS /k - # NHS cost of research (pilot cost = 0 in this case)
      Cost_research_definitive_NHS /k # NHS cost of research 
  ) +
  # If definitive trial DOES NOT happen (everybody just gets best treatment)
  (1 - Probability_of_definitive_research)*(
    Pop_total*NB_EVTCI_U -
      Cost_research_pilot_NHS /k # cost of the pilot (still have to pay it!)
  )

Cell_D <- NB_E_maxt_trial_U


# ICER for entire project:
# difference in effects (between trial and implementign best treatment)
QALYsProperPilotTrial <- NB_E_maxt_trial_U - NB_maxt_U
# difference in costs (between trial and implementign best treatment)
E_Cost_NETSCC  <- 
  Cost_research_pilot_NETSCC +  # always incur this cost
  # If definitive trial HAPPENS
  Probability_of_definitive_research*Cost_research_definitive_NETSCC

E_Cost_NETSCC
# the ICER
ICERProperPilotTrialQALYs <- E_Cost_NETSCC/(NB_E_maxt_trial_U - NB_maxt_U)


# research project has negative value as the VOI is low and the research costs are high
Cell_D - Cell_A # total value of the research project
Cell_C - Cell_A # implementation value 
Cell_D - Cell_C # net information value (benefit of information - costs)




#############################################################################################

######################################################################## ##########
 #            sensitivity analysis 
########################################################################


# inputs standard acorss scenarios
################################
k = 15000
D_rate = 0.035

MCouter <- 400000
P_t0_U <- 0.37 # probability of relapse at 1 year on APs in report
Odds_t0_U <- P_t0_U / (1 - P_t0_U)
LO_t0_U <- log(Odds_t0_U)
sigma_t1_U <- 0.5
LOR_t1_U <- rnorm(MCouter, 0, sigma_t1_U)
LO_t1_U <- LO_t0_U + LOR_t1_U 
Odds_t1_U <- exp(LO_t1_U)
P_t1_U <- Odds_t1_U / (Odds_t1_U + 1) 
sigma_t2_U <- 0.5 # for t0 prob 50% this is 95% 0.25 - 0.75 probability. plus minus 25%
LOR_t2_U <- rnorm(MCouter, 0, sigma_t2_U)
LO_t2_U <- LO_t0_U + LOR_t2_U 
Odds_t2_U <- exp(LO_t2_U)
P_t2_U <- Odds_t2_U / (Odds_t2_U + 1)


##################################################################################
# start pilot study function
##############################################################


PilotStudyVOI <- function(C_t0, C_t1, C_t2, MCD_t1, MCD_t2, INB_Event,
                          Utilisation_t0, Utilisation_t1, Utilisation_t2,
                          Incidence,  Time_research_definitive , Time_info,
                          Cost_research_pilot_NETSCC, Cost_research_pilot_NHS , Cost_research_definitive_NETSCC , Cost_research_definitive_NHS ,
                          Time_research_pilot,  Probability_of_definitive_research){
  
  
  # Put together mini economic model
  #########################################
  
  NB_t0_U <- P_t0_U*INB_Event - C_t0/k # 
  NB_t1_U <- (P_t1_U + MCD_t1)*INB_Event - C_t1/k # note this function uses P_t1_U i.e. vectors which are already in the global environment
  NB_t2_U <- (P_t2_U + MCD_t2)*INB_Event - C_t2/k # i.e. this function is not stand alone!
  
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
  
  
  # trial inputs
  ########################
  
  # Best outcome with uninformed prior 
  NB_EVTCI_U = max(E_NB_t0_U, E_NB_t1_U, E_NB_t2_U) # 
  
  # EVTPI with uninformed prior and natural outcome
  #EVTPI_U  <- mean(apply(INB_t_U , 1, max))
  NB_VTPI_U <- apply(NB_t_U, 1, max) #so I can check convergence
  NB_EVTPI_U <- mean(NB_VTPI_U)
  NB_EVPI_U <-  NB_EVTPI_U - NB_EVTCI_U
  
  # Population calculations
  Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
  Pop_during_pilot_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research_pilot) - exp(-D_rate*0))
  Pop_during_definitive_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*(Time_research_pilot+Time_research_definitive)) - exp(-D_rate*Time_research_pilot))
  Pop_after_definitive_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*(Time_research_pilot+Time_research_definitive)))
  
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
  
  ## Cell_D : Net benefit of maxt trial (AWR)
  # Pop during the trials: get treated with best treatment during trials 
  
  NB_E_maxt_trial_U <- 
    # If definitive trial HAPPENS
    Probability_of_definitive_research*(
      Pop_during_pilot_research*NB_EVTCI_U + Pop_during_definitive_research*NB_EVTCI_U +
        Pop_after_definitive_research*NB_EVTPI_U - 
        Cost_research_pilot_NHS /k - # NHS cost of research (pilot cost = 0 in this case)
        Cost_research_definitive_NHS /k # NHS cost of research 
    ) +
    # If definitive trial DOES NOT happen (everybody just gets best treatment)
    (1 - Probability_of_definitive_research)*(
      Pop_total*NB_EVTCI_U -
        Cost_research_pilot_NHS /k # cost of the pilot (still have to pay it!)
    )
  
  Cell_D <- NB_E_maxt_trial_U
  
  
  # ICER for entire project:
  # difference in effects (between trial and implementign best treatment)
  QALYsProperPilotTrial <- NB_E_maxt_trial_U - NB_maxt_U
  # difference in costs (between trial and implementign best treatment)
  E_Cost_NETSCC  <- 
    Cost_research_pilot_NETSCC +  # always incur this cost
    # If definitive trial HAPPENS
    Probability_of_definitive_research*Cost_research_definitive_NETSCC
  
  #E_Cost_NETSCC
  # the ICER
  ICERProperPilotTrialQALYs <- E_Cost_NETSCC/(NB_E_maxt_trial_U - NB_maxt_U)
  
  
  # research project has negative value as the VOI is low and the research costs are high
  #Cell_D - Cell_A # total value of the research project
  #Cell_C - Cell_A # implementation value 
  #Cell_D - Cell_C # net information value (benefit of information - costs)
  
  Total_NB_research <- Cell_D -  Cell_A
  Implementation_NB_research <- Cell_C -  Cell_A
  Information_NB_research <- Cell_D -  Cell_C
  # % of total value which is implementation value
  Perc_implementation_NB <- ( Cell_C -  Cell_A)/( Cell_D -  Cell_A)*100
  # % of total value which is pure information value
  Perc_information_NB <- ( Cell_D -  Cell_C)/( Cell_D -  Cell_A)*100
  
  
  output <- list(Cell_A=Cell_A, Cell_C=Cell_C, Cell_D=Cell_D, 
                 NETSCC_cost = E_Cost_NETSCC, ICER_project = ICERProperPilotTrialQALYs,
                 Total_NB_research= Total_NB_research, 
                 Implementation_NB_research = Implementation_NB_research, Information_NB_research=Information_NB_research,
                 Perc_implementation_NB=Perc_implementation_NB, Perc_information_NB=Perc_information_NB)
  
  return(output)
  
}



# base case analysis
#########################

# base case inputs
BaseCaseOutput <- PilotStudyVOI(
  Incidence = 1563
  ,Time_info = 15
  ,INB_Event = -0.7409
  ,Time_research_pilot = 2
  ,Time_research_definitive = 6
  ,Probability_of_definitive_research = 0.5
  ,C_t0 = mean(57.23 ,14.35 ,85.13 ,108.89 ,156.34 ,67.52 ,63.03 ,6.7)*12
  ,C_t1 = 97*18
  ,C_t2 = 97*18  + mean(57.23 ,14.35 ,85.13 ,108.89 ,156.34 ,67.52 ,63.03 ,6.7)*12
  ,MCD_t1 = 0.07
  ,MCD_t2 = 0
  ,Cost_research_pilot_NETSCC =   601480 
  ,Cost_research_pilot_NHS  = 150000 
  ,Cost_research_definitive_NETSCC =  2500000
  ,Cost_research_definitive_NHS  = 450000 
  ,Utilisation_t0 = 1
  ,Utilisation_t1 = 0
  ,Utilisation_t2 = 0)


BaseCaseICER <- BaseCaseOutput$ICER_project
BaseCaseNHE <- BaseCaseOutput$Total_NB_research
BaseCaseNETSCC_cost <- BaseCaseOutput$NETSCC_cost
BaseCasePerc_information_NB <- BaseCaseOutput$Perc_information_NB

# Optimistic analysis
#########################

# Optimistic inputs
OptimisticOutput <- PilotStudyVOI(
  Incidence = 1563*1.2
  ,Time_info = 15*1.2
  ,INB_Event = -0.7409*1.2
  ,Time_research_pilot = 2*0.8
  ,Time_research_definitive = 6*0.8
  ,Probability_of_definitive_research = 0.5*1.2 # not clear that this should improve NB unambiguously
  ,C_t0 = mean(57.23 ,14.35 ,85.13 ,108.89 ,156.34 ,67.52 ,63.03 ,6.7)*12
  ,C_t1 = 97*18
  ,C_t2 = 97*18  + mean(57.23 ,14.35 ,85.13 ,108.89 ,156.34 ,67.52 ,63.03 ,6.7)*12
  ,MCD_t1 = 0.07
  ,MCD_t2 = 0
  ,Cost_research_pilot_NETSCC =   601480 *0.8
  ,Cost_research_pilot_NHS  = 150000 *0.8
  ,Cost_research_definitive_NETSCC =  2500000*0.8
  ,Cost_research_definitive_NHS  = 450000 *0.8
  ,Utilisation_t0 = 1
  ,Utilisation_t1 = 0
  ,Utilisation_t2 = 0)

OptimisticICER <- OptimisticOutput$ICER_project
OptimisticOutput$Cell_A
OptimisticOutput$Cell_C
OptimisticOutput$Cell_D
OptimisticNHE <- OptimisticOutput$Total_NB_research
OptimisticNETSCC_cost <- OptimisticOutput$NETSCC_cost










































################################ NO LONGER NEEDED? ##############################
# DOES NOT HAVE ANY EFFECT ON RESULT - change in NB of trial with change in probability appears to be linear!
# could check this.


########### TRIAL ANALYSIS ################################################
# Uncertain about probability of definitive trial
# 2 year pilot with 40%-60% chance of 6 year definitive trial
# INPUTS
Time_research_pilot =  2
Time_research_definitive = 6
# define probability of research using a beta with 95% density over 0.4 and 0.6
# method of moments from spiegelhalter
# : if it was normal => mean (m)=0.5 and standard deviation (s) 
# within 0.5 +-2s includes 95% probability => s = 0.05
# https://stats.stackexchange.com/questions/12232/calculating-the-parameters-of-a-beta-distribution-using-the-mean-and-variance
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}
estBetaParams(0.5, 0.05^2)

#Probability_of_definitive_research simulation i  = pi_i 
pi_i_vec = rbeta(10000, 49.5, 49.5)
Cost_research_pilot_NETSCC =   601480 
Cost_research_pilot_NHS  = 150000 
Cost_research_definitive_NETSCC =  2500000
Cost_research_definitive_NHS  = 450000 


Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
Pop_during_pilot_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research_pilot) - exp(-D_rate*0))
Pop_during_definitive_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*(Time_research_pilot+Time_research_definitive)) - exp(-D_rate*Time_research_pilot))
Pop_after_definitive_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*(Time_research_pilot+Time_research_definitive)))

## Net benfit of immediately implemeting best treatment (Approval)
NB_maxt_U  <- Pop_total*NB_EVTCI_U 

## Net benefit of maxt trial (AWR)
# Pop during the trials: get treated with best treatment during trials 


NB_E_maxt_trial_U_i_vec <- 
  # If definitive trial HAPPENS
  pi_i_vec*(
    Pop_during_pilot_research*NB_EVTCI_U + Pop_during_definitive_research*NB_EVTCI_U +
      Pop_after_definitive_research*NB_EVTPI_U - 
      Cost_research_pilot_NHS /k - # NHS cost of research (pilot cost = 0 in this case)
      Cost_research_definitive_NHS /k # NHS cost of research 
  ) +
  # If definitive trial DOES NOT happen (everybody just gets best treatment)
  (1 - pi_i_vec)*(
    Pop_total*NB_EVTCI_U -
      Cost_research_pilot_NHS /k # cost of the pilot (still have to pay it!)
  )

E_NB_E_maxt_trial_U <- mean(NB_E_maxt_trial_U_i_vec)




# ICER
# difference in effects (between trial and implementign best treatment)
E_NB_E_maxt_trial_U - NB_maxt_U
# difference in costs (between trial and implementign best treatment)
E_Cost_NETSCC  <- 
  Cost_research_pilot_NETSCC +  # always incur this cost
  # If definitive trial HAPPENS
  mean(pi_i_vec*Cost_research_definitive_NETSCC)

E_Cost_NETSCC
# the ICER
E_Cost_NETSCC/(E_NB_E_maxt_trial_U - NB_maxt_U)































###########################################################################################################
#                       OLD STUFF?
###########################################################################################################
###########################################################################################################



# matrix for calculating VOI outputs- needs to use simulations
NB_t_U  <- cbind(P_t0_U*INB_Event_nat, 
                    P_t1_U*INB_Event_nat + MCD_t1, 
                    P_t2_U*INB_Event_nat + MCD_t2)


# trial impact per year
########################

# Best outcome with uninformed prior 
#EVTCI_U  = max(0, E_INB_t1_U ) # in this case should always equal zero if no Monte carlo error
NB_EVTCI_U  = max(E_NB_t0_U , E_NB_t1_U , E_NB_t2_U ) # 

# EVTPI with uninformed prior and natural outcome
#EVTPI_U  <- mean(apply(INB_t_U , 1, max))
NB_VTPI_U  <- apply(NB_t_U , 1, max) #so I can check convergence
NB_EVTPI_U  <- mean(NB_VTPI_U )
NB_EVPI_U  <-  NB_EVTPI_U  - NB_EVTCI_U 
#Running mean to check convergence
#EVTPI.run<-c(rep(0,150))
#for (i in 1:150){
#  EVTPI.run[i]<-mean(NB_VTPI_U [1:(i*(MCouter/150))])
#}
#plot(seq(1,i*(MCouter/150), length.out = length(EVTPI.run)), EVTPI.run, type="l",lty=1, xlab="Simulation", ylab="EVTPI")
# does not really appear to be converged until 45K as far as we can tell
# need to check when converted to natrual outcomes

# relapses prevented per year from doing the definitive trial
NB_EVTPI_U *Incidence - NB_EVTCI_U *Incidence





########### TRIAL ANALYSIS ################################################
# note *this completely ignores the costs imposed on the NHS from running trial (only NETSCC costs)
# SKIP pilot trial and go straight to instant definitive trial!
# INPUTS
Time_research_pilot =  0
Time_research_definitive = 0
Probability_of_definitive_research = 1
Cost_research_pilot_NETSCC =  0 # 601480 (assume there is no pilot)
#Cost_research_pilot_NHS  = 150000 # ignored in this analysis
Cost_research_definitive_NETSCC =  2500000
#Cost_research_definitive_NHS  = 450000 # ignored in this analysis


Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
Pop_during_pilot_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research_pilot) - exp(-D_rate*0))
Pop_during_definitive_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*(Time_research_pilot+Time_research_definitive)) - exp(-D_rate*Time_research_pilot))
Pop_after_definitive_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*(Time_research_pilot+Time_research_definitive)))

## Net benfit of immediately implemeting best treatment (Approval)
NB_maxt_U  <- Pop_total*NB_EVTCI_U 

## Net benefit of maxt trial (AWR)
# get treated with best treatment during trials
NB_E_maxt_trial_U  <- Pop_during_pilot_research*NB_EVTCI_U  + Pop_during_definitive_research*NB_EVTCI_U  +
  # definitive trial happens
  Probability_of_definitive_research*Pop_after_definitive_research*NB_EVTPI_U  +
  # definitive trial does not happen (just get treated with the best treatment )
  (1-Probability_of_definitive_research)*Pop_after_definitive_research*NB_EVTCI_U 

# ICER
# difference in effects (between trial and implementign best treatment)
NB_E_maxt_trial_U  - NB_maxt_U 
# difference in costs (between trial and implementign best treatment)
E_Cost_NETSCC  <- Cost_research_pilot_NETSCC + Probability_of_definitive_research*Cost_research_definitive_NETSCC
E_Cost_NETSCC
# the ICER
E_Cost_NETSCC/(NB_E_maxt_trial_U  - NB_maxt_U )







########### TRIAL ANALYSIS ################################################
# note *this completely ignores the costs imposed on the NHS from running trial (only NETSCC costs)
# 2 year pilot with 50% chance of 6 year definitive trial
# INPUTS
Time_research_pilot =  2
Time_research_definitive = 6
Probability_of_definitive_research = 0.5
Cost_research_pilot_NETSCC =   601480 
# Cost_research_pilot_NHS  = 150000 # ignored in this analysis
Cost_research_definitive_NETSCC =  2500000
#Cost_research_definitive_NHS  = 450000 # ignored in this analysis


Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
Pop_during_pilot_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research_pilot) - exp(-D_rate*0))
Pop_during_definitive_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*(Time_research_pilot+Time_research_definitive)) - exp(-D_rate*Time_research_pilot))
Pop_after_definitive_research <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*(Time_research_pilot+Time_research_definitive)))

## Net benfit of immediately implemeting best treatment (Approval)
NB_maxt_U  <- Pop_total*NB_EVTCI_U 

## Net benefit of maxt trial (AWR)
# Pop during the trials: get treated with best treatment during trials 


NB_E_maxt_trial_U  <- 
  # If definitive trial HAPPENS
  Probability_of_definitive_research*(
    Pop_during_pilot_research*NB_EVTCI_U  + Pop_during_definitive_research*NB_EVTCI_U  +
      Pop_after_definitive_research*NB_EVTPI_U 
  ) +
  # If definitive trial DOES NOT happen (everybody just gets best treatment)
  (1 - Probability_of_definitive_research)*(
    Pop_total*NB_EVTCI_U 
  )




# ICER
# difference in effects (between trial and implementign best treatment)
NB_E_maxt_trial_U  - NB_maxt_U 
# difference in costs (between trial and implementign best treatment)
E_Cost_NETSCC  <- 
  Cost_research_pilot_NETSCC +  # always incur this cost
  # If definitive trial HAPPENS
  Probability_of_definitive_research*Cost_research_definitive_NETSCC

E_Cost_NETSCC
# the ICER
E_Cost_NETSCC/(NB_E_maxt_trial_U  - NB_maxt_U )



# expected research costs imposed on NHS
Cost_research_pilot_NHS  = 150000 # ignored in this analysis
Cost_research_definitive_NHS  = 450000 # ignored in this analysis

E_Cost_NHS <- 
  Cost_research_pilot_NHS  +  # always incur this cost
  # If definitive trial HAPPENS
  Probability_of_definitive_research*Cost_research_definitive_NHS 
E_Cost_NHS









