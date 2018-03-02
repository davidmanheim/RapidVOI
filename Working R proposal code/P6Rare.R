##############################
# Proposal 6: Rare disease
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


###################
# Epi parameters


#########################
# health system model inputs (some may not be needed)
Utilisation =  0  

0.4 # cases per million
65.64 # population of UK

Incidence =  65.64*0.4
#Prevalence =  # ignore for now!
Time_info =  10 # could be lower as other trials currently underway!   
D_rate =  0.035 
k = 15000 
Time_research =  4
#Time_delay_informprior = 0.5 # set later in code: do for two options: 6 months (0.5 ) and 2 years (2)
Cost_research_NETSCC =  855403
#Cost_research_NHS = -10608500

# NHS research costs allocated equally over 4 years: 10608500
Cost_research_NHS_yr <- 10608500/4
Cost_research_NHS <- (Cost_research_NHS_yr/-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
# QALY impact of research costs
Cost_research_NHS/15000
# NETSCC cost per QALY lost
Cost_research_NETSCC/(Cost_research_NHS/15000)


P_t0_U <- 0.95
Odds_t0_U <- P_t0_U / (1 - P_t0_U)
LO_t0_U <- log(Odds_t0_U)

# normal simulations on LOR 
sigma_t1_U <- 0.5
LOR_t1_U <- rnorm(MCouter, 0, sigma_t1_U)
#plot(density(LOR_t1_U))

LO_t1_U <- LO_t0_U + LOR_t1_U 
Odds_t1_U <- exp(LO_t1_U)
P_t1_U <- Odds_t1_U / (Odds_t1_U + 1) 

#plot(density(P_t1_U)) # density between 0.02 and 0.12


#Odds_t1_U <- exp(LO_t1_U)
#P_t1_U <- Odds_t1_U / (1 + Odds_t1_U)

#summary(Odds_t1_U)
#summary(LOR_t1_U)
#plot(density(Odds_t1_U))
#plot(density(Odds_t1_U))

#################################
# NATURAL OUTCOME - NOT INCLUDING SECONDARY OUTCOMES!!!
##################################

# construct the (natural outcome) INB_t1 function 
# primary outcome is a good natural outcome (units = progression free survival at 2 years) 
INB_Event_nat = 1
NB_t0_U_nat <- P_t0_U*INB_Event_nat
NB_t1_U_nat <- P_t1_U*INB_Event_nat 
E_NB_t0_U_nat <- mean(NB_t0_U_nat)
E_NB_t1_U_nat <- mean(NB_t1_U_nat)
# expected events per year for each treatment
E_NB_t0_U_nat*Incidence
E_NB_t1_U_nat*Incidence


# vector for calculating VOI outputs- needs to use simulations
NB_t_U_nat <- cbind(NB_t0_U_nat, NB_t1_U_nat)


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

# VOI per year
NB_EVTPI_U*Incidence - NB_EVTCI_U*Incidence

# Value of implementation per year
NB_EVTCI_U*Incidence - E_NB_t0_U_nat*Incidence

# prepare for histogram
NB_t_U <- matrix(c(rep(E_NB_t0_U_nat,MCouter), NB_t1_U_nat), ncol = 2)
ENB_t_U <- apply(NB_t_U, 2, mean)

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
Cell_A <- sum(E_NB_t0_U_nat *Pop_total)


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










#################################
# NATURAL OUTCOME -  SECONDARY OUTCOMES INCLUDED
##################################



### Costs and consequences table
##########

# expected yearly additional costs of using control treatment
# patients expected to be on treatment for 35.47 years (undiscounted)

# costs per person!
costs_pr_t0 <- (360000/-D_rate) * (exp(-D_rate*35.5) - exp(-D_rate*0))
costs_pr_t0
# per person undiscounted
360000*35.47

# costs for new treatment (per person)
costs_pr_t1 <- 360000 + (360000*0.5/-D_rate) * (exp(-D_rate*34.5) - exp(-D_rate*0))
costs_pr_t1
# per person undiscounted
340000 + 0.5 * (35.47 * 340000)  


# costs for current treatment per year
costs_yr_t0 <- costs_pr_t0*Incidence
costs_yr_t0
# costs for new treatment per year
costs_yr_t1 <- costs_pr_t1*Incidence
costs_yr_t1

# additiontal costs per year
costs_yr_t0 - costs_yr_t1

# composite outcomes for current treatment per year
P_t0_U*Incidence

# non-inferiority of composite oucomes per year according to proposal
0.9*Incidence


# maximum additional events per year
P_t0_U*Incidence - 0.9*Incidence 
# equivalent to 
Incidence*0.05



# this implies a cost per composite endpoint avoided of:

(costs_yr_t0 - costs_yr_t1)/(0.9*Incidence - P_t0_U*Incidence)

# equivalent spent else where in health system

((costs_yr_t0 - costs_yr_t1)/(0.9*Incidence - P_t0_U*Incidence))/k


############### analysis with this MCD #######################################
#############################################################################


# MCD analysis done with generic binary function




#####################################################################################
# Economic inputs
# take account of QALYs and aquisition costs
# ignore any other secondary outcomes
######################################################################################



###########
# normal utility
0.5895 # from paper (Sullivan, 2011) on UK utilities "ICD 161: Other Diseases Of Kidney And Ureters"


#########
# composite end points
# assume that they occur at start of observing period (2 years)
# assume that they live for full time

# menningococcal disease
#http://www.sciencedirect.com/science/article/pii/S0264410X13003691?via%3Dihub
# utility 0.86, 0.2 for those without and with squelae 
# take lower

# multi organ failure
#https://www.ncbi.nlm.nih.gov/pubmed/10994572
# 72% die and assume remainer return to previous QOL



# NB MODEL depends on treatment and P (because aquisition costs depend on P and treatment interaction)

# costs if primary outcome occurs - assumed to be the same as event occurs at start of observation period and patient moved back to durg
C_t0_PO <- C_t1_PO <- 1/3*(0) + 1/3*(costs_pr_t0) + 1/3*(costs_pr_t0*0.28)

# costs if primary outcome does NOT progression free
C_t0_notPO <- costs_pr_t0
C_t1_notPO <- costs_pr_t1

C_t0_notPO - C_t0_PO
C_t1_notPO - C_t1_PO

(C_t0_notPO - C_t0_PO)/15000
(C_t1_notPO - C_t1_PO)/15000


# QALYs if primary outcome occurs / not
QALY_PO <- 1/3*(0) + 
  1/3*((0.2/-D_rate) * (exp(-D_rate*35.5) - exp(-D_rate*0))) + 
  1/3*((0.5895/-D_rate) * (exp(-D_rate*35.5) - exp(-D_rate*0))*0.28)
  # discount the same way as I discount 
QALY_notPO <- (0.5895/-D_rate) * (exp(-D_rate*35.5) - exp(-D_rate*0))

 QALY_notPO -QALY_PO

# bad outcome so should be negative - but actually very positive!!
#INB_Event_t0 <- (QALY_PO - C_t0_PO/k) - (QALY_notPO - C_t0_notPO/k)
#INB_Event_t1 <- (QALY_PO - C_t1_PO/k) - (QALY_notPO - C_t1_notPO/k)
# cost savings from patients dying far outweigh the QALY benefits

#t0 INB event occurs (event free)
# 
INB_Event_t0 <- -270.19 
INB_Event_t1 <- -46.69

# vector for calculating VOI outputs
# also need to inlcude aquisition costs at the individual level
NB_t_U <- cbind(
  P_t0_U*INB_Event_t0 - (1-P_t0_U)*costs_pr_t0/k,
  # new treatment 
  P_t1_U*INB_Event_t1 - (1-P_t1_U)*costs_pr_t1/k
)

E_NB_t0_U <- P_t0_U*INB_Event_t0 - (1-P_t0_U)*costs_pr_t0/k
E_NB_t1_U <- mean(P_t1_U*INB_Event_t1 - (1-P_t1_U)*costs_pr_t1/k)

# Population calculations - no delay (ignore prevalence!)
##############################################
#Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
#Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
#Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))


# Best outcome with uninformed prior 
#EVTCI_U_nat = max(0, E_INB_t1_U_nat) # in this case should always equal zero if no Monte carlo error
NB_EVTCI_U = max(E_NB_t0_U, E_NB_t1_U) # 

# EVTPI with uninformed prior and natural outcome
#EVTPI_U_nat <- mean(apply(INB_t_U_nat, 1, max))
NB_VTPI_U <- apply(NB_t_U, 1, max) #so I can check convergence
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
# ADDITIONAL POS per year from doing study (compared to best treatment)
NB_EVTPI_U*Incidence - NB_EVTCI_U*Incidence

# annual implementation value
NB_EVTCI_U*Incidence - E_NB_t0_U*Incidence



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
  Pop_after_research*NB_EVTPI_U # do not count these anymore - Cost_research_NHS/k


# this is the pure information value under different types of research and implementation assumptions
Value_of_maxt_perfect_info_imp <- NB_maxt_perfect_info_imp - Cell_C


# ICER of research relative to early access (assumed to be costless to the agency)
# all other costs assumed to be captured by the MCD
ICER_maxt_perfect_info_imp <- Cost_research_NETSCC/Value_of_maxt_perfect_info_imp




# NHS research costs allocated equally over 4 years
Cost_research_NHS_yr <- 10608500/4
Cost_research_NHS <- (Cost_research_NHS_yr/-D_rate) * (exp(-D_rate*4) - exp(-D_rate*0))
# QALY impact of research costs
Cost_research_NHS
Cost_research_NHS/15000
# NETSCC cost per QALY lost
Cost_research_NETSCC/(Cost_research_NHS/15000)


















#######################################################################################
# SENSITIVITY ANALYSIS **** DONE in old style - not NB of information!
########################################################################

#######################################################################################
# Optimistic 
########################################################################

increase <- 1.2
decrease <- 0.8

# optimistic inputs
Time_info = 10*increase
Time_research = 4*decrease 
Cost_research_NETSCC =  855403*decrease
Cost_research_NHS = -10608500*increase
INB_Event_t0 = 270.1498*increase
INB_Event_t1 = 46.73707*increase
Incidence = 26.256*increase

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
# reflect the savings during the 4 years to the NHS from giving
# X people in the trial the new (cheaper treatment)

# ICER
# difference in effects (between trial and implementign best treatment)
NB_E_mandatedt_trial_U - NB_mandatedt_U
# difference in costs (between trial and implementign best treatment)
Cost_research_NETSCC
# the ICER
Cost_research_NETSCC/(NB_E_mandatedt_trial_U - NB_mandatedt_U)




#######################################################################################
# pessimistic 
########################################################################

increase <- 1.2
decrease <- 0.8

# pessimistic inputs
Time_info = 10*decrease
Time_research = 4*increase 
Cost_research_NETSCC =  855403*increase
Cost_research_NHS = -10608500*decrease
INB_Event_t0 = 270.1498*decrease
INB_Event_t1 = 46.73707*decrease
Incidence = 26.256*decrease

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
# reflect the savings during the 4 years to the NHS from giving
# X people in the trial the new (cheaper treatment)

# ICER
# difference in effects (between trial and implementign best treatment)
NB_E_mandatedt_trial_U - NB_mandatedt_U
# difference in costs (between trial and implementign best treatment)
Cost_research_NETSCC
# the ICER
Cost_research_NETSCC/(NB_E_mandatedt_trial_U - NB_mandatedt_U)




















####################### old stuff ####################################


#######################################
# INSTANT trial result (proposal says 6 years)
# NATURAL OUTCOMES so completely ignores research costs to the NHS
# Population calculations - no delay (ignore prevalence!)
##############################################

#Time_info = 10 # specified above
Time_research = 0 # instant
#Cost_research_NETSCC =  # specified above
#Cost_research_NHS_budget = # specified above


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
# NATURAL OUTCOMES so completely ignores research costs to the NHS
# Population calculations - no delay (ignore prevalence!)
##############################################

#Time_info = 10
Time_research = 4 
#Cost_research_NETSCC =  
#Cost_research_NHS_budget = 

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
# NATURAL OUTCOMES so completely ignores research costs to the NHS
# Population calculations - no delay (ignore prevalence!)
##############################################

#Time_info = 10
Time_research = 7 
#Cost_research_NETSCC =  2522710
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
# NO INFORMATION VALUE!! "trial to change practice"
# INSTANT trial result (proposal says 4 years)
#
# Population calculations - no delay (ignore prevalence!)
##############################################

#Time_info = 10
Time_research = 0 # instant
#Cost_research_NETSCC # already defined above
#Cost_research_NHS 

Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))

# mandated treatment 
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
# NO INFORMATION VALUE!!
# trial takes the same time as stated in proposal
#
# Population calculations - no delay (ignore prevalence!)
##############################################

#Time_info = 10
Time_research = 4
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
# reflect the savings during the 4 years to the NHS from giving
# X people in the trial the new (cheaper treatment)

# ICER
# difference in effects (between trial and implementign best treatment)
NB_E_mandatedt_trial_U - NB_mandatedt_U
# difference in costs (between trial and implementign best treatment)
Cost_research_NETSCC
# the ICER
Cost_research_NETSCC/(NB_E_mandatedt_trial_U - NB_mandatedt_U)





