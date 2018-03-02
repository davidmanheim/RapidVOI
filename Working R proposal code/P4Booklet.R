##############################
# Proposal 4: Inexpensive
##############################

set.seed(20)
options(scipen=999) # trun off scientifc notation (for convergence axis)

###################################################

###################################################
#        NOTE: generic QALY and binary model exist :): 
# GenericBinaryQALY.R

################################################



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
Utilisation =  0   # proposal does not say anything

# relevant pop is the palliative care pop that would prefer to die at home
#355000 #need palliative care each year: p68 Palliative care funding review 2011
#0.73 # prefer to die at home
Incidence =  355000*0.73 
#Prevalence =  # ignore for now!
Time_info =  15   
D_rate =  0.035 
k = 15000 
Time_research =  3
#Time_delay_informprior = 0.5 # set later in code: do for two options: 6 months (0.5 ) and 2 years (2)
Cost_research_NETSCC =  882177
Cost_research_NHS = 4104



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

#plot(density(P_t1_U)) # looks good - 
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



#############################
# Full economic inputs
###############################

# cost of standard care
C_t0 <- 0

#
booklet_cost <- 4104/453
C_t1 <- booklet_cost

# expected yearly additional costs of using new treatment
booklet_cost*Incidence

Incidence*0.05

# hospital cost of end of life care
3000

# community cost of home death
2107.5

# cost saving of home death
3000 - 2107.5
# additional QALYs of home death
(3000 - 2107.5)/k


INB_Event <- (3000 - 2107.5)/k


###############################################
# Put together mini economic model

NB_t0_U <- P_t0_U*INB_Event - C_t0/k # 
NB_t1_U <- (P_t1_U)*INB_Event - C_t1/k # 


############################
# pre trial expected NB of outcomes
#########################

E_NB_t0_U <- mean(NB_t0_U) # 
E_NB_t1_U <- mean(NB_t1_U) # 

# incremental benefit compared to standard care (APs)
E_INB_t0_U <- 0
E_INB_t1_U <- E_NB_t1_U - E_NB_t0_U


# matrix for calculating VOI outputs- needs to use simulations
###########################################################

NB_t_U <- cbind(NB_t0_U, 
                NB_t1_U)


# trial impact per year
########################

# Best outcome with uninformed prior 
NB_EVTCI_U = max(E_NB_t0_U, E_NB_t1_U) # 

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

######################
# yearly outcomes


# VOI per year
NB_EVTPI_U*Incidence - NB_EVTCI_U*Incidence

# value of implementation per year
NB_EVTCI_U*Incidence -   E_NB_t0_U*Incidence

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













































############################# old stuff ##################################
###########################################################################

## Cell_A : net benefit of current situation
# here assume nobody gets booklet
Utilisation_t0 <- 1
Utilisation_t1 <- 0
Cell_A_yearly <- Incidence*E_NB_t0_U*Utilisation_t0 +
  Incidence*E_NB_t1_U*Utilisation_t1 

## Cell_C : Net benfit of immediately implemeting best treatment (Approval)
NB_maxt_U_yearly <- Incidence*NB_EVTCI_U
Cell_C_yearly <- NB_maxt_U_yearly

## Cell_D : Net benefit of having trial information
Cell_D_yearly <- Incidence*NB_EVTPI_U


# total benefit of trial per year
Cell_D_yearly - Cell_A_yearly

# information value of the trial per year
Cell_D_yearly - Cell_C_yearly

# implementation value of the trial per year
Cell_C_yearly - Cell_A_yearly

# % of yearly implementation value
(Cell_C_yearly - Cell_A_yearly)/(Cell_D_yearly - Cell_A_yearly)*100





######################
# full time outcomes
# INSTANT tiral, instant reporting trial
#
#############################

Time_research = 0 # instant

Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))

## Cell_A : net benefit of current situation
# here assume nobody gets booklet
Utilisation_t0 <- 1
Utilisation_t1 <- 0
Cell_A  <- Pop_total*E_NB_t0_U*Utilisation_t0 +
  Pop_total*E_NB_t1_U*Utilisation_t1 

## Cell_C : Net benfit of immediately implemeting best treatment (Approval)
NB_maxt_U  <- Pop_total*NB_EVTCI_U
Cell_C  <- NB_maxt_U 

## Cell_D : Net benefit of (instant) trial
Cell_D  <- Pop_after_trial*NB_EVTPI_U


#### total benefit of trial 
Cell_D  - Cell_A 
# ICER of total benefit for instant trial
Cost_research_NETSCC/(Cell_D  - Cell_A )


# information value of the trial 
Cell_D  - Cell_C 

# implementation value of the trial 
Cell_C  - Cell_A 

# % implementation value
(Cell_C  - Cell_A )/(Cell_D  - Cell_A )*100






######################
# full time outcomes
# Base case
#
#############################

Time_research = 3 

Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))

## Cell_A : net benefit of current situation
# here assume nobody gets booklet
Utilisation_t0 <- 1
Utilisation_t1 <- 0
Cell_A  <- Pop_total*E_NB_t0_U*Utilisation_t0 +
  Pop_total*E_NB_t1_U*Utilisation_t1 

## Cell_C : Net benfit of implemeting current best treatment after 3 years
#NB_maxt_U  <- Pop_total*NB_EVTCI_U
Cell_C  <- Pop_during_trial*E_NB_t0_U + Pop_after_trial*NB_EVTCI_U 

## Cell_D : Net benefit of trial
Cell_D  <- Pop_during_trial*E_NB_t0_U + Pop_after_trial*NB_EVTPI_U - Cost_research_NHS/k


#### total benefit of trial 
Cell_D  - Cell_A 
# ICER of total benefit for instant trial
Cost_research_NETSCC/(Cell_D  - Cell_A )


# information value of the trial 
Cell_D  - Cell_C 
Cost_research_NETSCC/(Cell_D  - Cell_C) # ICER for just information value

# implementation value of the trial 
Cell_C  - Cell_A 

# % implementation value
(Cell_C  - Cell_A )/(Cell_D  - Cell_A )*100







#######################################################################################
# SENSITIVITY ANALYSIS 
########################################################################

#######################################################################################
# Optimistic 
########################################################################

increase <- 1.2
decrease <- 0.8

# optimistic inputs
Time_info = 15*increase
Time_research = 3*decrease 
Cost_research_NETSCC =  882177*decrease
Cost_research_NHS = 4104*decrease
INB_Event = 0.0595*increase
Incidence = 259150*increase


Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))

## Cell_A : net benefit of current situation
# here assume nobody gets booklet
Utilisation_t0 <- 1
Utilisation_t1 <- 0
Cell_A  <- Pop_total*E_NB_t0_U*Utilisation_t0 +
  Pop_total*E_NB_t1_U*Utilisation_t1 

## Cell_C : Net benfit of implemeting current best treatment after 3 years
#NB_maxt_U  <- Pop_total*NB_EVTCI_U
Cell_C  <- Pop_during_trial*E_NB_t0_U + Pop_after_trial*NB_EVTCI_U 

## Cell_D : Net benefit of trial
Cell_D  <- Pop_during_trial*E_NB_t0_U + Pop_after_trial*NB_EVTPI_U - Cost_research_NHS/k


#### total benefit of trial 
Cell_D  - Cell_A 
# ICER of total benefit for instant trial
Cost_research_NETSCC/(Cell_D  - Cell_A )


# information value of the trial 
Cell_D  - Cell_C
Cost_research_NETSCC/(Cell_D  - Cell_C)

# implementation value of the trial 
Cell_C  - Cell_A 

# % implementation value
(Cell_C  - Cell_A )/(Cell_D  - Cell_A )*100




#######################################################################################
# Pessimistic
########################################################################

increase <- 1.2
decrease <- 0.8

# pessimistic inputs
Time_info = 15*decrease
Time_research = 3*increase 
Cost_research_NETSCC =  882177*increase
Cost_research_NHS = 4104*increase
INB_Event = 0.0595*decrease
Incidence = 259150*decrease


Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))

## Cell_A : net benefit of current situation
# here assume nobody gets booklet
Utilisation_t0 <- 1
Utilisation_t1 <- 0
Cell_A  <- Pop_total*E_NB_t0_U*Utilisation_t0 +
  Pop_total*E_NB_t1_U*Utilisation_t1 

## Cell_C : Net benfit of implemeting current best treatment after 3 years
#NB_maxt_U  <- Pop_total*NB_EVTCI_U
Cell_C  <- Pop_during_trial*E_NB_t0_U + Pop_after_trial*NB_EVTCI_U 

## Cell_D : Net benefit of trial
Cell_D  <- Pop_during_trial*E_NB_t0_U + Pop_after_trial*NB_EVTPI_U - Cost_research_NHS/k


#### total benefit of trial 
Cell_D  - Cell_A 
# ICER of total benefit for instant trial
Cost_research_NETSCC/(Cell_D  - Cell_A )


# information value of the trial 
Cell_D  - Cell_C 
Cost_research_NETSCC/(Cell_D  - Cell_C) # information only ICER

# implementation value of the trial 
Cell_C  - Cell_A 

# % implementation value
(Cell_C  - Cell_A )/(Cell_D  - Cell_A )*100

