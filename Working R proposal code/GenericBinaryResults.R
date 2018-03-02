##################################
# GENERIC BINARY oucome results NETSCC proposals
##################################


########################################################################################################
                   ##### Proposal 1  #####
########################################################################################################







########################################################################################################
                  ##### Proposal 2  #####
########################################################################################################

# mistake???
#############
# inputs

#P2_Cost_research_funder =  3310883
#P2_Incidence = 100000
#D_rate = 0.035
#P2_Time_research = 6
#P2_Time_info  = 20

# epi
#MCouter <- 40000 # NOTE not bad convergence of EVTPI at MCouter = 40000
#library(fdrtool) # to get half normal
#P_t0_U <- 0.51 # prob of progression free survival (benefit)
#Odds_t0_U <- P_t0_U / (1 - P_t0_U)
#LO_t0_U <- log(Odds_t0_U)
## halfnormal simulations on LOR 
#sigma_t1_U <- 0.5
#LOR_t1_U <- -rhalfnorm(MCouter, theta=sd2theta(sigma_t1_U))
#LOR_t1_U <- rnorm(MCouter, 0, sigma_t1_U)
#LO_t1_U <- LO_t0_U + LOR_t1_U 
#Odds_t1_U <- exp(LO_t1_U)
#P_t1_U <- Odds_t1_U / (Odds_t1_U + 1) 
#P2_P_t_U <- matrix(c(rep(P_t0_U,MCouter), P_t1_U), ncol = 2)


# MCD inputs
# MCD is added onto the new treatment(s) 
# if new treatment has better secondary outcomes than the base then +
# if new treatment has worse secondary outcomes than base then -
#P2_MCD_t <- c(0.079) # remember MCD is a relative input - if two treatmetns => just one MCD
#P2_Benefit = TRUE

# (utilisation) stat sig analysis
# inputs
#P2_Utilisation_t <- c(1, 0)
#n_t0 <- 1068/2
#n_t1 <- 1068/2
# analysis 
#a <- P_t1_U*n_t1 # number of events in the treatment arm
#b <- (1 - P_t1_U)*n_t1 # number of non-events in the treatment arm
#c <- rep(P_t0_U*n_t0, MCouter) # number of events in the control arm
#d <- rep((1 - P_t0_U)*n_t0, MCouter) # number of non-events in the control arm
#SE_U <- sqrt(1/a + 1/b + 1/c + 1/d) # standard error for each simulation
# here the cut-off is about the new treatment being below the MCD
# output
#P2_UtilMatrix <- NA
#P2_UtilMatrix <- UtilMatrix.fn(cbind(rep(0, length(LOR_t1_U)), LOR_t1_U), SE_U*qnorm(0.975), P2_Utilisation_t)
# probaiblity LOR>0
#sum(LOR_t1_U>0)/length(LOR_t1_U)
# probability of implementing new treatment
#sum(P2_UtilMatrix[,2])/length(LOR_t1_U)

# base case inputs
#GenericBinaryOutcome.v1(P2_P_t_U ,P2_Benefit,P2_MCD_t , P2_Incidence, 
#                        P2_Time_info ,P2_Utilisation_t,D_rate ,P2_Time_research ,
#                        P2_Cost_research_funder,P2_UtilMatrix)

# manual calculations
# current outcomes
#P2_Incidence*P_t0_U
# 2% required increase in outcomes
#P2_Incidence*P_t0_U*0.02

# try different MCD in last example
#P2_MCD_t <- c(0.20)
# require 2% increase in outcomes
#GenericBinaryOutcome.v1(P2_P_t_U ,P2_Benefit,P2_MCD_t , P2_Incidence, 
#                        P2_Time_info ,P2_Utilisation_t,D_rate ,P2_Time_research ,
#                        P2_Cost_research_funder,P2_UtilMatrix)




#################################################################################
# Proposal 2 as continuous outcome

P2_Cost_research_funder =  3310883
P2_Incidence = 100000
D_rate = 0.035
P2_Time_research = 6
P2_Time_info  = 20
P2_Utilisation_t <- c(1, 0, 0, 0)

# epi inputs
MCouter <- 400000 # NOTE not bad convergence of EVTPI at MCouter = 40000
# % chance of success
P_t0_U <- 0 # by definition
P_t1_U <- 1 / (200 +2)
P_t2_U <- 1 / (200 +2)
P_t3_U <-  1 / (200 +2)
# change in MMSE decline
# model change in delcine (not absolute level)

# choosing the correct sigma
###
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
plot(density(rnorm(999999, 0, sigma_star)))

sigma_t1 <- sigma_star
sigma_t2 <- sigma_star
sigma_t3 <- sigma_star
Delta_t1 <- rnorm(MCouter, 0, sigma_t1)
Delta_t2 <- rnorm(MCouter, 0, sigma_t2)
Delta_t3 <- rnorm(MCouter, 0, sigma_t3)

# equivalent to Delta_t_U matrix
P2_Delta_t_U <- matrix(c(rep(0,MCouter),Delta_t1, Delta_t2, Delta_t3), ncol = 4)


# MCD inputs
# MCD is added onto the new treatment(s) 
# if new treatment has better secondary outcomes than the base then +
# if new treatment has worse secondary outcomes than base then -
P2_MCD_t <- c(0, 0, 0)# remember MCD is a relative input - if two treatmetns => just one MCD
P2_Benefit = TRUE

P2_UtilMatrix <- NA
P2_Benefit <- TRUE

# base case inputs
GenericContinOutcome.v1(P2_Delta_t_U ,P2_Benefit,P2_MCD_t , P2_Incidence, 
                        P2_Time_info ,P2_Utilisation_t,D_rate ,P2_Time_research ,
                        P2_Cost_research_funder,P2_UtilMatrix)

# use 1.4 MCD
P2_MCD_t <- c(-1.4, -1.4, -1.4)# remember MCD is a relative input - if two treatmetns => just one MCD
GenericContinOutcome.v1(P2_Delta_t_U ,P2_Benefit,P2_MCD_t , P2_Incidence, 
                        P2_Time_info ,P2_Utilisation_t,D_rate ,P2_Time_research ,
                        P2_Cost_research_funder,P2_UtilMatrix)





########################################################################################################
##### Proposal 3  #####
########################################################################################################

# inputs for TBI proposal P3

P3_Cost_research_funder =  2522710
P3_Incidence = 1121
D_rate = 0.035
P3_Time_research = 6
P3_Time_info  = 10

# epi
MCouter <- 40000 # NOTE not bad convergence of EVTPI at MCouter = 40000
library(fdrtool) # to get half normal
P_t0_U <- 0.51 # prob of progression free survival (benefit)
Odds_t0_U <- P_t0_U / (1 - P_t0_U)
LO_t0_U <- log(Odds_t0_U)
# halfnormal simulations on LOR 
sigma_t1_U <- 0.5
LOR_t1_U <- -rhalfnorm(MCouter, theta=sd2theta(sigma_t1_U))
#LOR_t1_U <- rnorm(MCouter, 0, sigma_t1_U)
LO_t1_U <- LO_t0_U + LOR_t1_U 
Odds_t1_U <- exp(LO_t1_U)
P_t1_U <- Odds_t1_U / (Odds_t1_U + 1) 
P3_P_t_U <- matrix(c(rep(P_t0_U,MCouter), P_t1_U), ncol = 2)


# MCD inputs
# MCD is added onto the new treatment(s) 
# if new treatment has better secondary outcomes than the base then +
# if new treatment has worse secondary outcomes than base then -
P3_MCD_t <- c(0.079) # remember MCD is a relative input - if two treatmetns => just one MCD
P3_Benefit = TRUE
P3_Utilisation_t <- c(1, 0)


# (utilisation) stat sig analysis
# inputs

#n_t0 <- 1068/2
#n_t1 <- 1068/2
# analysis 
#a <- P_t1_U*n_t1 # number of events in the treatment arm
#b <- (1 - P_t1_U)*n_t1 # number of non-events in the treatment arm
#c <- rep(P_t0_U*n_t0, MCouter) # number of events in the control arm
#d <- rep((1 - P_t0_U)*n_t0, MCouter) # number of non-events in the control arm
#SE_U <- sqrt(1/a + 1/b + 1/c + 1/d) # standard error for each simulation
# here the cut-off is about the new treatment being below the MCD
# output
P3_UtilMatrix <- NA
#P3_UtilMatrix <- UtilMatrix.fn(cbind(rep(0, length(LOR_t1_U)), LOR_t1_U), SE_U*qnorm(0.975), P3_Utilisation_t)
# probaiblity LOR>0
#sum(LOR_t1_U>0)/length(LOR_t1_U)
# probability of implementing new treatment
#sum(P3_UtilMatrix[,2])/length(LOR_t1_U)

# base case inputs
GenericBinaryOutcome.v1(P3_P_t_U ,P3_Benefit,P3_MCD_t , P3_Incidence, 
                        P3_Time_info ,P3_Utilisation_t,D_rate ,P3_Time_research ,
                        P3_Cost_research_funder,P3_UtilMatrix)

# manual calculations
# current outcomes
P3_Incidence*P_t0_U
# 2% required increase in outcomes
#P3_Incidence*P_t0_U*0.02

# try different MCD in last example
P3_MCD_t <- c(0.20)
# require 2% increase in outcomes
GenericBinaryOutcome.v1(P3_P_t_U ,P3_Benefit,P3_MCD_t , P3_Incidence, 
                        P3_Time_info ,P3_Utilisation_t,D_rate ,P3_Time_research ,
                        P3_Cost_research_funder,P3_UtilMatrix)










########################################################################################################
                             ##### Proposal 4  #####
########################################################################################################

# inputs for booklet proposal P4 

P4_Cost_research_funder =  882177
P4_Incidence = 259150
D_rate = 0.035
P4_Time_research = 3
P4_Time_info  = 15

# epi
MCouter <- 40000 # NOTE not bad convergence of EVTPI at MCouter = 40000
library(fdrtool) # to get half normal
P_t0_U <- 0.30
Odds_t0_U <- P_t0_U / (1 - P_t0_U)
LO_t0_U <- log(Odds_t0_U)
# halfnormal simulations on LOR 
sigma_t1_U <- 0.5
LOR_t1_U <- rhalfnorm(MCouter, theta=sd2theta(sigma_t1_U))
#LOR_t1_U <- rnorm(MCouter, 0, sigma_t1_U)
LO_t1_U <- LO_t0_U + LOR_t1_U 
Odds_t1_U <- exp(LO_t1_U)
P_t1_U <- Odds_t1_U / (Odds_t1_U + 1) 
P4_P_t_U <- matrix(c(rep(P_t0_U,MCouter), P_t1_U), ncol = 2)

# MCD inputs
P4_MCD_t <- c(0) # remember MCD is a relative input - if two treatmetns => just one MCD
P4_Benefit = TRUE

# (utilisation) stat sig analysis
# inputs
n_t0 <- 227
n_t1 <- 453
P4_Utilisation_t <- c(1, 0)
# analysis
a <- P_t1_U*n_t1 # number of events in the treatment arm
b <- (1 - P_t1_U)*n_t1 # number of non-events in the treatment arm
c <- rep(P_t0_U*n_t0, MCouter) # number of events in the control arm
d <- rep((1 - P_t0_U)*n_t0, MCouter) # number of non-events in the control arm
SE_U <- sqrt(1/a + 1/b + 1/c + 1/d) # standard error for each simulation
# output
P4_UtilMatrix <- UtilMatrix.fn(cbind(rep(0, length(LOR_t1_U)), LOR_t1_U), SE_U*qnorm(0.975), P4_Utilisation_t)


# base case inputs
GenericBinaryOutcome.v1(P4_P_t_U ,P4_Benefit,P4_MCD_t , P4_Incidence, 
                                         P4_Time_info ,P4_Utilisation_t,D_rate ,P4_Time_research ,
                                         P4_Cost_research_funder,P4_UtilMatrix)


# require an increase in primary outcome of 5% before the new treatment is 
# worthwhile
# a 5% increase in deaths at home would mean
P4_Incidence*P_t0_U*1.05 -   P4_Incidence*P_t0_U

GenericBinaryOutcome.v1(P4_P_t_U ,P4_Benefit,-0.05 , P4_Incidence, 
                                         P4_Time_info ,P4_Utilisation_t,D_rate ,P4_Time_research ,
                                         P4_Cost_research_funder,P4_UtilMatrix)




GenericBinaryOutcome.v1(P4_P_t_U ,P4_Benefit,-0.1 , P4_Incidence, 
                        P4_Time_info ,P4_Utilisation_t,D_rate ,P4_Time_research ,
                        P4_Cost_research_funder,P4_UtilMatrix)














########################################################################################################
                        ##### Proposal 5  #####
########################################################################################################

# inputs for TBI proposal P5

P5_Cost_research_funder =  2854000
P5_Incidence = 8800
D_rate = 0.035
P5_Time_research = 5
P5_Time_info  = 15

# epi
MCouter <- 40000 # NOTE not bad convergence of EVTPI at MCouter = 40000
P_t0_U <- 0.555 # Nichol 2015
Odds_t0_U <- P_t0_U / (1 - P_t0_U)
LO_t0_U <- log(Odds_t0_U)
#  
sigma_t1_U <- 0.5
LOR_t1_U <- rnorm(MCouter, 0, sigma_t1_U)
#LOR_t1_U <- rnorm(MCouter, 0, sigma_t1_U)
LO_t1_U <- LO_t0_U + LOR_t1_U 
Odds_t1_U <- exp(LO_t1_U)
P_t1_U <- Odds_t1_U / (Odds_t1_U + 1) 
P5_P_t_U <- matrix(c(rep(P_t0_U,MCouter), P_t1_U), ncol = 2)

# MCD inputs
P5_MCD_t <- c(0) # remember MCD is a relative input - if two treatmetns => just one MCD
P5_Benefit = TRUE

# (utilisation) stat sig analysis
# inputs
P5_Utilisation_t <- c(1, 0)
n_t0 <- 1180/2
n_t1 <- 1180/2
# analysis 
a <- P_t1_U*n_t1 # number of events in the treatment arm
b <- (1 - P_t1_U)*n_t1 # number of non-events in the treatment arm
c <- rep(P_t0_U*n_t0, MCouter) # number of events in the control arm
d <- rep((1 - P_t0_U)*n_t0, MCouter) # number of non-events in the control arm
SE_U <- sqrt(1/a + 1/b + 1/c + 1/d) # standard error for each simulation
# output
P5_UtilMatrix <- UtilMatrix.fn(cbind(rep(0, length(LOR_t1_U)), LOR_t1_U), SE_U*qnorm(0.975), P5_Utilisation_t)
# probaiblity LOR>0
sum(LOR_t1_U>0)/length(LOR_t1_U)
# probability of implementing new treatment
sum(P5_UtilMatrix[,2])/length(LOR_t1_U)

# base case inputs
GenericBinaryOutcome.v1(P5_P_t_U ,P5_Benefit,P5_MCD_t , P5_Incidence, 
                        P5_Time_info ,P5_Utilisation_t,D_rate ,P5_Time_research ,
                        P5_Cost_research_funder,P5_UtilMatrix)

# manual calculations
# current outcomes
 P5_Incidence*P_t0_U
# 2% required increase in outcomes
 P5_Incidence*P_t0_U*0.02
 
# MCD refers to the new treatment (relative to t0)
# outcome is good so higher prob is good
# MCD is added to the P_t_U
# => penalty means adding a minus to the new treatment
P5_MCD_t <- c(-0.02)
# require 2% increase in outcomes
GenericBinaryOutcome.v1(P5_P_t_U ,P5_Benefit,P5_MCD_t , P5_Incidence, 
                         P5_Time_info ,P5_Utilisation_t,D_rate ,P5_Time_research ,
                         P5_Cost_research_funder,P5_UtilMatrix)
 



 
########################################################################################################
                  ##### Proposal 6  #####
########################################################################################################


# inputs for TBI proposal P3

P6_Cost_research_funder =  855403
P6_Incidence = 65.64*0.4
D_rate = 0.035
P6_Time_research = 4
P6_Time_info  = 10

# epi
P_t0_U <- 0.95
Odds_t0_U <- P_t0_U / (1 - P_t0_U)
LO_t0_U <- log(Odds_t0_U)
sigma_t1_U <- 0.5
LOR_t1_U <- rnorm(MCouter, 0, sigma_t1_U)
LO_t1_U <- LO_t0_U + LOR_t1_U 
Odds_t1_U <- exp(LO_t1_U)
P_t1_U <- Odds_t1_U / (Odds_t1_U + 1) 
P6_P_t_U <- matrix(c(rep(P_t0_U,MCouter), P_t1_U), ncol = 2)


# MCD inputs
# MCD is added onto the new treatment(s) 
# if new treatment has better secondary outcomes than the base then +
# if new treatment has worse secondary outcomes than base then -
P6_MCD_t <- c(0) # remember MCD is a relative input - if two treatmetns => just one MCD
P6_Benefit = TRUE
P6_Utilisation_t <- c(1, 0)

P6_UtilMatrix <- NA

# base case inputs
GenericBinaryOutcome.v1(P6_P_t_U ,P6_Benefit,P6_MCD_t , P6_Incidence, 
                        P6_Time_info ,P6_Utilisation_t,D_rate ,P6_Time_research ,
                        P6_Cost_research_funder,P6_UtilMatrix)

# manual calculations
# current outcomes
P6_Incidence*P_t0_U
# 2% required increase in outcomes
#P6_Incidence*P_t0_U*0.02

# try different MCD in last example
# MCD is added onto the new treatment(s) 
# if new treatment has better secondary outcomes than the base then +
# if new treatment has worse secondary outcomes than base then -
P6_MCD_t <- c(0.05)
# 
GenericBinaryOutcome.v1(P6_P_t_U ,P6_Benefit,P6_MCD_t , P6_Incidence, 
                        P6_Time_info ,P6_Utilisation_t,D_rate ,P6_Time_research ,
                        P6_Cost_research_funder,P6_UtilMatrix)




