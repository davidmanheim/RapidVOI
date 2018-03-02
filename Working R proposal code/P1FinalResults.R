###########################
# P1 final results

# see P1FinalModels.R for the functions required



# base case inputs in order #
#############################

# natural outcome

MCouter <- 900000
Benefit<- TRUE
P_t0_U <- 0.525 # prob of non-relapse = 1 - 0.475
mu_t1_U <- 0
sigma_t1_U <- 0.5
mu_t2_U <- 0
sigma_t2_U <- 0.5
MCD_t1 <- 0
MCD_t2 <- 0
Incidence<- 1563
Time_info <- 15
Utilisation_t0 <- 1
Utilisation_t1 <- 0
Utilisation_t2 <- 0
D_rate <- 0.035
Time_research_pilot <- 2
Time_research_definitive <- 6
Probability_of_definitive_research <- 0.5
Cost_research_pilot_NETSCC <- 601480
Cost_research_pilot_NHS  <- 150000
Cost_research_definitive_NETSCC <- 2522710
Cost_research_definitive_NHS<- 490000

baseInputs <- list(MCouter ,Benefit, P_t0_U ,mu_t1_U ,sigma_t1_U ,mu_t2_U ,sigma_t2_U ,MCD_t1 ,MCD_t2 , Incidence, 
                   Time_info ,Utilisation_t0 ,Utilisation_t1 ,Utilisation_t2 ,D_rate ,Time_research_pilot ,Time_research_definitive ,
                   Probability_of_definitive_research ,Cost_research_pilot_NETSCC ,Cost_research_pilot_NHS  ,
                   Cost_research_definitive_NETSCC ,Cost_research_definitive_NHS)

#basic results
basic <- do.call(PilotStudyBinaryOutcomeNat , as.list(baseInputs))
basic




# NETSCC cost per payoff
Cost_research_pilot_NETSCC * 258/basic$E_Cost_NETSCC


# if 100% probability of research
Probability_of_definitive_research <- 1
baseInputs <- list(MCouter ,Benefit, P_t0_U ,mu_t1_U ,sigma_t1_U ,mu_t2_U ,sigma_t2_U ,MCD_t1 ,MCD_t2 , Incidence, 
                   Time_info ,Utilisation_t0 ,Utilisation_t1 ,Utilisation_t2 ,D_rate ,Time_research_pilot ,Time_research_definitive ,
                   Probability_of_definitive_research ,Cost_research_pilot_NETSCC ,Cost_research_pilot_NHS  ,
                   Cost_research_definitive_NETSCC ,Cost_research_definitive_NHS)
do.call(PilotStudyBinaryOutcomeNat , as.list(baseInputs))

# if 80% probability of research
Probability_of_definitive_research <- 0.8
baseInputs <- list(MCouter ,Benefit, P_t0_U ,mu_t1_U ,sigma_t1_U ,mu_t2_U ,sigma_t2_U ,MCD_t1 ,MCD_t2 , Incidence, 
                   Time_info ,Utilisation_t0 ,Utilisation_t1 ,Utilisation_t2 ,D_rate ,Time_research_pilot ,Time_research_definitive ,
                   Probability_of_definitive_research ,Cost_research_pilot_NETSCC ,Cost_research_pilot_NHS  ,
                   Cost_research_definitive_NETSCC ,Cost_research_definitive_NHS)
do.call(PilotStudyBinaryOutcomeNat , as.list(baseInputs))

# if 20% probability of research
Probability_of_definitive_research <- 0.2
baseInputs <- list(MCouter ,Benefit, P_t0_U ,mu_t1_U ,sigma_t1_U ,mu_t2_U ,sigma_t2_U ,MCD_t1 ,MCD_t2 , Incidence, 
                   Time_info ,Utilisation_t0 ,Utilisation_t1 ,Utilisation_t2 ,D_rate ,Time_research_pilot ,Time_research_definitive ,
                   Probability_of_definitive_research ,Cost_research_pilot_NETSCC ,Cost_research_pilot_NHS  ,
                   Cost_research_definitive_NETSCC ,Cost_research_definitive_NHS)
do.call(PilotStudyBinaryOutcomeNat , as.list(baseInputs))

# reset to base case
Probability_of_definitive_research <- 0.5

# MCD to proxy additional costs but lower side effects of talking therapy
#########################################################################

C_t1 <- 97*18  
C_t0 <- mean(57.23 ,14.35 ,85.13 ,108.89 ,156.34 ,67.52 ,63.03 ,6.7)*12 # for 1 year of durgs
C_t2 <- 97*18  + mean(57.23 ,14.35 ,85.13 ,108.89 ,156.34 ,67.52 ,63.03 ,6.7)*12

C_t0_yrly <- C_t0*Incidence # APs
C_t1_yrly <- C_t1*Incidence # talking
C_t2_yrly <- C_t2*Incidence # combination

C_t0_yrly  # APs
C_t1_yrly 
C_t2_yrly

# additinal costs
C_t1_yrly -C_t0_yrly
C_t2_yrly -C_t0_yrly

# outcomes required        outcomes with standard care
Incidence*P_t0_U*(1.03) - Incidence*P_t0_U 

# outcome is a benefit so increases favour the intervention
# i.e. decreases penalise it
MCD_t1 <- 0.0
MCD_t2 <- -0.03
baseInputs <- list(MCouter ,Benefit, P_t0_U ,mu_t1_U ,sigma_t1_U ,mu_t2_U ,sigma_t2_U ,MCD_t1 ,MCD_t2 , Incidence, 
                   Time_info ,Utilisation_t0 ,Utilisation_t1 ,Utilisation_t2 ,D_rate ,Time_research_pilot ,Time_research_definitive ,
                   Probability_of_definitive_research ,Cost_research_pilot_NETSCC ,Cost_research_pilot_NHS  ,
                   Cost_research_definitive_NETSCC ,Cost_research_definitive_NHS)

#basic results
basic <- do.call(PilotStudyBinaryOutcomeNat , as.list(baseInputs))
basic

# NETSCC cost per payoff
Cost_research_pilot_NETSCC * 222/basic$E_Cost_NETSCC


########################################################################
# QALY analysis 
########################################################################


# no MCD at first
MCD_t1 <- 0.0
MCD_t2 <- 0.0

MCouter <- 900000
Benefit<- TRUE
INB_Event <- 0.7409
C_t0 <- mean(57.23 ,14.35 ,85.13 ,108.89 ,156.34 ,67.52 ,63.03 ,6.7)*12 # for 1 year of durgs
C_t1 <- 97*18  
C_t2 <- 97*18  + mean(57.23 ,14.35 ,85.13 ,108.89 ,156.34 ,67.52 ,63.03 ,6.7)*12
P_t0_U <- 0.525
mu_t1_U <- 0
sigma_t1_U <- 0.5
mu_t2_U <- 0
sigma_t2_U <- 0.5
MCD_t1 <- 0.02
MCD_t2 <- 0.0
Incidence<- 1563
Time_info <- 15
Utilisation_t0 <- 1
Utilisation_t1 <- 0
Utilisation_t2 <- 0
D_rate <- 0.035
Time_research_pilot <- 2
Time_research_definitive <- 6
Probability_of_definitive_research <- 0.5
Cost_research_pilot_NETSCC <- 601480
Cost_research_pilot_NHS  <- 150000
Cost_research_definitive_NETSCC <- 2522710
Cost_research_definitive_NHS<- 450000


baseInputsQALY <- list(MCouter ,Benefit,INB_Event, C_t0, C_t1, C_t2, P_t0_U ,mu_t1_U ,sigma_t1_U ,mu_t2_U ,sigma_t2_U ,MCD_t1 ,MCD_t2 , Incidence, 
                   Time_info ,Utilisation_t0 ,Utilisation_t1 ,Utilisation_t2 ,D_rate ,Time_research_pilot ,Time_research_definitive ,
                   Probability_of_definitive_research ,Cost_research_pilot_NETSCC ,Cost_research_pilot_NHS  ,
                   Cost_research_definitive_NETSCC ,Cost_research_definitive_NHS)
#basic results
basic <- do.call(PilotStudyBinaryQALY , as.list(baseInputsQALY))
basic

# NETSCC cost per payoff
Cost_research_pilot_NETSCC * 15.77/basic$E_Cost_NETSCC







# MCD to take account of reduced side effects with talking therapy 
#outcomes with standard care - outcomes accepted
Incidence*P_t0_U     -  Incidence*P_t0_U*(0.98) 
# outcome is a benefit so increases favour the intervention
# i.e. decreases penalise it
MCD_t1 <- 0.02
MCD_t2 <- 0.0
baseInputs <- list(MCouter ,Benefit, P_t0_U ,mu_t1_U ,sigma_t1_U ,mu_t2_U ,sigma_t2_U ,MCD_t1 ,MCD_t2 , Incidence, 
                   Time_info ,Utilisation_t0 ,Utilisation_t1 ,Utilisation_t2 ,D_rate ,Time_research_pilot ,Time_research_definitive ,
                   Probability_of_definitive_research ,Cost_research_pilot_NETSCC ,Cost_research_pilot_NHS  ,
                   Cost_research_definitive_NETSCC ,Cost_research_definitive_NHS)
BaseCaseQ <- do.call(PilotStudyBinaryQALY , as.list(baseInputs))
BaseCaseQ



# if 80% probability of research
Probability_of_definitive_research <- 0.8
baseInputsQALY <- list(MCouter ,Benefit,INB_Event, C_t0, C_t1, C_t2, P_t0_U ,mu_t1_U ,sigma_t1_U ,mu_t2_U ,sigma_t2_U ,MCD_t1 ,MCD_t2 , Incidence, 
                       Time_info ,Utilisation_t0 ,Utilisation_t1 ,Utilisation_t2 ,D_rate ,Time_research_pilot ,Time_research_definitive ,
                       Probability_of_definitive_research ,Cost_research_pilot_NETSCC ,Cost_research_pilot_NHS  ,
                       Cost_research_definitive_NETSCC ,Cost_research_definitive_NHS)
do.call(PilotStudyBinaryQALY , as.list(baseInputsQALY))

# if 20% probability of research
Probability_of_definitive_research <- 0.2
baseInputsQALY <- list(MCouter ,Benefit,INB_Event, C_t0, C_t1, C_t2, P_t0_U ,mu_t1_U ,sigma_t1_U ,mu_t2_U ,sigma_t2_U ,MCD_t1 ,MCD_t2 , Incidence, 
                       Time_info ,Utilisation_t0 ,Utilisation_t1 ,Utilisation_t2 ,D_rate ,Time_research_pilot ,Time_research_definitive ,
                       Probability_of_definitive_research ,Cost_research_pilot_NETSCC ,Cost_research_pilot_NHS  ,
                       Cost_research_definitive_NETSCC ,Cost_research_definitive_NHS)
do.call(PilotStudyBinaryQALY , as.list(baseInputsQALY))

# reset to base case
Probability_of_definitive_research <- 0.5


# NETSCC cost per QALY
Cost_research_pilot_NETSCC * (1/54160) 

# QALYs per 15K NETSCC
15000 * (1/54160) 



