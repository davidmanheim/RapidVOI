###############
# Reconsider evidence function 

# must return everything that the normal analysis does + the delay analysis
# just add into the regular functions with an if statement!
#if(reconsider == "Yes"){
#  
#  # insert reconsideration of evidence function
#  
#}


# extensions: calculate NB of other strategies other than the max strategy
# only calculates value of learning about the relative effect parameter
# continuous and survival outcomes
# how to maintain correlations??
# only works on Odds Ratio inputs - could extend to RR and RD, (and contin + survival)

########################
# Plan
########################

# 0) overall
# ReconBinary, ReconContinuous, ReconSurvial
# 
# input: vector indicating which treatments will be reconsidered c(0,1,0) [only reconisder second treatment]


# 1) Bayesian bit:
# simulate the possible situations after the reconsideration study has reported
# 
# inputs: mu_prior (mean LOR for prior), variance_prior, variance_study (the expected uncertainty in the study results assumed known with no uncertainty), 
# LOR_prior_vec (draws from LOR prior ~ N(mu_prior, variance_prior))
# MCsimsInner
# outputs: mu_post_vec, variance_post (assumed known with no uncertainty)
# method
# mu_liklihood <- rnorm(1, LOR_prior_vec, variance_study)
# tau_liklihood <- tau_study
# mu_post_vec <- see notes
# variance_post <- see notes


# 2) start loop
# for each simulation of mu_post_vec: mu_post_i

# 2.1) Probability bit
# 
# inputs: mu_post_i, variance_post, P_t1 (vector of length MCsimsOuter - may have to cut down the ususal MCsims)
# output: P_t_i (matrix of probabilities for each treatment)

# 2.2) Net benefit bit
# PMatrixToNBMatrix function 
#
# inputs: P_t_i, the specific inputs for 
# output: NB_t_i (matrix of net benefits for each treatment)

# 2.3) EVPI bit
# 
# input: NB_t_i
# output: EVTPI_post_vec, EVTCI_post_vec
#
# end loop

# 3) Decision value bit
#
# input: EVTPI_post_vec, EVTCI_post_vec, population, discount, special costs,...
# output: NB of the max strategy: always implement the best with current info


############################################
# BINARY
############################################

# inputs
# test data (the same as) P6
# numberOfTreatments =2
# P_t1 =0.95
# INBBinaryEvent = 9.5
# mu_t2=0
# variance_t2=0.25
# dist_t2="norm"
# direction_t2= NA
# mu_t3=NA
# variance_t3=NA
# dist_t3=NA
# direction_t3=NA
# mu_t4=NA
# variance_t4=NA
# dist_t4=NA
# direction_t4=NA
# nameOf_t1="continual treatment"
# nameOf_t2="withdrawal"
# nameOf_t3=NA
# nameOf_t4=NA
# tCostsDependOnEvent = "Yes"
# cost_t1 = 1000
# cost_t2 = 91000
# cost_t3 = NA
# cost_t4 = NA
# costEvent_t1 = 3100000
# costEvent_t2 = 3100000
# costEvent_t3 = NA
# costEvent_t4 = NA
# costNotEvent_t1 = 7300000
# costNotEvent_t2= 4000000
# costNotEvent_t3=NA
# costNotEvent_t4 = NA
# typeOfOutcome="benefit"
# incidence=26.26
# timeInformation=10
# discountRate=3.5
# durationOfResearch= 4
# costResearchFunder=855403
# MCD_t2=0
# MCD_t3=NA
# MCD_t4=NA
# utilisation_t1=100
# utilisation_t2=0
# utilisation_t3=NA
# utilisation_t4=NA
# costHealthSystem = 9899380
# k = 13000
# currencySymbol = "£"
# # additional inputs for reconsideration of evidence
# reconsider = "Yes" #  "No"
# MCsimsInner = 10 
# MCsimsOuter = 10




ReconBinary <- function(){
  
  # baseline probability of outcome in this simulation (always the same as no uncertainty)
  # the lenght of this vector will determine the outer loop
  P_t1_prior <- if(length(P_t1) == 1){
    # if there is no uncertinty in baseline probability
    rep(P_t1, MCsimsOuter)
  } else {
    # if there is uncertainty in baseline probability
    # *** update this when I have inputs for baseline uncertainty ***
    rep(P_t1, MCsimsOuter)
  }
  
  
  
  # prior
  # prior mean of LOR always = mu_t2
  # prior SE of LOR always = sigma_t2
  mu_t2_prior <- mu_t2
  var_t2_prior <- sigma_t2^2
  tau_t2_prior <- 1/var_t2_prior
  
  # continue by taking sections from P5 brain injury
  
}


























































































