#######################
# epi input functions
#######################

library(MASS)



##########################################
##########################################
# BINARY endpoint
##########################################
###########################################

########################################
# Binary endpoint: baseline probability
#######################################

# there are 2 methods to input baseline probability
# 2) UCI and LCI for probability: input using slider normalParameters() calculates mu and sigma
# 3) nEvents, nAtRisk: used to draw a beta distribution




# Binary endpoint, baseline probability 2) from LCI and UCI on baseline probability
############################################
# note: this is done as in Excel model by converting probability to odds
# (slighlty modified as linearlity/ normality holds better on the log scale so this is used here)
# ***theoretically possible to simulate negative probability values with this method? - probably not thanks to exponeitation etc
# also possible to do this by treating the LCI and UCI as a 95% interval on a beta distribtion
# and finding the alpha and beta parameters which best fit the LCI and UCI provided by the user
# these parameters are found by choosing alpha and beta to minimise a loss function:
# squared distance from LCI + squared distance from UCI as described in: https://www.johndcook.com/quantiles_parameters.pdf
# loss function in R
# (qbeta(0.025, alpha_hat, beta_hat) - LCI)^2 + (qbeta(0.975, alpha_hat, beta_hat) - UCI)^2
# not fully sure if this works!

# function
# input: prob_UCI and prob_LCI
# output: mu_prob (exact expected probability), P_t1
#  

# assumes normal log odds distribution

# test data (from Excel model)
#prob_UCI = 0.469
#prob_LCI = 0.248

# define function
BaselineProbCI <- function(MCsims, prob_LCI, prob_UCI){
  
  # if there is no uncertainty in the slider input
  if(prob_LCI == prob_UCI){
    
    mu_prob <- prob_LCI
    P_t1 <- rep(mu_prob,MCsims)
    
    outputs <- list(P_t1 = P_t1, mu_prob = mu_prob)
    
  } else {
    
    # convert prob CIs to odds CIs
    Odds_UCI <- prob_UCI/(1 - prob_UCI)
    Odds_LCI <- prob_LCI/(1 - prob_LCI)
    
    # convert to log scale as sigma is symmetrical around the mean on this scale
    LO_UCI <- log(Odds_UCI)
    LO_LCI <- log(Odds_LCI)
    
    sigma <- abs(LO_UCI - LO_LCI)/(2*1.96)
    mu <- LO_LCI + 1.96*sigma # mean on log odds scale
    mu_OR <- exp(mu)
    mu_prob <- mu_OR/(1 + mu_OR)
    
    LO <- rnorm(MCsims, mu, sigma)
    Odds <- exp(LO)
    P_t1 <- Odds/(1 + Odds)
    
    
    outputs <- list(P_t1 = P_t1, mu_prob = mu_prob)
    
  }
  
  
  return(outputs)
  
}

# test function (from Excel model)
#BaselineProbCI(10000, 0.248,0.469)
#BaselineProbCI(10, 0.2, 0.2)

# consistency test
#x <- BaselineProbCI(1009999, 0.248,0.469)
#x$mu_prob
#mean(x$P_t1)










# Binary endpoint: baseline probability 2) from events vs number at risk
############################################

# function
# input: nEvents and nAtRisk
# output: mu_prob (exact expected probability), P_t1
# assumes MCsims sufficient to express the distribution

# test data (from Excel model)
#nEvents = 100
#nAtRisk = 210
#MCsims = 100000

# define function
probEvents <- function(MCsims, nEvents, nAtRisk){
  
  # beta parameter for beta distribution
  nNonEvents <- nAtRisk - nEvents
  mu_prob <- nEvents/(nEvents + nNonEvents)
  
  P_t1 <- rbeta(MCsims,nEvents,nNonEvents )
  outputs <- list(P_t1 = P_t1, mu_prob = mu_prob)
  
  return(outputs)
}

# test function
#probEvents(10, 20, 10000)



















########################################
# Binary endpoint: relative effects
#######################################
# input: P_t1 vector (could be uncertain or single valued but must represent the MCsims in its length)
# output: P_tn

# there are # methods to input relative effects
# 1) UCI and LCI for natural odds ratio scale
# 2) UCI and LCI for natural RR scale
# 3) UCI and LCI for risk difference
# 4) HalfNormal range odds ratio scale
# 5) HalfNormal range RR scale
# 6) HalfNormal range risk difference



# Binary endpoint: relative effects 1) UCI and LCI for natural odds ratio scale
############################################

# function
# input: OR_UCI and OR_LCI
# output: mu_OR, mu (log scale), variance (log scale)
# assumes normal distribution

# plot user inputs
# use exactVectorNormal with mu, variance to take exact samples from normal on LOR scale
# take exponent of these draws and plot to get smooth natural scale OR plot
# include mu_OR, OR_UCI and OR_LCI on this plot

# test data (from Excel model)
# OR_UCI = 1.18
# OR_LCI = 0.71

# define function
oddsRatioCI <- function(OR_UCI, OR_LCI){
  
  LOR_UCI <- log(OR_UCI) 
  LOR_LCI <- log(OR_LCI)
  sigma <- abs(LOR_UCI - LOR_LCI)/(2*1.96)
  variance <- sigma^2
  
  mu <- LOR_LCI + 1.96*sigma # mean on log odds scale
  mu_OR <- exp(mu)
  
  outputs <- list(mu = mu, variance = variance, mu_OR = mu_OR)
  return(outputs)
}

# test function
# oddsRatioCI(OR_UCI, OR_LCI)
# oddsRatioCI(1.43, 0.05)


# Binary endpoint: relative effects 2) UCI and LCI for natural RR scale
############################################


# function
# input: RR_UCI and RR_LCI
# output: mu_RR (mean RR on natural scale),  sigma_LRR (se of log rr), mu_LRR (mean of log rr)
# assumes normal distribution for log(RR) 

# plot user inputs
# use exactVectorNormal with sigma_LRR, mu_LRR to take exact samples from normal on LRR scale
# take exponent of these draws and plot to get smooth RR plot
# include mu_RR, RR_UCI and RR_LCI on this plot

# get P_tn
# simulate uncertainty in LRR ~ N(mu_LRR, sigma_LRR)
# exp(LRR) = RR
# RR * P_t1 = P_tn

# similar to above function

# test data (from Excel model)
# RR_UCI = 1.18
# RR_LCI = 0.71

# define function
RiskRatioCI <- function(RR_UCI, RR_LCI){
  
  LRR_UCI <- log(RR_UCI) 
  LRR_LCI <- log(RR_LCI)
  sigma_LRR <- abs(LRR_UCI - LRR_LCI)/(2*1.96)
  # variance <- sigma_LRR^2
  
  mu_LRR <- LRR_LCI + 1.96*sigma_LRR # mean on log rr scale
  mu_RR <- exp(mu_LRR)
  
  outputs <- list(mu_RR = mu_RR, sigma_LRR = sigma_LRR, mu_LRR = mu_LRR)
  return(outputs)
}

# test function
# RiskRatioCI(RR_UCI, RR_LCI)
# RiskRatioCI(1.02, 0.94 )






# Binary endpoint: relative effects 3) UCI and LCI for risk difference
############################################


# function
# input: RD_UCI and RD_LCI
# very similar to function above

# plot user inputs

# get P_tn



########################################
# Binary endpoint: probabiliities for new interventions
#######################################


# Binary endpoint: find beta distribtion parameter estimates from vector of probabilitites
##################################################
# supplementary function
# useful to plot inputs neatly by smoothing monte carlo error
# 

# inputs: P_tn (some probability vector)
# outputs: alpha_hat, beta_hat

# test data
#P_tn <- rbeta(10000, 13.5, 10 )

# define function
aproxBetaParams <- function(P_tn){
  
  # stop function if input is an NA
  
  # function finds alpha and beta parameter estimates from data for a beta function
  # optimising function with initial values
  suppressWarnings( # suppress that NaNs are produced - the function appears to work well
    fit_beta <- fitdistr(P_tn,"beta",list(shape1=1,shape2=1)) 
  )
  
  # round the result (hopefully should mean that identical inputs will give identical estimates)
  alpha_hat <- round(fit_beta$estimate[1],1) 
  beta_hat <- round(fit_beta$estimate[2],1)
  
  outputs <- list(alpha_hat = alpha_hat, beta_hat = beta_hat)
  return(outputs)
}

# test function
#aproxBetaParams(P_tn)




# Binary endpoint: individually plot P_t2, P_t3, P_t4 with uncertain baseline and MCD
############################################




# Binary endpoint: comparative plot of probabilities with uncertain baseline
############################################
# requires MASS package
# put all on one diagram 
# fit a beta distribution to 


# function
# input: alpha_hat_t1, beta_hat_t1, alpha_hat_t2, beta_hat_t2, alpha_hat_t3, beta_hat_t3, alpha_hat_t4, beta_hat_t4 
# output: 

# test data 
# P_t <- simProbOfOutcomeMatrixBinary (numberOfTreatments = 3, P_t1 = rep(0.1, 50000),
#                        mu_t2 = 0, variance_t2 = 0.1, dist_t2 = "norm",  direction_t2 = "alwaysPositive",
#                        mu_t3 = 0.2, variance_t3 = 0.1, dist_t3 = "halfNorm", direction_t3 = "alwaysPositive",
#                        mu_t4 = NA, variance_t4 = NA, dist_t4 = "halfNorm", direction_t4 = NA
#                        )
# beta_params_t2 <- aproxBetaParams(P_t[,2])
# beta_params_t3 <- aproxBetaParams(P_t[,3])
# beta_params_t4 <- aproxBetaParams(P_t[,4])


# define function
# <- function(nEvents, nAtRisk, MCsims){


# function finds alpha and beta parameter estimates from data for a beta function
# optimising function with initial values
# fit_beta <- fitdistr(sims,"beta",list(shape1=1,shape2=1)) 
#    alpha_hat <- round(fit_beta$estimate[1],1) 
#    beta_hat <- round(fit_beta$estimate[2],1)
#    x_axis <- seq(0, 1, length.out = 100)
# plot(density(sims))
# lines(x_axis, dbeta(x_axis,alpha_hat, beta_hat ))











