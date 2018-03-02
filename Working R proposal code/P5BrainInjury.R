library(ggplot2)
library(ggthemes)

set.seed(20)

options(scipen=999) # trun off scientifc notation (for convergence axis)


######################################################################################################
##### INPUTS


###################
# simulation resoultion
MCouter <-  20000 # 40000 # NOTE not bad convergence of EVTPI at MCouter = 40000
MCinner <-  20000 # 40000 
# MCinner <- 10000 # m simulations for each inner loop
ptm <- proc.time() # start the clock


###################
# Epi parameters

# early day 3 treatment
mu_t1_U <- 0 # uninformed mean of treatment effect (log odds ratio scale)
sigma_t1_U <- 0.5 # uninformed sd of treatment effect (log odds ratio scale) 

# UCI  log odds ratio
0 + sigma_t1_U*1.96 # correct!
# UCI odds ratio
exp(0 + sigma_t1_U*1.96)  # correct!
# LCI odds ratio
exp(0 - sigma_t1_U*1.96)  # correct!





# https://s3.amazonaws.com/academia.edu.documents/35214348/Claxton_1999.pdf?AWSAccessKeyId=AKIAIWOWYYGZ2Y53UL3A&Expires=1509388840&Signature=5jSQvZOx5fbGUgBy%2FAEnNQo3Z7g%3D&response-content-disposition=inline%3B%20filename%3DMethods_to_elicit_experts_beliefs_over_u.pdf
# table III Soares
# Foam hazard ratio:  -0.96[-6.32 to 4.40]
# Alginate HR:         0.003 [-0.63 to 0.64]
# NPWT HR:             0.45 [- 0.66 to 1.56]

abs(-6.32 - 4.40)/(2*1.96) # SE Foam
abs(-0.63 - 0.64)/(2*1.96) # SE ALG
abs(- 0.66 - 1.56)/(2*1.96) # SE NPWT

# just take average:
mean( c(abs(-6.32 - 4.40)/(2*1.96) , abs(-0.63 - 0.64)/(2*1.96) , abs(- 0.66 - 1.56)/(2*1.96) ))
# 1.2 SE

# informed priors 
#sigma_t0_I <- 0.
#sigma_t1_I <- 0.566 # used the quantity under evaluation NPWT
#sigma_t1_I <- abs(-0.63 - 0.64)/(2*1.96) # SE ALG 
sigma_t1_I <- abs(-6.32 - 4.40)/(2*1.96) # SE Foam


#########################
# health system model inputs (some may not be needed)
Utilisation =  0  # for now assume no one useing the 3 day treatment
Incidence =  8800 
#Prevalence = 8800 # ignore for now!
Time_info =  15   
D_rate =  0.035 # because this is how the survival is discounted 
k = 15000 
#Cost_implementation =  3600000 #- maybe do a threshold analysis?
Time_research =  5
#Time_delay_informprior = 0.5 # set later in code: do for two options: 6 months (0.5 ) and 2 years (2)
Cost_research_NETSCC =  2854000
Cost_research_NHS = 490000



# Uninformed prior on LOR 
###################

######################################################################################################
# [2] Specify a diffuse prior on the log odds ratio of functional recovery

#LO_t0_U <- rnorm(MCouter, mu_t0_U, sigma_t0_U)
LOR_t1_U <- rnorm(MCouter, mu_t1_U, sigma_t1_U)
plot(density(LOR_t1_U))
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
P_t0_U <- 0.555   #### 0.445 chance of not recovering : Nichol 2015 => 0.555 chance of functional recovery 
Odds_t0_U <- P_t0_U / (1 - P_t0_U)
LO_t0_U <- log(Odds_t0_U)

LO_t1_U <- LO_t0_U + LOR_t1_U # note: with no MC error this would be 0 + 0 = 0
Odds_t1_U <- exp(LO_t1_U) # with no MC error this would be exp(0) = 1
P_t1_U <- Odds_t1_U / (Odds_t1_U + 1) # with no MC error this would be 1/2 = 0.5


#plot(density(P_t0_U)) # not very bimodal but slightly (could put uniform over probability)

#Odds_t1_U <- exp(LO_t1_U)
#P_t1_U <- Odds_t1_U / (1 + Odds_t1_U)

summary(Odds_t1_U)
summary(LOR_t1_U)
plot(density(Odds_t1_U))
plot(density(Odds_t1_U))
plot(density(P_t1_U))


#################################
# NATURAL OUTCOME NET BENEFIT 
##################################

# construct the (natural outcome) INB_t1 function 
# primary outcome is a good natural outcome (units = functional recoveries) 
INB_Event  = 1

# not using INB analysis here
#INB_t1_U  = P_t1_U*INB_Event  - P_t0_U*INB_Event 
#E_INB_t1_U  <- mean(INB_t1_U ) # expected INB with treatment 1 (with current info)

# remove MC error by assigning value - NB need to change this for VOI calculations
#NB_t0_U  <- P_t0_U*INB_Event 
#NB_t1_U  <- P_t1_U*INB_Event 
NB_t0_U  <- P_t0_U *INB_Event 
NB_t1_U  <- P_t1_U*INB_Event 

E_NB_t0_U  <- mean(NB_t0_U )
E_NB_t1_U  <- mean(NB_t1_U )

ENB_t_U <- c(E_NB_t0_U, E_NB_t1_U)
  
# events per year for each treatment
E_NB_t0_U *Incidence
E_NB_t1_U *Incidence

#plot(density(INB_t1_U )) # this spans the range from +1 to -1 with bulge of density
# at 0
# not using INB analysis here
#INB_t_U  <- cbind(rep(0, MCouter), INB_t1_U )

# vector for calculating VOI outputs- needs to use simulations
NB_t_U  <- cbind(P_t0_U*INB_Event , P_t1_U*INB_Event )


# Population calculations - no delay (ignore prevalence!)
##############################################
Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))


# Best outcome with uninformed prior 
#EVTCI_U  = max(0, E_INB_t1_U ) # in this case should always equal zero if no Monte carlo error
NB_EVTCI_U  = max(E_NB_t0_U , E_NB_t1_U ) # always 0.5 by definition

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
Optimal_t <- paste0("t_" ,which(ENB_t_U == max(ENB_t_U)) - 1) # tells you which treatment is best

Probability_t_is_max <- apply(NB_t_U, 2, function(x) sum(x==NB_VTPI_U))/MCouter


# annual outcomes
#####################
# ADDITIONAL functional recoveries per year from doing study (compared to best treatment)
NB_EVTPI_U *Incidence - NB_EVTCI_U *Incidence

# value of implemetation
NB_EVTCI_U *Incidence - E_NB_t0_U *Incidence

# outcomes with each treatment
ENB_t_U* Incidence


# histogram of effects per year
#               # NB per simulation with max(ENB_t_U) - max NB per simulation
#                 # best treament with current evidence - max NB per simulation
NB_loss_maxt <- NB_t_U[,which(ENB_t_U == max(ENB_t_U))] - NB_VTPI_U
Hist_value_of_trial_per_year <- hist(-NB_loss_maxt*Incidence)
# convert to probability plot, not density
Hist_value_of_trial_per_year$density = Hist_value_of_trial_per_year$counts/sum(Hist_value_of_trial_per_year$counts)*100
plot(Hist_value_of_trial_per_year,freq=FALSE,
     main = "",
     xlab = "Functional recoveries per annum",
     ylab = "Probability (%)", ylim = c(0,60))
# prob of being in a bin
# to inform : There is a greater chance of more limited consequences (e.g., a 45% chance of consequences between zero and 300 additional relapses per year) and a smaller chance of larger consequences 
Cumulative_prob_being_in_bin <- cumsum(Hist_value_of_trial_per_year$counts/sum(Hist_value_of_trial_per_year$counts) )
Mid_point_of_bin <- Hist_value_of_trial_per_year$mids

Prob_of_consequences <- rbind(Cumulative_prob_being_in_bin, Mid_point_of_bin)


# full time horizon outcomes 
##############################

# full time horizon effect of implementing best treatment (both treatments the same here!)
NB_maxt_U  <- Pop_total*NB_EVTCI_U 
# adjust to eliminate MC error
#E_imp_t0_U <- Pop_total*(Incidence/2) 

# vector of payoff vs time to researcdh report
#Time_research_vec <- c(0:15)
#Pop_total_vec <- rep(Pop_total, length(Time_research_vec))
#Pop_during_trial_vec <- ((Incidence) /-D_rate) * (exp(-D_rate*Time_research_vec) - exp(-D_rate*0))
#Pop_after_trial_vec <- ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research_vec))
#NB_E_maxt_trial_U _vec <- Pop_during_trial_vec*NB_EVTCI_U  + Pop_after_trial_vec*NB_EVTPI_U 
#NB_maxt_U _vec <- Pop_total*NB_EVTCI_U 
#natural_outcome_time_to_research_data <- data.frame(Time_research_vec = Time_research_vec,
#           )
#
#write.csv("natural_outcome_time_to_research_data")




################### NB from costlessly and instantly) Implementing best treatment
# and carrying out perfect trial (with perfect implementing of result)
# note *this completely ignores the costs imposed on the NHS from running trial
NB_E_maxt_trial_U  <- Pop_during_trial*NB_EVTCI_U  + Pop_after_trial*NB_EVTPI_U 

# note *this completely ignores the costs imposed on the NHS from running trial
# ICER
# difference in effects (between trial and implementign best treatment)
NB_E_maxt_trial_U  - NB_maxt_U 
# difference in costs (between trial and implementign best treatment)
Cost_research_NETSCC
# the ICER
Cost_research_NETSCC/(NB_E_maxt_trial_U  - NB_maxt_U )




#####################################################################################
# Economic inputs
# take account of QALYs and aquisition costs
# ignore any other secondary outcomes
######################################################################################


# new figure k = 15000
INB_Event = 15.86 # previously 15.89 for some reason

# includes MC error
#NB_t0_U <- P_t0_U*INB_Event
#NB_t1_U <- P_t1_U*INB_Event


# direct QALY effects per year (i.e. before taking account of op costs)
E_NB_t0_U *Incidence*INB_Event
E_NB_t1_U *Incidence*INB_Event

# direct QALY effects per year (i.e. before taking account of op costs)
#E_NB_t0_U*Incidence
#E_NB_t1_U*Incidence

# additional costs per year for new treatment (treat at 3 days)
14.1*Incidence

# NB per year for each treatmetn (include costs)
0.55*INB_Event*Incidence  # treat at 8 days
0.55*INB_Event*Incidence - (124080/k) # treat at 3 days

#plot(density(INB_t1_U)) # this spans the range from +1 to -1 with bulge of density
# at 0

# vector for calculating VOI outputs- needs to use simulations 
# also need to inlcude aquisition costs at the individual level
NB_t_U <- cbind(P_t0_U*INB_Event, (P_t1_U*INB_Event - 14.1/k))
# expected QALYs per person for each treatment - again symetrical so as MC goes to infinity - INB_Event/2
E_NB_t0_U <- mean(P_t0_U)*INB_Event 
E_NB_t1_U <- mean(P_t1_U)*INB_Event - 14.1/k

########################
# benefits of treatments over ful time horizon

# direct QALY effects full time horizon (i.e. before taking account of op costs)
E_NB_t0_U *Pop_total*INB_Event
E_NB_t1_U *Pop_total*INB_Event

# additional costs per year for new treatment (treat at 3 days)
14.1*Pop_total

# NB per year for each treatmetn (include costs)
E_NB_t0_U *Pop_total*INB_Event  # treat at 8 days
E_NB_t1_U *Pop_total*INB_Event - ((14.1*Pop_total)/k) # treat at 3 days


#  EVTCI_U 
NB_EVTCI_U <- max(E_NB_t0_U, E_NB_t1_U)

# EVTPI_U 
NB_EVTPI_U <- mean(apply(NB_t_U, 1, max))

# EVPI
NB_EVPI_U <- NB_EVTPI_U - NB_EVTCI_U

# Outcomes
#####################
# ADDITIONAL QALYs per year from doing study (compared to best treatment)
NB_EVTPI_U*Incidence - NB_EVTCI_U*Incidence


# Trial reports immediately
####################################
## Cell_A : net benefit of current situation
# here assume everybody gets antipsychotics
Utilisation_t0 <- 1
Utilisation_t1 <- 0
Cell_A <- Pop_total*E_NB_t0_U*Utilisation_t0 +
  Pop_total*E_NB_t1_U*Utilisation_t1 

## Cell_C : Net benfit of immediately implemeting best treatment (Approval)
NB_maxt_U <- Pop_total*NB_EVTCI_U
Cell_C <- NB_maxt_U

## Cell_D : Net benefit of immediate trial
# Pop during the trials: get treated with best treatment during trials 
Cell_D <- NB_EVTPI_U*Pop_total


Cell_D - Cell_A # total value of the research project
Cell_C - Cell_A # implementation value 
Cell_D - Cell_C # net information value (benefit of information - costs)



# additional costs
Cost_research_NETSCC
# ICER for total value of trial
Cost_research_NETSCC/(Cell_D - Cell_A)




# Base case; Trial takes 5 years
####################################

Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))


## Cell_A : net benefit of current situation
# here assume everybody gets antipsychotics
Utilisation_t0 <- 1
Utilisation_t1 <- 0
Cell_A <- Pop_total*E_NB_t0_U*Utilisation_t0 +
  Pop_total*E_NB_t1_U*Utilisation_t1 

## Cell_C : Net benfit of immediately implemeting best treatment (Approval)
NB_maxt_U <- Pop_total*NB_EVTCI_U
Cell_C <- NB_maxt_U

## Cell_D



# Net benefit of  trial
# Pop during the trials: get treated with best treatment during trials 
NB_maxt_perfect_info_imp <- NB_EVTCI_U*Pop_during_trial + NB_EVTPI_U*Pop_after_trial - Cost_research_NHS/k


# information value of trial without counting NHS effects
Cost_research_NETSCC/(NB_EVTCI_U*Pop_during_trial + NB_EVTPI_U*Pop_after_trial - Cell_C)


# information value of trial counting everything
Cost_research_NETSCC/(NB_maxt_perfect_info_imp - Cell_C)



#Cell_D - Cell_A # total value of the research project
#Cell_C - Cell_A # implementation value 
#Cell_D - Cell_C # net information value (benefit of information - costs)

#Cell_D - Cell_A
# additional costs
#Cost_research_NETSCC
# ICER for total value of trial
#Cost_research_NETSCC/(Cell_D - Cell_A)

# example of updating SE
sqrt(1/( 1/(0.5^2) + 1/(0.32^2) ))

#####################################################################################
# Delay analysis
# 
# 
######################################################################################


# reset base case inputs
Time_info = 15
Time_research = 5 
Cost_research_NETSCC =  2854000
Cost_research_NHS = Cost_research_NHS
INB_Event = 15.86
Incidence = 8800

# m simulations for each inner loop
#sigma_t0_I <- 0.5
#sigma_t1_I <- 0.5


# create vectors which will hold values
E_NB_t1_I <- rep(NA, MCouter) # expected value of treating with t1 for lth draw - should have mean centred on lth basis and so should average out to be equal to E_INB_t0_U
E_NB_t0_I <- rep(NA, MCouter) # expected value of treating with t1 for lth draw - should have mean centred on lth basis and so should average out to be equal to E_INB_t0_U

NB_EVTCI_I <- rep(NA, MCouter)
NB_EVTPI_I <- rep(NA, MCouter)

# prior mean: (will be updated with liklihood in each simulation of expert elicitation)
mu_t1_U
# prior sigma (will be used in updating to form the posterior in simulating
# the uncertainty after the expert elcitation)
sigma_t1_U
# simulations from prior (also double as "true" means which the liklihood
# will centre on i.e. the expert elicitation results)
LOR_t1_U



#MCinner = 50000

# for each possible realisation of the expert elicitation
for (l in 1:MCouter){
  
  #set.seed(30)
  #l = 1
  #MCinner <- 1000000
  #ptm <- proc.time() # start the clock
  

  # baseline probability of outcome in this simulation (always the same as no uncertainty)
  P_t0_I_l <- P_t0_U
  
  # prior
  # prior mean of LOR always = mu_t1_U
  # prior SE of LOR always = sigma_t1_U
  mu_t1_prior <- mu_t1_U
  var_t1_prior <- sigma_t1_U^2
  tau_t1_prior <- 1/var_t1_prior
  
  # liklihood
  # "new evidence" from expert elicitaiton will have mean = LOR_t1_U[l]
  # the new evidence is centred on the "true" value 
  # and SE = sigma_t1_I
  # MCouter simulations of liklihood means
  # simulate from this? 
  # (could take this out of the loop)
  X_t1_l <- rnorm(1, LOR_t1_U[l], sigma_t1_I)
  
  mu_t1_liklihood <- LOR_t1_U[l]
  var_t1_liklihood <-  sigma_t1_I^2
  tau_t1_liklihood <- 1/var_t1_liklihood
  
  # based on the website below!
  # Ben Lambert youtube chanel
  # https://www.youtube.com/watch?v=OGxHNPYLtko&list=PLFDbGp5YzjqXQ4oE4w9GVWdiokWB9gEpm&index=43
  
  # other options
  # https://stats.stackexchange.com/questions/237037/bayesian-updating-with-new-data
  
  # posterior means and variances
  # distribution of log odds ratio after updating with new evidence
  # take simulations from this for the VOI analysis
  # posterior mean = 
  # posterior SE = 
  var_t1_post <- 1/(tau_t1_liklihood + tau_t1_prior)
  
  mu_t1_post <- var_t1_post*(mu_t1_prior*tau_t1_prior + mu_t1_liklihood*tau_t1_liklihood)
    
    #(X_t1_l*var_t1_liklihood + mu_t1_prior*var_t1_prior)/(var_t1_liklihood + var_t1_prior)

  # simulations from "Informed" distribution for simulation l
  # i.e. for this realisation of the "true" effect
  LOR_t1_I_l <- rnorm(MCinner, mu_t1_post, sqrt(var_t1_post))
  
  
  
  #P_t0_U <- 0.5
  #Odds_t0_U <- P_t0_U / (1 - P_t0_U)
  #LO_t0_U <- log(Odds_t0_U)
  
  LO_t1_I_l <- LO_t0_U + LOR_t1_I_l 
  Odds_t1_I_l <- exp(LO_t1_I_l) 
  P_t1_I_l <- Odds_t1_I_l / (Odds_t1_I_l + 1) 
  
  # [7b] combine these log odds in the same way as step [3] to calculate (m) estimates
  # of the proportion with the event in both arms
  # corresponding to the lth simulation from the uninformed prior
  #LogOdds_t1_I_l = LOR_t1_I_l + LogOdds_t0_U # treatment : t1
  #Odds_t0_I_l = exp(LO_t0_I_l)
  #Odds_t1_I_l = exp(LO_t1_I_l)
  
  #P_t0_I_l = Odds_t0_I_l / (1 + Odds_t0_I_l)
  #P_t1_I_l = Odds_t1_I_l / (1 + Odds_t1_I_l)
  
  
  # [7c] calculate the NB associated with each (m) simulation, corresponding to lth draw
  
  #INB_t1_I_l = P_t1_I_l*INB_Event - P_t0_U*INB_Event
  # need to take account of aquisition costs!
  NB_t0_I_l = P_t0_I_l*INB_Event
  NB_t1_I_l = P_t1_I_l*INB_Event - 14.1/k
  
  #INB_t_I_l <- cbind(rep(0, MCinner), INB_t1_I_l)
  NB_t_I_l <- cbind(NB_t0_I_l, NB_t1_I_l)
  
  # expected NB from treating with either of the treatments according to informed simulations
  # note this is not a linear transformation of the LO from the outer loop - 
  # this is because converting LO to probability is a non linear transformation. in simulations I ran a higher 
  # sampling variance on the log odds scale resulted in lower mean probability (probably because the extreme upper values
  # were squashed to 1 but the extreme lower values were not squashed by zero)
  E_NB_t0_I[l] <- mean(NB_t0_I_l) 
  E_NB_t1_I[l] <- mean(NB_t1_I_l) # same as above
  
  
  NB_EVTCI_I[l] <- max(E_NB_t0_I[l], E_NB_t1_I[l])
  NB_VTPI_I <- apply(NB_t_I_l, 1, max) # to allow for convergence checks
  NB_EVTPI_I[l] <- mean(NB_VTPI_I)
  #proc.time() - ptm
}


#######################################################################################
# Checking and understanding sequential analysis
################################################################################

# runnign time analysis
# MCinner = 100000 time per analysis = 0.44 secs
# if MCouter = 40000 =>
#0.44*MCouter # seconds
#(0.44*MCouter)/60 # minutes
#((0.44*MCouter)/60)/60 # 4.88 hours

# MCinner = 50000 time per analysis = 0.27 secs
# if MCouter = 40000 =>
#0.27*MCouter # seconds
#(0.27*MCouter)/60 # minutes
#((0.27*MCouter)/60)/60 # 3 hours


# Stop the clock
proc.time() - ptm # took ~2 hours 
# save results MCinner, MCouter = 20K
#final_40Kin_out_data <- data.frame(LO_t0_U = LO_t0_U,
#                                LO_t1_U = LO_t1_U,
#                                NB_EVTCI_I = NB_EVTCI_I, 
#                                NB_EVTPI_I = NB_EVTPI_I,
#                                E_NB_t0_I = E_NB_t0_I,
#                                E_NB_t1_I = E_NB_t1_I
#)
#write.csv(final_40Kin_out_data, "final_40Kin_out_data")

#final data: 40Kin out - has updated INB etc
#final_40Kin_out_data <- read.csv("final_40Kin_out_data")
#NB_EVTCI_I = final_40Kin_out_data$NB_EVTCI_I
#NB_EVTPI_I = final_40Kin_out_data$NB_EVTPI_I
#E_NB_t0_I = final_40Kin_out_data$E_NB_t0_I
#E_NB_t1_I = final_40Kin_out_data$E_NB_t1_I


#Running mean to check convergence - based on estimate of EVTPI (deviations difficult to understand really!)
#NB_VTPI_U  <- apply(NB_t_U , 1, max) #so I can check convergence
#NB_EVTPI_U  <- mean(NB_VTPI_U )
EVTPI_I.run<-c(rep(0,150))
for (i in 1:150){
  EVTPI_I.run[i]<-mean(NB_VTPI_I[1:(i*(MCinner/150))])
}
plot(seq(1,i*(MCinner/150), length.out = length(EVTPI_I.run)), EVTPI_I.run, type="l",lty=1, xlab="Simulation", ylab="EVTPI_I, sim = 3")
# l = 1 convergence at MCinner = 200K +- 0.001, partial convergence at MCinner = 100K +- 0.002
# l = 2 convergence at MCinner = 100K 4.14<->4.145
# l = 3 convergence at MCinner = 400K +- 16.168<->16.1685, partial convergence at MCinner = 100K +- 16.168<->16.169

#Pop_before_and_during_trial_I*NB_EVTCI_U + 
#  Pop_after_trial*NB_EVTPI_I

NB_maxt_trial_I_NHS.run<-c(rep(0,150))
for (i in 1:150){
  NB_maxt_trial_I_NHS.run[i]<-mean((Pop_before_and_during_trial_I*NB_EVTCI_U + 
                                      Pop_after_trial*NB_VTPI_I)[1:(i*(MCinner/150))])
}
plot(seq(1,i*(MCinner/150), length.out = length(NB_maxt_trial_I_NHS.run)), NB_maxt_trial_I_NHS.run, type="l",lty=1, xlab="Simulation", ylab="NB maxt trial NHS effects, sim = 1, seed = 30")
# l = ? possible convergence at MCinner = 700K , reasonable convergence at 200K +-200QALYS, prior to this +- 600 QALYs
# set.seed(30)
# l = 1 possible full convergence at MCinner = 700K , reasonable convergence at 200K +-200QALYS, prior to this +- 600 QALYs
# l = 3 convergence at MCinner = 400K +- 16.168<->16.1685, partial convergence at MCinner = 100K +- 16.168<->16.169

# read in previously simulated data
#storeddata <- read_csv("W:/teehta/David G/NETCC/2017/Awayday meeting 2 2May2017/Awayday meeting 2/stored_delay_data MCouter 40K MCinner 50K")
#NB_EVTPI_I <- storeddata$NB_EVTPI_I

library(ggplot2)
library(ggthemes)
LO_demo_I <- rnorm(MCouter, 2.33, sigma_t0_I)
priorplot_demo.df <- as.data.frame(LO_demo_I)
ggplot(priorplot_demo.df, aes(x=LO_demo_I)) + geom_density(fill = "blue", alpha = 0.5) +
  geom_hline(yintercept = 0, size = 1) + 
  geom_vline(xintercept = 0, size = 1) +
  geom_vline(xintercept = 2.33, size = 0.15) +
  annotate("text", x=3.8, y=0.7, label="~N(mean = 2.33, sd = 0.5)", size = 4) + 
  annotate("text", x=2.5, y=0.05, label="2.33", size = 4) + 
  ggtitle("One sample from informed prior distribution \n of functional recovery") + 
  scale_x_continuous("Log odds of functional recovery") +
  scale_y_continuous("Density") +
  theme_igray()








#################################################################################################
# 6 month delay analysis (FINAL)
######################################################################################
# **here they stick with whichever treatment looks best according to uninformed prior (but
# NBs are assigned to infomred prior estimate)

# DELy for one month
Time_delay_informprior <- 0.5 # get results for 1 month

Pop_delay_I <- ((Incidence) /-D_rate) * (exp(-D_rate*Time_delay_informprior) - exp(-D_rate*(0)))
Pop_during_trial_I <-  ((Incidence) /-D_rate) * (exp(-D_rate*(Time_research+Time_delay_informprior)) - exp(-D_rate*(0+Time_delay_informprior)))
Pop_after_trial_I <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*(Time_research+Time_delay_informprior)))
Pop_before_and_during_trial_I <- Pop_delay_I + Pop_during_trial_I

# **Decision rule** for max_action_I

# two options for each simulation: fund trial, implement best treatment

# vector: NB of funding the maxt trial for each simulation
sixm_NB_maxt_trial_I_NHS <- Pop_delay_I*NB_EVTCI_U + Pop_during_trial_I*NB_EVTCI_I  + 
  Pop_after_trial_I*NB_EVTPI_I - Cost_research_NHS/k


# vector: NB OF funding the best treatment with the informed prior
sixm_NB_maxt_U_I <- Pop_delay_I*NB_EVTCI_U + (Pop_during_trial_I + Pop_after_trial_I)*NB_EVTCI_I
# sixm_E_NB_maxt_U_I <- mean(sixm_NB_maxt_U_I) is this a noisy estimate of the underlying thing estimated by just using the uninformed prior?



# enact decision rule here! NETSCC fund the trial if the overall NB (including NETSCC at k = 13K is higher than not funding trial)
Fund_trial_I <- (sixm_NB_maxt_trial_I_NHS - Cost_research_NETSCC/k) > sixm_NB_maxt_U_I
sixm_max_action_I_NHS <- rep(NA, MCouter)
sixm_max_action_I_NHS[Fund_trial_I] <- sixm_NB_maxt_trial_I_NHS[Fund_trial_I]
sixm_max_action_I_NHS[!Fund_trial_I] <- sixm_NB_maxt_U_I[!Fund_trial_I]

# expected NHS effects of POST 1 month DELAY max action (choose trial or no trial according to decision rule)
sixm_E_max_action_I_NHS <- mean(sixm_max_action_I_NHS)


# expectd costs to NETSCC
# proportion of time trial funded
sixm_Prob_fund_trial_I <- mean(Fund_trial_I)
sixm_E_disc_cost_trial_I <- (Cost_research_NETSCC*sixm_Prob_fund_trial_I)/(1+D_rate)^Time_delay_informprior

# no trial option
#NB_maxt_U # NHS healht benefit
#0           # cost to NETSCC
#NA       # ICER
# trial with uninformed prior
#NB_E_maxt_trial_U_NHS # NHS healht benefit
#Cost_research_NETSCC           # cost to NETSCC
#Cost_research_NETSCC/(NB_E_maxt_trial_U_NHS - NB_maxt_U)       # NETSCC ICER

# delay **1 month** - delay almost always dominates the trial? 
sixm_E_max_action_I_NHS     # NHS healht benefit for doing the best action every time
sixm_E_disc_cost_trial_I          # cost to NETSCC
(sixm_E_disc_cost_trial_I)/(sixm_E_max_action_I_NHS - NB_maxt_U)       # NETSCC ICER
# delay always dominates the trial if there is any chance the trial will not be
# funded under a realisation of the uncertainty
# as it appears that NB_E_maxt_trial_I_NHS > NB_E_maxt_trial_U_NHS

# gain from delay
sixm_E_max_action_I_NHS - NB_maxt_perfect_info_imp
# prob fund trial
sixm_Prob_fund_trial_I


sixm_E_max_action_I_NHS - NB_maxt_U
NB_maxt_perfect_info_imp - NB_maxt_U















































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
Time_research = 5*decrease 
Cost_research_NETSCC =  2854000*decrease
Cost_research_NHS = Cost_research_NHS*decrease
INB_Event = 15.86*increase
Incidence = 8800*increase

################################## optimistic model ####################################
NB_t_U <- cbind(P_t0_U*INB_Event, (P_t1_U*INB_Event - 14.1/k))
# expected QALYs per person for each treatment - again symetrical so as MC goes to infinity - INB_Event/2
E_NB_t0_U <- 0.5*INB_Event 
E_NB_t1_U <- 0.5*INB_Event - 14.1/k

#  EVTCI_U 
NB_EVTCI_U <- max(E_NB_t0_U, E_NB_t1_U)

# EVTPI_U 
NB_EVTPI_U <- mean(apply(NB_t_U, 1, max))

Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))

## Cell_A : net benefit of current situation
# here assume everybody gets antipsychotics
Utilisation_t0 <- 1
Utilisation_t1 <- 0
Cell_A <- Pop_total*E_NB_t0_U*Utilisation_t0 +
  Pop_total*E_NB_t1_U*Utilisation_t1 

## Cell_C : Net benfit of immediately implemeting best treatment (Approval)
NB_maxt_U <- Pop_total*NB_EVTCI_U
Cell_C <- NB_maxt_U

## Cell_D : Net benefit of  trial
# Pop during the trials: get treated with best treatment during trials 
Cell_D <- NB_EVTCI_U*Pop_during_trial + NB_EVTPI_U*Pop_after_trial - Cost_research_NHS/k

# research project has negative value as the VOI is low and the research costs are high
Cell_D - Cell_A # total value of the research project
Cell_C - Cell_A # implementation value 
Cell_D - Cell_C # net information value (benefit of information - costs)

# optimistic outputs
Cell_D - Cell_A
# additional costs
Cost_research_NETSCC
# ICER for total value of trial
Cost_research_NETSCC/(Cell_D - Cell_A)




#######################################################################################
# Pessimistic 
########################################################################

# pessimistic inputs
Time_info = 15*decrease
Time_research = 5*increase 
Cost_research_NETSCC =  2854000*increase
Cost_research_NHS = Cost_research_NHS*increase
INB_Event = 15.86*decrease
Incidence = 8800*decrease

################################## pessimistic model ####################################
NB_t_U <- cbind(P_t0_U*INB_Event, (P_t1_U*INB_Event - 14.1/k))
# expected QALYs per person for each treatment - again symetrical so as MC goes to infinity - INB_Event/2
E_NB_t0_U <- 0.5*INB_Event 
E_NB_t1_U <- 0.5*INB_Event - 14.1/k

#  EVTCI_U 
NB_EVTCI_U <- max(E_NB_t0_U, E_NB_t1_U)

# EVTPI_U 
NB_EVTPI_U <- mean(apply(NB_t_U, 1, max))

Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))

## Cell_A : net benefit of current situation
# here assume everybody gets antipsychotics
Utilisation_t0 <- 1
Utilisation_t1 <- 0
Cell_A <- Pop_total*E_NB_t0_U*Utilisation_t0 +
  Pop_total*E_NB_t1_U*Utilisation_t1 

## Cell_C : Net benfit of immediately implemeting best treatment (Approval)
NB_maxt_U <- Pop_total*NB_EVTCI_U
Cell_C <- NB_maxt_U

## Cell_D : Net benefit of  trial
# Pop during the trials: get treated with best treatment during trials 
Cell_D <- NB_EVTCI_U*Pop_during_trial + NB_EVTPI_U*Pop_after_trial - Cost_research_NHS/k

# research project has negative value as the VOI is low and the research costs are high
Cell_D - Cell_A # total value of the research project
Cell_C - Cell_A # implementation value 
Cell_D - Cell_C # net information value (benefit of information - costs)

# pessimistic outputs
Cell_D - Cell_A
# additional costs
Cost_research_NETSCC
# ICER for total value of trial
Cost_research_NETSCC/(Cell_D - Cell_A)






############################ old delay analysis ####################################################

#################################################################################################
# 1 month delay analysis (new way of calculating)
######################################################################################
# using informed values for prior to assign NB 
# taking the informed prior as reflecting the most appropriate measure of NB in both
# cases. As oppose
# **here they stick with whichever treatment looks best according to uninformed prior (but
# NBs are assigned to infomred prior estimate)

# DELy for one month
Time_delay_informprior <- 0.083 # get results for 1 month

Pop_delay_I <- ((Incidence) /-D_rate) * (exp(-D_rate*Time_delay_informprior) - exp(-D_rate*(0)))
Pop_during_trial_I <-  ((Incidence) /-D_rate) * (exp(-D_rate*(Time_research+Time_delay_informprior)) - exp(-D_rate*(0+Time_delay_informprior)))
Pop_after_trial_I <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*(Time_research+Time_delay_informprior)))
Pop_before_and_during_trial_I <- Pop_delay_I + Pop_during_trial_I

# **Decision rule** for max_action_I

# uninfomred prior chooses t0 in this case (could easily extend so that they choose max every time)
# range of NHS (health) effects of POST DELAY trial (not taking into accout NETSCC costs)
onem_NB_maxt_trial_I_NHS <- Pop_delay_I*E_NB_t0_U + Pop_during_trial_I*E_NB_t0_I  + 
  Pop_after_trial_I*NB_EVTPI_I - Cost_research_NHS/k

# Expectation of NHS (health) effects of POST DELAY trial (not taking into accout NETSCC costs)
onem_NB_E_maxt_trial_I_NHS <- mean(onem_NB_maxt_trial_I_NHS) # is this relevant to anything? 

# compare to NB of implementing t0 (taking account of infomred prior values for t0)
#NB_maxt_U <- Pop_total*NB_EVTCI_U previous calculation
onem_NB_maxt_U_I <- Pop_delay_I*E_NB_t0_U + (Pop_during_trial_I + Pop_after_trial_I)*E_NB_t0_I
# onem_E_NB_maxt_U_I <- mean(onem_NB_maxt_U_I) is this a noisy estimate of the underlying thing estimated by just using the uninformed prior?

# enact decision rule here! NETSCC fund the trial if the overall NB (including NETSCC at k = 13K is higher than not funding trial)
Fund_trial_I <- (onem_NB_maxt_trial_I_NHS - Cost_research_NETSCC/k) > onem_NB_maxt_U_I
onem_max_action_I_NHS <- rep(NA, MCouter)
onem_max_action_I_NHS[Fund_trial_I] <- onem_NB_maxt_trial_I_NHS[Fund_trial_I]
onem_max_action_I_NHS[!Fund_trial_I] <- onem_NB_maxt_U_I[!Fund_trial_I]

# expected NHS effects of POST 6 month DELAY max action (choose trial or no trial according to decision rule)
onem_E_max_action_I_NHS <- mean(onem_max_action_I_NHS)


# expectd costs to NETSCC
# proportion of time trial funded
onem_Prob_fund_trial_I <- mean(Fund_trial_I)
onem_E_disc_cost_trial_I <- (Cost_research_NETSCC*onem_Prob_fund_trial_I)/(1+D_rate)^Time_delay_informprior

# no trial option
#NB_maxt_U # NHS healht benefit
#0           # cost to NETSCC
#NA       # ICER
# trial with uninformed prior
#NB_E_maxt_trial_U_NHS # NHS healht benefit
#Cost_research_NETSCC           # cost to NETSCC
#Cost_research_NETSCC/(NB_E_maxt_trial_U_NHS - NB_maxt_U)       # NETSCC ICER

# delay **1 month** - delay almost always dominates the trial? 
onem_E_max_action_I_NHS     # NHS healht benefit for doing the best action every time
onem_E_disc_cost_trial_I          # cost to NETSCC
(onem_E_disc_cost_trial_I)/(onem_E_max_action_I_NHS - NB_maxt_U)       # NETSCC ICER
# delay always dominates the trial if there is any chance the trial will not be
# funded under a realisation of the uncertainty
# as it appears that NB_E_maxt_trial_I_NHS > NB_E_maxt_trial_U_NHS





#################################################################################################
# 6 month delay analysis (new way of calculating)
######################################################################################
# using informed values for prior to assign NB 
# taking the informed prior as reflecting the most appropriate measure of NB in both
# cases. As oppose
# **here they stick with whichever treatment looks best according to uninformed prior (but
# NBs are assigned to infomred prior estimate)

# DELy for six months
Time_delay_informprior <- 0.5 # get results for 6 months first

Cost_inform_prior <- 10000
# Populations for informed prior (currently:  ignore prevalence!)
# the pop total below only includes those after delay - dont use this!
#Pop_total_I <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*(0+Time_delay_informprior) ))
# Incidence                                                     time end                time start
#Pop_before_and_during_trial_I <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
#Pop_before_trial_I <- Pop_before_and_during_trial_I - Pop_during_trial_I
Pop_delay_I <- ((Incidence) /-D_rate) * (exp(-D_rate*Time_delay_informprior) - exp(-D_rate*(0)))
Pop_during_trial_I <-  ((Incidence) /-D_rate) * (exp(-D_rate*(Time_research+Time_delay_informprior)) - exp(-D_rate*(0+Time_delay_informprior)))
Pop_after_trial_I <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*(Time_research+Time_delay_informprior)))
Pop_before_and_during_trial_I <- Pop_delay_I + Pop_during_trial_I

# **Decision rule** for max_action_I

# normally would do this
# Cell_D <- NB_EVTCI_U*Pop_during_trial + NB_EVTPI_U*Pop_after_trial - Cost_research_NHS/k

# uninfomred prior chooses t0 in this case (could easily extend so that they choose max every time)
# range of NHS (health) effects of POST DELAY trial (not taking into accout NETSCC costs)
sixm_NB_maxt_trial_I_NHS <- Pop_delay_I*E_NB_t0_U + Pop_during_trial_I*E_NB_t0_I  + 
  Pop_after_trial_I*NB_EVTPI_I - Cost_research_NHS/k

# Expectation of NHS (health) effects of POST DELAY trial (not taking into accout NETSCC costs)
sixm_NB_E_maxt_trial_I_NHS <- mean(sixm_NB_maxt_trial_I_NHS) # is this relevant to anything? 

# compare to NB of implementing t0 (taking account of infomred prior values for t0)
#NB_maxt_U <- Pop_total*NB_EVTCI_U previous calculation
sixm_NB_maxt_U_I <- Pop_delay_I*E_NB_t0_U + (Pop_during_trial_I + Pop_after_trial_I)*E_NB_t0_I
# sixm_E_NB_maxt_U_I <- mean(sixm_NB_maxt_U_I) is this a noisy estimate of the underlying thing estimated by just using the uninformed prior?

# enact decision rule here! NETSCC fund the trial if the overall NB (including NETSCC at k = 13K is higher than not funding trial)
Fund_trial_I <- (sixm_NB_maxt_trial_I_NHS - Cost_research_NETSCC/k) > sixm_NB_maxt_U_I
sixm_max_action_I_NHS <- rep(NA, MCouter)
sixm_max_action_I_NHS[Fund_trial_I] <- sixm_NB_maxt_trial_I_NHS[Fund_trial_I]
sixm_max_action_I_NHS[!Fund_trial_I] <- sixm_NB_maxt_U_I[!Fund_trial_I]

# expected NHS effects of POST 6 month DELAY max action (choose trial or no trial according to decision rule)
sixm_E_max_action_I_NHS <- mean(sixm_max_action_I_NHS)


# expectd costs to NETSCC
# proportion of time trial funded
sixm_Prob_fund_trial_I <- mean(Fund_trial_I)
sixm_E_disc_cost_trial_I <- (Cost_research_NETSCC*sixm_Prob_fund_trial_I)/(1+D_rate)^Time_delay_informprior

# no trial option
#NB_maxt_U # NHS healht benefit
#0           # cost to NETSCC
#NA       # ICER
# trial with uninformed prior
#NB_E_maxt_trial_U_NHS # NHS healht benefit
#Cost_research_NETSCC           # cost to NETSCC
#Cost_research_NETSCC/(NB_E_maxt_trial_U_NHS - NB_maxt_U)       # NETSCC ICER

# compare to doing nothing
Utilisation_t0 <- 1
Utilisation_t1 <- 0
Cell_A <- Pop_total*E_NB_t0_U*Utilisation_t0 +
  Pop_total*E_NB_t1_U*Utilisation_t1 


# delay **6 months** - delay almost always dominates the trial? 
sixm_E_max_action_I_NHS  - Cell_A   # NHS healht benefit for doing the best action every time
sixm_E_disc_cost_trial_I + Cost_inform_prior         # cost to NETSCC
(sixm_E_disc_cost_trial_I+ Cost_inform_prior)/(sixm_E_max_action_I_NHS - Cell_A)       # NETSCC ICER
# delay always dominates the trial if there is any chance the trial will not be
# funded under a realisation of the uncertainty
# as it appears that NB_E_maxt_trial_I_NHS > NB_E_maxt_trial_U_NHS

# £10K (cost informing prior) creates:
Cost_inform_prior*(sixm_E_max_action_I_NHS - Cell_A)/sixm_E_disc_cost_trial_I


###############
# **repeat analysis for DELAY for 2 years** (new way of calculating)

# DELy for six months
Time_delay_informprior <- 2 # get results for 2 years (second)
# Populations for informed prior (currently:  ignore prevalence!)
#Pop_total_I <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*(0+Time_delay_informprior) ))
# Incidence                                                     time end                time start
Pop_delay_I <- ((Incidence) /-D_rate) * (exp(-D_rate*Time_delay_informprior) - exp(-D_rate*(0)))
Pop_during_trial_I <-  ((Incidence) /-D_rate) * (exp(-D_rate*(Time_research+Time_delay_informprior)) - exp(-D_rate*(0+Time_delay_informprior)))
Pop_after_trial_I <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*(Time_research+Time_delay_informprior)))
Pop_before_and_during_trial_I <- Pop_delay_I + Pop_during_trial_I

# range of NHS (health) effects of POST DELAY trial (not taking into accout NETSCC costs)
twoyr_NB_maxt_trial_I_NHS <- Pop_delay_I*E_NB_t0_U + Pop_during_trial_I*E_NB_t0_I  + 
  Pop_after_trial_I*NB_EVTPI_I - Cost_research_NHS/k

# Expectation of NHS (health) effects of POST DELAY trial (not taking into accout NETSCC costs)
twoyr_NB_E_maxt_trial_I_NHS <- mean(twoyr_NB_maxt_trial_I_NHS) # is this relevant to anything? 

# compare to NB of implementing t0 (taking account of infomred prior values for t0)
#NB_maxt_U <- Pop_total*NB_EVTCI_U previous calculation
twoyr_NB_maxt_U_I <- Pop_delay_I*E_NB_t0_U + (Pop_during_trial_I + Pop_after_trial_I)*E_NB_t0_I

# enact decision rule here! NETSCC fund the trial if the overall NB (including NETSCC at k = 13K is higher than not funding trial)
Fund_trial_I <- (twoyr_NB_maxt_trial_I_NHS - Cost_research_NETSCC/k) > twoyr_NB_maxt_U_I
twoyr_max_action_I_NHS <- rep(NA, MCouter)
twoyr_max_action_I_NHS[Fund_trial_I] <- twoyr_NB_maxt_trial_I_NHS[Fund_trial_I]
twoyr_max_action_I_NHS[!Fund_trial_I] <- twoyr_NB_maxt_U_I[!Fund_trial_I]

# expected NHS effects of POST DELAY max action (choose trial according to decision rule)
twoyr_E_max_action_I_NHS <- mean(twoyr_max_action_I_NHS)

# expectd costs to NETSCC
# proportion of time trial funded
twoyr_Prob_fund_trial_I <- mean(Fund_trial_I)
twoyr_E_disc_cost_trial_I <- (Cost_research_NETSCC*twoyr_Prob_fund_trial_I)/(1+D_rate)^Time_delay_informprior


# no trial option
#NB_maxt_U # NHS healht benefit
#0           # cost to NETSCC
#NA       # ICER
# trial with uninformed prior
#NB_E_maxt_trial_U_NHS # NHS healht benefit
#Cost_research_NETSCC           # cost to NETSCC
#Cost_research_NETSCC/(NB_E_maxt_trial_U_NHS - NB_maxt_U)       # NETSCC ICER

# delay **2 years** 
twoyr_E_max_action_I_NHS     # NHS healht benefit for doing the best action every time
twoyr_E_disc_cost_trial_I          # cost to NETSCC
(twoyr_E_disc_cost_trial_I)/(twoyr_E_max_action_I_NHS - NB_maxt_U)       # NETSCC ICER
# delay always dominates the trial if there is any chance the trial will not be
# funded under a realisation of the uncertainty
# as it appears that NB_E_maxt_trial_I_NHS > NB_E_maxt_trial_U_NHS


# full ICER anlaysis
#####################
# both 6 month and 2 year delay cheaper than funding trial at 
# 6 month delay slightly more expensive compared to 2 year
# No trial = base case
# ICER 2 year compared to sticking with best treatment (base case)
(Cost_research_NETSCC*twoyr_Prob_fund_trial_I)/(twoyr_E_max_action_I_NHS - NB_maxt_U) 
# ICER 6 months compared to 2 years
(Cost_research_NETSCC*sixm_Prob_fund_trial_I - Cost_research_NETSCC*twoyr_Prob_fund_trial_I) / (sixm_E_max_action_I_NHS - twoyr_E_max_action_I_NHS)
# 6 months not much more expensive but a lot more effective 
# => 6 months extendedly dominates 2 year

# FINAL ICER analysis: 
# No trial = base case
# ICER 6 months compared to base case
(Cost_research_NETSCC*sixm_Prob_fund_trial_I)/(sixm_E_max_action_I_NHS - NB_maxt_U)   




###############
# **repeat analysis for DELAY for 1 years "most appropriate time for 
# NETSCC" karl

# DELy for six months
Time_delay_informprior <- 1 # get results for 1 year
# Populations for informed prior (currently:  ignore prevalence!)
#Pop_total_I <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*(0+Time_delay_informprior) ))
# Incidence                                                     time end                time start
Pop_delay_I <- ((Incidence) /-D_rate) * (exp(-D_rate*Time_delay_informprior) - exp(-D_rate*(0)))
Pop_during_trial_I <-  ((Incidence) /-D_rate) * (exp(-D_rate*(Time_research+Time_delay_informprior)) - exp(-D_rate*(0+Time_delay_informprior)))
Pop_after_trial_I <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*(Time_research+Time_delay_informprior)))
Pop_before_and_during_trial_I <- Pop_delay_I + Pop_during_trial_I

# range of NHS (health) effects of POST DELAY trial (not taking into accout NETSCC costs)
oneyr_NB_maxt_trial_I_NHS <- Pop_delay_I*E_NB_t0_U + Pop_during_trial_I*E_NB_t0_I  + 
  Pop_after_trial_I*NB_EVTPI_I - Cost_research_NHS/k

# Expectation of NHS (health) effects of POST DELAY trial (not taking into accout NETSCC costs)
oneyr_NB_E_maxt_trial_I_NHS <- mean(oneyr_NB_maxt_trial_I_NHS) # is this relevant to anything? 

# compare to NB of implementing t0 (taking account of infomred prior values for t0)
#NB_maxt_U <- Pop_total*NB_EVTCI_U previous calculation
oneyr_NB_maxt_U_I <- Pop_delay_I*E_NB_t0_U + (Pop_during_trial_I + Pop_after_trial_I)*E_NB_t0_I

# enact decision rule here! NETSCC fund the trial if the overall NB (including NETSCC at k = 13K is higher than not funding trial)
Fund_trial_I <- (oneyr_NB_maxt_trial_I_NHS - Cost_research_NETSCC/k) > oneyr_NB_maxt_U_I
oneyr_max_action_I_NHS <- rep(NA, MCouter)
oneyr_max_action_I_NHS[Fund_trial_I] <- oneyr_NB_maxt_trial_I_NHS[Fund_trial_I]
oneyr_max_action_I_NHS[!Fund_trial_I] <- oneyr_NB_maxt_U_I[!Fund_trial_I]

# expected NHS effects of POST DELAY max action (choose trial according to decision rule)
oneyr_E_max_action_I_NHS <- mean(oneyr_max_action_I_NHS)

# expectd costs to NETSCC
# proportion of time trial funded
oneyr_Prob_fund_trial_I <- mean(Fund_trial_I)
oneyr_E_disc_cost_trial_I <- (Cost_research_NETSCC*oneyr_Prob_fund_trial_I)/(1+D_rate)^Time_delay_informprior


# no trial option
#NB_maxt_U # NHS healht benefit
#0           # cost to NETSCC
#NA       # ICER
# trial with uninformed prior
#NB_E_maxt_trial_U_NHS # NHS healht benefit
#Cost_research_NETSCC           # cost to NETSCC
#Cost_research_NETSCC/(NB_E_maxt_trial_U_NHS - NB_maxt_U)       # NETSCC ICER

# delay **one year** 
oneyr_E_max_action_I_NHS     # NHS healht benefit for doing the best action every time
oneyr_E_disc_cost_trial_I          # cost to NETSCC
(oneyr_E_disc_cost_trial_I)/(oneyr_E_max_action_I_NHS - NB_maxt_U)       # NETSCC ICER
# delay always dominates the trial if there is any chance the trial will not be
# funded under a realisation of the uncertainty
# as it appears that NB_E_maxt_trial_I_NHS > NB_E_maxt_trial_U_NHS








################################################################################################
# OUTPUTS for awayday 2 Proposal 5
###################################################################################################


####################################################################################################
# NATURAL OUTCOMES #
####################################################################################################
# NATURAL OUTCOMES #
####################################################################################################



# per year outcomes
###################
(NB_EVTPI_U  - NB_EVTCI_U )*Incidence # additional functional recoveries per year


# Full time horizon outcomes
##############

### trial reports IMMEDIATELY ####
Time_research <- 0
Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))
# full time horizon effect of implementing best treatment (both treatments the same here!)
NB_maxt_U  <- Pop_total*NB_EVTCI_U 
#NB from costlessly and instantly) Implementing best treatment
NB_E_maxt_trial_U  <- Pop_during_trial*NB_EVTCI_U  + Pop_after_trial*NB_EVTPI_U 
# additional recoveries from trial (5 years)
NB_E_maxt_trial_U   - NB_maxt_U 
# ICER compared to no trial
Cost_research_NETSCC/ (NB_E_maxt_trial_U   - NB_maxt_U )
### trial reports in IMMEDIATELY ####


### trial reports in 5 YEARS ####
Time_research <- 5
Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))
# full time horizon effect of implementing best treatment (both treatments the same here!)
NB_maxt_U  <- Pop_total*NB_EVTCI_U 
#NB from costlessly and instantly) Implementing best treatment
NB_E_maxt_trial_U  <- Pop_during_trial*NB_EVTCI_U  + Pop_after_trial*NB_EVTPI_U 
# additional recoveries from trial (5 years)
NB_E_maxt_trial_U   - NB_maxt_U 
# ICER compared to no trial
Cost_research_NETSCC/ (NB_E_maxt_trial_U   - NB_maxt_U )
### trial reports in 5 YEARS ####


### trial reports in 10 YEARS ####
Time_research <- 10
Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))
# full time horizon effect of implementing best treatment (both treatments the same here!)
NB_maxt_U  <- Pop_total*NB_EVTCI_U 
#NB from costlessly and instantly) Implementing best treatment
NB_E_maxt_trial_U  <- Pop_during_trial*NB_EVTCI_U  + Pop_after_trial*NB_EVTPI_U 
# additional recoveries from trial 
NB_E_maxt_trial_U   - NB_maxt_U 
# ICER compared to no trial
Cost_research_NETSCC/ (NB_E_maxt_trial_U   - NB_maxt_U )
### trial reports in 10 YEARS ####






####################################################################################################
# QALY ANALYSIS WITH UNINFORMED PRIOR #
####################################################################################################
# QALY ANALYSIS WITH UNINFORMED PRIOR #
####################################################################################################



# per year outcomes
###################
(NB_EVTPI_U - NB_EVTCI_U)*Incidence # additional QALYs per year


# Full time horizon outcomes
##############

### trial reports IMMEDIATELY ####
Time_research <- 0
Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))
# full time horizon effect of implementing best treatment (both treatments the same here!)
NB_maxt_U <- Pop_total*NB_EVTCI_U
#NB from costlessly and instantly) Implementing best treatment
NB_E_maxt_trial_U <- Pop_during_trial*NB_EVTCI_U + Pop_after_trial*NB_EVTPI_U
# additional recoveries from trial (5 years)
NB_E_maxt_trial_U  - NB_maxt_U
# ICER compared to no trial
Cost_research_NETSCC/ (NB_E_maxt_trial_U  - NB_maxt_U)
### trial reports in IMMEDIATELY ####


### trial reports in 5 YEARS ####
Time_research <- 5
Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))
# full time horizon effect of implementing best treatment (both treatments the same here!)
NB_maxt_U <- Pop_total*NB_EVTCI_U
#NB from costlessly and instantly) Implementing best treatment
NB_E_maxt_trial_U <- Pop_during_trial*NB_EVTCI_U + Pop_after_trial*NB_EVTPI_U
# additional recoveries from trial (5 years)
NB_E_maxt_trial_U  - NB_maxt_U
# ICER compared to no trial
Cost_research_NETSCC/ (NB_E_maxt_trial_U  - NB_maxt_U)
### trial reports in 5 YEARS ####


### trial reports in 10 YEARS ####
Time_research <- 10
Pop_total <- (Incidence/-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*0))
# Incidence                                                     time end                time start
Pop_during_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_research) - exp(-D_rate*0))
Pop_after_trial <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*Time_research))
# full time horizon effect of implementing best treatment (both treatments the same here!)
NB_maxt_U <- Pop_total*NB_EVTCI_U
#NB from costlessly and instantly) Implementing best treatment
NB_E_maxt_trial_U <- Pop_during_trial*NB_EVTCI_U + Pop_after_trial*NB_EVTPI_U
# additional recoveries from trial 
NB_E_maxt_trial_U  - NB_maxt_U
# ICER compared to no trial
Cost_research_NETSCC/ (NB_E_maxt_trial_U  - NB_maxt_U)
### trial reports in 10 YEARS ####










####################################################################################################
# DELAY QALY ANALYSIS WITH INFORMED PRIOR #
####################################################################################################
# DELAY QALY ANALYSIS WITH INFORMED PRIOR #
####################################################################################################

# new populations (2 years)
Time_delay_informprior <- 2 
Pop_delay_I <- ((Incidence) /-D_rate) * (exp(-D_rate*Time_delay_informprior) - exp(-D_rate*(0)))
Pop_during_trial_I <-  ((Incidence) /-D_rate) * (exp(-D_rate*(Time_research+Time_delay_informprior)) - exp(-D_rate*(0+Time_delay_informprior)))
Pop_after_trial_I <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*(Time_research+Time_delay_informprior)))
Pop_before_and_during_trial_I <- Pop_delay_I + Pop_during_trial_I

# new populations (1 year)
Time_delay_informprior <- 1 
Pop_delay_I <- ((Incidence) /-D_rate) * (exp(-D_rate*Time_delay_informprior) - exp(-D_rate*(0)))
Pop_during_trial_I <-  ((Incidence) /-D_rate) * (exp(-D_rate*(Time_research+Time_delay_informprior)) - exp(-D_rate*(0+Time_delay_informprior)))
Pop_after_trial_I <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*(Time_research+Time_delay_informprior)))
Pop_before_and_during_trial_I <- Pop_delay_I + Pop_during_trial_I

# new populations (6 months)
Time_delay_informprior <- 0.5 
Pop_delay_I <- ((Incidence) /-D_rate) * (exp(-D_rate*Time_delay_informprior) - exp(-D_rate*(0)))
Pop_during_trial_I <-  ((Incidence) /-D_rate) * (exp(-D_rate*(Time_research+Time_delay_informprior)) - exp(-D_rate*(0+Time_delay_informprior)))
Pop_after_trial_I <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*(Time_research+Time_delay_informprior)))
Pop_before_and_during_trial_I <- Pop_delay_I + Pop_during_trial_I

# new populations (1 month)
Time_delay_informprior <- 0.0833333 
Pop_delay_I <- ((Incidence) /-D_rate) * (exp(-D_rate*Time_delay_informprior) - exp(-D_rate*(0)))
Pop_during_trial_I <-  ((Incidence) /-D_rate) * (exp(-D_rate*(Time_research+Time_delay_informprior)) - exp(-D_rate*(0+Time_delay_informprior)))
Pop_after_trial_I <-  ((Incidence) /-D_rate) * (exp(-D_rate*Time_info) - exp(-D_rate*(Time_research+Time_delay_informprior)))
Pop_before_and_during_trial_I <- Pop_delay_I + Pop_during_trial_I

# each time point cost and QALYs to NHS
#######################################
# reject proposal
NB_maxt_U
0                    # does not cost NETSCC anything

# 2 years
twoyr_E_max_action_I_NHS #- NB_maxt_U
twoyr_E_disc_cost_trial_I

# 1 year
oneyr_E_max_action_I_NHS #- NB_maxt_U
oneyr_E_disc_cost_trial_I

# 6 months
sixm_E_max_action_I_NHS #- NB_maxt_U
sixm_E_disc_cost_trial_I

# 1 month
onem_E_max_action_I_NHS #- NB_maxt_U
onem_E_disc_cost_trial_I

# fund trial with no delay
NB_E_maxt_trial_U
Cost_research_NETSCC




# full ICER analysis (fund with no delay dominated by 1 month and 6 month delay)
# 2 years vs reject
( twoyr_E_disc_cost_trial_I )/(twoyr_E_max_action_I_NHS - NB_maxt_U)

# 1 year vs 2 years
( oneyr_E_disc_cost_trial_I - twoyr_E_disc_cost_trial_I )/(oneyr_E_max_action_I_NHS - twoyr_E_max_action_I_NHS)

# 1 year vs reject
( oneyr_E_disc_cost_trial_I )/(oneyr_E_max_action_I_NHS - NB_maxt_U)

# 6 months vs 1 year
(sixm_E_disc_cost_trial_I - oneyr_E_disc_cost_trial_I)/(sixm_E_max_action_I_NHS - oneyr_E_max_action_I_NHS)

# 6 months vs reject
(sixm_E_disc_cost_trial_I )/(sixm_E_max_action_I_NHS - NB_maxt_U)

# 1 month vs 6 months
(onem_E_disc_cost_trial_I - sixm_E_disc_cost_trial_I )/(onem_E_max_action_I_NHS -  sixm_E_max_action_I_NHS)

# 1 month vs reject
(onem_E_disc_cost_trial_I  )/(onem_E_max_action_I_NHS -  NB_maxt_U)











