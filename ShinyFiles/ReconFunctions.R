###############
# Reconsider evidence function 

# must return everything that the normal analysis does + the delay analysis
# just add into the regular functions with an if statement!
#if(reconsider == "Yes"){
#  
#  # insert reconsideration of evidence function
#  
#}




############################################
# BINARY
############################################

# inputs
# test data (the same as) P6
numberOfTreatments =2
P_t1 =0.95
INBBinaryEvent = 9.5
mu_t2=0
variance_t2=0.25
dist_t2="norm"
direction_t2= NA
mu_t3=NA
variance_t3=NA
dist_t3=NA
direction_t3=NA
mu_t4=NA
variance_t4=NA
dist_t4=NA
direction_t4=NA
nameOf_t1="continual treatment"
nameOf_t2="withdrawal"
nameOf_t3=NA
nameOf_t4=NA
tCostsDependOnEvent = "Yes"
cost_t1 = 1000
cost_t2 = 91000
cost_t3 = NA
cost_t4 = NA
costEvent_t1 = 3100000
costEvent_t2 = 3100000
costEvent_t3 = NA
costEvent_t4 = NA
costNotEvent_t1 = 7300000
costNotEvent_t2= 4000000
costNotEvent_t3=NA
costNotEvent_t4 = NA
typeOfOutcome="benefit"
incidence=26.26
timeInformation=10
discountRate=3.5
durationOfResearch= 4
costResearchFunder=855403
MCD_t2=0
MCD_t3=NA
MCD_t4=NA
utilisation_t1=100
utilisation_t2=0
utilisation_t3=NA
utilisation_t4=NA
costHealthSystem = 9899380
k = 13000
currencySymbol = "£"
# additional inputs for reconsideration of evidence
reconsider = "Yes" #  "No"
MCsimsInner = 10 
MCsimsOuter = 10




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

