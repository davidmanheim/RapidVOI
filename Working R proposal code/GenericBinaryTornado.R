####################################
# Generic binary tornado
#################################
# use generic binary MCD model


# Proposal 4 WRAPPER MODEL (to allow SA of epi inputs as well as others)
#################
# as epi inputs are to be varied - need to wrap these into the model

P4_GenericBinaryOutcome.v1 <- function(MCouter, Benefit, P_t0_U, sigma_t1_U,n_t0 ,n_t1 ,
                                       MCD_t , Incidence, 
                                       Time_info ,Utilisation_t,D_rate ,Time_research ,
                                       Cost_research_funder){

  #   epi model
  ############
  library(fdrtool) # to get half normal
  Odds_t0_U <- P_t0_U / (1 - P_t0_U)
  LO_t0_U <- log(Odds_t0_U)
  # halfnormal simulations on LOR 
  LOR_t1_U <- rhalfnorm(MCouter, theta=sd2theta(sigma_t1_U))
  #LOR_t1_U <- rnorm(MCouter, 0, sigma_t1_U)
  LO_t1_U <- LO_t0_U + LOR_t1_U 
  Odds_t1_U <- exp(LO_t1_U)
  P_t1_U <- Odds_t1_U / (Odds_t1_U + 1) 
  P_t_U <- matrix(c(rep(P_t0_U,MCouter), P_t1_U), ncol = 2)

  # Utilisation matrix
  ####################
  a <- P_t1_U*n_t1 # number of events in the treatment arm
  b <- (1 - P_t1_U)*n_t1 # number of non-events in the treatment arm
  c <- rep(P_t0_U*n_t0, MCouter) # number of events in the control arm
  d <- rep((1 - P_t0_U)*n_t0, MCouter) # number of non-events in the control arm
  SE_U <- sqrt(1/a + 1/b + 1/c + 1/d) # standard error for each simulation

  UtilMatrix <- UtilMatrix.fn(cbind(rep(0, length(LOR_t1_U)), LOR_t1_U), SE_U*qnorm(0.975), Utilisation_t)

  # VOI model
  ############
  GenericBinaryOutcome.v1(P_t_U ,Benefit,MCD_t , Incidence, 
                        Time_info ,Utilisation_t,D_rate ,Time_research ,
                        Cost_research_funder,UtilMatrix)

}
  
# P4 BASE CASE INPUTS
P4_Cost_research_funder =  882177
P4_Incidence = 259150
D_rate = 0.035
P4_Time_research = 3
P4_Time_info  = 15
P4_Benefit <- TRUE
P4_MCD_t <- c(-0.05)
P4_Utilisation_t <- c(1,0)
  
# epi inputs
MCouter <- 40000 # NOTE not bad convergence of EVTPI at MCouter = 40000
P4_P_t0_U <- 0.30
P4_sigma_t1_U <- 0.5
P4_n_t0 <- 227
P4_n_t1 <- 453

# base case results from wrapper model
P4_baseResult <- P4_GenericBinaryOutcome.v1(MCouter, P4_Benefit,P4_P_t0_U, P4_sigma_t1_U,P4_n_t0 ,P4_n_t1, 
                           P4_MCD_t , P4_Incidence, 
                           P4_Time_info ,P4_Utilisation_t,D_rate ,P4_Time_research ,
                           P4_Cost_research_funder)$Value_of_maxt_perfect_info_imp
P4_baseResult


# create inputs list
####################
baseInputs <- list(MCouter, P4_Benefit,P4_P_t0_U, P4_sigma_t1_U,P4_n_t0 ,P4_n_t1, 
                   P4_MCD_t , P4_Incidence, 
                   P4_Time_info ,P4_Utilisation_t,D_rate ,P4_Time_research ,
                   P4_Cost_research_funder)


# create results vectors
################
# vecor of base case inputs in order in which they enter the main model
#baseInputs <- c(90000,TRUE,0.37 ,0,0.5,0,0.5,0.07,0, 1563,15,1 ,0 ,0 ,0.035 ,2, 6, 0.5, 601480 ,150000 , 2500000 ,450000)
resultsVectorUp <- rep(NA, length(baseInputs)) 
# PilotStudyVOI2 defined above

# +20%
for (i in 3:length(baseInputs)){   # start at the third input do not use first two in SA (MCouter, benefit)- they will be left as NAs
  arguments <- baseInputs
  arguments[[i]] <- baseInputs[[i]]*1.2
  resultsVectorUp[i] <- do.call(P4_GenericBinaryOutcome.v1 , as.list(arguments))$Value_of_maxt_perfect_info_imp
}

# -20%
resultsVectorDown <- rep(NA, length(baseInputs)) 
for (i in 3:length(baseInputs)){   # start at the third input do not use first two in SA- they will be left as NAs
  arguments <- baseInputs
  arguments[[i]] <- baseInputs[[i]]*0.8
  resultsVectorDown[i] <- do.call(P4_GenericBinaryOutcome.v1 , as.list(arguments))$Value_of_maxt_perfect_info_imp
}

# put together in a tornado



# model code START # 
# example from: https://stackoverflow.com/questions/37059281/tornado-plot-in-r
data <- matrix(c(-0.02,0.02,-0.01,0.01,-0.03,0.02,-0.01,0.04), ncol = 4)
rownames(data) <- c('+10%','-10%')                       # Amount of change in variables
colnames(data) <- c('V_bar', 'alpha', 'rho','xi')        # Names of variables
x <- seq(-0.04,0.04, length=10)        

barplot(data[1,], horiz = T, las=1, xlim = c(-0.04,0.04), xaxt='n', ylab = '',
        beside=T, col=c('springgreen'))
barplot(data[2,], horiz = T, las=1, xlim = c(-0.04,0.04), xaxt='n', ylab = '',
        beside=T, col=c('indianred2'), add = TRUE)
axis(1, at=pretty(x),  lab=paste0(pretty(x) * 100," %"), las=TRUE)
# model code END # 




# create vector holding results
# centre results on zero for now (subtract the base result ) will add this back in on the x-axis

P4_data <- rbind(resultsVectorUp - P4_baseResult, resultsVectorDown - P4_baseResult)
# remove irrelevant columns
P4_data <- P4_data[,c(-1, -2, -5, -6,-10, -13)]

rownames(P4_data) <- c('+20%','-20%')                       # Amount of change in variables
colnames(P4_data) <- c("Baseline Probability", "Relative effect SD", 
                       "MCD" , "Incidence", 
                       "Time information" ,"Discount rate" ,"Duration of research")        # Names of variables


# create x axis sequence
P4_x_min <-min(P4_data, na.rm = TRUE)
P4_x_max <- max(P4_data, na.rm = TRUE)
P4_x <- seq(P4_x_min, P4_x_max, length=10) 

# adjust margins so that the left hand labels are shown (otherwise they would be cut off)
par(mar=c(6,10,2,4)) # par(mar=c(bottom,left,top,right))

# las = 1 means labels parallel (component of par)
# The option axes=FALSE suppresses both x and y axes. xaxt="n" and yaxt="n" suppress the x and y axis respectively
barplot(P4_data[1,], horiz = T, las=1, xlim = c(P4_x_min,P4_x_max), xaxt='n', ylab = '',
        beside=T, col=c('springgreen'))
barplot(P4_data[2,], horiz = T, las=1, xlim = c(P4_x_min,P4_x_max), xaxt='n', ylab = '',
        beside=T, col=c('indianred2'), add = TRUE)
# axis first argument is side: which side of the plot the axis is to be drawn on. The axis is placed as follows: 1=below, 2=left, 3=above and 4=right.
axis(1, at=pretty(P4_x),  lab=pretty(P4_x + P4_baseResult), las=TRUE)
title(main = "Tordado diagram: proposal 4", xlab = "Additional deaths in prefered place")


#################################
# use tornado analysis for best worst case analysis

# 10% +/-


















#### old code ################




















############################################
# PERCENTAGE CHANGE from base case TORNADO
############################################


NBbase <- do.call(PilotStudyVOI2, as.list(baseInputs))


# turn into one row matrixies 
Up <- matrix(resultsVectorUp/NBbase -1, ncol = length(baseInputs))
Down <- matrix(resultsVectorDown/NBbase -1, ncol = length(baseInputs))

data <- rbind(Up, Down)


#1,MCouter = 20000       # SKIP! i.e.do not vary in SA
#2 Benefit <- TRUE    # SKIP 
#3 ,P_t0_U <- 0.37    # bounded by 0 and 1 (should be done in program somewhere)
#4,mu_t1_U <- 0          # 20% of zero = 0,  could change to +20% of ~N(0, 0.5) something like -0.1
#5 ,sigma_t1_U <- 0.5 # must be > 0
#6 ,mu_t2_U <- 0      # 20% of zero = 0,  could change to +20% of ~N(0, 0.5) something like -0.1
#7 ,sigma_t2_U <- 0.5 # must be > 0
#8,MCD_t1 = 0.07         
#9,MCD_t2 = 0            # 20% of zero = 0, just go +- 0.1
#10 , Incidence = 1563
#11 ,Time_info = 15
#12,Utilisation_t0 = 1 # will not affect info value, MUST sum to 1! and cannot >1 <0
#13,Utilisation_t1 = 0 # will not affect info value, MUST sum to 1! and cannot >1 <0
#14,Utilisation_t2 = 0 # will not affect info value, MUST sum to 1! and cannot >1 <0
#15,D_rate =  0.035 
#16 ,Time_research_pilot = 2 # must be >0 (multipliation will handle this)
#17 ,Time_research_definitive = 6  # must be >0 (multipliation will handle this)
#18 ,Probability_of_definitive_research = 0.5 # must be >0 and <1 (multipliation will handle the first)
#19 ,Cost_research_pilot_NETSCC =   601480 # will enter calculation of NHE in this model
#20 ,Cost_research_pilot_NHS  = 150000  # will enter calculation of NHE in this model
#21 ,Cost_research_definitive_NETSCC =  2500000 # will enter calculation of NHE in this model
#22 ,Cost_research_definitive_NHS  = 450000  # will enter calculation of NHE in this model

# MC error has entered even where there is no chance of an input changing the
# result

# asign

# asign zeros to costs of research (they do not enter into this analysis)
data[,19:22] <- NA


# example from: https://stackoverflow.com/questions/37059281/tornado-plot-in-r
#data <- matrix(c(-0.02,0.02,-0.01,0.01,-0.03,0.02,-0.01,0.04), ncol = 4)
rownames(data) <- c('+20%','-20%')                       # Amount of change in variables
colnames(data) <- c('' ,'','P_t0_U' ,'mu_t1_U' ,'sigma_t1_U' ,'mu_t2_U' ,'sigma_t2_U ','MCD_t1' ,'MCD_t2 ', 'Incidence', 
                    'Time_info' ,'Utilisation_t0' ,'Utilisation_t1' ,'Utilisation_t2' ,'D_rate' ,'Time_research_pilot' ,'Time_research_definitive' ,
                    'Probability_of_definitive_research' ,'' ,''  ,
                    '' ,'')

#colnames(data) <- c('V_bar', 'alpha', 'rho','xi')        # Names of variables
x <- seq(-0.4,0.4, length=10)                          # For plotting '%' on x-axis

#barplot(data, horiz = T, las=1, xlim = c(-0.04,0.04), xaxt='n', ylab = '',
#        beside=T, col=c('springgreen','indianred2'))
#axis(1, at=pretty(x),  lab=paste0(pretty(x) * 100," %"), las=TRUE)


# NB need a different coluour for increasing and decreasing as red and green will be 
# confused with cost effectiveness
barplot(data[1,], horiz = T, las=1, xlim = c(-0.4,0.4), xaxt='n', ylab = '',
        beside=T, col=c('springgreen'))
barplot(data[2,], horiz = T, las=1, xlim = c(-0.4,0.4), xaxt='n', ylab = '',
        beside=T, col=c('indianred2'), add = TRUE)
axis(1, at=pretty(x),  lab=paste0(pretty(x) * 100," %"), las=TRUE)





############################################
# ABSOLUTE CHANGE from base case TORNADO
############################################
# include ICERs on the graph? 



