#############################################
## P1 Tornado
#########################################


#############################################
## P2 Tornado
#########################################


#############################################
## P3 Tornado
#########################################



#############################################
## P4 Tornado and SA
#########################################


# P4 WRAPPER MODEL (to allow SA of epi inputs as well as others)
#################
# as epi inputs are to be varied - need to wrap these into the model
# need to edit inputs to this wrapper
# require  : UtilMatrix.fn 


P4_QALYwrap.v1 <- function(MCouter, P_t0_U, sigma_t1_U,n_t0 ,n_t1, INB_Event,k,C_t, MCD_t , Incidence, 
                           Time_info ,Utilisation_t,D_rate ,Time_research ,
                           Cost_research_funder,Cost_research_system, UtilMatrix = NA, Time_delay = 0){
  
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
  
  UtilMatrix <- NA # UtilMatrix.fn(cbind(rep(0, length(LOR_t1_U)), LOR_t1_U), SE_U*qnorm(0.975), Utilisation_t)
  
  # VOI model
  ############
  # edit inputs** pre loaded QALY model
  GenericBinaryQALY.v1(P_t_U ,INB_Event,k,C_t, MCD_t , Incidence, 
                          Time_info ,Utilisation_t,D_rate ,Time_research ,
                          Cost_research_funder,Cost_research_system, UtilMatrix, Time_delay)
    
}

# P4 BASE CASE INPUTS
P4_Cost_research_funder =  882177
P4_Incidence = 259150
D_rate = 0.035
P4_Time_research = 3
P4_Time_info  = 15
P4_MCD_t <- c(0)
P4_Utilisation_t <- c(1,0)
MCouter <- 900000 # NOTE not bad convergence of EVTPI at MCouter = 40000
P4_P_t0_U <- 0.30
P4_sigma_t1_U <- 0.5
P4_n_t0 <- 227
P4_n_t1 <- 453

P4_INB_Event <- (3000 - 2107.5)/15000 # INB event always assumes a k value
P4_k <- 15000
P4_C_t <- c(0, 4104/453)
P4_Cost_research_system <- 4104
P4_UtilMatrix = NA
P4_Time_delay = 0


# create inputs list
####################
baseInputs <- list(MCouter, P4_P_t0_U, P4_sigma_t1_U,P4_n_t0 ,P4_n_t1, P4_INB_Event,P4_k,P4_C_t, P4_MCD_t , P4_Incidence, 
                   P4_Time_info ,P4_Utilisation_t,D_rate ,P4_Time_research ,
                   P4_Cost_research_funder,P4_Cost_research_system, P4_UtilMatrix, P4_Time_delay)




# base case results from wrapper model
P4_baseResult <- do.call(P4_QALYwrap.v1 , as.list(baseInputs))$QALY_research_vs_system_spend_maxt_perfect_info_imp
P4_baseResult





# create results vectors
################
# vecor of base case inputs in order in which they enter the main model
#baseInputs <- c(90000,TRUE,0.37 ,0,0.5,0,0.5,0.07,0, 1563,15,1 ,0 ,0 ,0.035 ,2, 6, 0.5, 601480 ,150000 , 2500000 ,450000)
resultsVectorUp <- rep(NA, length(baseInputs)) 
# PilotStudyVOI2 defined above

# +20%
for (i in 2:length(baseInputs)){   # start at the second input, do not vary (MCouter, benefit)- they will be left as NAs
  arguments <- baseInputs
  arguments[[i]] <- baseInputs[[i]]*1.2
  resultsVectorUp[i] <- do.call(P4_QALYwrap.v1 , as.list(arguments))$QALY_research_vs_system_spend_maxt_perfect_info_imp
}

# -20%
resultsVectorDown <- rep(NA, length(baseInputs)) 
for (i in 2:length(baseInputs)){   # start at the third input do not use first two in SA- they will be left as NAs
  arguments <- baseInputs
  arguments[[i]] <- baseInputs[[i]]*0.8
  resultsVectorDown[i] <- do.call(P4_QALYwrap.v1 , as.list(arguments))$QALY_research_vs_system_spend_maxt_perfect_info_imp
}

# put together in a tornado

# example code START # 
# example from: https://stackoverflow.com/questions/37059281/tornado-plot-in-r
#data <- matrix(c(-0.02,0.02,-0.01,0.01,-0.03,0.02,-0.01,0.04), ncol = 4)
#rownames(data) <- c('+10%','-10%')                       # Amount of change in variables
#colnames(data) <- c('V_bar', 'alpha', 'rho','xi')        # Names of variables
#x <- seq(-0.04,0.04, length=10)        

#barplot(data[1,], horiz = T, las=1, xlim = c(-0.04,0.04), xaxt='n', ylab = '',
#        beside=T, col=c('springgreen'))
#barplot(data[2,], horiz = T, las=1, xlim = c(-0.04,0.04), xaxt='n', ylab = '',
#        beside=T, col=c('indianred2'), add = TRUE)
#axis(1, at=pretty(x),  lab=paste0(pretty(x) * 100," %"), las=TRUE)
# example code END # 




# create vector holding results
# centre results on zero for now (subtract the base result ) will add this back in on the x-axis

P4_data <- rbind(resultsVectorUp - P4_baseResult, resultsVectorDown - P4_baseResult)
# remove irrelevant columns
P4_data <- P4_data[,c(-1, -4, -5, -7,-9 ,  -12, -13, -17, -18) ]
                  
              
# -1                                -4        -5                    -7              -9         
#MCouter, P4_P_t0_U, P4_sigma_t1_U,P4_n_t0 ,P4_n_t1, P4_INB_Event, P4_k, P4_C_t, P4_MCD_t , P4_Incidence,  
#                   -12            -13           
#P4_Time_info ,P4_Utilisation_t, D_rate ,P4_Time_research ,
#                                                    -17           -18
#P4_Cost_research_funder,P4_Cost_research_system, P4_UtilMatrix, P4_Time_delay

# remaining 
# -      
#P4_P_t0_U, P4_sigma_t1_U,P4_INB_Event, P4_k, P4_C_t,P4_Incidence,  
#                                       
#P4_Time_info ,D_rate ,P4_Time_research ,
#                                                
#P4_Cost_research_funder,P4_Cost_research_system, 

rownames(P4_data) <- c('+20%','-20%')                       # Amount of change in variables
colnames(P4_data) <- c("Baseline Probability", "Relative effect SD", "QALYs from event",
                       "Cost of booklet", "Incidence", 
                       "Time information"  ,"Duration of research",
                       "Cost to NETSCC", "Cost to NHS")        # Names of variables

# subtract the +20% column from the -20% column for each column
# take the absolute value
differences <- abs( P4_data[1,] - P4_data[2,] )

order(differences) # lowest to highest column numbers - last entries will be at top of tornado

# Reorder by columns number
P4_data <- P4_data[,order(differences)]


# create x axis sequence
P4_x_min <-min(P4_data, na.rm = TRUE)
P4_x_max <- max(P4_data, na.rm = TRUE)
P4_x <- seq(P4_x_min, P4_x_max, length=10) 

# adjust margins so that the left hand labels are shown (otherwise they would be cut off)
par(mar=c(6,10,2,4)) # par(mar=c(bottom,left,top,right))

# las = 1 means labels parallel (component of par)
# The option axes=FALSE suppresses both x and y axes. xaxt="n" and yaxt="n" suppress the x and y axis respectively
barplot(P4_data[1,], horiz = T, las=1, xlim = c(P4_x_min,P4_x_max), xaxt='n', ylab = '',
        beside=T, col=colours()[1])
barplot(P4_data[2,], horiz = T, las=1, xlim = c(P4_x_min,P4_x_max), xaxt='n', ylab = '',
        beside=T, col=colours()[6], add = TRUE)
# axis first argument is side: which side of the plot the axis is to be drawn on. The axis is placed as follows: 1=below, 2=left, 3=above and 4=right.
axis(1, at=pretty(P4_x),  lab=pretty(P4_x )+ round(P4_baseResult, digits = 1), las=TRUE)
title(main = "", xlab = "QALYs per £15,000 NETSCC spend")

# note added +1 to the axis to make them proper!
#axis(1, at=pretty(P4_x),  lab=pretty(P4_x + P4_baseResult), las=TRUE)


#################################
# use tornado diagram for best worst case analysis
#################################
# 10% +/-

Increase <- 1.1
Decrease <- 0.9

BestbaseInputs <- list(MCouter, P4_P_t0_U*Decrease, 
                       P4_sigma_t1_U*Decrease, P4_n_t0 ,P4_n_t1, 
                       P4_INB_Event*Decrease,
                       P4_k,
                       P4_C_t*Increase, P4_MCD_t , 
                       P4_Incidence*Increase,
                       P4_Time_info*Increase ,P4_Utilisation_t,
                       D_rate ,
                       P4_Time_research*Decrease ,
                       P4_Cost_research_funder*Decrease,
                       P4_Cost_research_system*Decrease, P4_UtilMatrix, 
                       P4_Time_delay)

do.call(P4_QALYwrap.v1 , as.list(BestbaseInputs))$QALY_research_vs_system_spend_maxt_perfect_info_imp


WorstbaseInputs <- list(MCouter, P4_P_t0_U*Increase, 
                       P4_sigma_t1_U*Increase, P4_n_t0 ,P4_n_t1, 
                       P4_INB_Event*Increase,
                       P4_k,
                       P4_C_t*Decrease, P4_MCD_t , 
                       P4_Incidence*Decrease,
                       P4_Time_info*Decrease ,P4_Utilisation_t,
                       D_rate ,
                       P4_Time_research*Increase ,
                       P4_Cost_research_funder*Increase,
                       P4_Cost_research_system*Increase, P4_UtilMatrix, 
                       P4_Time_delay)

do.call(P4_QALYwrap.v1 , as.list(WorstbaseInputs))$QALY_research_vs_system_spend_maxt_perfect_info_imp



