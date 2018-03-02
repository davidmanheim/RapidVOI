
####
# for proposal 5

nsim <- 10000

###############
# new treatment: t1 (only treat for 12 months)
###############

# 60% chance of a successful trial
# in this case that mean being above the 2year non inferiority cutoff
cutoff <- -log(1-0.431)/2 # 60% ABOVE this value

# the hypothesised effect size for the new treatment
# 41.3% PFS is the non inferiority cutoff
# 51% is the current standard of care
# we expect the new treatment to be around 44%
mu_target <- -log(1-0.44)/2 # mean of new distribution

# assume normal distribution of log hazards
log_cutoff <- log(cutoff)
log_mu_target <- log(mu_target)

sd_to_cutoff <- qnorm(.60) # number of standard deviations from mean
# so that 60% of distribution lies below this new value
# (for 60% to lie above this value take the negative of this)

# - sd_to_cutoff as 60% above
# sigma value which corresponds to mean at target mean with 60% above cutoff value
sigma_target <- (cutoff - mu_target)/(-sd_to_cutoff)

# check
# 60% ABOVE so 1-normal function
1 - pnorm(cutoff, mu_target, sigma_target)

# simulate assuming normality on log scale then scale back up
plot(density(rnorm(nsim, log_mu_target, sigma_target)))
# natural scale
plot(density(exp(rnorm(nsim, log_mu_target, sigma_target))))
mean(exp(rnorm(nsim, log_mu_target, sigma_target)))
summary(exp(rnorm(nsim, log_mu_target, sigma_target)))
# no chance of beating current treatment (good!)

# yearly transition RATE from pre prog (stable) to post prog (in new treatment)

rate.SP<- exp(rnorm(nsim, log_mu_target, sigma_target))

# yearly transition PROBABILITY from pre prog (stable) to post prog (in new treatment)
p.SP <- 1 - exp(-rate.SP)
plot(density(p.SP))

###############################################################################################

n.t <- 60                                    # number of cycles
v.n <- c("stable", "progressed","dead")  # state names
n.s<- length(v.n)                            # number of states

# stable = pre prog

p <- Sys.time()
set.seed(1)
p.SD <- 0.01 #made up  #rbeta(nsim, 20,988)                 # probability from stable to dead (leave as is for now)
p.SP # defined above                # probability from stable to progressed
# probability from progressed to dead
rate.PD <- -log(1-0.5)/(12.9/12) # from proposal document: mean time from progressed to dead = 12.9 months
p.PD <- 1 - exp(-rate.PD)
# original value: p.PD <- rbeta(nsim, 100,900)                

# cost of being in stable state: short term survival (<1yr) £300 per month, longer term: £100 per month
# just take midpoint: £200 per month = 200*12
c.S  <- 200*12          
c.P  <- 200*12                 # cost of being in progressed state - just copy for stable
c.D  <- 0

u.S  <- 0.79065              # utility of being in stable state (pre prog) - from NICE
u.P  <- 0.71655              # utility of being in the progressed state - from NICE
u.D  <- 0

S <- P <- D <- matrix(0, ncol=n.t, nrow =  nsim,
                      dimnames = list(1:nsim, 1:n.t))  #initialize State matrices

S[,1] <- 1  # assign every proportion across the simulations to the stable state
for (t in 2:n.t)
{
  S[, t] <- S[ ,t - 1] * (1 - p.SD - p.SP)                       # calulate the prop of cohort in S
  P[, t] <- P[ ,t - 1] * (1 - p.PD) + S[ ,t -1] * p.SP           # calulate the prop of cohort in P
  D[, t] <- D[ ,t - 1] + S[ ,t - 1] * p.SD + P[ ,t - 1] * p.PD   # calulate the prop of cohort in D
}

TE_t1 <- rowSums(u.S * S + u.P * P + u.D * D)    # total QALYs for all simulations
# drug costs for first 12 months pre progression
C.S_drug <- rep(0, n.t)
C.S_drug[1] <- 12*6000
  
TC_t1 <- rowSums(c.S * S  + c.P * P + c.D * D)    # total cost for all simulations
# need to add 12 month treatment costs

mean(TE_t1)
mean(TC_t1)
Sys.time() - p



###############
# old treatment: t0 (treat to progression)
###############


n.t <- 60                                    # number of cycles
v.n <- c("stable", "progressed","dead")  # state names
n.s<- length(v.n)                            # number of states

# stable = pre prog

p <- Sys.time()
set.seed(1)
p.SD <- 0.01 #made up  # probability from stable to dead (leave as is for now)
rate.SP <- -log(1-0.51)/2 # from sample size calculation
p.SP <- 1 - exp(-rate.SP) # defined above    # probability from stable to progressed
# probability from progressed to dead
rate.PD <- -log(1-0.5)/(12.9/12) # from proposal document: mean time from progressed to dead = 12.9 months
p.PD <- 1 - exp(-rate.PD)
# original value: p.PD <- rbeta(nsim, 100,900)                

# cost of being in stable state: short term survival (<1yr) £300 per month, longer term: £100 per month
# just take midpoint: £200 per month = 200*12
# plus yearly drug costs
c.S  <- 200*12 + 6000*12
c.P  <- 200*12                 # cost of being in progressed state - just copy for stable
c.D  <- 0

u.S  <- 0.79065              # utility of being in stable state (pre prog) - from NICE
u.P  <- 0.71655              # utility of being in the progressed state - from NICE
u.D  <- 0

S <- P <- D <- matrix(0, ncol=n.t, nrow =  nsim,
                      dimnames = list(1:nsim, 1:n.t))  #initialize State matrices

S[,1] <- 1  # assign every proportion across the simulations to the stable state
for (t in 2:n.t)
{
  S[, t] <- S[ ,t - 1] * (1 - p.SD - p.SP)                       # calulate the prop of cohort in S
  P[, t] <- P[ ,t - 1] * (1 - p.PD) + S[ ,t -1] * p.SP           # calulate the prop of cohort in P
  D[, t] <- D[ ,t - 1] + S[ ,t - 1] * p.SD + P[ ,t - 1] * p.PD   # calulate the prop of cohort in D
}

TE_t2 <- rowSums(u.S * S + u.P * P + u.D * D)    # total QALYs for all simulations
TC_t2 <- rowSums(c.S * S + c.P * P + c.D * D)    # total cost for all simulations
# need to add 12 month treatment costs

mean(TE_t2)
mean(TC_t2)


