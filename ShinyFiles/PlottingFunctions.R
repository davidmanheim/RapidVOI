#
# uses functions from EpiInputFunctions.R
# probCI() exactVectorNormal() so far
# 
# wrapper functions which take inputs and create approprate functions
# one for:
# 1) baseline probability
# 2) distribution of relative effect inputs
# 3) probability for each intervention
# 4) all probabilities with current evidence
# 5) .. continuous and survival outcomes

# BASELINE PROBABILITY plotting
###########################

# input: all inputs relevant to plotting the baseline probability
# output: plot (if no input: outputs an appropriate blank plot)

# takes all inputs and decides which function to run
# usage: output$baselinePlot <- renderPlot({ baselinePlot(input$...) })

# improvements: take away y axis ticks and label? 
# improve resolution of graph 

# test data
#baselineProbExpression <- "natural" #  # "events" #   NA # if nothing has been entered yet
#sliderBaselineProb <- c(0.80, 0.90) # if "natural" expression (assume 95% CI)
#eventsBaselineProb <- 10 # if "events"
#atRiskBaselineProb <- 30 # if "events"



baselinePlot <- function(baselineProbExpression, 
                         sliderBaselineProb,
                         eventsBaselineProb,
                         atRiskBaselineProb){
  
  # if NA, quit function and return blank plot - contains no axis etc..
  if(is.na(baselineProbExpression)){
    return(
      plot(1, type="n",main = "Baseline Probability of outcome", xlab="Probability", ylab="Density", xlim=c(0, 1), ylim=c(0, 10))
    )
  }
  
  # slider input
  if(baselineProbExpression == "natural"){
    
    
    
    if(sliderBaselineProb[1] == sliderBaselineProb[2]){
      # code for baseline estimates with NO uncertainty
      
      plot(1, type="n",main = "Baseline Probability of outcome", xlab="Probability", ylab="Density", xlim=c(0, 1), ylim=c(0, 15))
      lines(x = c(sliderBaselineProb[1], sliderBaselineProb[1]), y = c(0, 15), col= "firebrick", lwd = 2)
      
    } else {
      # code for baseline estimates with WITH uncertainty
      
      # from EpiInputFunctions.R
      probCIOutput <- probCI(sliderBaselineProb[1], sliderBaselineProb[2])
      mu <- probCIOutput$mu # mean on LO scale
      sigma <- probCIOutput$sigma # sigma on LO scale
      mu_prob <- probCIOutput$mu_prob # natural mean
      
      # from EpiInputFunctions.R - takes perfect samples from theoretical density
      LO_vector <- exactVectorNormal(mu, sigma)
      Odds_vector <- exp(LO_vector)
      Prob_vector <- Odds_vector/(1 + Odds_vector)
      
      plot(density(Prob_vector),main = "Baseline Probability of outcome", xlab="Probability", ylab="Density", xlim=c(0, 1), col= "firebrick", lwd = 2)
      points(x = mu_prob, y = 0,col= "firebrick" )
    }
    
  }
  
  # events input
  if(baselineProbExpression == "events"){
    
    # beta parameter for beta distribution
    nonEventsBaselineProb <- atRiskBaselineProb - eventsBaselineProb
    x_axis <- seq(0, 1, length.out = 100)
    y_axis <- dbeta(x_axis, eventsBaselineProb, nonEventsBaselineProb)
    
    plot(x = x_axis, y = y_axis ,type = "l", main = "Baseline Probability of outcome", xlab="Probability", ylab="Density", xlim=c(0, 1), col= "firebrick", lwd = 2)

  }
  
}

# test function - works well
#baselinePlot(baselineProbExpression = NA, # "events", # "natural", #
#             sliderBaselineProb = c(0.01, 0.99),
#             eventsBaselineProb = 1,
#             atRiskBaselineProb = 2)
