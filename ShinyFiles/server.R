#######################
# potential server side bugs: 
# if the app is open on a number of windows on the browser (i.e. if it has not been "stopped" in a while)
# then Binary/Survival Feas NetHealth models crash. Some problem with trying to find popDurationResearch 
# 
#


# must load the required functions! SupplementaryFunctions.R

library(shiny)

############################
# load up required functions

library(scales) # required to format tables in renderTable
library(fdrtool) # required for halfnormal simulations
library(MASS) # for use in EpiInputFunctions.R to fit beta distributions to unknown probabilities

#W:/teehta/David G/ShinyApps/RShinyVOI/ShinyFiles
#absolute paths for use in desktop development
source("W:/teehta/David G/ShinyApps/RapidVOI/RapidVOIv2/ShinyFiles/BinaryOutcomeFunction.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RapidVOI/RapidVOIv2/ShinyFiles/BinaryQALYFunction.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RapidVOI/RapidVOIv2/ShinyFiles/SupplementaryFunctions.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RapidVOI/RapidVOIv2/ShinyFiles/ContinuousOutcomeFunction.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RapidVOI/RapidVOIv2/ShinyFiles/ContinuousQALYFunction.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RapidVOI/RapidVOIv2/ShinyFiles/SurvivalOutcomeFunction.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RapidVOI/RapidVOIv2/ShinyFiles/SurvivalQALYFunction.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RapidVOI/RapidVOIv2/ShinyFiles/SupplementaryFunctionsFeas.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RapidVOI/RapidVOIv2/ShinyFiles/master.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RapidVOI/RapidVOIv2/ShinyFiles/ReconFunctions.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RapidVOI/RapidVOIv2/ShinyFiles/EpiInputFunctions.R", local = TRUE)
source("W:/teehta/David G/ShinyApps/RapidVOI/RapidVOIv2/ShinyFiles/PlottingFunctions.R", local = TRUE)


# relative paths for publishing in shinyapps.io
# source("BinaryOutcomeFunction.R", local = TRUE)
# source("BinaryQALYFunction.R", local = TRUE)
# source("SupplementaryFunctions.R", local = TRUE)
# source("ContinuousOutcomeFunction.R", local = TRUE)
# source("ContinuousQALYFunction.R", local = TRUE)
# source("SurvivalOutcomeFunction.R", local = TRUE)
# source("SurvivalQALYFunction.R", local = TRUE)
# source("SupplementaryFunctionsFeas.R", local = TRUE)
# source("master.R", local = TRUE)
# source("ReconFunctions.R", local = TRUE)
# source("EpiInputFunctions.R", local = TRUE)
# source("PlottingFunctions.R")



shinyServer(function(input, output) {
  
  #############################
  # fixes to rename variables 
  ###########################
  
  # reproduce the typeOfOutcome input from previous iteration of the app
  # previously took values of "benefit","harm", "netHealth"
  newTypeOfOutcome <- reactive(
    if(input$outcomeExpression == "netHealth"){
      "netHealth"
    } else {
      input$benefitOrHarm # takes values "benefit" or "harm"
    }
  )
  
  # note MCsims directly changed in master() inputs
  
  newNameOfOutcome <- reactive(
    if(input$outcomeExpression == "netHealth"){
      "QALY"
    } else {
      input$nameOfOutcome # 
    }
  )
  
  
  ##############################
  # Update plots showing user inputs
  ##############################
  # plots show what user has inputted
  
  
  

  ##########################
  # Run VOI analysis with ACTION BUTTON 
  ##########################
  
  # create "managed state variable" - a list which can be repeatedly overwritten by user
  VOIResults <- reactiveValues()
  
  observeEvent(input$run, {
    
      # a list which holds the results of the appropriate analysis
      resultsHolder <- reactive({
        # the master function takes all inputs, runs the appropriate model and returns a list of the results
        master(
               # type of analysis 
               typeOfEndpoint = input$typeOfEndpoint,
               typeOfOutcome= newTypeOfOutcome(), # change
               tCostsDependOnEvent= input$tCostsDependOnEvent,
               numberOfTreatments= input$numberOfTreatments,
               typeOfResearch= input$typeOfResearch,
               reconsider = input$reconsider,
               MCsims= 50000, # change
               # report writing inputs
               nameOf_t1= input$nameOf_t1,
               nameOf_t2= input$nameOf_t2,
               nameOf_t3= input$nameOf_t3,
               nameOf_t4= input$nameOf_t4,
               nameOfOutcome= newNameOfOutcome(), # change
               currencySymbol= input$currencySymbol,
               # basic health system info
               incidence= input$incidence,
               timeInformation= input$timeInformation,
               discountRate= input$discountRate,
               utilisation_t1= input$utilisation_t1,
               utilisation_t2= input$utilisation_t2,
               utilisation_t3= input$utilisation_t3,
               utilisation_t4= input$utilisation_t4,
               MCD_t2= input$MCD_t2,
               MCD_t3= input$MCD_t3,
               MCD_t4= input$MCD_t4,
               # epidemiology: binary + generic
               P_t1= input$P_t1,
               dist_t2= input$dist_t2,
               mu_t2= input$mu_t2,
               variance_t2= input$variance_t2,
               direction_t2= input$direction_t2,
               dist_t3= input$dist_t3,
               mu_t3= input$mu_t3,
               variance_t3= input$variance_t3,
               direction_t3= input$direction_t3,
               dist_t4= input$dist_t4,
               mu_t4= input$mu_t4,
               variance_t4= input$variance_t4,
               direction_t4= input$direction_t4,
               # epidemiology: survival
               survivalDist= input$survivalDist,
               scaleParameter_t1= input$scaleParameter_t1,
               shapeParameter_t1= input$shapeParameter_t1,
               # trial info: RCT
               durationOfResearch= input$durationOfResearch,
               costResearchFunder= input$costResearchFunder,
               costHealthSystem= input$costHealthSystem,
               # trial info: feasibility
               probabilityOfDefinitiveResearch= input$probabilityOfDefinitiveResearch,
               durationOfResearchFeas= input$durationOfResearchFeas,
               durationOfResearchDefinitive= input$durationOfResearchDefinitive,
               costResearchFunderFeas = input$costResearchFunderFeas,
               costResearchFunderDefinitive= input$costResearchFunderDefinitive,
               costHealthSystemFeas= input$costHealthSystemFeas,
               costHealthSystemDefinitive= input$costHealthSystemDefinitive,
               # cost and QALY inputs
               k= input$k,
               INBBinaryEvent= input$INBBinaryEvent,
               INBContinEvent= input$INBContinEvent,
               INBSurvivalEndpoint= input$INBSurvivalEndpoint,
               cost_t1= input$cost_t1,
               costEvent_t1= input$costEvent_t1,
               costNotEvent_t1= input$costNotEvent_t1,
               cost_t2= input$cost_t2,
               costEvent_t2= input$costEvent_t2,
               costNotEvent_t2= input$costNotEvent_t2,
               cost_t3= input$cost_t3,
               costEvent_t3= input$costEvent_t3,
               costNotEvent_t3= input$costNotEvent_t3,
               cost_t4= input$cost_t4,
               costEvent_t4= input$costEvent_t4,
               costNotEvent_t4= input$costNotEvent_t4
               
              )
        })
    
    
    
    # assign results for all models
    VOIResults$optimalTreatment <- resultsHolder()$optimalTreatment
    VOIResults$probTreatment1isMax <- resultsHolder()$probTreatment1isMax
    VOIResults$probTreatment2isMax <- resultsHolder()$probTreatment2isMax
    VOIResults$probTreatment3isMax <- resultsHolder()$probTreatment3isMax
    VOIResults$probTreatment4isMax <- resultsHolder()$probTreatment4isMax
    VOIResults$probOptimalTisMax <- resultsHolder()$probOptimalTisMax
    VOIResults$probOptimalTisNotMax <- resultsHolder()$probOptimalTisNotMax
    VOIResults$expectedOutcomesPerYearoptimalTreatment <- resultsHolder()$expectedOutcomesPerYearoptimalTreatment
    VOIResults$implementationValueExists <- resultsHolder()$implementationValueExists            # new output
    VOIResults$uncertaintyInCurrentEvidenceExists <- resultsHolder()$uncertaintyInCurrentEvidenceExists
    VOIResults$popDuringResearch <- resultsHolder()$popDuringResearch
    VOIResults$popAfterResearch <- resultsHolder()$popAfterResearch
    VOIResults$popTotal <- resultsHolder()$popTotal
    VOIResults$listForhistVOIYear <- resultsHolder()$listForhistVOIYear
    VOIResults$valueOfResearchPerYear <- resultsHolder()$valueOfResearchPerYear
    VOIResults$valueOfImplementationPerYear <- resultsHolder()$valueOfImplementationPerYear
    VOIResults$tableEventsPerYearDF <- resultsHolder()$tableEventsPerYearDF                        # new
    VOIResults$tableProbabilityMaxDF <- resultsHolder()$tableProbabilityMaxDF
    VOIResults$tableTreatmentCostsDF <- resultsHolder()$tableTreatmentCostsDF
    VOIResults$Cell_A <- resultsHolder()$Cell_A
    VOIResults$Cell_C <- resultsHolder()$Cell_C
    VOIResults$Cell_D <- resultsHolder()$Cell_D
    VOIResults$maxvalueOfImplementation <- resultsHolder()$maxvalueOfImplementation
    VOIResults$maxvalueOfResearch <- resultsHolder()$maxvalueOfResearch
    VOIResults$healthOpportunityCostsOfResearch <- resultsHolder()$healthOpportunityCostsOfResearch
    VOIResults$valueOfResearchWithCurrentImplementation <- resultsHolder()$valueOfResearchWithCurrentImplementation
    VOIResults$valueOfResearchWithPerfectImplementation <- resultsHolder()$valueOfResearchWithPerfectImplementation
    VOIResults$ICER_ResearchWithCurrentImplementation <- resultsHolder()$ICER_ResearchWithCurrentImplementation
    VOIResults$ICER_ResearchWithPerfectImplementation <- resultsHolder()$ICER_ResearchWithPerfectImplementation
    VOIResults$valuePer15KResearchSpend <- resultsHolder()$valuePer15KResearchSpend
    VOIResults$valuePerOpCostResearchSpend <- resultsHolder()$valuePerOpCostResearchSpend
    VOIResults$absoluteExpectedHealthOutcomesFromResearchProject <- resultsHolder()$absoluteExpectedHealthOutcomesFromResearchProject
    # additional feasibility outputs
    VOIResults$popDuringFeasResearch <- resultsHolder()$popDuringFeasResearch               # unique Feasibility output
    VOIResults$popDuringDefinitiveResearch <- resultsHolder()$popDuringDefinitiveResearch    # unique Feasibility output
    VOIResults$popAfterDefinitiveResearch <- resultsHolder()$popAfterDefinitiveResearch     # unique Feasibility output
    VOIResults$expectedCostResearchFunder <- resultsHolder()$expectedCostResearchFunder                 # unique
    VOIResults$expectedCostHealthSystem <- resultsHolder()$expectedCostHealthSystem                 # unique
    VOIResults$valueOfCertainResearchWithPerfectImplementation <- resultsHolder()$valueOfCertainResearchWithPerfectImplementation # unique Feasibility output

    
    
  }) # end observe event expression
  
  
  ########################################################
  # Results and report writing
  #########################################################
  ############################################################
  
  ##########################
  # Report writing
  ##############################
  # approach based on : https://shiny.rstudio.com/articles/generating-reports.html
  
  # NB: when adding objects to params list - must set default value in report.Rmd equal to NA
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.doc",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list( 
                     optimalTreatment = VOIResults$optimalTreatment,
                     probTreatment1isMax = VOIResults$probTreatment1isMax,
                     probTreatment2isMax = VOIResults$probTreatment2isMax,
                     probTreatment3isMax = VOIResults$probTreatment3isMax ,
                     probTreatment4isMax = VOIResults$probTreatment4isMax ,
                     probOptimalTisMax = VOIResults$probOptimalTisMax ,
                     probOptimalTisNotMax = VOIResults$probOptimalTisNotMax, 
                     expectedOutcomesPerYearoptimalTreatment = VOIResults$expectedOutcomesPerYearoptimalTreatment,
                     implementationValueExists = VOIResults$implementationValueExists ,
                     uncertaintyInCurrentEvidenceExists = VOIResults$uncertaintyInCurrentEvidenceExists ,
                     popDuringResearch = VOIResults$popDuringResearch ,
                     popAfterResearch = VOIResults$popAfterResearch ,
                     popTotal = VOIResults$popTotal ,
                     listForhistVOIYear = VOIResults$listForhistVOIYear ,
                     valueOfResearchPerYear = VOIResults$valueOfResearchPerYear ,
                     valueOfImplementationPerYear = VOIResults$valueOfImplementationPerYear ,
                     tableEventsPerYearDF = VOIResults$tableEventsPerYearDF    ,              
                     tableProbabilityMaxDF = VOIResults$tableProbabilityMaxDF ,
                     tableTreatmentCostsDF = VOIResults$tableTreatmentCostsDF,
                     Cell_A = VOIResults$Cell_A ,
                     Cell_C = VOIResults$Cell_C ,
                     Cell_D = VOIResults$Cell_D ,
                     maxvalueOfImplementation = VOIResults$maxvalueOfImplementation ,
                     maxvalueOfResearch = VOIResults$maxvalueOfResearch, 
                     healthOpportunityCostsOfResearch = VOIResults$healthOpportunityCostsOfResearch,
                     valueOfResearchWithCurrentImplementation = VOIResults$valueOfResearchWithCurrentImplementation ,
                     valueOfResearchWithPerfectImplementation = VOIResults$valueOfResearchWithPerfectImplementation ,
                     ICER_ResearchWithCurrentImplementation = VOIResults$ICER_ResearchWithCurrentImplementation ,
                     ICER_ResearchWithPerfectImplementation = VOIResults$ICER_ResearchWithPerfectImplementation ,
                     valuePer15KResearchSpend = VOIResults$valuePer15KResearchSpend ,
                     valuePerOpCostResearchSpend = VOIResults$valuePerOpCostResearchSpend ,
                     absoluteExpectedHealthOutcomesFromResearchProject = VOIResults$absoluteExpectedHealthOutcomesFromResearchProject ,
                     # additional feasibility outputs
                     popDuringFeasResearch = VOIResults$popDuringFeasResearch    ,          
                     popDuringDefinitiveResearch = VOIResults$popDuringDefinitiveResearch ,
                     popAfterDefinitiveResearch = VOIResults$popAfterDefinitiveResearch   ,
                     expectedCostResearchFunder = VOIResults$expectedCostResearchFunder   ,          
                     expectedCostHealthSystem = VOIResults$expectedCostHealthSystem    ,          
                     valueOfCertainResearchWithPerfectImplementation = VOIResults$valueOfCertainResearchWithPerfectImplementation
                     
                     
                     ) # end of list of objects saved to params 
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  ###########################################################
  # Results 
  ####################################################
  # render INPUT and OUTPUT objects and pass to output list
  ####################################################
  # have to update typeOfOutcome to newTypeOfOutcome

  # Create conditional text segments for results section
  ###########################
  
  # Headline results and overview
  ########
  
  output$introduceResearch <- renderText({
    paste("This proposal is for a",input$numberOfTreatments, "arm", 
          ifelse(input$typeOfResearch == "RCT",
                 "randomised controlled trial.",
                 "feasibility study."),
          "The primary endpoint in the trial is", newNameOfOutcome(), "."
          ) # end paste
  })
  
  #****
  # adjust for when research is not valuable!!!
  # ICER in primary natural outcome for RCT and feasibility study
  output$ICERresult <- renderText({
   paste("Considering the uncertainty in the primary endpoint",
         # extra text for feasibility studies
         ifelse(input$typeOfResearch == "feasibility",
                paste("and a", paste0(input$probabilityOfDefinitiveResearch*100, "%"), "chance of the feasibility study leading to a follow-up trial,"),
                ""),
         # text if there is/isnt value in the research
         ifelse(VOIResults$maxvalueOfResearch > 0,
                # text if there is value in the research
                paste("the value of the research is calculated to be approximately", VOIResults$ICER_ResearchWithPerfectImplementation,
                      "per", newNameOfOutcome(), ifelse(newTypeOfOutcome() != "harm","gained.","avoided."),
                      "This means that the research funder must spend",VOIResults$ICER_ResearchWithPerfectImplementation,
                      "to", ifelse(newTypeOfOutcome() != "harm", "gain", "avoid"), "one", newNameOfOutcome(), ".",
                      "As the research funder has limited resources, whether this represents good value for money depends on how this compares to other proposals competing for funding."),
                # text if NO value in research
                paste(VOIResults$optimalTreatment, "is certainty the optimal treatment and therefore there is no value in any further research.
                      ", ifelse(VOIResults$implementationValueExists == TRUE, 
                                paste("Because utilisation of",VOIResults$optimalTreatment , " is not 100%, outcomes can be improved by encouraging it's use in the health system." ),
                                paste("Because the current utilisation of", VOIResults$optimalTreatment , " is already 100%, outcomes can not be improved by encouraging it's use in the health system."))
                )
   )
   )
  })
  

  # treatment cost table
  output$tableTreatmentCosts <- renderTable({VOIResults$tableTreatmentCostsDF}, include.rownames = FALSE)
  
  # text for discussion about treatment costs
  output$discussTableTreatmentCosts <- renderText({
    paste("The table above shows the estimated costs associated with each of the treatments.
          The yearly costs are calculated by multiplying the cost of treating one individual with the incidence per year (", formatC(input$incidence, big.mark = ',', format = 'd'), ").", 
          "The additional costs are calculated by subtracting the yearly costs of the baseline treatment (treatment 1) from the costs of the other treatments.
          Total costs are obtained by multiplying the cost of treating one individual with the total number of individuals who will face this decision (", VOIResults$popTotal, ").", 
          "The total number who face the decision will depend on the incidence, the time over which the evidence is expected to be valuable and the discount rate.")
  
  })
  
  

  # expected outcomes per year table
  output$tableEventsPerYear <- renderTable({VOIResults$tableEventsPerYearDF}, include.rownames = FALSE)
  
  # text for general discussion about current information (common text for RCT and Feas)
  output$resultsCurrenInformation <- renderText({
    paste("From the table above",VOIResults$optimalTreatment, "is favoured by the evidence with", 
          VOIResults$expectedOutcomesPerYearoptimalTreatment, paste0(newNameOfOutcome(), "s"), "expected per year.",
          ifelse(VOIResults$implementationValueExists == TRUE, 
                 # text if there is implementation value
                 paste(" Because utilisation of",VOIResults$optimalTreatment , "is not 100%, outcomes can be improved by encouraging the use of", VOIResults$optimalTreatment,
                 ". The benefits of switching practice to",VOIResults$optimalTreatment , 
                 " are estimated to be",VOIResults$valueOfImplementationPerYear,paste0(newNameOfOutcome(),"s") ,"per year and",VOIResults$maxvalueOfImplementation ,paste0(newNameOfOutcome(),"s"), "over the", input$timeInformation, "year time horion."),
                 # text if there is NOT implementation value
                 paste(" Because the current utilisation of", VOIResults$optimalTreatment , 
                       " is already 100%, outcomes can not be improved by encouraging the use of", VOIResults$optimalTreatment ,".")))
  })
  
  # first block of text for discussion of the potential value of research
  # (common text for RCT and Feas)
  #     - when there is value in research 
  #     - when there is NOT value in research
  # note: conditional panel JavaScript has a problem when the condition is an object created by server operations
  # need to render text and diagrams and so cannot do everything in one output$ object.
  output$resultsValueOfResearch <- renderText({
    ifelse(VOIResults$maxvalueOfResearch > 0,
           # text if there is value in the research
           paste("There is uncertainty about which treatment is optimal in this decision.",
                 "From the previous section",  VOIResults$optimalTreatment, "appears to be the optimal treatment with current evidence as it has the best expected outcomes.
                 However uncertainty about this means that for every individual treated there is a",VOIResults$probOptimalTisMax ,"chance that this is the incorrect choice of treatment.
                 This uncertainty translates into consequences for patient outcomes, i.e. health consequences due to uncertainty about the best treatment.
                 This is calculated by combining the uncertain relative effect with", 
                    ifelse(input$typeOfEndpoint != "continuous", 
                                paste("an estimate of the baseline risk of the outcome and multiplying by the number of individuals affected by the decision each year."),
                                                paste("the number of individuals affected by the decision each year.")),
                 "This results in a distribution of health consequences in number of",paste0(newNameOfOutcome(), "s"), "per year.
                 The distribution of these consequences is illustrated in the diagram below:"
                 ),
           # text if there is NO value of research
           paste("The evidence suggests that there is no uncertainty and that",VOIResults$optimalTreatment , "is definitely the optimal treatment.
                 This lack of uncertainty means there is no value in", 
                 ifelse(input$typeOfResearch == "feasibility",
                        "carrying out research. Even if the follow up research was possible, it would not provide useful information. Therefore neither the feasibility study nor the follow up study are expected to improve health outcomes.",
                        "further research."),
                 "To improve health outcomes, resources should be focused on implementation.")
    )
  })
  
  
  # bug
  # problem in ui.R conditional planel
  # cannot make javaScript condition depend on results of VOI calcluation
  # must display this even if there is value in the research
  # (common to both RCT and Feas)
  output$histVOIYear <- renderPlot({
        plot(VOIResults$listForhistVOIYear, freq = FALSE,
                                         main = "Consequences of uncertainty (per year)",
                                         xlab = "Primary outcomes",
                                         ylab = "Probability (%)")
    })

  # discuss the histogram of VOI results
  # if there is no value in research - there is no commentary on the diagram.
  # (common text for RCT and Feas)
  output$discussHistVOIYear <- renderText({
    ifelse(VOIResults$maxvalueOfResearch > 0,
           paste("From the tall left hand bar in the diagram, it can be seen that there is over a ",VOIResults$probOptimalTisMax ,
                 "chance that there will be zero or very small consequences if the optimal treatment with current evidence (", 
                 VOIResults$optimalTreatment, ") is used. 
                 However, there is a small chance of larger consequnces, which are illustrated by the reamaining bars in the graph.
                 The average over this distribution provides an estimate of the expected consequences of uncertainty, 
                 which is",VOIResults$valueOfResearchPerYear ,paste0(newNameOfOutcome(),"s"), 
                 "per year due to uncertainty.
                 These expected consequences can be interpreted as an estimate of the health benefits that could potentially be gained each year 
                 if the uncertainty in the decision could be resolved, i.e., 
                 it indicates an expected upper bound on the health benefits of further research which would confirm which treatment is optimal.
                 These expected benefits will increase with the size of the patient population whose treatment choice can be informed by additional evidence and the time over which the evidence is expected to be useful.
                 
                 "),
           
           ""
           )
  })
  
  
  # discussion of results : common to both RCT and Feas
  output$VOIresultsPart1 <- renderText({
    ifelse(VOIResults$maxvalueOfResearch > 0,
           paste("It is expected that the information will be valuable for about", input$timeInformation ,"years. 
                 This means that the consequences of uncertainty surrounding the decision become magnified by the fact that 
                 (in the absence of better evidence) we might not be making the optimal decision every year for", input$timeInformation, "years. 
                 Taking this time horizon into account means that the expected consequences of uncertainty are",
                 VOIResults$maxvalueOfResearch ,paste0(newNameOfOutcome(),"s"), "over a", input$timeInformation, "year period (after discounting appropriately)."
           ),
           # if there is no value in research just leave blank
           ""
    )
  })
  
  # ** the op costs of research assume that carrying out research cannot save money in the health system
  # this could be a problem for p3 AND P6**
  # that weird A formatting problem with paste0(currencySymbol, formatC(input$costResearchFunder, big.mark = ',',format = 'd'))
  # discussion of results Text only for RCT analysis
  output$RCTVOIresults <- renderText({
    ifelse(VOIResults$maxvalueOfResearch > 0,
           paste("However, the proposed trial will not report immediately and the value of additional evidence will decline the longer it takes to report.
           As the trial is expected to take",input$durationOfResearch ,"years to report, 
           the expected value of the additional evidence is",VOIResults$valueOfResearchWithPerfectImplementation ,paste0(newNameOfOutcome(), "s"),ifelse(newTypeOfOutcome() != "harm", "gained", "avoided") ,"over the",input$durationOfResearch, "year period. 
           The trial is expected to cost the research funder",paste0(currencySymbol, formatC(input$costResearchFunder, big.mark = ',',format = 'd')) ,". 
           Therefore, the maximum value of the trial is (",paste0(currencySymbol, formatC(input$costResearchFunder, big.mark = ',',format = 'd'), "/",VOIResults$valueOfResearchWithPerfectImplementation), "=)",
           VOIResults$ICER_ResearchWithPerfectImplementation,"per", newNameOfOutcome(), ifelse(newTypeOfOutcome() != "harm", "gained.", "avoided."),
           ifelse(newTypeOfOutcome() == "netHealth",
                  # final text if QALY analysis
                  paste("Funding this research, imposes a cost of",paste0(currencySymbol, formatC(input$costHealthSystem, big.mark = ',',format = 'd')) ,"on the health system.
                  These resources could have been used in direct patient care.
                  In order to reflect the health opportunity costs associated with these costs we use the value of",paste0(currencySymbol, formatC(input$k, big.mark = ',',format = 'd')) ,"per QALY.
                  This means that for every",paste0(currencySymbol, formatC(input$k, big.mark = ',',format = 'd')) ,"of health system resources displaced the system can expect to lose one QALY.
                  The opportunity costs associated with the health system research costs is estimated to be (",paste0(currencySymbol, formatC(input$costHealthSystem, big.mark = ',',format = 'd')),"/",paste0(currencySymbol, formatC(input$k, big.mark = ',',format = 'd')),"=)",VOIResults$healthOpportunityCostsOfResearch ,"QALYs.
                  These opportunity costs have already been subtracted from the trial benefits in calculating the value of the trial.
                  As the value of the trial is expressed in a generic measure of health outcome it can be compared to other candidates competing for funding.
                  As stated previously, for every",paste0(currencySymbol, formatC(input$k, big.mark = ',',format = 'd'))  ,"spent the health system can expect to produce one QALY. 
                  By funding this proposal the funding agency has to spend",VOIResults$ICER_ResearchWithPerfectImplementation ,"to produce one QALY. 
                  This means that the proposal offers",ifelse(VOIResults$valuePerOpCostResearchSpend > 1, "better", "worse") ,"value for money to the health system compared to general service provision.
                  Every",paste0(currencySymbol, formatC(input$k, big.mark = ',',format = 'd')),"spent on this research project is expected to produce",VOIResults$valuePerOpCostResearchSpend," QALYs.
                  This can be compared to 1 QALY produced in the general health system from",paste0(currencySymbol, formatC(input$k, big.mark = ',',format = 'd')),"of spending.
                  However, given a fixed budget for research funding, whether the proposed trial represents good value for research spending depends on how it compares to other proposals competing for funding."),
                  # final text if not QALY analysis
                  paste("The value of the proposed research can now be compared to the other proposals competing for funding. 
                  Whether this research represents good value to the health system depends on the value of the other potential uses of these resources.")
                  )
        
           ),
           # if there is no value in research just leave blank
           ""
           )
  })
  
  # 
  # that weird A formatting problem with paste0(currencySymbol, formatC(input$costResearchFunder, big.mark = ',',format = 'd'))
  # Text only for Feasibility analysis
  output$FeasVOIresults <- renderText({
    ifelse(VOIResults$maxvalueOfResearch > 0,
           paste("From the above analysis, the maximum that can be gained from the follow-up research is",VOIResults$maxvalueOfResearch ,paste0(newNameOfOutcome(),"s."), 
           "However, the feasibility trial takes",input$durationOfResearchFeas ,"years to report and has a",paste0(input$probabilityOfDefinitiveResearch*100, "%") ,
           "chance of leading to the follow-up trial, which would then take an additional",input$durationOfResearchDefinitive ,"years to report. 
           Naturally, the value of the additional evidence will decline the longer it takes the follow-up trial to report. 
           If the follow-up trial was certain to report, it would take (",input$durationOfResearchFeas ,"+",input$durationOfResearchDefinitive ,"=)",input$durationOfResearchFeas + input$durationOfResearchDefinitive ,
           "years in total and the expected value of the additional evidence would be",VOIResults$valueOfCertainResearchWithPerfectImplementation ,paste0(newNameOfOutcome(),"s"),ifelse(newTypeOfOutcome() != "harm", "gained", "avoided"),"over the",input$timeInformation ,"year period. 
           As there is a",paste0(input$probabilityOfDefinitiveResearch*100, "%") ,"chance that the follow-up trial is possible, the value of the project falls to",VOIResults$valueOfResearchWithPerfectImplementation ,paste0(newNameOfOutcome(),"s"),ifelse(newTypeOfOutcome() != "harm", "gained.", "avoided."),
           "The feasibility trial is expected to cost the research funder",paste0(currencySymbol, formatC(input$costResearchFunderFeas, big.mark = ',',format = 'd')) ,"and the definitive trial is expected to cost",paste0(currencySymbol, formatC(input$costResearchFunderDefinitive, big.mark = ',',format = 'd')),
           ". As the feasibility study costs will always be incurred and there is a",paste0(input$probabilityOfDefinitiveResearch*100, "%") ,"chance that the follow-up research will not occur, 
           the total expected cost to the research funder is (",paste0(currencySymbol, formatC(input$costResearchFunderFeas, big.mark = ',',format = 'd')) ,"+",paste0(currencySymbol, formatC(input$costResearchFunderDefinitive, big.mark = ',',format = 'd')) ,"x",paste0(input$probabilityOfDefinitiveResearch*100, "%") ,"=)",VOIResults$expectedCostResearchFunder,
           ". Therefore, the expected value of funding the feasibility trial is (",VOIResults$expectedCostResearchFunder,"/",VOIResults$valueOfResearchWithPerfectImplementation  ,"=)",VOIResults$ICER_ResearchWithPerfectImplementation,"per",newNameOfOutcome(),ifelse(newTypeOfOutcome() != "harm", "gained.", "avoided."),
           ifelse(newTypeOfOutcome() == "netHealth",
                  # final text if QALY analysis
                  paste("Funding this research, imposes an expected cost of",paste0(currencySymbol, formatC(input$costHealthSystemFeas, big.mark = ',',format = 'd')) ,"+",paste0(currencySymbol, formatC(input$costHealthSystemDefinitive, big.mark = ',',format = 'd')) ,"x",paste0(input$probabilityOfDefinitiveResearch*100, "%") ,"=)",VOIResults$expectedCostHealthSystem,
                        "on the health system. These resources could have been used in direct patient care.
                        In order to reflect the health opportunity costs associated with these costs we use the value of",paste0(currencySymbol, formatC(input$k, big.mark = ',',format = 'd')) ,"per QALY.
                        This means that for every",paste0(currencySymbol, formatC(input$k, big.mark = ',',format = 'd')) ,"of health system resources displaced the system can expect to lose one QALY.
                        The opportunity costs associated with the health system research costs is estimated to be (",VOIResults$expectedCostHealthSystem,"/",paste0(currencySymbol, formatC(input$k, big.mark = ',',format = 'd')),"=)",VOIResults$healthOpportunityCostsOfResearch ,"QALYs.
                        These opportunity costs have already been subtracted from the trial benefits in calculating the value of the trial.
                        As the value of the trial is expressed in a generic measure of health outcome it can be compared to other candidates competing for funding.
                        As stated previously, for every",paste0(currencySymbol, formatC(input$k, big.mark = ',',format = 'd'))  ,"spent the health system can expect to produce one QALY. 
                        By funding this proposal the funding agency has to spend",VOIResults$ICER_ResearchWithPerfectImplementation ,"to produce one QALY. 
                        This means that the proposal offers",ifelse(VOIResults$valuePerOpCostResearchSpend > 1, "better", "worse") ,"value for money to the health system compared to general service provision.
                        Every",paste0(currencySymbol, formatC(input$k, big.mark = ',',format = 'd')),"spent on this research project is expected to produce",VOIResults$valuePerOpCostResearchSpend," QALYs.
                        This can be compared to 1 QALY produced in the general health system from",paste0(currencySymbol, formatC(input$k, big.mark = ',',format = 'd')),"of spending.
                        However, given a fixed budget for research funding, whether the proposed trial represents good value for research spending depends on how it compares to other proposals competing for funding."),
                  # final text if not QALY analysis
                  paste("The value of the proposed research can now be compared to the other proposals competing for funding. 
                        Whether this research represents good value to the health system depends on the value of the other potential uses of these resources.")
                  )
           
           ),
           # if there is no value in research just leave blank
           ""
    )
  })  

  
  
 
  
}) # end server function












































#######################################################################################
# ZOMBIE CODE 
#######################################################################################



# Raw input and output objects
#########################

# input objects
#output$nameOf_t1 <- renderText({
#  paste("The name of treatment 1 is", input$nameOf_t1)
#})
#output$nameOf_t2 <- renderText({input$nameOf_t2})
#output$nameOf_t3 <- renderText({input$nameOf_t3})
#output$nameOf_t4 <- renderText({input$nameOf_t4})
#output$nameOfOutcome <- renderText({newNameOfOutcome()})

# output objects
# output$optimalTreatment <- renderText({VOIResults$optimalTreatment})
# output$expectedOutcomesPerYearoptimalTreatment <- renderText({VOIResults$expectedOutcomesPerYearoptimalTreatment})
# output$implementationValueExists <- renderText({VOIResults$implementationValueExists})            # new output
# output$uncertaintyInCurrentEvidenceExists <- renderText({VOIResults$uncertaintyInCurrentEvidenceExists})
#output$probTreatment1isMax <- renderText({VOIResults$probTreatment1isMax })
#output$probTreatment2isMax <- renderText({VOIResults$probTreatment2isMax })
#output$probTreatment3isMax <- renderText({VOIResults$probTreatment3isMax })
#output$probTreatment4isMax <- renderText({VOIResults$probTreatment4isMax})
# output$popDuringResearch <- renderText({VOIResults$popDuringResearch})
# output$popAfterResearch <- renderText({VOIResults$popAfterResearch})
# output$popTotal <- renderText({VOIResults$popTotal })
# output$popDuringFeasResearch <- renderText({VOIResults$popDuringFeasResearch})       # feas outputs
# output$popDuringDefinitiveResearch <- renderText({VOIResults$popDuringDefinitiveResearch})       # feas outputs
# output$popAfterDefinitiveResearch <- renderText({VOIResults$popAfterDefinitiveResearch})        # feas outputs
# 
#output$valueOfResearchPerYear <- renderText({paste("value of research per year is",  VOIResults$valueOfResearchPerYear)})
# output$valueOfImplementationPerYear <- renderText({paste("value of implementation per year is", VOIResults$valueOfImplementationPerYear)})
# 
#output$tableProbabilityMax <- renderTable({VOIResults$tableProbabilityMaxDF}, include.rownames = FALSE)

#output$Cell_A <- renderText({VOIResults$Cell_A})
#output$Cell_C <- renderText({VOIResults$Cell_C})
#output$Cell_D <- renderText({VOIResults$Cell_D})
#output$maxvalueOfImplementation <- renderText({VOIResults$maxvalueOfImplementation})
#output$maxvalueOfResearch <- renderText({VOIResults$maxvalueOfResearch})
#output$healthOpportunityCostsOfResearch <-   renderText({VOIResults$healthOpportunityCostsOfResearch})
#output$expectedCostResearchFunder <-   renderText({paste("expected costs to research funder" ,VOIResults$expectedCostResearchFunder)})
#output$valueOfResearchWithCurrentImplementation <- renderText({paste("value of research with current implementation ",VOIResults$valueOfResearchWithCurrentImplementation)})
#output$valueOfResearchWithPerfectImplementation <- renderText({paste("Value of research with perfect implementation", VOIResults$valueOfResearchWithPerfectImplementation)})
#output$valueOfCertainResearchWithPerfectImplementation <- renderText({paste("value of research with certain definitive trial", VOIResults$valueOfCertainResearchWithPerfectImplementation)})
#output$ICER_ResearchWithCurrentImplementation <- renderText({paste("ICER with current info is",VOIResults$ICER_ResearchWithCurrentImplementation)})
#output$ICER_ResearchWithPerfectImplementation <- renderText({paste("ICER with perfect info is",VOIResults$ICER_ResearchWithPerfectImplementation)})
#output$valuePer15KResearchSpend <- renderText({paste("value per 15K research spend is",VOIResults$valuePer15KResearchSpend)})
#output$absoluteExpectedHealthOutcomesFromResearchProject <- renderText({paste("absolute expected outcomes from research project",VOIResults$absoluteExpectedHealthOutcomesFromResearchProject)})

#output$costResearchFunderFeas <- renderText({paste("cost funder feasibility ", input$costResearchFunderFeas)})
#output$costResearchFunderDefinitive <- renderText({paste("cost funder definitive", input$costResearchFunderDefinitive)})
#output$probabilityOfDefinitiveResearch <- renderText({paste("prob of definitive research ", input$ProbabilityOfDefinitiveResearch)})
#output$test1 <- renderText({VOIResults$test1})
#output$test2 <- renderText({VOIResults$test2})
#output$test3 <- renderText({VOIResults$test3})





