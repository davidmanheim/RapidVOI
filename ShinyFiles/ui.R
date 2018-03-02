############################
# potential bugs: 
# how many MCsims should be the max? some funny behavour at large numbers.
# if "reconsider == Yes" (when there is no model that runs under this condition) the model evaluates the previous model again.
# 
# possible improvements:
# 
# tests:
# need to see how it works with NAs as defaults for everything!



library(shiny)

shinyUI(fluidPage(
  titlePanel("Rapid value of information (VOI) decision tool"),
  
  tabsetPanel(
    
    ##################
    # HOW TO ESTIMATE tab
    ##################
    tabPanel("Welcome",
             br(),
             p("Demo release version 0.1"),
             br(),
             h4("How to use this app"),
             p("This is an R Shiny App which facilitates calculations of the value of research proposals in a timely manner. 
               The inputs required in the app represent the minimum needed to understand the consequences of uncertainty and the need for further research.
               Full details of the approach used and applied examples using these methods are forthcoming. In the meantime click",a("here", href = "https://www.york.ac.uk/che/research/teehta/research-prioritisation/") ,"for further details.
               "),
             p(strong("Users unfamiliar with value of informaiton methods"), "are encouraged to read the information in the 'How to estimate research value' tab. This section describes the value of information approach and how it applies to research funding in a resource constrainted health care system.
               "),
             p(strong("Those who have not used this app before"), "click the'Inputs and how to use this app' tab. This section describes the types of analysis which are possible with this app and the inputs that are required.
               "),
             p(strong("To carry out an analysis"), "click the 'Inputs' tab
               "),
             br(),
             tags$em("This code has been produced under a GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007")
             
             
             ), # end Welcome tabPanel
    
    
    ##################
    # HOW TO ESTIMATE tab
    ##################
    tabPanel("How to estimate research value", 
             br(),
             h4("A video introducing value of information"),
             HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/tbv9E9D2BRQ" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>'),
             br(),
             h4("How can health oucomes be improved?"),
             p("Additional evidence is valuable because it can improve patient outcomes by resolving existing uncertainty about the effectiveness of the interventions available. This helps inform treatment decisions for subsequent patients. 
               A judgement about the level of uncertainty in the existing evidence base can come from a number of sources including a systematic literature review and meta-analysis, expert elicitation, extrapolation, meta-epidemiological study, or a combination of these different sources. 
               The expected health benefit provided by reducing uncertainty is called the information value for a research project."
               ),
             p("In addition to funding research, it is also possible to improve health outcomes by ensuring that the treatment option that is expected to be best based on the findings of existing evidence is implemented into clinical practice.  In fact, the improvements in health outcomes from implementing the findings of the current evidence base (implementation value) may be greater than the potential improvements in health outcomes through conducting further research."
               ),
             p("Drawing a distinction between the information value and the implementation value is important because conducting further evaluative research is not the only way to change clinical practice. The results of a new research study may influence clinical practice and may contribute to the implementation of research findings but this is not the only, or necessarily the most effective, way to do so. There are other mechanisms (e.g., more effective dissemination of existing evidence) and policies which are within the remit of other bodies (e.g., incentives and sanctions) to affect implementation. Therefore, conducting research to influence implementation rather than because there is real value in acquiring additional evidence itself would seem inappropriate. This is because limited research capacity could have been used elsewhere to conduct research in areas where it would have offered greater potential health benefits
               "),
             h4("What change in the primary endpoint is required?"),
             p("Uncertainty in a decision about alternative treatment options arises from the range of plausible values that the important endpoints can take.  When the range of plausible values for a particular parameter such as the relative treatment effect can support more than one intervention (e.g., the confidence interval for the estimate of relative effect crosses the line of no difference) this uncertainty has consequences for health outcomes.  This is because for any treatment choice there is a chance that an alternative intervention could have improved health outcomes to a greater extent.  The importance of this uncertainty is indicated by the scale of the health consequences of the uncertainty.   The chance that an intervention is not the most effective, how much less effective it is likely to be, and the size of the patient population facing the uncertain treatment choice all contribute to the health consequences of uncertainty.
               "),
             p("The primary endpoint, which usually captures the most important aspects of health outcome, can be used as a starting point to understand the consequences of current uncertainty. However, in situations where there are a number of other important considerations that are not captured in the primary outcome, we can specify a minimum clinical difference (MCD) in the primary outcome that would need to be detected in future research.  This represents the change in the primary endpoint that would need to be detected for the results of any new research study to be considered clinically significant and have an impact on clinical practice. 
               This MCD concept will be explained further in the next section: 'How to use this app'.")
             
             
             ), # end how to estimate tabPanel
    
    ##################
    # HOW TO use this app tab
    ##################
    tabPanel("Inputs and how to use this app",
             br(),
             h4("A video on using this app"),
             br(),
             p("<INSERT SHORT YOUTUBE VIDEO ON HOW TO USE THIS APP>"),
             br(),
             h4("Inputs required to estimate research value"),
             br(),
             
             wellPanel(
               h4("Decision problem inputs"),
               strong("Type of primary endpoint"),
               p("The primary outcome measure or endpoint captures the most important aspects of health outcome. The value of additional research is expressed in terms of ‘benefits gained’ or ‘harms avoided’ depending on whether this outcome is a benefit or harm. Alternative scenarios based on different endpoints can also be used to consider the impact of additional evidence on different aspects of outcome. 
                 Where the analysis is restricted to a primary outcome but there are a number of other important aspects of outcome that are not captured in the analysis, we can specify a minimum clinical difference in effect to implicitly account for these other unquantified aspects of outcome and/or costs (see the MCD section below)
                 "),
               strong("Type of outcome"),
               p(""),
               strong("Number of treatments investigated"),
               p("
               "),
               strong("Type of research"),
               p("This app currently facilitates value of information calculations for randomised controlled trials and feasibility studies.
                 The inputs required for the analysis will depend on the type of study planned.
                 "),
               strong("The value of reconsidering the evidence"),
               p("
                 ")
             ),
             br(),
             
             wellPanel(
               h4("Health system inputs"),
               strong("Incidence per annum"),
               p("An estimate of the number of individuals facing the uncertain choice between alternative interventions is required in order to establish the size of the benefits to the target population. 
               "),
               strong("Length of time for which the new evidence would be valuable"),
               p("The information generated by new research will not be valuable indefinitely because other changes occur over time. For example, over time new and more effective interventions become available which will eventually make those currently available obsolete. This means that new information about effectiveness is only relevant for a specific amount of time. A judgement about the length of time that the evidence from the proposed RCT might be valuable is required to estimate the expected benefits over an appropriate time horizon.  
                 "),
               strong("Discount rate"),
               p("When a time horizon greater than one year is considered in the analysis, discounting should be used to reflect the fact that resources committed today could be invested at a real rate of return to free up more resources in the future. 
                 "),
               strong("Opportunity cost of health expenditure"),
               p("
                 "),
               strong("Net health effect of survival endpoint (in QALYs)"),
               p("
                 "),
               strong("Name of outcome"),
               p("
                 "),
               strong("Currency used"),
               p("
                 ")
             ),
             br(),
             
             wellPanel(
               h4("Trial inputs"),
               strong("Duration of the research"),
               p("**discuss feasibility durations required here too** Some assessment of the duration of time for the proposed research to be conducted and for the  results of the research to report is required since the value of research declines the longer it takes to report. This might be informed by an assessment of sample size, recruitment rates, or historical experience from conducting similar types of studies. 
                 "),
               strong("Costs of the research"),
               p("**discuss all costs here** Some assessment of the likely costs of the proposed RCT is required to establish whether the expected benefits from the study are sufficient to justify the expected costs. 
                 "),
               strong("Liklihood of feasibility research leading to follow-up study"),
               p("
                 ")
             ),
             br(),
             
             wellPanel(
               h4("Run analysis"),
               strong("Run analysis button"),
               p(""),
               strong("Number of simulations"),
               p("**talk about reconsider evidence here")
             ),
             br(),
      
             wellPanel(
               h4("Treatment inputs"),
               strong("Baseline event rate"),
               p(""),
               strong("Current level of utilisation of the interventions"),
               p("Health systems can improve health outcomes in two distinct ways: (i) generating additional evidence to reduce uncertainty about which treatment improves health (information value); and (ii) changing clinical practice such that the optimal treatment based on current evidence is implemented (implementation value).
                 Some estimate of the current level of utilisation of the interventions in clinical practice is required to establish the value of changing practice if the results of new research suggest a change. It can also be used to establish whether there is greater value from encouraging the implementation of what existing evidence suggests is the most effective intervention rather than conducting new research. 
                 "),
               strong("Relative treatment effects"),
               p("An estimate of the relative effectiveness of the intervention is required for the primary outcome, along with an estimate of its uncertainty. 
                 This information is required in the form of a mean and variance of the log odds ratio (for binary and continuous outcomes) or a mean and variance of the log hazard ratio (for survival outcomes). 
                 This estimate is usually obtained from a standard meta-analysis. However, if the estimate is unavailable or considered inadequate, alternative values can be used to represent different judgements about the uncertain relative treatment effect. 
                 "),
               strong("Distribution of relative effects"),
               p("")
              
             )
             
             
            
      
    ), # end how to use this app tabPanel
    
    ##################
    # INPUTs page tab
    ##################
    tabPanel("Inputs", 
               
             br(),
             fluidPage(
               
               # top fluid row (non treatment inputs)
               fluidRow(
                 column(3, 
                        ##########
                        # Primary outcome measure inputs
                        ##########
                        wellPanel(
                          h4("Primary outcome measure"),
                          #p("This information determines the inputs required for the analysis and so this section",
                          #  strong("should be completed first.")),
                        
                            selectInput(inputId = "typeOfEndpoint", label = "Type of primary endpoint", 
                                        choices = c("Binary" = "binary", 
                                                    "Continuous" = "continuous", 
                                                    "Survival" = "survival"),
                                        selected = "Binary"), 
                            
                            # old
                            # selectInput(inputId = "typeOfOutcome", label = "Type of outcome", 
                            #             choices = c("Benefit" = "benefit", 
                            #                         "Harm" = "harm", 
                            #                         "Net health effect (QALYs)" = "netHealth"),
                            #             selected = "Net health effect (QALYs)"), # benefit , harm, net health effect
                            
                            # new
                            selectInput(inputId = "outcomeExpression", label = "Express results in natural outcomes (e.g. heart attacks avoided) or in QALYs?", 
                                        choices = c("Natural outcomes" = "natural",  
                                                    "QALYs" = "netHealth"),
                                        selected = "QALYs"), 
                          
                            # new
                            conditionalPanel(condition = "input.outcomeExpression == 'natural'",
                                             selectInput(inputId = "benefitOrHarm", label = "Is the outcome a benefit (e.g. cures) or a harm (e.g. heart attack)?", 
                                                        choices = c("Benefit" = "benefit", 
                                                                    "Harm" = "harm"),
                                                        selected = "Benefit")),
                          

                            conditionalPanel(condition = "input.outcomeExpression == 'natural'",
                                             textInput("nameOfOutcome", "Name of outcome e.g. heart attack", 
                                                       value = "functional recovery"))


                            
                            
                            
                        ), # end wellPanel
                        
                        # conditional well panel
                        conditionalPanel(condition = "input.outcomeExpression == 'netHealth'", # start conditional well panel
                                         
                                         wellPanel( # START wellPanel
                                           
                                           h4("Comprehensive meausure of health outcome"),
                                           
                                           numericInput("k", "Opportunity cost of health system expenditure (£)",
                                                        value = 15000, min = 0, max = NA, step = 500),
                                           
                                           conditionalPanel(condition = "input.typeOfEndpoint == 'binary'",
                                                            numericInput("INBBinaryEvent", "Net health effect of binary event occuring (in QALYs)",
                                                                         value = 2, min = NA, max = NA, step = 0.05)),
                                           
                                           conditionalPanel(condition = "input.typeOfEndpoint == 'binary' && input.outcomeExpression == 'netHealth'",
                                                            radioButtons(inputId = "tCostsDependOnEvent", label = "Do the treatment costs depend on the primary outcome?", 
                                                                         choices = c("Yes" = "Yes", 
                                                                                     "No" = "No"),
                                                                         selected = "No")),
                                           
                                           conditionalPanel(condition = "input.typeOfEndpoint == 'continuous'",
                                                            numericInput("INBContinEvent", 
                                                                         "Net health effect of unit increase in continuous outcome (in QALYs)",
                                                                         value = 0.05, min = NA, max = NA, step = 0.05)),
                                           
                                           # BUG!! If this has NA value then the app crashes
                                           conditionalPanel(condition = "input.typeOfEndpoint == 'survival'",
                                                            numericInput("INBSurvivalEndpoint", "Net health effect of survival endpoint (in QALYs)",
                                                                         value = 0.5, min = NA, max = NA, step = 0.05))
                                           
                                         ) # end wellPanel
                        ) # end conditional well panel
                        
                        
                        
                        ), # end decision problem inut column
                 
                 column(3, 
                        ##########
                        # General inputs (1st panel): health system
                        ##########
                        
                        wellPanel(
                          
                          h4("Proposed research study"),
          
                          selectInput(inputId = "typeOfResearch", label = "Type of research", 
                                      choices = c("RCT" = "RCT", 
                                                  "Feasibility study" = "feasibility"),
                                      selected = "RCT"),
                          
                          # RCT trial design inputs
                          conditionalPanel(condition = "input.typeOfResearch == 'RCT'",
                                           
                                           numericInput("durationOfResearch", "Expected duration of research (years)",
                                                        value = 5, min = 0, max = NA, step = 0.1),
                                           
                                           numericInput("costResearchFunder", "Cost of research to funder",
                                                        value = 2854000, min = 0, max = NA, step = 100),
                                           
                                           conditionalPanel(condition = "input.outcomeExpression == 'netHealth'",
                                                            numericInput("costHealthSystem", "Costs of research imposed on health system",
                                                                         value = 1000000, min = 0, max = NA, step = 100) )
                          ), # end RCT trial design conditional panel
                          
                          
                          # Feasibility trial design inputs
                          conditionalPanel(condition = "input.typeOfResearch == 'feasibility'",
                                           
                                           numericInput("probabilityOfDefinitiveResearch", "Probability of feasibility research leading to follow-up study",
                                                        value = 0.5, min = 0, max = 1, step = 0.05),
                                           
                                           numericInput("durationOfResearchFeas", "Expected duration of feasibility research (years)",
                                                        value = 2, min = 0, max = NA, step = 0.1),
                                           
                                           numericInput("durationOfResearchDefinitive", "Expected duration of follow-up research (years)",
                                                        value = 5, min = 0, max = NA, step = 0.1),
                                           
                                           numericInput("costResearchFunderFeas", "Costs of feasibility research to funder",
                                                        value = 1000000, min = 0, max = NA, step = 100),
                                           
                                           numericInput("costResearchFunderDefinitive", "Costs of follow-up research to funder",
                                                        value = 1000000, min = 0, max = NA, step = 100),
                                           
                                           conditionalPanel(condition = "input.outcomeExpression == 'netHealth'",
                                                            numericInput("costHealthSystemFeas", "Costs of feasibility research imposed on health system",
                                                                         value = 1000000, min = 0, max = NA, step = 100),
                                                            
                                                            # display if: outcomeExpression == "netHealth"
                                                            numericInput("costHealthSystemDefinitive", "Costs of follow-up research imposed on health system",
                                                                         value = 1000000, min = 0, max = NA, step = 100) )
                          ), # end Feasibility trial design conditional panel
                          
                          
                          numericInput("numberOfTreatments", "How many treatment options are under consideration? (Maximum of 4)",
                                       value = 2, min = 2, max = 4),

                          numericInput("timeInformation", "Time over which evidence would be valuable (years)",
                                       value = 15, min = 0, max = NA, step = 0.1)
                          
                          
      
                        ) # end of wellPanel 
                        ), # end of second column
                 
                 column(3,
                        ##########
                        # General inputs (2st panel): Other inputs
                        ##########
                        
                        wellPanel( # START "other inputs" well panel
                          
                          h4("Other inputs"),
                        
                          numericInput("discountRate", "Discount rate (%)",
                                       value = 3.5, min = 0, max = 100, step = 0.1),
                          
                          numericInput("incidence", "Incidence per annum",
                                       value = 8800, min = 0, max = NA, step = 20),
                          
                          textInput("currencySymbol", "Currency used in analysis", 
                                    value = "£"),
                          
                          
                          # add warning about time taken to do this calculation?
                          conditionalPanel(condition = "input.typeOfResearch == 'RCT'",
                                           radioButtons(inputId = "reconsider", label = "Calculate the value of reconsidering the evidence?", 
                                                        choices = c("Yes" = "Yes", 
                                                                    "No" = "No"),
                                                        selected = "No"))
                          
               
                        ) # end "other inputs" wellpanel 
                        

                        
                        ), # end 3rd column inputs
                 
                 column(3, 
                        ##########
                        # Run buttons 
                        ##########
                        
                        wellPanel(
                        
                          br(),
                          actionButton("run", label = "Run analysis"),
                          br(),
                          br(),
                          
                          conditionalPanel(condition = "input.reconsider != 'Yes'",
                                          p("Click once and go to the Results tab")),

                          conditionalPanel(condition = "input.reconsider == 'Yes'",
                                           p("Click once, and go to the Results tab. The calculation to reconsider the evidence can take up to 10 minutes to report."))
                          
                          
                          # do not display MC simulations to user
                          # just inputted 50000 directly into master() input
                          # **check maximum number feasible
                          #conditionalPanel(condition = "input.reconsider != 'Yes'",
                          #                 numericInput("MCsims", "Number of simulations",
                          #                              value = 50000, min = 0, max = 10000000, step = 500)),
                          
                          # do not display MC simulations to user
                          # **check maximum number feasible
                          # conditionalPanel(condition = "input.reconsider == 'Yes'",
                          #                  
                          #                  p("Note that this analysis will take between ## and ## minutes to report. This is due to the large number of simulations required"),
                          #                  numericInput("MCsimsInner", "Number of simulations for inner loop",
                          #                               value = 50000, min = 0, max = 10000000, step = 500),
                          #                  
                          #                  numericInput("MCsimsOuter", "Number of simulations for outer loop",
                          #                               value = 50000, min = 0, max = 10000000, step = 500))
                          
                          
                        ) # end well panel run button
                        ) # end column 4: run button
                        
                        ), # end top fluid row
               
               # LOWER ROW
               # finish changing names!
               # need to do something about t1 = t0, t2 = t1,...
               # treatment 1 = baseline treatment, treatment 2 = intervention 1,...
               fluidRow(
                 column(3, 
                        ##########
                        # Baseline treatment (formerly: treatment 1 )
                        ##########
                        
                        wellPanel(
                          h4("Baseline treatment"),
                          #p("If a no treatment or standard practice option is considered then it should be entered here"),
                        
                          textInput("nameOf_t1", "Name of baseline treatment", 
                                  value = "late PTP"),
                        
                          numericInput("utilisation_t1", "Current utilisation of baseline treatment (%)",
                                     value = 100, min = 0, max = 100, step = 0.1),
                        
                          conditionalPanel(condition = "input.typeOfEndpoint == 'binary'",
                                           numericInput("P_t1", "Baseline probability of outcome",
                                                        value = 0.5, min = 0, max = 1, step = 0.05)),
                          
                          # survival inputs for t1
                          conditionalPanel(condition = "input.typeOfEndpoint == 'survival'",
                                           
                                           selectInput(inputId = "survivalDist", label = "Type of survival distribution", 
                                                        choices = c("Exponential" = "exponential", 
                                                                    "Weibull" = "weibull"),
                                                        selected = "exponential"),
                                           
                                           numericInput("scaleParameter_t1", "Scale parameter for baseline treatment (natural scale)",
                                                        value = 5, min = 0, max = NA, step = 1),
                                           
                                           conditionalPanel(condition = "input.survivalDist == 'weibull'",
                                                            numericInput("shapeParameter_t1", "Shape parameter for baseline treatment (natural scale)",
                                                                         value = 1.1, min = 0, max = NA, step = 0.1))
                          ), # end survival inputs for t1
                          
                          #conditionalPanel(condition = "input.typeOfEndpoint == 'continuous'",
                          #                 p("Note that if the primary endpoint is continuous the expected outcome on the continuous scale with the baseline treatment is not required. For further details see ####INSERT REFERENCE")),
                          
                          # Cost inputs for t1
                          conditionalPanel(condition = "input.outcomeExpression == 'netHealth'",
                                        
                                           conditionalPanel(condition = "input.tCostsDependOnEvent == 'No'",
                                                            numericInput("cost_t1", "Lifetime treatment costs associated with the baseline treatment",
                                                                         value = 100, min = NA, max = NA, step = 10) ),
                                           
                                           conditionalPanel(condition = "input.tCostsDependOnEvent == 'Yes'",
                                                            numericInput("costEvent_t1", "Lifetime treatment costs associated with the baseline treatment if the primary outcome occurs",
                                                                         value = 100, min = NA, max = NA, step = 10),
                                                            
                                                            numericInput("costNotEvent_t1", "Lifetime treatment costs associated with the baseline treatment if the primary outcome does not occur",
                                                                         value = 100, min = NA, max = NA, step = 10) )
                          ) # end Cost inputs for t1
                          
                        
                        
                        ) # end wellPanel t1
                        ), # end column t1
                 
                 column(3, 
                        ##########
                        # Intervention 1 (formerly: treatment 2 )
                        ##########
                        
                        wellPanel(
                          h4("Intervention 1"),
                        textInput("nameOf_t2", "Name of intervention 1", 
                                  value = "early PTP"),
                        
                        numericInput("utilisation_t2", "Current utilisation of intervention 1 (%)",
                                     value = 0, min = 0, max = 100, step = 0.1),
                        
                        selectInput("dist_t2", label = "Distribution of intervention 1 relative effects", 
                                    choices = c("Normal" = "norm", 
                                                "Half Normal" = "halfNorm"),
                                    selected = "Normal"),
                        
                        # normal dist inputs for t2
                        conditionalPanel(condition = "input.dist_t2 == 'norm'",
                                         numericInput("mu_t2", "Mean log odds / log hazard ratio for intervention 1",
                                                      value = 0, min = NA, max = NA, step = 0.05),
                                         
                                         numericInput("variance_t2", "Variance of log odds / log hazard ratio for intervention 1",
                                                      value = 0.25, min = NA, max = NA, step = 0.05)
                                         ), # end normal dist inputs for t2
                        
                        conditionalPanel(condition = "input.dist_t2 == 'halfNorm'",
                                         selectInput("direction_t2", label = "Direction of distribution for intervention 1", 
                                                     choices = c("Always positive" = "alwaysPositive", 
                                                                 "Always negative" = "alwaysNegative"),
                                                     selected = "alwaysPositive")),
                        
                        numericInput("MCD_t2", "MCD for intervention 1",
                                     value = 0, min = NA, max = NA, step = 0.05),
                        
                        # Cost inputs for t2
                        conditionalPanel(condition = "input.outcomeExpression == 'netHealth'",
                                         
                                         conditionalPanel(condition = "input.tCostsDependOnEvent == 'No'",
                                                          numericInput("cost_t2", "Lifetime treatment costs associated with intervention 1",
                                                                       value = 100, min = NA, max = NA, step = 10) ),
                                         
                                         conditionalPanel(condition = "input.tCostsDependOnEvent == 'Yes'",
                                                          numericInput("costEvent_t2", "Lifetime treatment costs associated with intervention 1 if the primary outcome occurs",
                                                                       value = 100, min = NA, max = NA, step = 10),
                                                          
                                                          numericInput("costNotEvent_t2", "Lifetime treatment costs associated with intervention 1 if the primary outcome does not occur",
                                                                       value = 100, min = NA, max = NA, step = 10) )
                        ) # end Cost inputs for t2
                        ) # end wellPanel t2
                        ), # end column t2
                 
                 column(3, 
                        ##########
                        # interverntion 2 (formerly treatment 3 )
                        ##########
                        
                        conditionalPanel(condition = "input.numberOfTreatments >= 3",
                                         
                                         wellPanel(
                                           h4("Intervention 2"),
                                           # display if: numberOfTreatments >= 3
                                           textInput("nameOf_t3", "Name of intervention 2", 
                                                     value = "intervention 2"),
                                           
                                           # display if: numberOfTreatments >= 3
                                           numericInput("utilisation_t3", "Current utilisation of intervention 2 (%)",
                                                        value = 0, min = 0, max = 100, step = 0.1),
                                           
                                           # display if: numberOfTreatments >= 3 & typeOfEndpoint != successFail 
                                           selectInput("dist_t3", label = "Distribution of intervention 2 relative effects", 
                                                       choices = c("Normal" = "norm", 
                                                                   "Half Normal" = "halfNorm"),
                                                       selected = "Normal"),
                                   
                                           # normal dist inputs for t3
                                           conditionalPanel(condition = "input.dist_t3 == 'norm'",
                                                            numericInput("mu_t3", "Mean log odds / log hazard ratio for intervention 2",
                                                                         value = 0, min = NA, max = NA, step = 0.05),
                                                            
                                                            numericInput("variance_t3", "Variance of log odds / log hazard ratio for intervention 2",
                                                                         value = 0.25, min = NA, max = NA, step = 0.05)
                                           ), # end normal dist inputs for t3
                                           
                                           conditionalPanel(condition = "input.dist_t3 == 'halfNorm'",
                                                            selectInput("direction_t3", label = "Direction of distribution for intervention 2", 
                                                                        choices = c("Always positive" = "alwaysPositive", 
                                                                                    "Always negative" = "alwaysNegative"),
                                                                        selected = "alwaysPositive")),
                                           
                                           numericInput("MCD_t3", "MCD for intervention 2",
                                                        value = 0, min = NA, max = NA, step = 0.05),
                                           
                                           # Cost inputs for t3
                                           conditionalPanel(condition = "input.outcomeExpression == 'netHealth'",
                                                            
                                                            conditionalPanel(condition = "input.tCostsDependOnEvent == 'No'",
                                                                             numericInput("cost_t3", "Lifetime treatment costs associated with intervention 2",
                                                                                          value = 100, min = NA, max = NA, step = 10) ),
                                                            
                                                            conditionalPanel(condition = "input.tCostsDependOnEvent == 'Yes'",
                                                                             numericInput("costEvent_t3", "Lifetime treatment costs associated with intervention 2 if the primary outcome occurs",
                                                                                          value = 100, min = NA, max = NA, step = 10),
                                                                             
                                                                             numericInput("costNotEvent_t3", "Lifetime treatment costs associated with intervention 2 if the primary outcome does not occur",
                                                                                          value = 100, min = NA, max = NA, step = 10) )
                                           ) # end cost inputs t3
                                           
                
                                         ) # end wellPanel t3
                                         ) # end conditional panel t3
                        ), # end column t3
                 
                 column(3,
                        ##########
                        # intervention 3 (formerly treatment 4)
                        ##########
                        
                        conditionalPanel(condition = "input.numberOfTreatments >= 4",
                                         
                              wellPanel(
                                h4("Intervention 3"),
                              # display if: numberOfTreatments >= 4
                              textInput("nameOf_t4", "Name of intervention 3", 
                                        value = "intervention 3"),
                              # display if: numberOfTreatments >= 4
                              numericInput("utilisation_t4", "Current utilisation of intervention 3 (%)",
                                           value = 0, min = 0, max = 100, step = 0.1),
                              
                              # display if: numberOfTreatments >= 4 & typeOfEndpoint != successFail 
                              selectInput("dist_t4", label = "Distribution of intervention 3 relative effects", 
                                          choices = c("Normal" = "norm", 
                                                      "Half Normal" = "halfNorm"),
                                          selected = "Normal"),
                              
                              
                              # normal dist inputs for t4
                              conditionalPanel(condition = "input.dist_t4 == 'norm'",
                                               numericInput("mu_t4", "Mean log odds / log hazard ratio for intervention 3",
                                                            value = 0, min = NA, max = NA, step = 0.05),
                                               
                                               numericInput("variance_t4", "Variance of log odds / log hazard ratio for intervention 3",
                                                            value = 0.25, min = NA, max = NA, step = 0.05)
                              ), # end normal dist inputs for t4
                              
                              conditionalPanel(condition = "input.dist_t4 == 'halfNorm'",
                                               selectInput("direction_t4", label = "Direction of distribution for intervention 3", 
                                                           choices = c("Always positive" = "alwaysPositive", 
                                                                       "Always negative" = "alwaysNegative"),
                                                           selected = "alwaysPositive")),
                              
                              numericInput("MCD_t4", "MCD for intervention 3",
                                           value = 0, min = NA, max = NA, step = 0.05),
                              
                              # Cost inputs for t4
                              conditionalPanel(condition = "input.outcomeExpression == 'netHealth'",
                                               
                                               conditionalPanel(condition = "input.tCostsDependOnEvent == 'No'",
                                                                numericInput("cost_t4", "Lifetime treatment costs associated with intervention 3",
                                                                             value = 100, min = NA, max = NA, step = 10) ),
                                               
                                               conditionalPanel(condition = "input.tCostsDependOnEvent == 'Yes'",
                                                                numericInput("costEvent_t4", "Lifetime treatment costs associated with intervention 3 if the primary outcome occurs",
                                                                             value = 100, min = NA, max = NA, step = 10),
                                                                
                                                                numericInput("costNotEvent_t4", "Lifetime treatment costs associated with intervention 3 if the primary outcome does not occur",
                                                                             value = 100, min = NA, max = NA, step = 10) )
                              ) # end cost inputs t4
                              
                              ) # end wellPanel t4
                        ) # end of conditionalPanel t4
                        ) # end intervention 3 column
               ) # end lower fluidRow
             ) # end of inputs fluidPage
      ), # end of inputs tab panel
    
    ##################
    # RESULTS page
    ##################
    tabPanel("Results", 
          
             br(),
             wellPanel(
               h4("Headline results and overview"),
               textOutput("introduceResearch"),
               br(),
               textOutput("ICERresult")
             ),
             br(),
             
             # heading 1
             h4("Type of analysis"),
             # if it is an RCT: 
             conditionalPanel(condition = "input.typeOfResearch != 'feasibility'",
                              p("This proposal is for a randomised controlled trial (RCT).
                                In this type of study, individuals are randomised to different treatments and the outcomes are compared accross the groups.")),
             # if it is a feasibility study:
             conditionalPanel(condition = "input.typeOfResearch == 'feasibility'",
                              p("This proposal is for a feasibility study. 
                                There are challenges and uncertainties associated with running a full trial. 
                                Due to these uncertainties it is unclear whether the larger follow-up trial is possible.
                                Research only impacts health in so far as it changes clinical practice. 
                                This feasibility trial is unlikely to generate enough evidence to justifying changing practice on its own. 
                                Therefore the impact of this feasibility trial on population health is through the potential future follow-up trial . 
                                If the follow-up trial is not possible the cost of funding it will not result in health benefit. 
                                Since the value of the feasibility trial depends on the follow-up trial, an evaluation of the future follow-up trial is required to value feasibility trial.")),
             br(),
             
             
             # if cost + QALY study: (require this extra bit)
             conditionalPanel(condition = "input.outcomeExpression == 'netHealth'",
                              h4("Summary of treatment costs"),
                              tableOutput("tableTreatmentCosts"),
                              textOutput("discussTableTreatmentCosts"),
                              br()),

             
             
             # heading 4
             h4("Value of implementing current evidence findings"),
             # table showing expected outcomes with each treatment
             tableOutput("tableEventsPerYear"),
             # text for general discussion about current information (common accross all models and endpoints?)
             textOutput("resultsCurrenInformation"),
             br(),
             
             
             # heading 5
             h4("Value of the proposed research"),
             # CONDITIONAL TEXT and HEADING: if feasibility study: (require this extra bit)
             conditionalPanel(condition = "input.typeOfResearch == 'feasibility'",
                              p("Understanding the value of a feasibility trial requires two steps.
                                First the value of the follow-up trial must be estimated. 
                                Second, this value must be adjusted for the fact that the follow-up trial may not take place."),
                              strong("Value of potential follow-up trial"),
                              br()),
             tableOutput("tableProbabilityMax"),
             # text for discussion about value of research (common accross all models and endpoints?)
             textOutput("resultsValueOfResearch"),
             # bug
             # problem in ui.R conditional planel
             # cannot make javaScript condition depend on results of VOI calcluation
             # must display this even if there is value in the research
             plotOutput("histVOIYear"),
             # **problem**
             # the probabilies do not match between the histogram output and the analysis
             # the histogram is probably wrong and needs to be changed.
             textOutput("discussHistVOIYear"),
             br(),
             textOutput("VOIresultsPart1"), # this section of the results is common to both RCT and Feas
             br(),
             # extra text for RCT results and interpretation
             conditionalPanel(condition = "input.typeOfResearch == 'RCT'",
                              textOutput("RCTVOIresults")),
             
             # extra text for Feasibility results and interpretation
             conditionalPanel(condition = "input.typeOfResearch == 'feasibility'",
                              strong("Adjust value of potential follow up trial to value the feasibility study"),
                              textOutput("FeasVOIresults")),
             
             # leave some space at the end of the page
             br(),
             br(),
             br(),
             br(),
             br()
            
             
             
    ), # end results tabPanel
    
    ##################
    # Write report page
    ##################
    # use textAreaInput to provide boxes so that analysts can justify their variable choices
    # 
    
    
    tabPanel("Write and Download Report",
             fluidPage(
               fluidRow(
                 column(4, "col 1"),
                 column(4, 
                        downloadButton("report", label = "Download report"),
                        p("Save file to your computer with a .doc extension")
                        )
               ) # end 1st Write Report fluidRow 
             ) # end Write Report fluidPage 
    ), # end write report tabPanel
    
    ##################
    # ABOUT page
    ##################
    tabPanel("About", 
             fluidPage(
               fluidRow(
                 column(4, "<code for about ui>"),
                 column(4, "next row")
               ) # end 1st About fluidRow  
               
             ) # end About fluidPage 
    ) # end About tabPanel
    
  ) # end App tabSetPanel
  
) # end App fluid page
) # end App shinyUi function










































#######################################################################################
# ZOMBIE CODE 
#######################################################################################




#textOutput("nameOf_t1"),

#textOutput("nameOf_t2"), 

#textOutput("nameOf_t3"), # conditional - is this a problem?

#textOutput("nameOf_t4"),# conditional - is this a problem?

#textOutput("nameOfOutcome"), # conditional - is this a problem?

# outputs

#textOutput("optimalTreatment" ) ,

#textOutput("expectedOutcomesPerYearoptimalTreatment"),

#textOutput("implementationValueExists"),        # new output

#textOutput("uncertaintyInCurrentEvidenceExists"),

#textOutput("probTreatment1isMax" ) ,

#textOutput("probTreatment2isMax" ) ,

#textOutput("probTreatment3isMax" ) ,

#textOutput("probTreatment4isMax" ) ,

#textOutput("popDuringResearch" ) ,

#textOutput("popAfterResearch" ) ,

#textOutput("popTotal" ) ,

#textOutput("popDuringFeasResearch" ) ,

#textOutput("popDuringDefinitiveResearch" ) ,

#textOutput("popAfterDefinitiveResearch" ) ,
#textOutput("valueOfResearchPerYear" ),

#textOutput("valueOfImplementationPerYear" ) ,

#textOutput("Cell_A" ) ,

#textOutput("Cell_C" ) ,

#textOutput("Cell_D" ) ,

#textOutput("maxvalueOfImplementation" ) ,

#textOutput("maxvalueOfResearch" ) ,

#textOutput("healthOpportunityCostsOfResearch" ) ,

#textOutput("expectedCostResearchFunder" ) ,                # unique Feas

#textOutput("valueOfResearchWithCurrentImplementation" ) ,

#textOutput("valueOfResearchWithPerfectImplementation" ) ,

#textOutput("valueOfCertainResearchWithPerfectImplementation" ) ,  # unique feas

#textOutput("ICER_ResearchWithCurrentImplementation" ) ,

#textOutput("ICER_ResearchWithPerfectImplementation" ) ,

#textOutput("valuePer15KResearchSpend"),

#textOutput("absoluteExpectedHealthOutcomesFromResearchProject"),

#textOutput("costResearchFunderFeas"),

#textOutput("costResearchFunderDefinitive"),

#textOutput("probabilityOfDefinitiveResearch")

#textOutput("test1"),

#textOutput("test2"),

#textOutput("test3")





