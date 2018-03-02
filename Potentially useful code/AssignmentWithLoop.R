
VOIResults$optimalTreatment <- resultsHolder()$optimalTreatment
VOIResults$probTreatment1isMax <- resultsHolder()$probTreatment1isMax 



names <- list(x = "x", y = "y")



for(i in 1:6) { #-- Create objects  'r.1', 'r.2', ... 'r.6' --
  nam <- paste("r", i, sep = ".")
  assign(nam, 1:i)
}
ls(pattern = "^r..$")



outputnames <- c("optimalTreatment", "probTreatment1isMax")
resultsHolder <- list(optimalTreatment = "t1", probTreatment1isMax = 0.2)

for(i in 1:length(outputnames)) { #-- Create objects  'r.1', 'r.2', ... 'r.6' --
  nam <- paste("VOIResults", outputnames[i], sep = "$")
  assign(nam, resultsHolder[[i]])
}






outputnames <- c("optimalTreatment", "probTreatment1isMax", 
                 "probTreatment2isMax", 
                 "probTreatment3isMax", 
                 "probTreatment4isMax",
                 "popDuringResearch",
                 "popAfterResearch",
                 "PopTotal", 
                 "histVOIYear", 
                 "ListForhistVOIYear",
                 "valueOfResearchPerYear",
                 "valueOfImplementationPerYear",
                 "Cell_A",
                 "Cell_C",
                 "Cell_D",
                 "maxvalueOfImplementation",
                 "maxvalueOfResearch",
                 "healthOpportunityCostsOfResearch",
                 "valueOfResearchWithCurrentImplementation",
                 "valueOfResearchWithPerfectImplementation",
                 "ICER_ResearchWithCurrentImplementation",
                 "ICER_ResearchWithPerfectImplementation",
                 "valuePer15KResearchSpend")

for(i in 1:length(outputnames)) { 
  nam <- paste("VOIResults", outputnames[i], sep = "$")
  assign(nam, resultsHolder()[[i]])
}


