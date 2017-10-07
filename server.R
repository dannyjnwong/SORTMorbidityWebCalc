
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(xtable)

procedures <- read.csv("SNAP2_procedurelist.csv", stringsAsFactors = FALSE)

shinyServer(function(input, output) {

  output$ProcedureTable <- renderTable({

    tab <- procedures %>% filter(SurgeryProcedure %in% input$Procedure) %>%
      select(SurgeryProcedure, SurgeryProcedureSeverity) %>%
      mutate(ASA = input$ASA) %>%
      mutate(Specialty = input$Specialty) %>%
      mutate(Malignancy = input$Malignancy) %>%
      mutate(Age = input$Age) %>%
      mutate(SORT_Logit_Score = ((ASA == "2") * 0.332 +
               (ASA == "3") * 1.140 + 
               (ASA == "4") * 1.223 +
               (ASA == "5") * 1.223 +
               (Specialty == "Colorectal") * 1.658 +
               (Specialty == "Upper GI") * -0.929 +
               (Specialty == "Vascular") * 0.296 +
               (Specialty == "Bariatric") * -1.065 +
               (Specialty == "Other") * 0.181 +
               (SurgeryProcedureSeverity == "Xma") * 1.238 + 
               (SurgeryProcedureSeverity == "Com") * 1.238 +
               (Malignancy == "Yes") * 0.897 + 
               (Age == "65-79") * 0.118 + 
               (Age == ">80") * 0.550 -
               3.228)) %>%
      mutate(POMS_Risk = paste0(signif((arm::invlogit(SORT_Logit_Score) * 100), digits = 4),"%")) %>%
      mutate(Low_grade = paste0(signif((arm::invlogit(SORT_Logit_Score * 1.008 - 0.316) * 100), digits = 4), "%")) %>%
      mutate(High_grade = paste0(signif((arm::invlogit(SORT_Logit_Score * 0.827 - 0.874) * 100), digits = 4), "%")) %>%
      mutate(Day14 = paste0(signif((arm::invlogit(SORT_Logit_Score * 0.894 - 1.478) * 100), digits = 4), "%")) %>%
      mutate(Day21 = paste0(signif((arm::invlogit(SORT_Logit_Score * 1.081 - 2.327) * 100), digits = 4), "%")) %>%
      mutate(Day28 = paste0(signif((arm::invlogit(SORT_Logit_Score * 1.048 - 2.770) * 100), digits = 4), "%")) %>%
      select(SORT_Logit_Score:Day28)
    
    xtable(tab)
  
  })
  

})
