
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(tidyr)
library(DT)

procedures <- read.csv("SNAP2_procedurelist.csv", stringsAsFactors = FALSE)

shinyServer(function(input, output) {
  
  morbid_tab <- reactive({
    
    procedures %>% filter(SurgeryProcedure %in% input$Procedure) %>%
      select(SurgeryProcedure, SurgeryProcedureSeverity) %>%
      mutate(ASA = input$ASA) %>%
      mutate(Specialty = input$Specialty) %>%
      mutate(Malignancy = input$Malignancy) %>%
      mutate(Age = input$Age) %>%
      mutate(SORT_morbid_logit = ((ASA == "2") * 0.332 +
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
      mutate(POMS_Risk = arm::invlogit(SORT_morbid_logit)) %>%
      mutate(Low_grade = arm::invlogit(SORT_morbid_logit * 1.008 - 0.316)) %>%
      mutate(High_grade = arm::invlogit(SORT_morbid_logit * 0.827 - 0.874)) %>%
      mutate(Day14 = arm::invlogit(SORT_morbid_logit * 0.894 - 1.478)) %>%
      mutate(Day21 = arm::invlogit(SORT_morbid_logit * 1.081 - 2.327)) %>%
      mutate(Day28 = arm::invlogit(SORT_morbid_logit * 1.048 - 2.770)) %>%
      select(POMS_Risk:Day28) %>%
      rename(`D7 POMS` = "POMS_Risk",
             `D7 Low-grade POMS` = "Low_grade",
             `D7 High-grade POMS` = "High_grade",
             `D14 POMS` = "Day14",
             `D21 POMS` = "Day21",
             `D28 POMS` = "Day28")
    
  })

  output$POMSTable <- renderDT({
    
    morbid_tab() %>%
      gather(key = "Outcome", value = "Risk") %>%
      datatable(options = list(dom = 't')) %>% 
      formatPercentage(digits = 2,
                       c("Risk"))
    
  })

})
