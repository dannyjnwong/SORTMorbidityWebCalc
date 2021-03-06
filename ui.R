
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)

procedures_list <- read.csv("SNAP2_procedurelist.csv", stringsAsFactors = FALSE)

# Sidebar with a number of inputs
shinyUI(fluidPage(

  # Application title
  titlePanel("SORT-morbidity Web Calculator v.0.4"),
  p("Disclaimer: The SORT uses some information about patient health and the planned surgical procedure to provide an estimate of the risk of death within 30 days of an operation. The SORT has been extended to predict postoperative morbidity (SORT-morbidity). For more information about this, please read the British Journal of Anaesthesia paper (link below)."), 
  p("The percentages provided by the calculator are only estimates taking into account the general risks of the procedure and some information about the patient, but should not be confused with a patient-specific estimate in an individual case. As with all risk prediction tools, not every factor which may affect outcome can be included, and there may well be other patient-specific and surgical factors which may influence the risk of death significantly. This resource is not intended to be used in isolation for clinical decision making and should not replace the advice of a healthcare professional about the potential risks or benefits of a planned procedure. The author of this calculator will not be held responsible for decisions made by healthcare professionals or patients which are based on the estimates provided by the SORT, as these estimates are provided only for the purposes of background information."),
  p("Patients should always consult a healthcare professional in decision-making about their health and treatment."),
  a(href="https://dannyjnwong.shinyapps.io/SORTWebCalc", "**Click here for a newer version of this calculator!**"),
  p(""),
  a(href="https://doi.org/10.1093/bja/aex117", "D. J. N. Wong, C. M. Oliver, S. R. Moonesinghe; Predicting postoperative morbidity in adult elective surgical patients using the Surgical Outcome Risk Tool (SORT). Br J Anaesth 2017; 119 (1): 95-105. doi: 10.1093/bja/aex117"),
  p("MIT License; Copyright (c) 2017-2018 Danny Jon Nian Wong."), 
  a(href="https://github.com/dannyjnwong/SORTMorbidityWebCalc", "Source code available here on Github."),

  # Sidebar with inputs
  sidebarLayout(
    sidebarPanel(
      
      #Surgical Procedure
      selectizeInput("Procedure", "Search for Procedure",
                     choices = list("Type in a procedure" = "", "Procedures" = procedures_list$SurgeryProcedure)),
      
      #ASA
      radioButtons("ASA", "ASA-PS class", 
                   choices = c("1", "2", "3", "4", "5")),
      
      #Urgency (not included in SORT-morbidity)
      #radioButtons("Urgency", "Surgical urgency", 
      #             choices = c("Elective", "Expedited", "Urgent", "Immediate")),
      
      #Specialty
      radioButtons("Specialty", "Surgical specialty", 
                   choices = c("Orthopaedic", "Colorectal", "Upper GI", "Vascular", "Bariatric", "Other")),
      
      #Malignancy
      radioButtons("Malignancy", "Does the patient have a malignancy?", 
                   choices = c("Yes", "No")),
      
      #Age
      radioButtons("Age", "What is the patient's age?",
                   choices = c("<65", "65-79", ">80"))
      
    ),

    # Show a table output
    mainPanel(
      DTOutput("POMSTable")
    )
  )
))
