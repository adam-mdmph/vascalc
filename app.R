#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Survival Predication after revascularization for patients with CLTI"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("age", "Patient Age", 
            						 choices = list("<60 Years" = 0,
            						 							 "60-70 Years" = 1,
            						 							 "71-80 Years" = 2,
            						 							 ">80 Years" = 3)),
            radioButtons("race", "Patient Race",
            						 choices = list("White" = 0,
            						 							 "Non-white" = 1)),
            radioButtons("indication", "Indication for Procedure",
            						 choices = list("Rest pain" = 0,
            						 							 "Tissue loss" = 1)),
            radioButtons("tob", "Smoking History",
            						 choices = list("Prior History" = 1,
            						 							 "Current" = 2)),
            radioButtons("cad", "History of coronary artery disease",
            						 choices = list("None" = 0,
            						 							 "History of MI, asymptomatic or stable angina" = 1,
            						 							 "Unstable angina/MI within 6 months" = 2)),
            radioButtons("chf", "Congestive Heart Failure",
            						 choices = list("No" = 0,
            						 							 "Yes" = 1)),
            radioButtons("copd", "COPD",
            						 choices = list("None" = 0,
            						 							 "Not treated or on medication" = 1,
            						 							 "Home oxygen" = 2)),
            radioButtons("ckd", "Chronic kidney disease stage",
            						choices = list("1 (GFR >90 mL/min/1.73 m2)" = 0,
            													 "2 (GFR 60-89 mL/min/1.73 m2)" = 1,
            													 "3 (GFR 30-59 mL/min/1.73 m2)" = 2,
            													 "4 (GFR 15-29 mL/min/1.73 m2)" = 3,
            													 "5 (GFR <15 mL/min/1.73 m2)" = 4)),
            radioButtons("amb", "Ambulation Status",
            						 choices = list("Independent" = 0,
            						 							 "With Assistance" = 1,
            						 							 "Wheelchair bound" = 2,
            						 							 "Bedbound" = 3)),
            radioButtons("statin", "Pre-opreative medications: Statin",
            						 choices = list("No" = 0,
            						 							 "Yes" = 1))
        ),

       #Display calculated probability
        mainPanel(
           textOutput("mort30"),
           print("“Survival Prediction in Patients with Chronic Limb-Threatening Ischemia Who Undergo Infrainguinal Revascularization | Elsevier Enhanced Reader.” Accessed December 20, 2021. https://doi.org/10.1016/j.ejvs.2019.04.009.
")
        )
    )
)

# Define server logic to calculate risk score
server <- function(input, output) {
	
	clti_mort_30 = reactive({
		age_1 = ifelse(input$age == 1, 1, 0)
		age_2 = ifelse(input$age == 2, 1, 0)
		age_3 = ifelse(input$age == 3, 1, 0)
		race_1 = ifelse(input$race == 1, 1, 0)
		indication_1 = ifelse(input$indication == 1, 1, 0)
		tob_1 = ifelse(input$tob == 1, 1, 0)
		tob_2 = ifelse(input$tob == 2, 1, 0)
		cad_1 = ifelse(input$cad == 1, 1, 0)
		cad_2 = ifelse(input$cad == 2, 1, 0)
		chf_0 = ifelse(input$chf == 0, 1, 0)
		chf_1 = ifelse(input$chf == 1, 1, 0)
		copd_1 = ifelse(input$copd == 1, 1, 0)
		copd_2 = ifelse(input$copd == 2, 1, 0)
		ckd_1 = ifelse(input$ckd == 1, 1, 0)
		ckd_2 = ifelse(input$ckd == 2, 1, 0)
		ckd_3 = ifelse(input$ckd == 3, 1, 0)
		ckd_4 = ifelse(input$ckd == 4, 1, 0)
		amb_1 = ifelse(input$amb == 1, 1, 0)
		amb_2 = ifelse(input$amb == 2, 1, 0)
		amb_3 = ifelse(input$amb == 3, 1, 0)
		statin_1 = ifelse(input$statin == 1, 1, 0)
		term = age_1*0.51 + age_2*0.97 + age_3*1.5 + race_1*-0.38 + indication_1*0.42 + cad_1*0.25 + cad_2*0.78 + 
			chf_1*0.53 + copd_1*0.27 + copd_2*0.86 + ckd_1*-0.05 + ckd_2*0.26 + ckd_3*0.76 + ckd_4*1.45 + amb_1*0.41 + 
			amb_2*0.60 + amb_3*1.34 + statin_1*-0.29
		clti_mort_30 = round(
			(100/(1+2.71^-term)),
			0)
		return(list(clti_mort_30))
	})

	output$mort30 <- renderText({
		clti_mort_30()[[1]]
	})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
