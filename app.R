

library(shiny)
library(shinydashboard)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

      # Navbar structure
        navbarPage("VasCalc",
    	    
        	#Panel for Peripheral Vascular Disease
        	tabPanel("CLTI", fluid = TRUE,
        					 
        					 # Sidebar for inputing patient characteristics
    				       sidebarLayout(
    				       	source(file.path("ui", "ui_input_pvd.R"))$value,
    					 				
    				        # Display calculated probability
    					      mainPanel(
    					      	fluidRow(
    					      		column(width = 4,
    					      					 print("testing")
    					      		)
    				          )
    					      )
    		          )
        	),
    			#Panel for Aortic Aneurysm
    			tabPanel("Aortic Aneurysm", fluid = TRUE,
    							 sidebarLayout(
    							 	
    							 	
    							 	# Sidebar for inputing patient characteristics
    							 	source(file.path("ui", "ui_input_aorta.R"))$value,
    							 	
    							  # Display calculated probabilities
    							  mainPanel(
    							  	fluidRow(
    							  		column(width = 4,
    							  					 infoBoxOutput("evar_mort_30_Box", width=12)),
    							  		column(width = 4,
    							  					 infoBoxOutput("iraaa_mort_30_Box", width=12)),
    							  		column(width = 4,
    							  					 infoBoxOutput("sraaa_mort_30_Box", width=12))
    							  	)
    							  )
    							 )
    			 ),
    			 # Panel for About
    			 tabPanel("About", fluid = TRUE,
    			 				 print("This app is a collaborative effort to aggregate published risk models into an interface
    			 				 			for busy active clinicians.  This is a decision aide and is not intended to supercede clinician
    			 				 			or patient preference.  All models are wrong but some are useful.  For questions, comments or
    			 				 			suggestions please visit the github page: "),
    			 				 a(href="https://github.com/adam-mdmph/vascalc",
    			 				 	"adam_mdmph/vascalc", target="_blank"),br())
    		 )
       )

        

# Define server logic to calculate risk score
server <- function(input, output, session) {
	
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
			100/(1+exp(-term)),
			0)
		return(list(clti_mort_30, "Simms et al"))
	})
	
	output$clti_mort_30_Box <- renderInfoBox({
		infoBox(
			"30 Day Mortality", h1(clti_mort_30()[[1]]), clti_mort_30()[[2]], icon = icon("bar-chart"),
			color = "aqua", fill=TRUE
		)
	})
	
	# Calculate AAA 30 day mortality
	source(file.path("server", "serv_aaa_mort.R"), local = TRUE)$value
  
}

# Run the application 
shinyApp(ui = ui, server = server)
