aaa_mort_30 = reactive({
	proc_1 = ifelse(input$proc_a == 1, 1, 0)
	proc_2 = ifelse(input$proc_a == 2, 1, 0)
	age_1 = ifelse(input$age_a > 75, 1, 0)
	sex_1 = ifelse(input$sex_a == 1, 1, 0)
	cad_1 = ifelse(input$cad_a > 0, 1, 0)
	cvd_1 = ifelse(input$cvd_a == 1, 1, 0)
	copd_1 = ifelse(input$copd_a > 0, 1, 0)
	ckd_1 = ifelse(input$ckd_a == 1, 1, 0)
	ckd_2 = ifelse(input$ckd_a == 2, 1, 0)
	diam_1 = ifelse(input$diam_a == 1, 1, 0)
	term = -6.76 + 1.08*proc_1 + 1.905*proc_2 + 0.78*age_1 + 0.69*sex_1 + 0.56*cad_1 + 0.71*cvd_1 + 0.95*copd_1 + 
		0.89*ckd_1 + 1.31*ckd_2 + 0.91*diam_1
	aaa_mort_30 = round(
		100/(1+exp(-term)),
		2)
	return(list(aaa_mort_30, "Eslami et al"))
})

aaa_mort_30_status = reactive({
	ifelse(aaa_mort_30()[[1]] > 5, "red", 
				 ifelse(aaa_mort_30()[[1]] < 2, "green", "orange"))
})

output$aaa_mort_30_Box <- renderInfoBox({
	infoBox(
		"30 Day Mortality", h1(aaa_mort_30()[[1]]), aaa_mort_30()[[2]], icon = icon("bar-chart"),
		color = "aqua", fill=TRUE
	)
})
