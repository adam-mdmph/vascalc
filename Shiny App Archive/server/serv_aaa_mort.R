# Calculate 30d mortality if patient were to have an evar
evar_mort_30 = reactive({
	age_1 = ifelse(input$age_a > 75, 1, 0)
	sex_1 = ifelse(input$sex_a == 1, 1, 0)
	cad_1 = ifelse(input$cad_a > 0, 1, 0)
	cvd_1 = ifelse(input$cvd_a == 1, 1, 0)
	copd_1 = ifelse(input$copd_a > 0, 1, 0)
	ckd_1 = ifelse(input$ckd_a == 1, 1, 0)
	ckd_2 = ifelse(input$ckd_a == 2, 1, 0)
	diam_1 = ifelse(input$diam_a == 1, 1, 0)
	evar_term = -6.76 + 0.78*age_1 + 0.69*sex_1 + 0.56*cad_1 + 0.71*cvd_1 + 0.95*copd_1 + 
		0.89*ckd_1 + 1.31*ckd_2 + 0.91*diam_1
	evar_mort_30 = round(
		100/(1+exp(-evar_term)),
		2)
	return(list(evar_mort_30, "Eslami et al"))
})

evar_mort_30_status = reactive({
	ifelse(evar_mort_30()[[1]] > 5, "red", 
				 ifelse(evar_mort_30()[[1]] < 2, "green", "orange"))
})

output$evar_mort_30_Box <- renderInfoBox({
	infoBox(
		"EVAR 30 Day Mortality (%)", h1(evar_mort_30()[[1]]), evar_mort_30()[[2]],
		color = "aqua", fill=TRUE
	)
})

iraaa_mort_30 = reactive({
	age_1 = ifelse(input$age_a > 75, 1, 0)
	sex_1 = ifelse(input$sex_a == 1, 1, 0)
	cad_1 = ifelse(input$cad_a > 0, 1, 0)
	cvd_1 = ifelse(input$cvd_a == 1, 1, 0)
	copd_1 = ifelse(input$copd_a > 0, 1, 0)
	ckd_1 = ifelse(input$ckd_a == 1, 1, 0)
	ckd_2 = ifelse(input$ckd_a == 2, 1, 0)
	diam_1 = ifelse(input$diam_a == 1, 1, 0)
	iraaa_term = -6.76 + 1.08 + 0.78*age_1 + 0.69*sex_1 + 0.56*cad_1 + 0.71*cvd_1 + 0.95*copd_1 + 
		0.89*ckd_1 + 1.31*ckd_2 + 0.91*diam_1
	iraaa_mort_30 = round(
		100/(1+exp(-iraaa_term)),
		2)
	return(list(iraaa_mort_30, "Eslami et al"))
})

iraaa_mort_30_status = reactive({
	ifelse(iraaa_mort_30()[[1]] > 5, "red", 
				 ifelse(iraaa_mort_30()[[1]] < 2, "green", "orange"))
})

output$iraaa_mort_30_Box <- renderInfoBox({
	infoBox(
		"IR AAA 30 Day Mortality (%)", h1(iraaa_mort_30()[[1]]), iraaa_mort_30()[[2]],
		color = "aqua", fill=TRUE
	)
})

sraaa_mort_30 = reactive({
	age_1 = ifelse(input$age_a > 75, 1, 0)
	sex_1 = ifelse(input$sex_a == 1, 1, 0)
	cad_1 = ifelse(input$cad_a > 0, 1, 0)
	cvd_1 = ifelse(input$cvd_a == 1, 1, 0)
	copd_1 = ifelse(input$copd_a > 0, 1, 0)
	ckd_1 = ifelse(input$ckd_a == 1, 1, 0)
	ckd_2 = ifelse(input$ckd_a == 2, 1, 0)
	diam_1 = ifelse(input$diam_a == 1, 1, 0)
	sraaa_term = -6.76 + 1.905 + 0.78*age_1 + 0.69*sex_1 + 0.56*cad_1 + 0.71*cvd_1 + 0.95*copd_1 + 
		0.89*ckd_1 + 1.31*ckd_2 + 0.91*diam_1
	sraaa_mort_30 = round(
		100/(1+exp(-sraaa_term)),
		2)
	return(list(sraaa_mort_30, "Eslami et al"))
})

sraaa_mort_30_status = reactive({
	ifelse(sraaa_mort_30()[[1]] > 5, "red", 
				 ifelse(sraaa_mort_30()[[1]] < 2, "green", "orange"))
})

output$sraaa_mort_30_Box <- renderInfoBox({
	infoBox(
		"SR AAA 30 Day Mortality (%)", h1(sraaa_mort_30()[[1]]), sraaa_mort_30()[[2]],
		color = "aqua", fill=TRUE
	)
})
