library(shiny)
library(bslib)
library(LexisPlotR)
library(tidyverse)
library(colorspace)

s19 <- sequential_hcl(19, palette = "BluYl")

ui <- page_sidebar(
  title = "CYDRP retrospective cohort study design tool",
  sidebar = sidebar(
    sliderInput(
      "year",
      "Set year range",
      min = 1986,
      max = 2019,
      value = c(1999,2019),
      sep = ""
    ),
    sliderInput(
      "age",
      "Set age range",
      min = 0,
      max = 30,
      value = c(0,18)
    ),
    sliderInput(
      "cohort",
      "Set birth cohort range",
      min = 1986,
      max = 2019,
      value = c(1999,2004),
      sep = ""
    ),
    accordion(open = FALSE,
      accordion_panel(title = "Health Data",
        checkboxInput("peri", "Perinatal Trends Registry"),
        checkboxInput("pcis", "Primary Care Activity"),
        checkboxInput("edac", "Emergency Department Activity"),
        checkboxInput("inac", "Inpatient Activity"),
        checkboxInput("hotr", "Hospital Trends"),
        checkboxInput("mhac", "Mental Health Activity"),
        checkboxInput("hhur", "Hearing Health (Urban)"),
        checkboxInput("hhre", "Hearing Health (Remote)"),
        checkboxInput("mehr", "Menzies Ear Health Research")
      ),
      accordion_panel(title = "Education Data",
        checkboxInput("aedc", "AEDI/C"),
        checkboxInput("pssi", "Public School Enrolment & Student Information"),
        checkboxInput("psat", "Public School Attendance"),
        checkboxInput("npln", "NAPLAN")
      ),
      accordion_panel(title = "Welfare and Safety Data",
        checkboxInput("chpr", "Child Protection Involvement"),
        checkboxInput("poli", "Police (PROMIS)"),
        checkboxInput("ijis", "Charged Offences and Court Outcomes (IJIS)"),
        checkboxInput("ioms", "Corrections (IOMS)")
      ),
      accordion_panel(title = "Mortality",
        checkboxInput("acrc", "ACR COD URF"),
        checkboxInput("ntdc", "Death Certificates (NT BDM)")
      )
    )
  ),
  plotOutput("lexis")
)

server <- function(input, output) {
  output$lexis <- renderPlot({
    l <- lexis_grid(year_start = input$year[1], year_end = input$year[2], age_start = input$age[1], age_end = input$age[2])

    if(input$peri) {
      peri_start <- paste(input$year[1], "-01-01", sep = "")
      peri_end <- paste(min(2017, input$year[2]), "-01-01", sep = "")
      l <- lexis_polygon(lg = l, x = c(peri_start, peri_start, peri_end, peri_end), y = c(0, 1, 1, 0), fill = s19[12])
    }
    if(input$pcis) {
      l <- lexis_year(lg = l, fill = s19[19], max(input$year[1], 2010), 2019 - max(input$year[1], 2010))
    }
    if(input$edac) {
      l <- lexis_year(lg = l, fill = s19[2], max(input$year[1], 2000), 2019 - max(input$year[1], 2000))
    }
    if(input$inac) {
      l <- lexis_year(lg = l, fill = s19[18], max(input$year[1], 2000), 2019 - max(input$year[1], 2000))
    }
    if(input$hotr) {
      l <- lexis_year(lg = l, fill = s19[3], max(input$year[1], 1992), 2019 - max(input$year[1], 1992))
    }
    if(input$mhac) {
      l <- lexis_year(lg = l, fill = s19[17], max(input$year[1], 2004), 2019 - max(input$year[1], 2004))
    }
    if(input$hhur) {
      l <- lexis_year(lg = l, fill = s19[4], max(input$year[1], 2004), 2019 - max(input$year[1], 2004))
    }
    if(input$hhre) {
      l <- lexis_year(lg = l, fill = s19[16], max(input$year[1], 2004), 2019 - max(input$year[1], 2004))
    }
    if(input$mehr) {
      l <- lexis_year(lg = l, fill = s19[5], max(input$year[1], 1992), 2019 - max(input$year[1], 1992))
    }
    if(input$aedc) {
      l <- lexis_polygon(lg = l, x = c("2009-01-01", "2009-01-01", "2011-01-01", "2011-01-01"), y = c(5, 6, 6, 5), fill = s19[15])
      l <- lexis_polygon(lg = l, x = c("2012-01-01", "2012-01-01", "2013-01-01", "2013-01-01"), y = c(5, 6, 6, 5), fill = s19[15])
      l <- lexis_polygon(lg = l, x = c("2015-01-01", "2015-01-01", "2016-01-01", "2016-01-01"), y = c(5, 6, 6, 5), fill = s19[15])
      l <- lexis_polygon(lg = l, x = c("2018-01-01", "2018-01-01", "2019-01-01", "2019-01-01"), y = c(5, 6, 6, 5), fill = s19[15])
    }
    if(input$pssi) {
      pssi_start <- paste(max(2005, input$year[1]), "-01-01", sep = "")
      pssi_end <- paste(input$year[2], "-01-01", sep = "")
      l <- lexis_polygon(lg = l, x = c(pssi_start, pssi_start, pssi_end, pssi_end), y = c(5, 18, 18, 5), fill = s19[6])
    }
    if(input$psat) {
      pssi_start <- paste(max(2005, input$year[1]), "-01-01", sep = "")
      pssi_end <- paste(input$year[2], "-01-01", sep = "")
      l <- lexis_polygon(lg = l, x = c(pssi_start, pssi_start, pssi_end, pssi_end), y = c(5, 18, 18, 5), fill = s19[14])
    }
    if(input$npln) {

    }
    if(input$chpr) {
      chpr_start <- paste(max(1999, input$year[1]), "-01-01", sep = "")
      chpr_end <- paste(input$year[2], "-01-01", sep = "")
      l <- lexis_polygon(lg = l, x = c(chpr_start, chpr_start, chpr_end, chpr_end), y = c(0, 18, 18, 0), fill = s19[13])
    }
    if(input$poli) {
      poli_start <- paste(max(2014, input$year[1]), "-01-01", sep = "")
      poli_end <- paste(input$year[2], "-01-01", sep = "")
      l <- lexis_polygon(lg = l, x = c(poli_start, poli_start, poli_end, poli_end), y = c(10, input$age[2], input$age[2], 10), fill = s19[8])
    }
    if(input$ijis) {
      ijis_start <- paste(max(1997, input$year[1]), "-01-01", sep = "")
      ijis_end <- paste(input$year[2], "-01-01", sep = "")
      l <- lexis_polygon(lg = l, x = c(ijis_start, ijis_start, ijis_end, ijis_end), y = c(10, input$age[2], input$age[2], 10), fill = s19[12])
    }
    if(input$ioms) {
      ioms_start <- paste(max(2009, input$year[1]), "-01-01", sep = "")
      ioms_end <- paste(input$year[2], "-01-01", sep = "")
      l <- lexis_polygon(lg = l, x = c(ioms_start, ioms_start, ioms_end, ioms_end), y = c(10, input$age[2], input$age[2], 10), fill = s19[9])
    }
    if(input$acrc) {
      l <- lexis_year(lg = l, fill = s19[11], max(input$year[1], 2006), 2017 - max(input$year[1], 2006))
    }
    if(input$ntdc) {
      l <- lexis_year(lg = l, fill = s19[10], max(input$year[1], 1986), 2019 - max(input$year[1], 1986))
    }
    
    cohort_start <- input$cohort[1] + input$age[1]
    l <- lexis_cohort(lg = l, cohort = cohort_start, delta = input$cohort[2]-input$cohort[1], fill = "orange")
    l <- l + geom_hline(yintercept=0, color="red")
    l
  })
}

shinyApp(ui = ui, server = server)