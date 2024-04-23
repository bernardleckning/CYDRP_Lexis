library(shiny)
library(bslib)
library(LexisPlotR)
library(tidyverse)

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
    checkboxInput("peri", "Perinatal Trends Registry"),
    checkboxInput("ia", "Inpatient Activity"),
    checkboxInput("poli", "Police (PROMIS)")
  ),
  plotOutput("lexis")
)

server <- function(input, output) {
  output$lexis <- renderPlot({
    l <- lexis_grid(year_start = input$year[1], year_end = input$year[2], age_start = input$age[1], age_end = input$age[2])
    if(input$peri) {
      peri_start <- paste(input$year[1], "-01-01", sep = "")
      peri_end <- paste(min(2017, input$year[2]), "-01-01", sep = "")
      l <- lexis_polygon(lg = l, x = c(peri_start, peri_start, peri_end, peri_end), y = c(0, 1, 1, 0), fill = "#0E3F5C")
    }
    if(input$ia) {
      l <- lexis_year(lg = l, fill = "#144D65", max(input$year[1], 2000), 2019-max(input$year[1], 2000))
    }
    if(input$poli) {
      poli_start <- paste(max(2014, input$year[1]), "-01-01", sep = "")
      poli_end <- paste(input$year[2], "-01-01", sep = "")
      l <- lexis_polygon(lg = l, x = c(poli_start, poli_start, poli_end, poli_end), y = c(10, input$age[2], input$age[2], 10), fill = "#1D5B6E")
    }
    cohort_start <- input$cohort[1] + input$age[1]
    l <- lexis_cohort(lg = l, cohort = cohort_start, delta = input$cohort[2]-input$cohort[1], fill = "orange")
    l <- l + geom_hline(yintercept=0, color="green")
    l
  })
}

shinyApp(ui = ui, server = server)