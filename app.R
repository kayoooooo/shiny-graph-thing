library(shiny)
library(ggplot2)
library(lattice)

data = read.csv(file = "Multiple_Ascending_Dose_Dataset3.csv")

ui = fluidPage(

  titlePanel("Dashboard"),

  sidebarLayout(

    sidebarPanel(

      #sliderInput(inputId = "rang",
       #           label = "Range:",
        #          min = min(data$TIME),
         #         max = max(data$TIME),
          #        value = median(data$TIME)),
      sliderInput(inputId = "id",
                  label = "ID:",
                  min = 1,
                  max = max(data$ID),
                  value = 30,
                  step = 1),
      
      selectInput("TumorSort", "Sort by:", choices = c("AMT", "ID", "SEX"), selected = "AMT"),
    ),
    

    mainPanel(
      plotOutput("AEPlot"), 
      plotOutput("TumorPlot"),
      plotOutput("test")
      
    )
  )
)





server = function(input, output) {
  dat = read.csv(file = "Multiple_Ascending_Dose_Dataset3.csv")
    #print(dat[dat$ID == 7,][AE == "Severe"])
    output$AEPlot = renderPlot( xyplot( tumor ~ TIME, groups = ID, data = dat[dat$ID == input$id,], xlab = "Time", ylab = "Dose", panel = function(...) {
      panel.xyplot(...)
      panel.abline(v = dat[dat$ID == input$id,][dat$AE == "Severe",]$TIME, col = "red")
      panel.abline(v = dat[dat$ID == input$id,][dat$AE == "Mild",]$TIME, col = "yellow")
    }))
    output$TumorPlot <- renderPlot({
      ggplot(dat, aes(x = TIME, y = tumor, color = ID)) + geom_point()+ facet_wrap( ~ get(input$TumorSort), ncol=3)
      #xyplot( tumor ~ TIME | input$TumorSort, groups = ID, data = dat, xlab = "Time", ylab = "Tumor Size")
    #xyplot(AMT ~ TIME | ID, groups = ID, data = dat[dat$ID == input$id,], xlab = "Time", ylab = "Dose Amount")
    })

}

shinyApp(ui = ui, server = server)