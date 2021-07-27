library(shiny)
library(DT)
library(plyr)
library(dplyr)
library(mefa)
library(mrgsolve)
library(Rcpp)
library(ggplot2)
library(dmutate)
library(vroom)
library(foreign)
library(lattice)

#setwd("M:/Summer_Intern/")

ui<-fluidPage(
  titlePanel("DOSE MODIFICATION AND ADVERSE EVENTS GRAPHICAL TOOL"),
  
  navlistPanel("Graphical exploration",
               tabPanel("Import Data",
                        fileInput("file1", "Dose Data", buttonLabel = "Upload..."),
                        fileInput("file2", "AE Data", buttonLabel = "Upload..."),
                        fileInput("file3", "Tumor Data", buttonLabel = "Upload..."),
                        radioButtons("format", "Data format",
                                     choices = c("csv","xpt"),
                                     selected = "csv"),
                        numericInput("rows", "Rows to preview", 10, min = 1),
                        actionButton(input="go",label="Read data in apps and preview")
               ),
               
               tabPanel("DOSE",
                        actionButton(input="go1",label="View Full Data"),
                        actionButton(input="view1",label="Show graph"),
                        uiOutput("TrtOutput"),
                        selectInput("DoseInput", "Dose", choices = NULL),
                        tableOutput("head1"),
                        dataTableOutput("full1"),
                        plotOutput("pl", click = "plot_click"),
                        verbatimTextOutput("info"),
                        verbatimTextOutput("selected_rows")
               ),
               tabPanel("AE",
                        actionButton(input="view2",label="Show graph"),
                        tableOutput("head2"),
                        dataTableOutput("full2")
               ),
               tabPanel("Tumor Size",
                        actionButton(input="view3",label="Show graph"),
                        selectInput("TumorSort", "Sort by:", choices = c("AMT", "ID"), selected = "AMT"),
                        tableOutput("head3"),
                        dataTableOutput("full3"),
                        plotOutput("plot3", "500px", "500px")
              )
               ),
  
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }")
)





server <- function(input, output,session) {
  
  u1 <- reactiveValues(doTable = FALSE)
  u2 <- reactiveValues(doTable = FALSE)  
  u3 <- reactiveValues(doTable = FALSE) 
  
  observeEvent(input$go1, {  u1$doTable <- input$go1  })
    # 0 will be coerced to FALSE
    # 1+ will be coerced to TRUE
  observeEvent(input$go2, {  u2$doTable <- input$go2  })
  observeEvent(input$go3, {  u3$doTable <- input$go3  })
  
  
  v1 <- reactiveValues(doTable = FALSE)
  v2 <- reactiveValues(doTable = FALSE) 
  v3 <- reactiveValues(doTable = FALSE) 
  
  observeEvent(input$view1, { v1$doTable <- input$view1  })
  observeEvent(input$view2, { v2$doTable <- input$view2  })
  observeEvent(input$view3, { v3$doTable <- input$view3  })
  
  observeEvent(input$go, {
    v1$doTable <- FALSE
    v2$doTable <- FALSE
    v3$doTable <- FALSE
    u1$doTable <- FALSE
    u2$doTable <- FALSE
    u3$doTable <- FALSE
  }) 

  # variable selection from table
  varnames<-c("NAME","TRTACT","ID","TIME","LIDV","DOSE","AMT1","AE","RESP","tumor","tumorpc")
  
  # load dosing data  
  exdata <- eventReactive(input$go,{
    if(input$format=="csv"){temp<-read.csv(input$file1$datapath,header=T)}
    if(input$format=="xpt"){temp<-read.xport(input$file1$datapath)}
    temp<-temp[,varnames]
    return(temp)
  })
  
  # load ae data   
  aedata <- eventReactive(input$go,{
    if(input$format=="csv"){temp<-read.csv(input$file2$datapath,header=T)}
    if(input$format=="xpt"){temp<-read.xport(input$file2$datapath)}
    temp<-temp[,varnames]
    return(temp)
  })

  # load response (tumor size) data     
  rsdata <- eventReactive(input$go,{
    if(input$format=="csv"){temp<-read.csv(input$file3$datapath,header=T)}
    if(input$format=="xpt"){temp<-read.xport(input$file3$datapath)}
    temp<-temp[,varnames]
    return(temp)
  })

  
  # --------------------data preview, fired by "Read data in apps and preview"---------------------------------#
  output$head1 <- renderTable({
    if (v1$doTable==FALSE&u1$doTable==FALSE){head(exdata(), input$rows)}
  })
  
  output$head2 <- renderTable({
    if (v2$doTable==FALSE&u2$doTable==FALSE){head(aedata(), input$rows)}
  })
  
  output$head3 <- renderTable({
    if (v3$doTable==FALSE&u3$doTable==FALSE){head(rsdata(), input$rows)}
  })
  

  # --------------------Generate hierarchical button for treatment and dose---------------------------------# 
  exdata_list <- reactive({ exdata() %>% mutate(Trt=case_when(DOSE==0~"Placebo",TRUE~"Treatment"))
    })  
  
  output$TrtOutput <- renderUI({                     # uiOutput("###") need to match output$### 
     selectInput("TrtInput", "Treatment",
                choices = c("All",unique(exdata_list()$Trt)),selected = "All")
   })
 
  #TRT_exdata subset by TrtInput from user to allow for interaction
  TRT_exdata <- reactive({subset(exdata_list(), Trt %in% unlist(ifelse(input$TrtInput=="All",
                                                                list(unique(exdata_list()$Trt)),
                                                                list(input$TrtInput))))
  }) 
  
  #update dose selection button by TrtInput from user 
  observeEvent(TRT_exdata(), {
    choices <- unlist(ifelse(input$TrtInput=="Placebo",list(unique(TRT_exdata()$DOSE)),list(c("All",unique(TRT_exdata()$DOSE)))))
    updateSelectInput(session,inputId = "DoseInput", choices = choices) 
  }) 
  
  #DOSE_exdata subset by DoseInput from user to allow for interaction  
  DOSE_exdata <- reactive({subset(TRT_exdata(), DOSE %in% unlist(ifelse(input$DoseInput=="All",
                                                                       list(unique(TRT_exdata()$DOSE)),
                                                                       list(input$DoseInput))))
  }) 
  
  
  #--------------------------- output table rendering -------------------------------#
  output$full1 <- renderDataTable({
    if (v1$doTable==FALSE&u1$doTable>0){DOSE_exdata()}
  },
  filter = 'top')
  
  output$full2 <- renderDataTable({
    if (v2$doTable==FALSE&u2$doTable>0){aedata()}       # not included yet
  },
  filter = 'top')
  
  output$full3 <- renderDataTable({
    if (v3$doTable==FALSE & u3$doTable>0){rsdata()}       # not included yet
  }, 
  filter = 'top')
  
  
  
  #-------------------------- output graph rendering -------------------------------#  
  unicode = list(triangle=sprintf('\u25B2'),
                 circle=sprintf('\u25CF'),
                 square=sprintf('\u25A0'),
                 arrow=sprintf('\u2794'))
  
  output$plot3 = renderPlot(xyplot( tumor ~ TIME | input$TumorSort, groups = ID, data = input$file3, xlab = "Time", ylab = "Tumor Size"))
  
  output$pl<-renderPlot({
    if (v1$doTable>0) {
      ex<-DOSE_exdata() %>% filter(NAME=="Dosing") %>% group_by(ID) %>% mutate(TIMENEXT=lead(TIME)) %>% slice(1:(n()-1)) 
      rs<-DOSE_exdata() %>% filter(NAME=="PD - Binary",LIDV>0,TIME<=max(ex$TIME)) 
      
      pl1<-ggplot(ex)+
        geom_segment(aes(x=ID,xend=ID,y=TIME,yend=TIMENEXT,color=factor(AMT1)),size=2)+
        geom_point(data= rs,aes(x=ID,y=TIME,shape=factor(LIDV)),size=2)+
        coord_flip()+
        scale_shape_manual(values=c(rep(unicode[["triangle"]], 2), unicode[["circle"]], unicode[["square"]], unicode[["arrow"]])) +
        scale_x_continuous(breaks=unique(ex$ID))+
        labs(fill="Dose", colour="Symbol Key", shape="Symbol Key", 
             x="Subject ID ", y="Months since diagnosis",
             #          caption="Durable defined as subject with six months or more of confirmed response"
             title="Swimmer Plot") +
        theme(plot.title = element_text(hjust = 0.5), 
              plot.caption = element_text(size=7, hjust=0),
              axis.text.x=element_text(angle=90)) 
      return(pl1)
    }
  })
  
  output$info <- renderText({
   paste0("Click point shows ", round(input$plot_click$x), " months since diagnosis")
  })
  
  output$selected_rows <- renderPrint({
    if (is.null(input$plot_click$y)) return("Click on plot for full dosing history")
    else {
      ex<-DOSE_exdata() 
      keeprows <- round(input$plot_click$y) == as.numeric(ex$ID)
      temp<-subset(ex[keeprows,],NAME=='Dosing')
      head(temp, 10)
    }
  })
  
  
}



myapp <- shinyApp(ui = ui, server = server)

runApp(myapp)