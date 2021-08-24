library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(dplyr)
library(ggplot2)
library(foreign)
library(EnvStats)

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
                        actionButton(input="view2",label="Toggle graph/data"),
                        tableOutput("head2"),
                        dataTableOutput("full2"),
                        #sliderInput(inputId = "id",
                        #           label = "Patient ID:",
                        #          min = min(aedata()$ID),
                        #         max = 60, #max(dat2$ID),
                        #        value = median(output$aedata()$ID), #median(output$AEplot$dat2$ID),
                        #       step = 1),
                        uiOutput("AESlider"),
                        plotOutput("AEPlot")
               ),
               tabPanel("Tumor Size",
                        actionButton(input="view3",label="Show graph"),
                        tableOutput("head3"),
                        dataTableOutput("full3"),
                        plotOutput("TumorSizePlot"),
                        radioButtons("TumorSort", "Sort by:", choices = c("TRTACT","ID","LIDV","DOSE","AMT1","AE","RESP"), selected = "DOSE")
               ),
               tabPanel("AE Summary Data",
                        uiOutput("SummarySelect"),
                        dataTableOutput("AESummary")
               )),
  
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
  observeEvent(input$view2, { v2$doTable = input$view2  })
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
  varnames<-c("NAME","TRTACT","ID","TIME", "NOMTIME", "LIDV","DOSE","AMT1","AE","RESP","tumor","tumorpc")
  
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
  
  output$AESlider = renderUI({
    sliderInput(inputId = "id",
                label = "Patient ID:",
                min = min(aedata()$ID),
                max = max(aedata()$ID),
                value = median(aedata()$ID),
                step = 1)
  })
  
  output$SummarySelect = renderUI({
    #checkboxGroupInput(inputId = "Selected",
    #                  label = "Data Shown",
    #                 choices = unique(aedata()$DOSE)
    #                )
    pickerInput(inputId = "Selected", 
                label = "Data shown", 
                choices = unique(aedata()$DOSE)[!unique(aedata()$DOSE) == 0], 
                selected = unique(aedata()$DOSE)[!unique(aedata()$DOSE) == 0],
                options = list("actions-box" = TRUE), multiple = T)
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
    if (v3$doTable==FALSE&u3$doTable>0){rsdata()}       # not included yet
  }, 
  filter = 'top')
  
  
  
  #-------------------------- output graph rendering -------------------------------#  
  unicode = list(triangle=sprintf('\u25B2'),
                 circle=sprintf('\u25CF'),
                 square=sprintf('\u25A0'),
                 arrow=sprintf('\u2794'))
  
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
             x="Subject ID ", y="Days since diagnosis",
             #          caption="Durable defined as subject with six months or more of confirmed response"
             title="Swimmer Plot") +
        theme(plot.title = element_text(hjust = 0.5), 
              plot.caption = element_text(size=7, hjust=0),
              axis.text.x=element_text(angle=90)) 
      return(pl1)
    }
  })
  
  output$info <- renderText({
    paste0("Click point shows ", round(input$plot_click$x), " Days since diagnosis")
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
  
  output$AEPlot = renderPlot({
    if (v2$doTable > 0) {
      dat2 = aedata()
      firstAE = c(dat2[dat2$ID == input$id & dat2$AE != "No",]$TIME)[1]
      dat2 = dat2[dat2$NAME == "Dosing",]
      dat2 = dat2[dat2$TIME >= firstAE,]
      ggplot(dat2[dat2$ID == input$id,], aes(x = TIME, y = AMT1)) + geom_point() + 
        ggtitle(paste("Individual", input$id)) + theme(plot.title = element_text(hjust = 0.5)) + 
        geom_vline(xintercept = dat2[dat2$ID == input$id & dat2$AE == "Severe",]$TIME, color = "red", alpha = 0.5) + 
        geom_vline(xintercept = dat2[dat2$ID == input$id & dat2$AE == "Mild",]$TIME, color = "yellow") +
        labs(y = "Dose Amount (mg)", x = "Time (Days)") +
        ylim(0,max(dat2$DOSE))
    }
  })
  
  output$TumorSizePlot <- renderPlot({
    if (v3$doTable > 0) {
      dat3 = rsdata()
      dat3 = dat3[dat3$NAME == "PD - Continuous",]
      dat3_means = dat3 %>% group_by(get(input$TumorSort), NOMTIME) %>% summarize(tumorpc = mean(tumorpc, na.rm = TRUE))
      colnames(dat3_means) = c(input$TumorSort, "NOMTIME", "tumorpc")
      ggplot(data = dat3, aes(x = NOMTIME, color = ID, na.rm = TRUE)) + 
        geom_point(aes(y = tumorpc)) +
        facet_wrap( ~ get(input$TumorSort), ncol=3) +
        geom_line(data = dat3_means, aes(y = tumorpc), color = "red")
    }
  })
  
  output$AESummary = renderDataTable({
    AEdat = aedata()
    AETable = AEdat[(AEdat$NAME == "Dosing" & AEdat$DOSE %in% input$Selected),] %>% 
      mutate(Interruption = ifelse(is.na(AMT1), "Y", "N")) %>%
      mutate(Reduction = ifelse(Interruption == "N" & lag(Interruption) == "N", ifelse(lag(AMT1) > AMT1, "Y", "N"), "N")) %>%
      filter(TIME != 0)
    reductTimesTable = count(AETable, Reduction, ID) %>% filter(Reduction == "Y")
    AETable = AETable %>% left_join(reductTimesTable[,c(2,3)]) %>% mutate(n = ifelse(is.na(n), 0, n))
    AEDisplay = AETable %>%
      group_by(DOSE) %>% 
      summarise("Dose Intensity" = round(geoMean(AMT1 / DOSE, na.rm = TRUE), digits = 4), 
                "Coefficient of Variation" = round(cv(AMT1, na.rm = TRUE), digits = 4),
                "% of Patients Experiencing AE" = length(unique(ID[AE != "No"])) / length(unique(ID)) * 100,
                "% of Patients Experiencing Dose Interruption" = length(unique(ID[Interruption == "Y"])) / length(unique(ID)) * 100,
                "Time of first AE" = round(geoMean(TIME[!duplicated(ID[AE != "No"])], na.rm = TRUE), digits = 4),
                "CV of Time of first AE" = round(cv(TIME[!duplicated(ID[AE != "No"])], na.rm = TRUE), digits = 4)
      )
     for (x in 0:max(AETable$n)) {
       AEDisplay = AEDisplay %>% mutate(temp = length(unique(AETable$ID[AETable$n == x & DOSE == AETable$DOSE])))
       names(AEDisplay)[which(names(AEDisplay)=="temp")] = paste("Patients with ", x, " dose reduction(s).")
     }
    AEDisplay
  })
  
}



myapp <- shinyApp(ui = ui, server = server)

runApp(myapp)