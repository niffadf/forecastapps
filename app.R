library(shiny)
library(shinythemes)
library(forecast)
library(DT)


ui <- navbarPage(
 theme=shinytheme("flatly"),
  strong("Building A Sales Forecasting Apps"),
  tabPanel("Data Input and Summary",sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      tags$hr(),
      checkboxInput("header", "Header", TRUE),
      checkboxInput("log", "Log Data", TRUE),
     #numericInput("column","Column to Analysis",value=2),
      #numericInput("freq", "Frequency of Time Series: ", value=12),
      #numericInput("start","Start your Time Series: ", value= 2000),
      #numericInput("end", "End your Time Series", value = 2004)
     selectInput("freq", "Frequency of Time Series", choices = c("Daily","Weekly","Monthly","Quarterly","Annually"),selected = "Annually")
  ),

  mainPanel(
      fluidRow(
        column(10, h4("Summary"),verbatimTextOutput("summary")),
        column(10,h4("Plot"),plotOutput("plot")),
        column(10, h4("Data Table"),dataTableOutput("contents"))
      )
    )
    )
    ),
  
  navbarMenu("Smoothing Method",
        tabPanel("Single Exponential Smoothing",
             mainPanel(
               fluidRow(column(10,
                 h3('Model Introduction'),
                 p('Single Exponential Smoothing is used when data doesn\'t have any trend and seasonality',align='justify')),
                 column(10,h4(strong('Forecasting Plot')),plotOutput("plot0")),
                 column(10,h4(strong('Accuracy Table')),tableOutput("accu0")),
                 column(10,h4(strong('Predict Table')),tableOutput("pr0"))
                 
                 )
             )
           ),
           tabPanel("Holt Smoothing",
                      mainPanel(
                        fluidRow(column(10,
                          h3('Model Introduction'),
                          p('Holt Smoothing is used when data have any trend ',align='justify')),
                          
                          column(10,h4(strong('Forecasting Plot')),plotOutput("plot1")),
                          
                          column(10,h4(strong('Accuracy Table')),tableOutput("accu1")),
                          
                          column(10,h4(strong('Predict Table')),tableOutput("pr1"))
                        )
                      )
                    ),
        tabPanel("Holt Winter Smoothing",
                   mainPanel(
                     fluidRow(column(10,
                       h3('Model Introduction'),
                       p('Holt Smoothing is used when data have any trend and seasonality',align='justify')),
                       
                       column(10,h4(strong('Forecasting Plot')),plotOutput("plot2")),
                      
                       column(10,h4(strong('Accuracy Table')),tableOutput("accu2")),
                       column(10,h4(strong('Predict Table')),tableOutput("pr2"))
                       
                     )
                   )
                 )
        
        
        
),
tabPanel("ARIMA", 
         mainPanel(
           fluidRow(
             column(10,h4(strong('ARIMA Models')),plotOutput("plot3"))
             
           )
         )
         )
)

server <- function(input, output) {
  
  datar<-reactive({
    inFile<-input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    if(input$log == 1) {
      df <- read.csv(inFile$datapath, header=input$header, sep=",",
                     quote='"')
      df[,2] <- log(df[,2])
      df
    } else {
      read.csv(inFile$datapath, header=input$header, sep=",",
               quote='"')
    }
    
    
  })

  freq<-reactive({if (input$freq=="Daily"){
    freq<-365.25
  } else if (input$freq=="Weekly"){
    freq<-52
  } else if (input$freq=="Monthly"){
    freq<-12
  } else if (input$freq=="Yearly"){
    freq<-1
  } else if (input$freq=="Quarterly"){
    freq<-4
    }
  })
  #start<-reactive({input$start})
  #freq<-reactive({input$freq})
  #end<-reactive({input$end})
  
  
output$contents <- renderDataTable({
  datar()
  })
col<-2
output$summary<-renderPrint({
  summary(datar()[,col])
})

output$plot<-renderPlot({
  if(!is.null(datar())){
    plot(datar()[,col],ylab="Observations", type="l")
  }
})


output$plot0<-renderPlot({
  tsdata<-ts(datar()[,col],freq=freq())
    forecast1<-ses(tsdata,h=4)
    plot(forecast1,xlab="Time",ylab="Observations")
    lines(forecast1$fit,col="red",lty=2)
})

output$accu0 <-renderTable({
  tsdata<-ts(datar()[,col],freq=freq())
  forecast1<-ses(tsdata,h=4)
  accuracy(forecast1)
})
output$pr0 <-renderTable({
  tsdata<-ts(datar()[,col],freq=freq())
  forecast1<-ses(tsdata,h=4)
  data.frame(forecast1)
})
output$plot1<-renderPlot({
  tsdata<-ts(datar()[,col],freq=freq())
  forecast2<-holt(tsdata,h=4)
  plot(forecast2,xlab="Time",ylab="Observations")
  lines(forecast2$fit,col="red",lty=2)
})

output$accu1 <-renderTable({
  tsdata<-ts(datar()[,col],freq=freq())
  forecast2<-holt(tsdata,h=4)
  accuracy(forecast2)
})

output$pr1 <-renderTable({
  tsdata<-ts(datar()[,col],freq=freq())
  forecast2<-holt(tsdata,h=4)
  data.frame(forecast2)
})

output$plot2<-renderPlot({
  tsdata<-ts(datar()[,col],freq=freq())
  forecast3<-hw(tsdata,h=4)
  plot(forecast3,xlab="Time",ylab="Observations")
  lines(forecast3$fit,col="red",lty=2)
})

output$accu2 <-renderTable({
  tsdata<-ts(datar()[,col],freq=freq())
  forecast3<-hw(tsdata,h=4)
  accuracy(forecast3)
})

output$pr2 <-renderTable({
  tsdata<-ts(datar()[,col],freq=freq())
  forecast3<-hw(tsdata,h=4)
  data.frame(forecast3)
})

output$plot3<-renderPlot({
  tsdata<-ts(datar()[,col],freq=freq())
  fit<-auto.arima(tsdata)
  forecast3<-forecast::forecast(fit,h=4)
  plot(forecast3,xlab="Time",ylab="Observations")
})

}

shinyApp(ui, server)
