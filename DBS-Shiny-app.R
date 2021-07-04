
library(shiny)
library(ECharts2Shiny)

### load data from MySQL database
library(DBI)
library(dplyr)
library(dbplyr)
library(odbc)
library(RMySQL) 
con <- dbConnect(RMySQL::MySQL(),host="127.0.0.1",port=3306,
                 dbname="DBSproject",user="root",
                 password="****")
dbSendQuery(con,"set character_set_results=gbk") 
dbSendQuery(con,"set character_set_client=gbk") 
dbSendQuery(con,"set character_set_connection=gbk") 
dbSendQuery(con,"set character_set_database=gbk") 
dbSendQuery(con, "SET GLOBAL local_infile = true;") 

res <- dbSendQuery(con, "SELECT * FROM Cdata_97_17e")
Cdata_97_17e <- dbFetch(res, n=-1)    
res <- dbSendQuery(con, "SELECT * FROM CC_97_17")
CC_97_17 <- dbFetch(res, n=-1)

### UI define
ui <- shinyUI(fluidPage(
  
  loadEChartsLibrary(),
  
  h2("DBS Project Data Visualization", align = "center"),
  h4("Group: Peiran, David, Lin", align = "center"),
  
  selectInput("select0", label = h3("Select Country/Area"),
              choices = unique(as.character(Cdata_97_17e$Country.Name)),
              selected = "Germany"),
  
  fluidRow(
    column(6,
           h3("Line Chart of Each Country/Area Indicators", align = "center"),
           h5("(from 1997 to 2017)", align = "center"),
           tags$div(id="test_1", style="width:100%;height:400px;"),  # Specify the div for the chart. Can also be considered as a space holder
           deliverChart(div_id = "test_1")  # Deliver the plotting
    )),
  
  selectInput("select_indicator", label = h4("Select Indicator"),
              #choices = colnames(CC_97_17)[c(6,22:35)],
              choices = colnames(CC_97_17)[c(5:35)],
              selected = "PopulationGrowth"),
  selectInput("select_year", label = h4("Select Year"),
              choices = 1997:2017,
              selected = 2017),
  
  fluidRow(  
    column(6,
           h3("Scatter Chart of Different Indicator vs. GDP per capita", align = "center"),
           # Compare normal line chart and stack line chart
           tags$div(id="test_scatter", style="width:100%;height:400px;"),  # Specify the div for the chart. Can also be considered as a space holder
           deliverChart(div_id = "test_scatter")  # Deliver the plotting
    )
  )
)
)


### Server difine
server <- shinyServer(function(input, output) {

  dat <- reactive({
    dat <- na.omit(Cdata_97_17e[,c(1,2,3,4,6)])
    dat <- dat[dat$Country.Name == input$select0,] # select two or three categories to display
    row.names(dat)<-dat$Year
    dat[,c(3:5)]
  })
  
  dat2 <- reactive({
    dat2 <- na.omit(CC_97_17)
    dat2 <- dat2[,c("Country.Name","Year","GDP_Cluster","GDP_pc",input$select_indicator)] # select two or three categories to display
    dat2 <- dat2[dat2$Year == input$select_year,]
    dat2 <- data.frame(x = as.numeric(dat2$GDP_pc),
                       y = as.numeric(dat2[,input$select_indicator]),
                       group = dat2$GDP_Cluster)
    dat2
  })
  
  # Call functions from ECharts2Shiny to render charts
  observeEvent(dat(),
               {
                 # set line.width with a single value
                 renderLineChart(div_id = "test_1",
                                 data = dat(),
                                 line.width = 8,
                                 line.type = "dotted")
               })
  
  observeEvent(dat2(),
               {
                 renderScatter("test_scatter", data = dat2(),
                               point.type = c('diamond', 'rect', 'triangle'))
               })
})


### launch the app
shinyApp(ui = ui, server = server)

### Deploy app
#library(rsconnect)
#rsconnect::deployApp('../shiny_app/project-app')






