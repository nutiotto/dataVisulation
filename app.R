#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
source("global.R")
# Define UI for application that draws a histogram

ui <- fluidPage(

    # Application title
    tags$h1("Kolledži õppeandmebaasi andmete visualiseerimine"),
    tags$br(),
    tags$h2("Andmete visualiseerimise projekt."),
    tags$h4("Autor: Karle Nutonen"),
    tags$br(),

    # Sidebar with a slider input for number of bins 
    tabsetPanel(
        tabPanel("Projekti sisu",
            tags$h2("Sissejuhatus:"),
            tags$br(),
            tags$p("Antud teostud töö on magistri õpingute aine Andmete visualiseermise, mille käigus tuleb igal tudengi või rühmana looma rojekti, mida tuleb kaitsta õppejõule.
                   Projekti käigus tuleb püstida ülesanne, kasutada vähemalt 4 tabsetti, kasutama 4-8 vidinat."),
            tags$br(),
            tags$p("Olen kasutanud oma projektis Virumaa kolledži õppe andmebaasi, kuhu kogutakse erinevate andurite näitusid. Üelsandes on näha milised tabelid on andmebaasis kasutuses. Vaadeldatakse iga kontrelleri
                   sensorite omadusi. Ning viimakseks on võimalik vaadelda andmegraafikud iga sensori kohta käivat informatsiooni ajavahemikkus.")),
            
        tabPanel("Tabelite ülevaade",
                 sidebarPanel(
                     
                     selectInput("tables", "Tabeli valik",
                                 choices = tables$table_name)),
                
                     # Show a plot of the generated distribution
                     mainPanel(
                         DT::dataTableOutput("table")
                     )),
        tabPanel("Kontrolleri ja selle sensori andmed",
                 sidebarPanel(
                     selectInput("cntr", "Kontrolleri valik", choices = controller$controllername),
                     selectInput("snr", "Sensori valik", choices= NULL),
                     selectInput("vtype", "Sensori täpsem valik", choices= NULL)),
                 
                 mainPanel(
                     DT::dataTableOutput("table2"),
                     plotOutput("hist")
                 )),
        tabPanel("Anmete visualiseerimine ajavahemikus",
                 sidebarPanel(
                     selectInput("cntr2", "Kontrolleri valik", choices = controller2$controller),
                     selectInput("snr2", "Sensori valik", choices= NULL),
                     selectInput("vtype2", "Sensori täpsem valik", choices= NULL),
                     dateRangeInput("dater", "Kuupäevade vahemik", start = date$min, end = as.character(Sys.Date()))),
                 mainPanel(
                     DT::dataTableOutput("table3"),
                     plotlyOutput("plot")
                 )),
        tabPanel("Kasutatud kirjandus",
                 tags$h2("Sissejuhatus:"),
                 tags$br(),
                 tags$p("Antud töös sai kasutud palju interneti lehekülgesid, mida ei fikseeritud ning samuti olid suureks abiks projekti teostamisel õppeaine moodle keskkonnas olevatest õppematerjalidest.
                        "))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$table <- DT::renderDataTable({
        query <- sqlInterpolate(ANSI(),"SELECT * FROM ?table",table = dbQuoteIdentifier(ANSI(), input$tables))
        outp <- dbGetQuery(pool, query)
        ret <- DT::datatable(outp)
        return(ret)
    })
    
    observe({
        query <- sqlInterpolate(ANSI(),"SELECT sensorname FROM sensor WHERE id IN (SELECT id_sensor FROM controller_sensor WHERE id_controller = (SELECT id FROM controller WHERE controllername = ?name))", name = input$cntr)
        outp <- dbGetQuery(pool, query)
        updateSelectInput(session, "snr", choices = outp )
    })
    observe({
        query <- sqlInterpolate(ANSI(),"SELECT valuetype FROM typevalue WHERE id IN (SELECT id_typevalue FROM datasensor WHERE id_controllersensor = (SELECT id FROM controller_sensor WHERE id_controller = (SELECT id FROM controller WHERE controllername = ?contr) AND id_sensor = (SELECT id FROM sensor WHERE sensorname = ?senor)))", contr = input$cntr, senor = input$snr)
        outp <- dbGetQuery(pool, query)
        updateSelectInput(session, "vtype", choices = outp )
    })
    
    observe({
        query <- sqlInterpolate(ANSI(),"SELECT sensor FROM vw_sensorsdata WHERE controller = ?name", name = input$cntr2)
        outp <- dbGetQuery(pool, query)
        updateSelectInput(session, "snr2", choices = outp )
    })
    observe({
        query <- sqlInterpolate(ANSI(),"SELECT valuetype FROM vw_sensorsdata WHERE controller = ?contr AND sensor = ?senor ", contr = input$cntr2, senor = input$snr2)
        outp <- dbGetQuery(pool, query)
        updateSelectInput(session, "vtype2", choices = outp )
    })
    
    output$table2 <- DT::renderDataTable({
        query <- sqlInterpolate(ANSI(),"SELECT * FROM datasensor WHERE id_controllersensor = (SELECT id FROM controller_sensor WHERE id_controller = (SELECT id FROM controller WHERE controllername = ?contr) AND id_sensor = (SELECT id FROM sensor WHERE sensorname = ?senor)) AND id_typevalue = (SELECT id FROM typevalue WHERE valuetype = ?type)", contr = input$cntr, senor = input$snr, type = input$vtype)
        outp <- dbGetQuery(pool, query)
        ret <- DT::datatable(outp)
        return(ret)
    })
    
    output$hist <- renderPlot({
        query <- sqlInterpolate(ANSI(),"SELECT data FROM datasensor WHERE id_controllersensor = (SELECT id FROM controller_sensor WHERE id_controller = (SELECT id FROM controller WHERE controllername = ?contr) AND id_sensor = (SELECT id FROM sensor WHERE sensorname = ?senor)) AND id_typevalue = (SELECT id FROM typevalue WHERE valuetype = ?type)", contr = input$cntr, senor = input$snr, type = input$vtype)
        outp <- dbGetQuery(pool, query)
        query <- sqlInterpolate(ANSI(),"SELECT COUNT(DISTINCT(EXTRACT(DAY FROM date_time))) AS date FROM datasensor WHERE id_controllersensor = (SELECT id FROM controller_sensor WHERE id_controller = (SELECT id FROM controller WHERE controllername = ?contr) AND id_sensor = (SELECT id FROM sensor WHERE sensorname = ?senor)) AND id_typevalue = (SELECT id FROM typevalue WHERE valuetype = ?type)", contr = input$cntr, senor = input$snr, type = input$vtype)
        outp2 <- dbGetQuery(pool, query)
        hist(outp$data, breaks = outp2$date, col = 'darkgray', border = 'white', xlab = input$vtype, ylab = "Count", main = "Sensorite hinstorramm")
    })
    
    data <- reactive( 
        {invalidateLater(300*1000)
            y <<- c(y[2:20],rnorm(1,mean = 20, sd=2))
            query <- sqlInterpolate(ANSI(), "SELECT * from vw_sensorsdata WHERE controller = ?contr AND sensor = ?senor AND valuetype = ?value AND date_time BETWEEN ?dater1 AND ?dater2 ORDER BY date_time DESC LIMIT 2000", contr = input$cntr2, senor = input$snr2, value = input$vtype2, dater1 = input$dater[1], dater2 = input$dater[2])
            dbGetQuery(pool, query)
        }
    )
    
    output$table3 <- DT::renderDataTable({
        #query <- sqlInterpolate(ANSI(),"SELECT * FROM vw_sensorsdata WHERE controller = ?contr AND sensor = ?senor AND valuetype = ?value ORDER BY EXTRACT(HOUR FROM date_time)", contr = input$cntr2, senor = input$snr2, value = input$vtype2)
        #outp <- dbGetQuery(pool, query)
        ret <- DT::datatable(data())
        return(ret)
    })
    
    
    inputPlot1 <- reactive({
        
        df_postgres1 <- data()
        andmed <- data()[,c("date_time","data")]
        p <- ggplot(andmed,aes(x=date_time, y=data)) +
            geom_line(color="deepskyblue3", size=1) +
            labs(title=paste("plotly: ",input$var)) +xlab("time")+ylab(input$var)+theme(plot.title=element_text(hjust = 0.5,size=14, face="bold"))
        p <- ggplotly(p)
    })
    
    output$plot <- renderPlotly({
        print(inputPlot1())
        
    }) 
    
    
        
}

# Run the application 
shinyApp(ui = ui, server = server)
