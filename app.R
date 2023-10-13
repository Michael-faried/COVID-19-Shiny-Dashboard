#MiCkey
#install.packages('DT')
# install.packages("magrittr") # package installations are only needed the first time you use it
# install.packages("dplyr")    # alternative installation of the %>%
# install.packages("ggrepel")
# install.packages("scales")
# install.packages("readr")
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(ggrepel)
library(scales)
library(magrittr) # needs to be run every time you start R and want to use %>%

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  skin = "green", dashboardHeader(title = " Covid-19 Dashboard",titleWidth = 230),
    
    dashboardSidebar
    ( 
      # sidebarUserPanel(
      #   name = "Welcome Onboard!" ),
      sidebarMenu
      (
      sidebarSearchForm(textId = "searchText", buttonId = "searchButton",label = "Search..."),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"), badgeLabel = "Summery", badgeColor = "orange"),
      menuItem("Raw data", tabName = "Rawdata", icon = icon("th"),badgeLabel = "data", badgeColor = "green"),
      menuItem("Charts",
               tabName = "charts", 
               icon=icon("stats", lib = "glyphicon"),
               menuSubItem("Top countries deaths", tabName = "chart1",icon=icon("line-chart")),
               menuSubItem("Lowest countries deaths", tabName = "chart2",icon=icon("line-chart")),
               menuSubItem("Continent Death Percentage", tabName = "chart3",icon=icon("line-chart")),
               menuSubItem("Continent Cases Numbers", tabName = "chart4",icon=icon("line-chart")),
               menuSubItem("chart 5", tabName = "chart5",icon=icon("line-chart"))
               
               
               )

        
      )
    ),
    
    dashboardBody
    (       tags$head(tags$style(HTML('            /* body */
                                .content-wrapper, .right-side {
                                background-color: #D1F2EB;
                                }
                                
                                '))),
      

        tabItems(
          
          
          tabItem(tabName = "dashboard",

                  fluidRow(
                    valueBoxOutput("cases"),
                    valueBoxOutput("deaths"),
                    valueBoxOutput("DeathPercentage")
                  ),
                  fluidRow(
                    infoBoxOutput("africa", width = 3),
                    infoBoxOutput("egyptdeath", width = 3),
                    infoBoxOutput("egyptcases", width = 3),
                    infoBoxOutput("egyptpercentage", width = 3)
                    
                  ),
                  box(
                    # title = "Dataset Summery",
                    status = "info",
                    # solidHeader = TRUE,
                    background = "yellow",
                    width = 12,
                    height = 300,
                    verbatimTextOutput("summaryDset"))
                  ),
          
          
          tabItem(tabName = "Rawdata",
                  fluidPage( DT::dataTableOutput("data"),style = 'font-size:10px;'
                  )),
          
          tabItem(tabName = "chart1",
                  fluidRow(
                    box(title = h2("Top 10 countries covid deaths", align="center"),status="success",width = 8,
                        collapsible = T,solidHeader = T,plotOutput("plot"),collapsed = TRUE),
                    box(title = h2("Observations from charts", align="center"),status="warning",
                        collapsible = T,solidHeader = T,collapsed = TRUE,width=4,
                        "The COVID-19 pandemic spreads in countries with the highest population and the least social distance, even when they have good medical care.
                        The excess mortality rate was highest in the United States, Brazil, India, and Russia.",style = 'font-size:25px;')
                    
                    
                    )),
          tabItem(tabName = "chart2",
                  fluidRow(
                    box(title = h2("Lowest 10 countries covid deaths", align="center"),status="success",width = 8,
                        collapsible = T,solidHeader = T,plotOutput("plot2"),collapsed = TRUE),
                    box(title = h2("Observations from charts", align="center"),status="warning",
                        collapsible = T,solidHeader = T,collapsed = TRUE,width=4,
                        " (San-Martin, Monaco and GreenLand ), Most of these places has just a few numbers of people with high social distance and they are 
                        isolated from the world,which cause decreasing in number of deaths  ",style = 'font-size:25px;')
                    
                  )),

          tabItem(tabName = "chart3",
                  fluidRow(
                    box(plotOutput("pie"),solidHeader = T,status="success",
                        title = h4("Continent Death Percentage" ,align="center"),
                        width = 7
                        ),
                    box(title = h2("Observations from pie", align="center"),status="info",
                        solidHeader = T,width=5,
                        " As shown from pie chart, Europe is the most continent effected from the virus
                        followed by (South America and Asia) with the greatest effect",style = 'font-size:25px;')
                    
                  )),
          tabItem(tabName = "chart4",
                  fluidRow(
                  box(plotOutput("Donut"),solidHeader = T,status="success",
                      title = h4("Continent Cases Numbers" ,align="center"),
                      width = 7
                  ),
                  box(title = h2("Observations from Donut", align="center"),status="info",
                      solidHeader = T,width=5, " As shown from Donut chart,The virus is spreading at a terrible speed in the continent of
                      Europe and Asia in a way that requires rapid and immediate intervention ",style = 'font-size:25px;')
            
          )),
          
          tabItem(tabName = "Statistical Histogram",
                  fluidRow(
                    box(title = h2("Total Death in Europe", align="center"),status="success",
                        collapsible = T,solidHeader = T,plotOutput("plotHistogram1"),
                        sliderInput(inputId = "Bins1",label = "Number Of Bins", min = 1,max = 50,value = 5),
                        collapsed = TRUE),
                    box(title = h2("Observations from histogram", align="center"),status="warning",
                        collapsible = T,solidHeader = T,collapsed = TRUE,
                        "- Most deaths in Europe range between 0 and 50,000 deaths per country",style = 'font-size:25px;')
                  ),
                  fluidRow(
                    box(title = h2("Total Cases in Eruope", align="center"),status="success",
                        collapsible = T,solidHeader = T,plotOutput("plotHistogram2"),
                        sliderInput(inputId = "Bins2",label = "Number Of Bins", min = 1,max = 50,value = 5),
                        collapsed = TRUE),
                    box(title = h2("Observations from Histogram 2", align="center"),status="warning",
                        collapsible = T,solidHeader = T,collapsed = TRUE,
                        "-Most cases in Europe range from 0 to 50,000 deaths per country",style = 'font-size:25px;')
                  ),
                  fluidRow(
                    box(title = h2("Total Cases in Asia", align="center"),status="success",
                        collapsible = T,solidHeader = T,plotOutput("plotLine"),
                        ##sliderInput(inputId = "Bins2",label = "Number Of Bins", min = 1,max = 50,value = 5),
                        collapsed = TRUE),
                    box(title = h2("Observations from linechart", align="center"),status="warning",
                        collapsible = T,solidHeader = T,collapsed = TRUE,
                      "-The highest death toll in Asian countries is 500000",style = 'font-size:25px;')
                    
                  ))

          )
      
      
      
    )
  )



# Define server logic required to draw a histogram
server <- function(input, output) {

  
  output$data<-DT::renderDataTable({
    data <-read.csv(file="E:\\7th Semester\\Information visualization\\Project\\project\\COVID-19 Coronavirus.csv") 
    data
  })
  
  output$cases<-renderValueBox({
    data <-read.csv(file="E:\\7th Semester\\Information visualization\\Project\\project\\COVID-19 Coronavirus.csv") 
    cases<-sum(data$Total_Cases)
    valueBox(cases,"(491M)","Number of COVID-19 cases worldwide",icon = icon("virus-covid"),color = "blue")
  })
  output$deaths<-renderValueBox({
    data <-read.csv(file="E:\\7th Semester\\Information visualization\\Project\\project\\COVID-19 Coronavirus.csv") 
    deaths<-sum(data$Total_Deaths)
    valueBox(value= paste(deaths)," COVID-19 Deaths worldwide",icon = icon("skull-crossbones"),color = "red")
  })
  output$DeathPercentage<-renderValueBox({
    data <-read.csv(file="E:\\7th Semester\\Information visualization\\Project\\project\\COVID-19 Coronavirus.csv") 
    DeathPercentage<-round(sum(data$Death.percentage)/100,digits = 2)
    valueBox(paste(DeathPercentage," %"),subtitle = " Covid-19 Deaths percentage from worldwide Cases",icon = icon("percent"),color = "yellow")
  })
  
  
  mydf <- reactive({
    data <-read.csv(file="E:\\7th Semester\\Information visualization\\Project\\project\\COVID-19 Coronavirus.csv")
    by_continent <- data%>%
      select(c(Continent, Total_Cases, Total_Deaths)) %>%
      group_by(Continent) %>%
      summarise(Cases=sum(Total_Cases),Deaths=sum(Total_Deaths))
  })
  output$africa<-renderInfoBox({
    data <-mydf()
    x<-data[1,]
    x<-x$Deaths
    infoBox(value=x,"Africa Deaths",icon = icon("virus-covid"),fill = F)
  })
  
  output$egyptdeath<-renderInfoBox({
    x<-24417	
    infoBox(value=x,"Egypt Deaths",icon = icon("skull-crossbones"),color = "maroon",fill = T)
  })
  output$egyptcases<-renderInfoBox({
    x<-505264	
    infoBox(value=x,"Egypt Total Cases ",subtitle = "(until now)",icon = icon("circle-exclamation"),color = "purple",fill = T)
  })
  output$egyptpercentage<-renderInfoBox({
    x<-4.833
    infoBox(value=paste(x," %"),title = "Egypt Death ",subtitle = "Percentage",icon = icon("arrow-trend-up"),color = "aqua",fill = T)
  }) 
  output$summaryDset <- renderPrint({ 
    data <-read.csv(file="E:\\7th Semester\\Information visualization\\Project\\project\\COVID-19 Coronavirus.csv") 
    summary(data[]) 
  })
  
  
  output$plot<-renderPlot({
    
    library(ggplot2)
    data <-read.csv(file="E:\\7th Semester\\Information visualization\\Project\\project\\COVID-19 Coronavirus.csv") 
    #print("sort the data in decreasing order based on subjects ")
    x<- data[order(data$Total_Deaths, decreasing = TRUE), ]   
    x<-head(x,10)
    #x<- x[order(x$Total_Deaths), ]   
    country<-x$Country
    deaths<-x$Total_Deaths
    ggplot(x,aes(x=reorder(country,+deaths),y=deaths)) + 
      geom_bar(fill = "#E95D5D",stat='identity')+
      ggtitle("top 10 countries covid deaths")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(axis.text = element_text(size = 20))+
      theme(axis.title = element_text(size = 20))+
      theme(plot.title  = element_text(size = 20))+
      xlab("Countries")+ylab("Deaths")+
      theme(axis.text.x = element_text(angle = 40,hjust = 1))+
      theme(plot.background = element_rect(fill = "#D1F2EB"))+
      theme(panel.background = element_rect(fill = "#D1F2EB",
                                        colour = "#D1F2EB",
                                        size = 0.5, linetype = "solid"))
    
  })
  output$plot2<-renderPlot({
    
    library(ggplot2)
    data <-read.csv(file="E:\\7th Semester\\Information visualization\\Project\\project\\COVID-19 Coronavirus.csv") 
    #print("sort the data in decreasing order based on subjects ")
    x<- data[order(data$Total_Deaths, decreasing = TRUE), ]   
    x<-head(x, -20)     
    x<-tail(x,10)
    #x<- x[order(x$Total_Deaths), ]   
    country<-x$Country
    deaths<-x$Total_Deaths
    ggplot(x,aes(x=reorder(country,+deaths),y=deaths)) + 
      geom_bar(fill = "#E95D5D",stat='identity')+
      ggtitle("Lowest 10 countries covid deaths")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(axis.text = element_text(size = 20))+
      theme(axis.title = element_text(size = 20))+
      theme(plot.title  = element_text(size = 20))+
      xlab("Countries")+ylab("Deaths")+
      theme(axis.text.x = element_text(angle = 40,hjust = 1))+
      theme(plot.background = element_rect(fill = "#D1F2EB"))+
      theme(panel.background = element_rect(fill = "#D1F2EB",
                                            colour = "#D1F2EB",
                                            size = 0.5, linetype = "solid"))
  })
  output$pie<-renderPlot({
    data <-read.csv(file="E:\\7th Semester\\Information visualization\\Project\\project\\COVID-19 Coronavirus.csv") 
    
    by_continent <- data %>% 
      select(c(Continent, Total_Cases, Total_Deaths)) %>%
      group_by(Continent) %>%
      summarise(Cases=sum(Total_Cases),Deaths=sum(Total_Deaths))
    
    by_continent<-by_continent[-6,]
    Continent <- c(by_continent$Continent)
    det <- c(by_continent$Deaths)
    data <- data.frame(Continent, det)
    
    data%>%
      group_by(Continent)%>%
      summarise(Continent_death_percentage = sum(det)) %>%
      #add varibles in data frame
      mutate(Continent_death_percentage = Continent_death_percentage/sum(Continent_death_percentage)) %>%
      
      ggplot(aes(x = "", y = Continent_death_percentage, fill = Continent))+
      geom_col()+geom_bar(stat = "identity",color="#E95D5D")+
      xlab("Percentage")+ylab("Continent death")+
      geom_text(aes(label = scales::percent(round(Continent_death_percentage,2))),
                position  = position_stack(vjust = 0.5))+
      coord_polar(theta = "y")+
      theme(plot.background = element_rect(fill = "#D1F2EB"))+
      theme(panel.background = element_rect(fill = "#D1F2EB",
                                            colour = "#D1F2EB",
                                            size = 0.5, linetype = "solid"))
    
    

  })
  output$Donut<-renderPlot({
    
    covid_data <- read.csv('E:\\7th Semester\\Information visualization\\Project\\project\\COVID-19 Coronavirus.csv')
    
    by_continent <- covid_data %>% 
      select(c(Continent, Total_Cases, Total_Deaths)) %>%
      group_by(Continent) %>%
      summarise(Cases=sum(Total_Cases),Deaths=sum(Total_Deaths))
    #Donut Chart (Draw a stacked rectangle chart then change coordinates to polar)
    #percentages 
    by_continent$fraction = by_continent$Cases / sum(by_continent$Cases)
    #cumulative percentage
    #(Top of each reactangle)
    by_continent$ymax = cumsum(by_continent$fraction)
    #(Bottom of each reactangle)
    by_continent$ymin = c(0, head(by_continent$ymax, n=-1))
    #Compute Label position
    by_continent$label_pos = (by_continent$ymax+by_continent$ymin)/2
    #get data label
    casescale <- trunc((by_continent$Cases)/1000000*10)/10
    #Compute Label position
    by_continent$label = paste0(by_continent$Continent, "\n Cases = ",
                                casescale, "M")
    #Plot Stacked Rectangles
    ggplot(by_continent, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Continent)) +
      geom_rect() +
      #Change Coordinate System to Polar
      coord_polar(theta="y") +
      #Remove center part
      xlim(c(2,4))+
      scale_fill_brewer(palette=4) +
      theme_void() +
      theme(legend.position = "none")+
      geom_label_repel(aes(x = 3.5, y=label_pos, label=label),
                       min.segment.length = 0.25,
                       size=3)
    
  })
  
  
  library(readr)
  DataCorona <-read.csv('E:\\7th Semester\\Information visualization\\Project\\project\\COVID-19 Coronavirus.csv')

  ##Mario --> Render Plot 1 Histogram
  output$plotHistogram1 <- renderPlot(
    {
      
      ##Histogram 1 --> Total Death in Asia
      Asia <- subset(DataCorona,DataCorona$Continent=="Europe")
      options("scipen"=10)
      totalDeathAsia <- Asia$Total_Deaths
      
      hist(totalDeathAsia,main = "Total Death in Europe",breaks = input$Bins1, col="#75AADB", borders="black", xlab = "Deaths", ylab="Countries")
    }
  )
  ##Histogram 2 --> Total Cases in Asia
  output$plotHistogram2 <- renderPlot(
    {
      library(readr)
      ##DataCorona <- read.csv(file="COVID-19 Coronavirus.csv") 
      options("scipen"=10)
      ##Histogram 2 --> Total Cases/Month
      europe <- subset(DataCorona,DataCorona$Continent=="Europe")
      totalcaseseurpoe <- europe$Total_Cases
      hist(totalcaseseurpoe, main="Total Cases in Europe", breaks = input$Bins2 ,col="#75AADB", borders="black", xlab = "Cases", ylab="Countires")
    }
  )
  
  ##Line Graph For Asian deaths 
  output$plotLine <- renderPlot(
    {
      options("scipen"=10)
      Asia <- subset(DataCorona,DataCorona$Continent=="Asia")
      AsiaDeath <- Asia$Total_Deaths
      plot(AsiaDeath,type="o",xlab="Asian countries", ylab="Asian Deaths")
    }
  )

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
