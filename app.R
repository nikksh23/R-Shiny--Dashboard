## Load packages ----
library(devtools)
library(googleCharts)
library(shiny)
library(maps)
library(mapproj)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(countrycode)
library(rworldmap)
library(ggrepel)
library(leaflet)
library(plotly)
library(tidyverse)
library(gghighlight)
library(reshape2)



# Loading  data
data <- read.table("data.csv", sep=",", header=TRUE)
colnames(data)[5] <- "Life expectancy"
colnames(data)[7] <- "Population growth"
defaultColors <- c( "red", "blue", "orange", "magenta", "cyan", "gold", "black", "darkgreen")

samp2 <- data[,-1]
rownames(samp2) <- data[,1]
samp2
sapply(samp2, class)

# Ui
ui <- navbarPage(title="Data Visualizer", fluid = TRUE,
                 tabPanel("Tab 1",
                          
                          # loads the Google Charts JS library
                          googleChartsInit(),
                          
                          # Use the Google webfont "Source Sans Pro"
                          tags$link(
                            href=paste0("http://fonts.googleapis.com/css?",
                                        "family=Source+Sans+Pro:300,600,300italic"),
                            rel="stylesheet", type="text/css"),
                          tags$style(type="text/css",
                                     "body {font-family: 'Source Sans Pro'}"
                          ),
                          h2("Bubble Chart for Europe dataset", align = "center"),
                          # sidebar with one input
                          sidebarPanel(width = 4,
                                       selectInput("variableX", "X Axis:", 
                                                   selected = "Unemployment", choices = c("GDP","Inflation","Life expectancy","Military","Population growth","Unemployment"))
                          ),
                          # sidebar with one input
                          sidebarPanel(width = 4,
                                       selectInput("variableY", "Y Axis:", 
                                                   selected = "Life expectancy", choices = c("GDP","Inflation","Life expectancy","Military","Population growth","Unemployment"))             ),
                          # sidebar with one input
                          sidebarPanel(width = 4,
                                       numericInput('variableC', 'Cluster count', 3,
                                                    min = 1, max = 8)),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          splitLayout(cellArgs = list(style = "padding: 0px"), cellWidths = c("50%", "50%"), 
                                      googleBubbleChart("chart",
                                                        width="95%", height = "450px",
                                                        options = list(
                                                          fontName = "Source Sans Pro",
                                                          fontSize = 13,
                                                          # The default padding is a little too spaced out
                                                          chartArea = list(
                                                            top = 35, left = 50,
                                                            height = "80%", width = "100%"
                                                          ),
                                                          # Allow pan/zoom
                                                          explorer = list(),
                                                          # Set bubble visual properties
                                                          bubble = list(
                                                            opacity = 0.8, stroke = "none", 
                                                            # Show label
                                                            textStyle = list(
                                                              color = "black"
                                                            )
                                                          ),
                                                          # Set fonts
                                                          titleTextStyle = list(
                                                            fontSize = 16
                                                          ),
                                                          tooltip = list(
                                                            textStyle = list(
                                                              fontSize = 12
                                                            )
                                                          )
                                                        )
                                      ),
                                      plotOutput(width="99%", height="500px", outputId = "countriesGraph") 
                          )
                 ),
                 tabPanel("Tab 2", 
                          h2("Bar plots", align = "center"),
                          br(),
                          splitLayout(cellArgs = list(style = "padding: 0px"), cellWidths = c("50%", "50%"), 
                                      
                                      sidebarPanel(width=12,
                                                   selectInput("selectedCountry", "Country", choices = data$Country, selected = "Austria"
                                                   ),
                                                   plotOutput(width="99%", outputId = "bar1")
                                      ),
                                      sidebarPanel(width=12,
                                                   selectInput("selectedCountry2", "Country", choices = data$Country, selected = "Germany"
                                                   ),
                                                   plotOutput(width="99%", outputId = "bar2")
                                      )
                                      
                          ),
                          h4("Compared to mean: \n\n 
                             Green bar = Positive result, Red bar = Negative result,  Blue bar = Not comparable", align="center")   
                          ),
                 tabPanel("Tab 3", 
                          h2("Grouped Bar plots for selected countries", align = "center"),
                          br(),
                          splitLayout(cellArgs = list(style = "padding: 0px"), cellWidths = c("20%", "80%"), 
                                      
                                  checkboxGroupInput("checkGroup", label = h4("Select Country below: "), 
                                                     choices = data$Country,
                                                     selected = "Austria"
                                      ),
                                  verticalLayout(
                                    br(), br(), br(), br(), br(),
                                      plotOutput(width="99%", outputId = "summaryCountryGraph2")
                                                                        )
                                      
                          )
                          ),
                 
                 tabPanel("Tab 4",  
                          h2("Scatter plot", align = "center"),
                          
                          sidebarPanel(width=6,
                                       selectInput("selectedParameter1", "X - Axis", selected = "Inflation", choices = c("Inflation","GDP", "Population growth","Life expectancy", "Military", "Unemployment"))             
                          ),
                          sidebarPanel(width=6,
                                       selectInput("selectedParameter2", "Y - Axis", selected = "GDP", choices = c("GDP","Military","Inflation","Life expectancy", "Population growth", "Unemployment"))             
                          ),
                        
                          br(),br(),br(),br(),br(),br(),br(), br(),
                          plotOutput(width="99%", outputId = "scatterPlot", hover = "plot_hover"
                          ),
                          br(), 
                          br(), 
                          br(), 
                          br(), 
                          br(), 
                          br(), 
                          br(), 
                          
                          verbatimTextOutput("plot_hoverInfo")
                 ),
                 
                 tabPanel("Tab 5",  
                          h2("3D Scatter plot", align = "center"),
                          
                          sidebarPanel(width=4,
                                       selectInput("selectedParam1", "X - Axis", selected = "Inflation", choices = c("Inflation","GDP", "Population growth","Life expectancy", "Military", "Unemployment"))             
                          ),
                          sidebarPanel(width=4,
                                       selectInput("selectedParam2", "Y - Axis", selected = "GDP", choices = c("GDP","Military","Inflation","Life expectancy", "Population growth", "Unemployment"))             
                          ),
                          sidebarPanel(width=4,
                                       selectInput("selectedParam3", "YZ - Axis", selected = "Unemployment", choices = c("GDP","Military","Inflation","Life expectancy", "Population growth", "Unemployment"))             
                          ),
                          br(),br(),br(),br(),br(),br(),br(),
                          splitLayout(cellArgs = list(style = "padding: 0px"), cellWidths = c("50%", "50%"), 
                                      plotlyOutput(width="99%", outputId = "scatterPlot2"),
                                      plotlyOutput(width="99%", height="500px", outputId = "scatterPlot3D") 
                          )
                 ),
                 
                 tabPanel("Tab 6",
                          h2("Data Manipulation", align="center"),
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("area", "Area", min = 2000, max = 600000, value = c(2000, 600000)),
                              sliderInput("gdp", "GDP", min = 7200, max = 90000, value = c(7200, 90000)),
                              sliderInput("inflation", "Inflation", min = 0.2, max = 8, value = c(0.2, 8), step = 1.3),
                              sliderInput("military", "Military", min = 0, max = 4.5, value = c(0, 4.5), step = 0.5),
                              sliderInput("life", "Life Expectancy", min = 68, max = 82, value = c(68, 82), step = 2),
                              sliderInput("pop", "Population Growth", min = -0.8, max = 1.2, value = c(-0.8, 1.2)),
                              sliderInput("unemp", "Unemployment", min = 2.5, max = 22, value = c(2.5, 22))
                            ),
                            
                            mainPanel(
                              DT::dataTableOutput('mytable3')
                            )
                          )
                          )
                 )


# Server
server <- function(input, output, session) {

  selectedData1 <- reactive({
    data[, c(input$variableX, input$variableY)]
  })
  
  clusters1 <- reactive({
    kmeans(selectedData1(), input$variableC)
  })
  
  data2 <- reactive({
    cbind(data[,1:8], Cluster=as.character(clusters1()$cluster))
  })
  
  series <- reactive({
    structure(
      lapply(defaultColors[1:input$variableC], function(color) { list(color=color) }),
      names = levels(data2()$Cluster)
    )
  })
  
  Data <- reactive({
    
    df <- data2() %>%
      select(Country, input$variableX, input$variableY,
             Cluster, Area) %>%
      arrange(Cluster)
  })
  
  output$chart <- reactive({
    # Return the data and options
    list(
      data = googleDataTable(Data()),
      options = list(
        hAxis = list(
          title = sprintf("X = %s",input$variableX)
        ),
        vAxis = list(
          title = sprintf("Y = %s",input$variableY)
        ),
        title = sprintf(
          "Coloring based on clusters"),
        series=series()
      )
    )
  })
  # map
  output$countriesGraph <- renderPlot({
    theCountryCode <- countrycode(data$Country, 'country.name', 'iso3c')
    # ISO3 names of the countries to plot in cluster colors
    
    malDF <- data.frame(country = theCountryCode,
                        Europe = data2()$Cluster)
    
    malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                                  nameJoinColumn = "country")
    myPalette <- defaultColors[1:input$variableC]
    mapCountryData(malMap, nameColumnToPlot="Europe", catMethod = "categorical",
                   missingCountryCol = gray(0.7), xlim = c(-20,34),colourPalette = myPalette, ylim = c(37,67),
                   addLegend = FALSE)
    
    # coordinates for each country
    country_coord<-data.frame(coordinates(malMap),stringsAsFactors=F)
    # label the countries
    text(x=country_coord$X1,y=country_coord$X2,labels=row.names(country_coord))
    # palette as for clusters
  })
  
  # bar
  #https://stackoverflow.com/questions/30634148/issue-with-geom-text-when-using-position-dodge
  output$summaryCountryGraph2 <- renderPlot({
    getMeanIntoOne <- function(x)
    {
      if(is.numeric(x))
      {
        return (x / mean(x))
      }
      else
        return(x)
    }
    displayData <- data[,1:8]
    displayData$Country <- 'Mean'
    res <- as.data.frame(apply(displayData[,-1],2, getMeanIntoOne))
    displayData <- cbind(displayData$Country, res)
    names(displayData)[1] <- "Country"
    print(input$checkGroup[1])
    
    selectedFRow <- which(data$Country == input$checkGroup[1])
    #displayData[selectedFRow, "Country"] <- as.factor(input$checkGroup[1])
    displayDataSub <- displayData[which(data$Country == input$checkGroup[1]),]
    displayDataSub$Country <- as.character(displayDataSub$Country)
    displayDataSub[1, "Country"] <- input$checkGroup[1]
    
    
    if (length(input$checkGroup) > 1)
    {
      for(i in 2:length(input$checkGroup)){
        selectedRow <- which(data$Country == input$checkGroup[i])
        #displayData[selectedRow, "Country"] <- input$checkGroup[i]
        displayDataSub <- rbind(displayDataSub, displayData[which(data$Country == input$checkGroup[i]),])
        displayDataSub[i, "Country"] <- input$checkGroup[i]
      }
    }
 
    
    displayDataSub <- melt(displayDataSub, id.vars='Country')
    
    displayDataSub %>%
      ggplot(., aes(Country, value*100))+
      geom_bar(stat='identity', aes(fill=variable), width = 0.4, position = position_dodge(width=0.5))+
      #geom_text(data=displayDataSub, aes(position=position_dodge(width = 0.9),label=displayDataSub$value))+
      geom_text(aes(displayDataSub$Country, displayDataSub$value + 10,label=round(displayDataSub$value, digits = 1), group=displayDataSub$variable
                    ),
                position=position_dodge(width=0.5), size=4)+
      labs(y = "% of european mean")+
      labs(x = "Chosen characteristics") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 15)) 
  })
  
  # bar
  output$bar1 <- renderPlot({
    selectedRow <- which(data$Country == input$selectedCountry)
    displayData <- data[,1:8]
    displayData$Country <- 'Mean'
    displayData[selectedRow, "Country"] <- input$selectedCountry
    getMeanIntoOne <- function(x)
    {
      if(is.numeric(x))
      {
        return (x / mean(x))
      }
      else
        return(x)
    }
    getColor <- function(x){
      vec <- c(2,1,-1,1,1,1,-1)
      for(i in 1: length(vec))
      {
        if(vec[i] != 2)
        {
          if(x[,i] < 1)
          {
            vec[i] <- vec[i]*(-1)
          }  
        }
      }
      vec <- as.character(vec)
    }
    
    res <- as.data.frame(apply(displayData[,-1],2, getMeanIntoOne))
    displayData <- cbind(displayData$Country, res)
    names(displayData)[1] <- "Country"
    displayData[selectedRow,] %>%  
      gather(Characteristics, Proportion_Of_Mean, -Country) %>%
      ggplot(., aes(x=Characteristics, y=Proportion_Of_Mean*100, fill=getColor(displayData[selectedRow,-1])))+
      geom_bar(stat='identity', position='dodge', colour="black")+
      labs(y = "% of european mean")+
      labs(x = "Chosen characteristics") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 15)) +
      guides(fill=FALSE)     
    
  })
  # bar
  output$bar2 <- renderPlot({
    selectedRow <- which(data$Country == input$selectedCountry2)
    displayData <- data[,1:8]
    displayData$Country <- 'Mean'
    displayData[selectedRow, "Country"] <- input$selectedCountry2
    getMeanIntoOne <- function(x)
    {
      if(is.numeric(x))
      {
        return (x / mean(x))
      }
      else
        return(x)
    }
    getColor <- function(x){
      vec <- c(2,1,-1,1,1,1,-1)
      for(i in 1: length(vec))
      {
        if(vec[i] != 2)
        {
          if(x[,i] < 1)
          {
            vec[i] <- vec[i]*(-1)
          }  
        }
      }
      vec <- as.character(vec)
    }
    
    res <- as.data.frame(apply(displayData[,-1],2, getMeanIntoOne))
    displayData <- cbind(displayData$Country, res)
    names(displayData)[1] <- "Country"
    displayData[selectedRow,] %>%  
      gather(Characteristics, Proportion_Of_Mean, -Country) %>%
      ggplot(., aes(x=Characteristics, y=Proportion_Of_Mean*100, fill=getColor(displayData[selectedRow,-1])))+
      geom_bar(stat='identity', position='dodge', colour="black")+
      labs(y = "% of european mean")+
      labs(x = "Chosen characteristics") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 15)) +
      guides(fill=FALSE)     
    
  })
  
  #scatterplot  
  #  https://drsimonj.svbtle.com/pretty-scatter-plots-with-ggplot2
  
  #     3d graph    
      output$scatterPlot <- renderPlot({ggplot(samp2, aes(x = samp2[,input$selectedParameter1], y = samp2[,input$selectedParameter2])) +
  #        geom_text(aes(label = row.names(samp2[1:28,]))) +
         geom_text_repel(aes(label = row.names(samp2[1:28,]), colour = row.names(samp2[1:28,])),
                    box.padding   = 0.35,
                    point.padding = 0.5,
                    segment.color = 'grey',
                    show.legend = FALSE) +
         geom_point(shape = 16, size = 5, show.legend = FALSE) +
         theme_minimal() +
         labs(y = input$selectedParameter2) +
         labs(x = input$selectedParameter1) +
         geom_smooth(method=lm)  # Add a loess smoothed fit curve with confidence region
        })
        
      
      output$plot_hoverInfo <- renderText({
      xy_str <- function(e) {
      if(is.null(e)) return("Please hover on a data point\n\n\n")
        paste0("x=", round(e$x,1), " y=", round(e$y,1), "\n")
      }

      paste0(
       "hover: ", xy_str(input$plot_hover)
       )
      })
      output$scatterPlot2 <- renderPlotly({
        plot_ly(data = samp2, x = samp2[,input$selectedParam1], y = samp2[,input$selectedParam2], 
                text = rownames(samp2),
                mode = 'markers',
                type = 'scatter' )%>%
          layout(dragmode = "select")
        
      })
      output$scatterPlot3D <- renderPlotly({
        s <- event_data("plotly_selected")
        col1 <- samp2[,input$selectedParam1];
        col2 <- samp2[,input$selectedParam2];
        col3 <- samp2$Area
        col4 <- samp2[,input$selectedParam3]
        
        getCountryArea <- function(x, y, z)
        {
          for(i in 1:(nrow(samp2))){
            if (col1[i] == x & col2[i] == y){
              a <- col3[i]
            }
          }
          return(a)
        }
        GetZAxis <- function(x, y){
          for(i in 1:(nrow(samp2))){
            if (col1[i] == x & col2[i] == y){
              z <- col4[i]
            }
          }
          return(z)
        }
        newDF = data.frame(xx=numeric(0),yy=numeric(0),zz=numeric(0), aa=numeric(0))
        
        if (length(s$y) > 0) {
          
          for(i in 1:(length(s$y))){
            
            xx <- s$x[i]
            yy <- s$y[i]
            zz <- GetZAxis(xx, yy)
            aa <- getCountryArea(xx, yy)
            newDF = rbind(newDF, data.frame(xx, yy, zz, aa))
            
          }
        }
        
        if(nrow(newDF) == 0)
        {
          p <- plotly_empty(newDF, x = newDF$xx, y = newDF$yy, z = newDF$zz)
        }
        else
        {
            p <- plot_ly(newDF, x = newDF$xx, y = newDF$yy, z = newDF$zz, 
                     text = ~paste0("My X = ", newDF$xx, "\n My Y = ", newDF$yy, "\n My Z = ", newDF$zz), hoverinfo="text",
                     marker = list(color = ~newDF$aa, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))

        
        
            p %>%
              #config(displayModeBar = F, showLink = F) %>%
              add_markers() %>%
              layout(width = 700, height = 500,
                     scene = list(xaxis = list(title = input$selectedParam1),
                                  yaxis = list(title = input$selectedParam2),
                                  zaxis = list(title = input$selectedParam3)),
                     annotations = list(
                       x = 1,
                       y = 0.7,
                       text = 'Country Area',
                       xref = 'paper',
                       yref = 'paper',
                       showarrow = FALSE))
        }
      })
  
  # data table
  sliderVal <- reactive({
  })
  
  output$mytable3 = DT::renderDataTable({
    dtData <- data

    newData <- subset(dtData,
                      (dtData$Area >= input$area[1] & dtData$Area <= input$area[2]) &
                        (dtData$GDP >= input$gdp[1] & dtData$GDP <= input$gdp[2]) &
                        (dtData$Inflation >= input$inflation[1] & dtData$Inflation <= input$inflation[2]) &
                        (dtData$Military >= input$military[1] & dtData$Military <= input$military[2]) &
                        (dtData$Unemployment >= input$unemp[1] & dtData$Unemployment <= input$unemp[2])) 
    isolate({ DT::datatable(newData, options = list(pageLength = 14, scrollX = TRUE))

    })
  })
  
  #https://gist.github.com/wch/9606002
  output$datascatterPlot <- renderPlot({
    input$goButton
    newData <- subset(data, data$Military <= input$slider)
    rownames(newData) <- newData[,1]
    n = nrow(newData)
    
    
    isolate({
      ggplot(newData, aes(x = newData$Unemployment, y = newData$Military)) +
        geom_text_repel(aes(label = row.names(newData[1:n,]), colour = row.names(samp2[1:n,])),
                        box.padding   = 0.35, 
                        point.padding = 0.5,
                        segment.color = 'grey',
                        show.legend = FALSE) +
        geom_point(shape = 16, size = 5, show.legend = FALSE) +
        theme_minimal() +
        labs(y = "Military") +
        labs(x = "Unemployment")
    })
  })
}

# Running the app
shinyApp(ui, server)