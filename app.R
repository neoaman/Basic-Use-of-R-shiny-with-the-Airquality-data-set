library(shinydashboard)
library(shiny)
library(tibble)
library(tidyr)
library(ggplot2)
library(MASS)
aq.no.missing <-drop_na(airquality)
options <- c("Ozone (parts per billion)" = "Ozone","Solar (Langleys)" = "Solar.R","Wind (MPH)" = "Wind","Temperature (F)" = "Temp")
df.options <- data.frame(options)
df.lv <- rownames_to_column(df.options)
colnames(df.lv) <- c("label","value")


ui <- dashboardPage(
  
  # Application title
  dashboardHeader(title = "Old Faithful Geyser Data"),
  dashboardSidebar(
    selectInput(inputId = "X",label = "X Values",choices = options),
    selectInput(inputId = "Y",label = "Y Values",choices = options),
    selectInput(inputId = "Plot",label = "Which Kind of Plot You want",choices = c("simple","ggplot"))
    
  ),
  
  # Show a plot of the generated distribution
  dashboardBody(
    fluidRow(
      
      column(width = 4,    
             valueBoxOutput("correlation",width = NULL),
             valueBoxOutput("intercept",width = NULL),
             valueBoxOutput("slope",width = NULL),
             valueBoxOutput("rsquare",width = NULL),
             box((verbatimTextOutput("point") ),width = NULL
             )
      ),
      column(width = 6,
        box(title = "ScatterPlot",width = NULL,
            plotOutput("scatter",height = 525,click = "clicking"),solidHeader = TRUE,status = "primary"
            )
        
      
    )
  )
  
    
))



#---------------------------------------------------------------------

server <- function(input, output) {
  selection <- reactive({
    aq.no.missing[,c(input$X,input$Y)]
    
  }) 
  
    
 
  output$scatter <- renderPlot({
    x_column <- selection()[,1]
    y_column <- selection()[,2]
    # X_lable <- df.lv$label[whose corresponding df.lv$value matches input$X]
    X_lable <- df.lv$label[which(df.lv$value==input$X)]
    Y_lable <- df.lv$label[which(df.lv$value==input$Y)]
    
    correlation <- cor(x_column,y_column)
    regression <- lm(y_column ~ x_column)
    intercept <- regression$coefficients[1]
    slope <- regression$coefficients[2]
    rsquare<- summary(lm(y_column ~ x_column))$adj.r.squared
  
    if ( input$Plot == "simple"){
      plot(x = x_column,y = y_column,xlab = X_lable,ylab = Y_lable,
           col="blue"
           ,main =  paste(Y_lable,"vs",X_lable,"\n r =",round(correlation,3),"Y' =",round(intercept,3),"+",round(slope,3),"X"),cex.main=1.8)
      abline(intercept,slope,col="green")
    }
    else if(input$Plot == "ggplot") {
      ggplot(selection(),aes(x = x_column,y = y_column))+ 
        geom_point(size=3) +
        labs(x = X_lable,y = Y_lable,title = paste(Y_lable,"vs",X_lable,"\n r = ",round(correlation,3),"Y' =",round(intercept,3),"+",round(slope,3),"X"))+
        theme(axis.title.x = element_text(size=18),axis.text.x = element_text(size=17),axis.title.y = element_text(size=18),axis.text.y = element_text(size=17),plot.title = element_text(hjust = 0.5,size=20))+
        geom_smooth(method="lm",col="green")+geom_abline(intercept = intercept,slope = slope)
      #abline(intercept,slope)
    }
    
    
    
  })
  
  output$correlation <- renderValueBox({
    x_column <- selection()[,1]
    y_column <- selection()[,2]
    correlation <- cor(x_column,y_column)
    correlation <- cor(x_column,y_column)
    valueBox(
    
    round(correlation,3),"Correlation",
    color = "aqua"
    )
  })
  
  
  output$intercept <- renderValueBox({
    x_column <- selection()[,1]
    y_column <- selection()[,2]
    regression <- lm(y_column ~ x_column)
    intercept <- regression$coefficients[1]
    valueBox(
      round(intercept,3),"Intercept",
      color = "aqua"
    )
  })
  
  output$slope <- renderValueBox({
    x_column <- selection()[,1]
    y_column <- selection()[,2]
    regression <- lm(y_column ~ x_column)
    slope <- regression$coefficients[2]
    valueBox(
      round(slope,3),"Slope",
      color = "aqua"
    )
  })
  
  
  output$rsquare <- renderValueBox({
    x_column <- selection()[,1]
    y_column <- selection()[,2]
    regression <- lm(y_column ~ x_column)
    rsquare<- summary(regression)$adj.r.squared
    valueBox(
      round(rsquare,3),"R-Square",
      color = "aqua"
    )
  })
  
  output$point <- renderPrint({
    
    X_lable <- df.lv$label[which(df.lv$value==input$X)]
    Y_lable <- df.lv$label[which(df.lv$value==input$Y)]
    data <- data.frame(selection()[,1],selection()[,2])
    colnames(data) <- c(X_lable,Y_lable)
    
    nearPoints(df = data,coordinfo = input$clicking,xvar = colnames(data)[1],yvar =colnames(data)[2])
    
  })
  
}


shinyApp(ui = ui, server = server)

