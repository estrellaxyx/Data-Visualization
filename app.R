# Load packages ----
library(shiny)
library(maps)
library(mapproj)
library(ggmosaic)
library(ggplot2)
library(plotly)

# Load data ----
data <- data.frame(read.csv("Data/data.csv"))
data_Production <- subset(data, Department == "Production")
data_IT_IS <- subset(data, Department == "IT_IS")
data_Software_Engineering <- subset(data, Department == "Software_Engineering")
data_Admin <- subset(data, Department == "Admin_Offices")
data_Sales <- subset(data, Department == "Sales")
data_Executive_Office <- subset(data, Department == "Executive_Office")

department <- unique(data$Department)

# Source helper functions -----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  titlePanel("Human Resources Data App"),
  
  fluidRow(style ="background-color: #F4F5F5;padding: 5px;",
    helpText("1. Select employees' personal information and working situation to see the distribution."),
    
    column(5,selectInput("var1", 
                         label = "Choose a personal information",
                         choices = list("Department", 
                                        "HispanicLatino",
                                        "CitizenDesc",
                                        "MaritalDesc",
                                        "RaceDesc",
                                        "RecruitmentSource",
                                        "Sex"),
                         selected = "Department")),
    
    column(5,selectInput("var2", 
                         label = "Choose a working situation indicator",
                         choices = list("Salary", 
                                        "PerformanceScore",
                                        "EmpSatisfaction"), 
                         selected = "Salary")),
  ),
  plotlyOutput(outputId = "InteractivePlot"),
  fluidRow(style ="background-color: #F4F5F5;padding: 5px;",
           helpText("2. Select the job industry and range of salary to see the salary map."),
    column(5,selectInput("var", 
                         label = "Choose a job industry",
                         choices = c("Production", "IT_IS", "Software_Engineering", "Sales", "Admin_Offices", "Executive_Office"),
                         selected = "Production")),
    
    column(5,sliderInput("range", 
                         label = "Range of Salary:",
                         min = 45046, max = 250000, value = c(45046, 250000), pre='$')),
  ),
  
  plotOutput("map"),
  
  
  
  fluidRow(style ="background-color: #F4F5F5;padding: 5px;",
           helpText("3. Select the expected job area, salary and if you mind the candidates' previous working performance."),
           column(3,
                  selectInput("depart", label="Choose the area of the job title", 
                              choices = department, selected = "IT_IS")),
           
           column(4,
                  sliderInput("salary2", label="Choose the range of salary expected",
                              min = as.integer(min(data$Salary)), max = as.integer(max(data$Salary)), 
                              value = c(50000, 120000), pre="$" )),
           
           column(5,
                  radioButtons("fired", label="Do you mind if candidates were fired due to personal reasons (e.g. poor attendance, bad performance...)?",
                               choices = list("Yes" = 1, "No" = 2),selected = 2))
  ),
  
  fluidRow(
    column(
      width = 6,
      plotlyOutput("candidate"),
      #tableOutput ("check")
    )
  ),
)

# Server logic ----
server <- function(input, output) {
  
  output$InteractivePlot <- renderPlotly({
    
    
    if (input$var2 == 'Salary'){
      plot <- plot_ly(x = ~data[[input$var1]],  y = ~data[[input$var2]]
                      , type = 'violin'
                      , box = list(visible = TRUE))
      plot <- plot %>% layout(title = paste("Interactive plot of", input$var1, "-", input$var2)
                              , xaxis = list(title = input$var1)
                              , yaxis = list(title = input$var2)) 
    }
    else{
      if(input$var2 == "PerformanceScore"){
        plot <- ggplot(data = data) +
          geom_mosaic(aes(x = product(!!sym(input$var2)), fill = !!sym(input$var1))) +
          theme(axis.text.x=element_text(angle=-90, hjust= .1))
      }
      else{
        plot <- ggplot(data = data) +
          geom_mosaic(aes(x = product(!!sym(input$var2)), fill = !!sym(input$var1)))
      }
      
    }
    
    plot
  })
  
  
  output$map <- renderPlot({
    data <- switch(input$var, 
                   "Production" = data_Production$Salary,
                   "IT_IS" = data_IT_IS$Salary,
                   "Software_Engineering" = data_Software_Engineering$Salary,
                   "Admin_Offices" = data_Admin$Salary,
                   "Sales" = data_Sales$Salary,
                   "Executive_Office" = data_Executive_Office$Salary)
    
    color <- switch(input$var, 
                    "Production" = "darkorange",
                    "IT_IS" = "darkorange",
                    "Software_Engineering" = "darkorange",
                    "Admin_Offices" = "darkorange",
                    "Sales" = "darkorange",
                    "Executive_Office" = "darkorange")
    
    legend <- switch(input$var, 
                     "Production" = " Salary of Production",
                     "IT_IS" = "Salary of IT_IS",
                     "Software_Engineering" = "Salary of Software_Engineering",
                     "Admin_Offices" = "Salary of Admin_Offices",
                     "Sales" = "Salary of Sales",
                     "Executive_Office" = "Salary of Executive_Office")
    
    percent_map(data, color, legend, input$range[1], input$range[2])
  })
  
  dataInput <- reactive({
    
    newdata <- data
    #Job related=1, not related=0
    newdata$JobRelated <-NA
    newdata$JobRelated[newdata$Department==input$depart]<- 1
    newdata$JobRelated[newdata$Department!=input$depart]<- 0
    
    
    #Salary range
    newdata <- newdata[data$Salary %in% (input$salary2[1]:input$salary2[2]), ]
    
    #If mind fired
    if(input$fired==1) newdata <- newdata[newdata$EmploymentStatus != "Terminated for Cause", ]
    else newdata <- newdata
    
    rownames(newdata) <- newdata$Employee_Name
    
    newdata$performance_score <- as.integer(as.factor(newdata$PerformanceScore))-2
    newdata <- newdata[,c("performance_score","Salary", "JobRelated","PerformanceScore","Sex","Employee_Name")]
    return(newdata)
  })
  
  output$candidate <- renderPlotly({
    #ggplotly(ggparcoord(dataInput(), columns = (1:3),groupColumn = 5, alphaLines=0.6))
    # ggplotly(
    #   ggparcoord(dataInput(), columns = (1:3),groupColumn = 4,
    #            alphaLines=0.6, mapping = aes(size = 0.5,label=Employee_Name)) +
    #     ggplot2::scale_size_identity()+geom_point())
    #,tooltip = c("label")
    m <- list(
      l = 150,
      r = 50,
      b = 50,
      t = 100,
      pad = 6
    )
    dataInput() %>% plot_ly(width = 800, height = 400)%>%
      layout(title = "Parallel Coordinates Plot to choose candidates (Male-Blue, Female-Brown)",
             xaxis = list(title = ""),
             yaxis = list(title = ""),
             margin = m) %>%
      layout(hovermode = 'compare',
             separators = ',')%>%
      add_trace(
        type = 'parcoords',
        line = list(
          color = ~as.integer(as.factor(Sex)),
          colorscale = list(c(0,'aqua'),c(1,'chocolate')),
          showscale=TRUE
        ),
        dimensions = list(
          list(
            visible=TRUE,
            tickvals = c(-1, 0, 1, 2),
            ticktext = c('Exceeds','Fully Meets', 'Needs Improvement', 'PIP'),
            label = 'Performance Score',
            values = ~performance_score
          ),
          list(range = c(40000,250000),
               visible = TRUE,
               label = 'Salary',
               values = ~Salary),
          list(
            visible=TRUE,
            tickvals = c(0, 1),
            ticktext = c('No', 'Yes'),
            label = 'Previous job related?',
            values = ~JobRelated
          )
        )
      )
    
  })
  
}

# Run app ----
shinyApp(ui, server)