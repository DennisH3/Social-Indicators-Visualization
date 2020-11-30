# Task: Create a prototype data visualization for SII-IIS
# Author: Dennis Huynh
# Date: 11/02/2020

# Create a double legend with employment income and VisMin
# Add a chloropleth

#install.packages("tidyverse")

# Load packages
library(shiny)
library(tidyverse)
library(data.table)

# Original combined file size was 847.158 MB (274) and 17.062 MB (275)
# Reduced size = 561.656176 MB
# Save 302.563824 MB
# The datatable with the columns of interest (Year, Geography, and the DIM columns)

# Should use merge(select(read.csv(file1)), select(read.csv(file2)), all = TRUE) instead. Have to rename columns too before merge.
# This way, can have only one column for visible minority

ogDT <- select(merge(read.csv("98-400-X2016274_English_CSV_data.csv"), 
                     read.csv("98-400-X2016275_English_CSV_data.csv"), 
                     all = TRUE), 
               c(1, 4, 8, 11, 14, 17:24, 27, 30:38, 41:48))

# Rename the columns
setnames(ogDT, colnames(ogDT), c("Year", "Geography", "Education", "Age", "Sex", "Chinese", "Black", "Filipino", "Latin American",
                                 "Arab", "Korean", "Japanese", "Immigrant Status", "Field of Study", "Total Visible Minority",
                                 "Total visible minority population", "South Asian", "Southeast Asian", "West Asian", 
                                 "Visible minority, n.i.e.", "Multiple visible minorities", "Not a visible minority",
                                 "Generation Status", "Total Visible Minority 2", "Total visible minority population 2", 
                                 "South Asian 2", "Southeast Asian 2", "West Asian 2", "Visible minority, n.i.e. 2", 
                                 "Multiple visible minorities 2", "Not a visible minority 2"))

# Read file for scatter plot
degInc <- read.csv("combined214and275.csv", check.names = FALSE)

# Define UI ----
ui <- fluidPage(
  titlePanel("Ethnocultural Groups Visualizations, 2016"),
  tabsetPanel(
    tabPanel("Intersectionality Analyses", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 
                 selectizeInput("year", 
                                label = "Year",
                                choices = list("2016", 
                                               "2017",
                                               "2018", 
                                               "2019"),
                                selected = "2016"),
                 
                 selectizeInput("geo", 
                                label = "Geography",
                                choices = unique(ogDT$Geography),
                                selected = "Canada"
   
                 ),
                 
                 h4("Framework Components Participation"),
                 
                 # Will change to multi-select selective input with limit 6
                 checkboxGroupInput("LM", 
                                    label = "Labour Market", 
                                    choices = list("Employment Income" = 1, 
                                                   "Annual full-time full-year wage" = 2, 
                                                   "Full-time full year employment" = 3,
                                                   "Labour force particpation" = 4,
                                                   "Employment rate" = 5,
                                                   "Unemployment rate" = 6,
                                                   "Youth NEET" = 7,
                                                   "Overqualification" = 8,
                                                   "Self-employment" = 9,
                                                   "Precarious employment" = 10)
                 ),
                 
                 h4("Indicators"),
                 
                 selectizeInput("deg", 
                                label = "Degree of Study", 
                                choices = unique(ogDT$Education),
                                selected = "Total - Highest certificate, diploma or degree"
                                
                 ),
                 
                 selectizeInput("fos", 
                                label = "Field of Study", 
                                choices = sort(unique(ogDT$`Field of Study`)),
                                selected = "Total - Major field of study - Classification of Instructional Programs (CIP) 2016"
                 ),
                 
                 checkboxGroupInput("VM", 
                                    label = "Visible Minority",
                                    choices = list("Total Visible Minority",
                                                   "Total visible minority population",
                                                   "South Asian",
                                                   "Chinese",
                                                   "Black",
                                                   "Filipino",
                                                   "Latin American",
                                                   "Arab",
                                                   "Southeast Asian",
                                                   "West Asian",
                                                   "Korean",
                                                   "Japanese",
                                                   "Visible minority, n.i.e.",
                                                   "Multiple visible minorities",
                                                   "Not a visible minority"),
                                    selected = "Total Visible Minority"
                 ),
                 
                 checkboxGroupInput("VM2", 
                                    label = "Visible Minority 2",
                                    choices = list("Total Visible Minority 2",
                                                   "Total visible minority population 2",
                                                   "South Asian 2",
                                                   "Chinese",
                                                   "Black",
                                                   "Filipino",
                                                   "Latin American",
                                                   "Arab",
                                                   "Southeast Asian 2",
                                                   "West Asian 2",
                                                   "Korean",
                                                   "Japanese",
                                                   "Visible minority, n.i.e. 2",
                                                   "Multiple visible minorities 2",
                                                   "Not a visible minority 2"),
                                    selected = "Total Visible Minority 2"
                 ),
                 
                 h4("Sub-populations"),
                 
                 selectizeInput("age", 
                                label = "Age Group",
                                choices = unique(ogDT$Age),
                                selected = "Total - Age"
                 ),
                 
                 selectizeInput("sex", 
                                label = "Sex",
                                choices = sort(unique(ogDT$Sex), decreasing = TRUE),
                                selected = "Total - Sex"
                 )
                 # selectizeInput("immStatus", 
                 #                label = "Immigrant Status",
                 #                choices = unique(ogDT$`Immigrant Status`),
                 #                options = list( placeholder = 'Please select an option below',
                 #                                onInitialize = I('function() { this.setValue(""); }')
                 #                )
                 # ),
                 # 
                 # selectizeInput("gen", 
                 #                label = "Generation Status",
                 #                choices = unique(ogDT$`Generation Status`),
                 #                options = list( placeholder = 'Please select an option below',
                 #                                onInitialize = I('function() { this.setValue(""); }')
                 #                )
                 # )
               ),
               
               mainPanel(
                 h1("Graphs"),
                 h4("To filter the data, please select Geography, Degree, Field of Study, Age, and Sex first.
                   Then select from either the Visible Minorities. This will produce 2 column graphs"),
                 plotOutput("ecplot"),
                 plotOutput("ecplot2")
               )
             )
    ),
    
    # Second Tab
    tabPanel("Similarity and Differences", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 
                 selectizeInput("yr", 
                                label = "Year",
                                choices = list("2016", 
                                               "2017",
                                               "2018", 
                                               "2019"),
                                selected = "2016"
                 ),
                 
                 selectizeInput("g", 
                                label = "Geography",
                                choices = unique(degInc$Geography),
                                selected = "Canada"
                                
                 ),
                 
                 h4("Framework Components Participation"),
                 
                 # Will change to multi-select selective input with limit 6
                 checkboxGroupInput("Lm", 
                                    label = "Labour Market", 
                                    choices = list("Employment Income" = 1, 
                                                   "Annual full-time full-year wage" = 2, 
                                                   "Full-time full year employment" = 3,
                                                   "Labour force particpation" = 4,
                                                   "Employment rate" = 5,
                                                   "Unemployment rate" = 6,
                                                   "Youth NEET" = 7,
                                                   "Overqualification" = 8,
                                                   "Self-employment" = 9,
                                                   "Precarious employment" = 10)
                                    
                 ),
                 
                 h4("Indicators"),
                 
                 helpText("For the example, only No certificate, diploma or degree is provided"),
                 
                 selectizeInput("dos", 
                                label = "Degree of Study", 
                                choices = unique(degInc$Education),
                                selected = "No certificate, diploma or degree"
                                
                 ),
                 
                 checkboxGroupInput("VisM", 
                                    label = "Visible Minority",
                                    choices = unique(degInc$`Visible Minority`),
                                    selected = "TOTAL VISIBLE MINORITY"
                 ),
                 
                 h4("Sub-populations"),
                 
                 selectizeInput("ag", 
                                label = "Age Group",
                                choices = unique(degInc$Age),
                                selected = "Total - Age"
                 ),
                 
                 selectizeInput("Sex", 
                                label = "Sex",
                                choices = sort(unique(degInc$Sex), decreasing = TRUE),
                                selected = "Total - Sex"
                 ),
                 
                 selectizeInput("gen",
                                label = "Generation Status",
                                choices = unique(degInc$`Generation Status`),
                                selected = "TOTAL GENERATION STATUS"
                 ),
                 
                 helpText("For the example, only Median total income ($) is provided"),
                 
                 selectizeInput("Income",
                                label = "Income",
                                choices = unique(degInc$`Total Income Groups`),
                                selected = "Median total income ($)"
                 )
                 
              ),
                 
             mainPanel(
               h1("Scatter Plot"),
               h4("To filter the data, please select Geography, Degree, Generation Status, Age, and Sex first.
                  Then select from either the Visible Minorities. This will produce a scatter plot"),
               plotOutput("splot")
             )
         )
    ),
  
    # Third Tab to check if tables are properly filtering
    tabPanel(
      "Debug", fluid = TRUE,
      h1("Filtered Data Tables"),
      dataTableOutput("df"), # filter_data()
      dataTableOutput("df2"), # data frame for ec()
      dataTableOutput("df3"), # filter_data2()
      dataTableOutput("df4"), # data frame for ec2()
      dataTableOutput("DI") # data frame for degIncome
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  # Reactive values ----------------------------------------------------------

  # This reactive will filter ogDT by inputs for Immigrant Status
  filtered_data <- reactive(
    {
      # if no input, display ogDT
      if (input$year == "2016" & input$geo == "") {
        return(ogDT)
      } else {
      
      # Require degree, field of study, age, and sex inputs
      req(input$deg, input$fos, input$age, input$sex)
      
      # Filter values
      newDT <- ogDT
      
      newDT <- filter(newDT, Geography == input$geo)
      newDT <- filter(newDT, Education == input$deg)
      newDT <- filter(newDT, `Field of Study` == input$fos)
      newDT <- filter(newDT, Age == input$age)
      newDT <- filter(newDT, Sex == input$sex)
      # Use this attribute for 2nd graph
      #ogDT$`Generation Status` == input$gen
      
      # Remove rows where column 13 (Immigrant Status is NA)
      newDT <- newDT[complete.cases(newDT[ , 13]),]
      
      return(newDT)
    }
  })
  
  # Select Ethnocultural groups for the first bar plot
  ec <- reactive ({
    
    # Require the filtered table and the Visible Minority input
    req(filtered_data(), input$VM)
  
    # From the filtered data select columns based on Visible Minority 2 Widget
    df <- filtered_data() %>%
      select(input$VM)
    
    # Transpose the data
    dft <- as.data.frame(t(as.matrix(df)))
    
    # Change row names and column names
    rownames(dft) <- colnames(df)
    colnames(dft) <- filtered_data()[,13]
    
    # Make the row name into a column
    dft <- cbind(rownames(dft), data.frame(dft, row.names=NULL))
    
    # Name the first column
    colnames(dft)[1] <- "Visible Minority Group"
    
    return(dft)
  })
  
  # This reactive will filter ogDT by inputs for Generation Status
  filtered_data2 <- reactive(
    {
      # Require Geography, degree, age, and sex inputs
      req(input$geo, input$deg, input$age, input$sex)
      
      # Filter values
      newDT <- ogDT
      
      newDT <- filter(newDT, Geography == input$geo)
      newDT <- filter(newDT, Education == input$deg)
      newDT <- filter(newDT, Age == input$age)
      newDT <- filter(newDT, Sex == input$sex)
      
      # Remove rows where column 23 (Generation Status is NA)
      newDT <- newDT[complete.cases(newDT[ , 23]),]
      
      return(newDT)
    }
  )
  
  # Select Ethnocultural groups for the first bar plot
  ec2 <- reactive (
    {
      
      # Require the filtered table and the Visible Minority input
      req(filtered_data2(), input$VM2)
      
      # From the filtered data select columns based on Visible Minority 2 Widget
      df <- filtered_data2() %>%
        select(input$VM2)

      # Transpose the data
      dft <- as.data.frame(t(as.matrix(df)))
      
      # Change row names and column names
      rownames(dft) <- colnames(df)
      colnames(dft) <- filtered_data2()[,23]
      
      # Make the row name into a column
      dft <- cbind(rownames(dft), data.frame(dft, row.names=NULL))
      
      # Name the first column
      colnames(dft)[1] <- "Visible Minority Group 2"
      
      return(dft)
    }
  )
  
  # This reactive will filter degInc
  filtered_degInc <- reactive(
    {
      # Require Geography, sex, gen, and VisM inputs
      req(input$g, input$Sex, input$gen, input$VisM)
      
      # Note that degree and Income are static
      
      # Filter values
      newDT <- degInc
      
      newDT <- filter(newDT, Geography == input$g)
      #newDT <- filter(newDT, Age == input$ag)
      newDT <- filter(newDT, Sex == input$Sex)
      newDT <- filter(newDT, `Generation Status` == input$gen)
      newDT <- filter(newDT, `Visible Minority` == input$VisM)
      
      return(newDT)
    }
  )
  
  # Output ---------------------------------------------------
  output$df <- renderDataTable({filtered_data()})
  
  output$df2 <- renderDataTable({ec()})
  
  output$df3 <- renderDataTable({filtered_data2()})
  
  output$df4 <- renderDataTable({ec2()})
  
  output$DI <- renderDataTable({filtered_degInc()})
  
  output$ecplot <- renderPlot({
    
    # Require the ethnocultural data frame
    req(ec())
  
    df <- melt(data.table(ec()), id.vars = 'Visible Minority Group')
    
    # Plot the column graph
    ggplot(df, aes(x = `Visible Minority Group`, y = value, fill = variable)) +
     geom_col(position = "dodge") + 
     labs(title = "Ethnocultural Indicator",
          x = "Visible Minority Group",
          y = "Number of People",
          fill = "Immigrant Status")
    
   })
  
  output$ecplot2 <- renderPlot({
    
    # Require the ethnocultural data frame
    req(ec2())
    
    df <- melt(data.table(ec2()), id.vars = 'Visible Minority Group 2')
    
    # Plot the column graph
    ggplot(df, aes(x = `Visible Minority Group 2`, y = value, fill = variable)) +
      geom_col(position = "dodge") + 
      labs(title = "Ethnocultural Indicator",
           x = "Visible Minority Group 2",
           y = "Number of People",
           fill = "Generation Status")
    
  })
  
  output$splot <- renderPlot({
    
    # Require filtered_degInc data frame
    req(filtered_degInc)
    
    # Plot the scatter plot
    ggplot(filtered_degInc(), aes(x = `Number of People 2`, y = `Number of People`)) +
      geom_point(aes(color = `Visible Minority`)) +
      labs(title = "Generation vs Employment Income",
           x = "Generation status with No certificate, diploma, or degree",
           y = "Number of People with Median Total income ($)")
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)