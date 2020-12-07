# Task: Create a prototype data visualization for SII-IIS
# Author: Dennis Huynh
# Date: 11/02/2020

# Notes on next steps:
# Change Immigrant Status to Sex

# Later stages:
# Test synthetic data
# Add a choropleth (will use voter turn out data, get GeoJSON file)

# Install packages
#install.packages("tidyverse")
#install.packages("plotly")

# Load packages
library(shiny)
library(tidyverse)
library(data.table)
library(plotly)
library(RColorBrewer)

# Original combined file size was 847.158 MB (274) and 17.062 MB (275)
# Reduced size = 561.656176 MB
# Save 302.563824 MB
# The datatable with the columns of interest (Year, Geography, and the DIM columns)

# Should use merge(select(read.csv(file1)), select(read.csv(file2)), all = TRUE) instead. Have to rename columns too before merge.
# This way, will have only one column for visible minority

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

# Calculate percentages by VisMin
degInc <- degInc %>%
            dplyr::group_by(`Visible Minority`) %>%
            dplyr::mutate(`Percentage by Generation` = `Number of People 2`/sum(`Number of People 2`) * 100) %>%
            dplyr::mutate(`Percentage by Total Income` = `Number of People`/sum(`Number of People`) * 100)

# Set color ramp 
my_colors <- colorRampPalette(brewer.pal(8, 'Dark2'))(15)

# Define UI ----
ui <- fluidPage(
  titlePanel("Ethnocultural Groups Visualizations, 2016"),
  tabsetPanel(
    tabPanel("Intersectionality Analyses", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 
                 helpText("Changing this does nothing"),
                 
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
                 
                 helpText("Changing this does nothing"),
                 
                 # Will change to multi-select selective input with limit 6
                 selectizeInput("LM", 
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
                                               "Precarious employment" = 10),
                                options = list(placeholder = 'Please select an option below',
                                               onInitialize = I('function() { this.setValue(""); }')
                                )
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
                 
                 # selectizeInput("immStatus",
                 #                label = "Immigrant Status",
                 #                choices = unique(ogDT$`Immigrant Status`),
                 #                selected = "Total - Immigrant Status"
                 # ),
                 
                 checkboxGroupInput("VM", 
                                    label = "Visible Minority for Immigrant Status",
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
                                    label = "Visible Minority for Generation Status",
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

                 # 
                 # selectizeInput("gen", 
                 #                label = "Generation Status",
                 #                choices = unique(ogDT$`Generation Status`),
                 #                options = list( placeholder = 'Please select an option below',
                 #                                onInitialize = I('function() { this.setValue(""); }')
                 #                )
                 #)
               ),
               
               mainPanel(
                 h1("Graphs"),
                 h4("To filter the data, please select Geography, Degree, Field of Study, Age, and Sex first.
                   Then select from either the Visible Minorities. This will produce 2 column graphs"),
                 plotlyOutput("sBar", inline = TRUE),
                 br(),
                 plotlyOutput("ecplot"),
                 br(),
                 plotlyOutput("ecplot2")
               )
             )
    ),
    
    # Second Tab
    tabPanel("Similarity and Differences", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 
                 helpText("Changing this does nothing"),
                 
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
                 
                 helpText("Changing this does nothing"),
                 
                 # Will change to multi-select selective input with limit 6
                 selectizeInput("Lm", 
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
                                                 "Precarious employment" = 10),
                                options = list(placeholder = 'Please select an option below',
                                               onInitialize = I('function() { this.setValue(""); }')
                                )
                                    
                 ),
                 
                 h4("Indicators"),
                 
                 helpText("For the example, only No certificate, diploma or degree is provided"),
                 
                 selectizeInput("dos", 
                                label = "Degree of Study", 
                                choices = unique(degInc$Education),
                                selected = "No certificate, diploma or degree"
                                
                 ),
                 
                 selectizeInput("gen",
                                label = "Generation Status",
                                choices = unique(degInc$`Generation Status`),
                                selected = "TOTAL GENERATION STATUS"
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
               plotlyOutput("splot", inline = TRUE),
               br(),
               p("Note: the regression lines overlap.")
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
      dataTableOutput("DI"), # data frame for degIncome
      dataTableOutput("sDF")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  # Reactive values ----------------------------------------------------------

  # This reactive will filter ogDT by inputs for Sex
  filtered_data <- reactive(
    {
      # Require geography, degree, field of study, age, and sex inputs
      req(input$geo, input$deg, input$fos, input$age, input$sex)
      
      # Filter values
      newDT <- ogDT
      
      newDT <- filter(newDT, Geography == input$geo)
      newDT <- filter(newDT, Education == input$deg)
      newDT <- filter(newDT, `Field of Study` == input$fos)
      newDT <- filter(newDT, Age == input$age)
      newDT <- filter(newDT, Sex == input$sex)
      
      # Remove rows where column 13 (Immigrant Status is NA)
      newDT <- newDT[complete.cases(newDT[ , 13]),]
      
      return(newDT)
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
      newDT <- newDT %>% filter(`Visible Minority` %in% input$VisM)
      
      return(newDT)
    }
  )
  
  filtered_sex <- reactive({
    # Require geography, degree, field of study, and age inputs
    req(input$geo, input$deg, input$fos, input$age)
    
    # Filter values
    newDT <- ogDT
    
    newDT <- filter(newDT, Geography == input$geo)
    newDT <- filter(newDT, Education == input$deg)
    newDT <- filter(newDT, `Field of Study` == input$fos)
    newDT <- filter(newDT, Age == input$age)
    #newDT <- filter(newDT, `Visible Minority` %in% input$VisM)
    
    # Remove rows where column 13 (Immigrant Status is NA)
    newDT <- newDT[complete.cases(newDT[ , 13]),]
    
    # Transform the data frame
    newDT <- newDT %>%
      select(c(1:22)) %>%
      pivot_longer(c(6:12, 15:22), names_to = "Visible Minority Groups", values_to = "Number of People") %>%
      pivot_wider(names_from = Sex, values_from = "Number of People", values_fn = sum)
    
    return(newDT)
  })
  
  # Output ---------------------------------------------------
  output$df <- renderDataTable({filtered_data()})
  
  output$df2 <- renderDataTable({ec()})
  
  output$df3 <- renderDataTable({filtered_data2()})
  
  output$df4 <- renderDataTable({ec2()})
  
  output$DI <- renderDataTable({filtered_degInc()})
  
  output$sDF <- renderDataTable({filtered_sex()})
  
  output$sBar <- renderPlotly({
    req(filtered_sex())
    
    fig <- plot_ly(filtered_sex(), x = ~`Visible Minority Groups`, y = ~Female, type = 'bar', name = "Female",
                   width = 1000, height = 600) %>%
      add_trace(y = ~Male, name = "Male") %>%
      add_trace(y = ~`Total - Sex`, name = "Total - Sex") %>%
      layout(title = "Population of Visible Minority Groups by Education and Field of Study, 2016", 
             yaxis = list(title = 'Number of People'), barmode = 'group')
    
    fig
  })
  
  output$ecplot <- renderPlotly({
    
    # Require the ethnocultural data frame
    req(ec())
  
    df <- melt(data.table(ec()), id.vars = 'Visible Minority Group')
    
    # Plot the column graph
    ecp <- ggplot(df, aes(x = `Visible Minority Group`, y = value, fill = variable)) +
     geom_col(position = "dodge") + 
      scale_y_continuous(labels = scales::number) +
     labs(title = "Immigrant Status Indicator",
          x = "Visible Minority Group",
          y = "Number of People",
          fill = "Immigrant Status")
    
    ggplotly(ecp)
    
   })
  
  output$ecplot2 <- renderPlotly({
    
    # Require the ethnocultural data frame
    req(ec2())
    
    df <- melt(data.table(ec2()), id.vars = 'Visible Minority Group 2')
    
    # Plot the column graph
    ecp2 <- ggplot(df, aes(x = `Visible Minority Group 2`, y = value, fill = variable)) +
      geom_col(position = "dodge") + 
      scale_y_continuous(labels = scales::number) +
      labs(title = "Generation Status Indicator",
           x = "Visible Minority Group 2",
           y = "Number of People",
           fill = "Generation Status")
    
    ggplotly(ecp2)
    
  })
  
  output$splot <- renderPlotly({
    
    # Require filtered_degInc data frame
    req(filtered_degInc())
    
    fit <- lm(`Percentage by Total Income` ~ `Percentage by Generation`, data = filtered_degInc()) %>%
          fitted.values()
    
    # Plot the scatter plot
    sp <- plot_ly(data = filtered_degInc(), x = ~`Percentage by Generation`,
                  y = ~`Percentage by Total Income`, type = 'scatter', mode = 'markers',
                  text = ~paste('Age: ', Age,
                                 '<br>Sex: ', Sex,
                                 '<br>Level of Degree:', Education,
                                 '<br>Number of People by Generation: ', `Number of People 2`,
                                 '<br>Number of People by Total Income:', `Number of People`),
                  color = ~`Visible Minority`,
                  colors = my_colors,
                  width = 1000,
                  height = 600) %>%
      add_trace(x = ~`Percentage by Generation`, y = fit, mode = "lines") # Regression lines are overlapping

    sp <- sp %>%
      layout(title = "Generation vs Employment Income",
             xaxis = list(title = "Generation status with No certificate, diploma, or degree (%)"),
             yaxis = list(title = "Number of People with \"Median Total income ($)\" (%)")
             )

    sp
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)