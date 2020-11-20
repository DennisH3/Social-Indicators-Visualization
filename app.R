# Task: Create a prototype data visualization for SII-IIS
# Author: Dennis Huynh
# Date: 11/02/2020
# Log:
# 11/10/2020 - Completed UI input

# Load packages
library(shiny)
library(tidyverse)
library(data.table)

# Read csv files into a dataframe
df <- read.csv("98-400-X2016274_English_CSV_data.csv")
df2 <- read.csv("98-400-X2016275_English_CSV_data.csv")

# Full outer merge of both files
mergeDF <- merge(df, df2, all = TRUE)
#print(colnames(mergeDF))

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
                                choices = unique(mergeDF$GEO_NAME),
                                options = list( placeholder = 'Please select an option below',
                                                onInitialize = I('function() { this.setValue(""); }')
                                )
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
                                choices = unique(mergeDF$DIM..Highest.certificate..diploma.or.degree..15.),
                                options = list(placeholder = 'Please select an option below',
                                               onInitialize = I('function() { this.setValue(""); }')
                                )
                 ),
                 
                 selectizeInput("fos", 
                                label = "Field of Study", 
                                choices = sort(unique(mergeDF$DIM..Major.field.of.study...Classification.of.Instructional.Programs..CIP..2016..43.)),
                                options = list(placeholder = 'Please select an option below',
                                               onInitialize = I('function() { this.setValue(""); }')
                                )
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
                                                   "Not a visible minority")
                 ),
                 
                 h4("Sub-populations"),
                 
                 selectizeInput("age", 
                                label = "Age Group",
                                choices = unique(mergeDF$DIM..Age..9.),
                                options = list( placeholder = 'Please select an option below',
                                                onInitialize = I('function() { this.setValue(""); }')
                                )
                 ),
                 
                 selectizeInput("sex", 
                                label = "Sex",
                                choices = sort(unique(mergeDF$DIM..Sex..3.), decreasing = TRUE),
                                options = list( placeholder = 'Please select an option below',
                                                onInitialize = I('function() { this.setValue(""); }')
                                )
                 ),
                 
                 selectizeInput("immStatus", 
                                label = "Immigration Status",
                                choices = unique(mergeDF$DIM..Immigrant.status..4.),
                                options = list( placeholder = 'Please select an option below',
                                                onInitialize = I('function() { this.setValue(""); }')
                                )
                 ),
                 
                 selectizeInput("gen", 
                                label = "Generation Status",
                                choices = unique(mergeDF$DIM..Generation.status..4.),
                                options = list( placeholder = 'Please select an option below',
                                                onInitialize = I('function() { this.setValue(""); }')
                                )
                 )
               ),
               mainPanel(
                 h1("Filtered Data Table and Graphs"),
                 p("To filter the data, please select Geography, Degree, Field of Study, Age, and Sex first.
                   Then select the Visible Minorities"),
                 dataTableOutput("df"),
                 dataTableOutput("df2"),
                 plotOutput("ecplot")
               )
             )
    ),
    
    # Second Tab (Can ignore for now)
    tabPanel(
      "Similarities and Differences", fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("fcp", 
                             label = "Framework Components Participation" , 
                             choices = list("Labour Market" = 1, 
                                            "Choice 2" = 2, 
                                            "Choice 3" = 3),
                             selected = 1),
          
          checkboxGroupInput("dv", 
                             label = "Dependent Variable" , 
                             choices = list("Income" = 1, 
                                            "Choice 2" = 2, 
                                            "Choice 3" = 3),
                             selected = 1),
          
          checkboxGroupInput("iv", 
                             label = "Independent Variable" , 
                             choices = list("Employment" = 1, 
                                            "Choice 2" = 2, 
                                            "Choice 3" = 3),
                             selected = 1),
          
          selectInput("year", 
                      label = "Year",
                      choices = list("2016", 
                                     "2017",
                                     "2018", 
                                     "2019"),
                      selected = "2016"),
          
          selectInput("sex", 
                      label = "Sex",
                      choices = list("Male", 
                                     "Female"
                      ),
                      selected = "Male"),
          
          selectInput("immStatus", 
                      label = "Immigration Status",
                      choices = list("Immigrated before 1980", 
                                     "Immigrated before 1990",
                                     "Immigrated before 2000", 
                                     "Immigrated before 2010"),
                      selected = "Immigrated before 1980"),
          
          checkboxInput("observed", "Observed", value = FALSE),
          
          h4("Adjusted"),
          
          selectInput("ag", 
                      label = "Age Group",
                      choices = list("18-24", 
                                     "25-33",
                                     "34-41", 
                                     "42-50"),
                      selected = "18-24"),
          
          selectInput("geo", 
                      label = "Geography",
                      choices = list("Canada", 
                                     "United States"
                      ),
                      selected = "Canada")
        ),
        mainPanel(
          h1("Graphs to be plotted")
          # plotOutput("splot")
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  # The datatable with the columns of interest (Year, Geography, and the DIM columns)
  ogDT <- select(mergeDF, c(1, 4, 8, 11, 14, 17:24, 27, 30:38, 41:48))
  
  # Rename the columns
  setnames(ogDT, colnames(ogDT), c("Year", "Geography", "Education", "Age", "Sex", "Chinese", "Black", "Filipino", "Latin American",
                                   "Arab", "Korean", "Japanese", "Immigrant Status", "Field of Study", "Total Visible Minority",
                                   "Total visible minority population", "South Asian", "Southeast Asian", "West Asian", 
                                   "Visible minority, n.i.e.", "Multiple visible minorities", "Not a visible minority",
                                   "Generation Status", "Total Visible Minority 2", "Total visible minority population 2", 
                                   "South Asian 2", "Southeast Asian 2", "West Asian 2", "Visible minority, n.i.e. 2", 
                                   "Multiple visible minorities 2", "Not a visible minority 2"))
  
  # Reactive values ----------------------------------------------------------
  #
  # This reactive will filter ogDT by inputs
  filtered_data <- reactive(
    {
      
      # if no input, display ogDT
      if (input$year == "2016" & input$geo == "") {
        return(ogDT)
      } else {
        
        # Require degree, field of study, age, and sex inputs
        req(input$deg, input$fos, input$age, input$sex)
        
        # Filter for these values
        newDT <- ogDT %>%
          filter(ogDT$Geography == input$geo &
                 ogDT$Education == input$deg &
                 ogDT$`Field of Study` == input$fos &
                 ogDT$Age == input$age &
                 ogDT$Sex == input$sex
          # Use these 2 attributes for 2nd graph
          #& ogDT$`Immigrant Status` == input$immStatus &
          #ogDT$`Generation Status` == input$gen
                )
          return(newDT)
      }
    }
  )
  
  # Select Ethnocultural groups for the first bar plot
  ec <- reactive (
    {
      
      # Require the filtered table and the Visible Minority input
      req(filtered_data(), input$VM)
      
      # From the filtered data select columns based on Visible Minority Widget
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
    }
  )
  
  # Output ---------------------------------------------------
  output$df <- renderDataTable({filtered_data()})
  
  output$df2 <- renderDataTable({ec()})
  
  output$ecplot <- renderPlot({
    
    # Require the ethnocultural data frame
    req(ec())
  
    df <- melt(ec(), id.vars = 'Visible Minority Group')
    
    # Plot the column graph
    ggplot(df, aes(x = `Visible Minority Group`, y = value, fill = variable)) +
     geom_col(position = "dodge") + 
     labs(title = "Ethnocultural Indicator",
          x = "Visible Minority Group",
          y = "Number of People",
          fill = "Immigrant Status")
    
   })
}

# Run the app ----
shinyApp(ui = ui, server = server)