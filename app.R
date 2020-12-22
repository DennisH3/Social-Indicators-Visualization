# Task: Create a prototype data visualization for SII-IIS
# Author: Dennis Huynh
# Date: 11/02/2020

# Install packages
#install.packages("plotly")
#install.packages("leaflet")
#Need to conda install r-rgdal

# Load packages
library(shiny)
library(tidyverse)
library(data.table)
library(readxl)
library(plotly)
library(leaflet)
library(rgdal)
library(htmltools)
library(RColorBrewer)

# Original combined file size was 847.158 MB (274) and 17.062 MB (275)
# Reduced size = 561.656176 MB
# Save 302.563824 MB
# The datatable with the columns of interest (Year, Geography, and the DIM columns)


# Data loading and preprocessing
# Should use merge(names(select(read.csv(file1), c(list of columns to keep))) <- c(list of new colnames), names(select(read.csv(file2), c(list of columns to keep))) <- c(list of colnames), all = TRUE) instead.
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
synData <- read.csv("Synthethic data -income and ethnocultural vars.csv", check.names = FALSE)

# Read file for scatter plot (Generation and Income)
#degInc <- read.csv("combined214and275.csv", check.names = FALSE)

# Important to set check.names TRUE here. There is something about the column
# names that makes dplyr not recognize them if check.names is FALSE.
#degInc <- read.csv("combined214and275.csv", check.names = TRUE)
# After using check.names, this adds spaces to the column names so that the rest
# of the code can be left unchanged.
#names(degInc) <- gsub("\\.", " ", names(degInc))

# Calculate percentages by VisMin
#degInc <- degInc %>%
#            dplyr::group_by(`Visible Minority`) %>%
#            dplyr::mutate(`Percentage by Generation` = `Number of People 2`/sum(`Number of People 2`) * 100) %>%
#            dplyr::mutate(`Percentage by Total Income` = `Number of People`/sum(`Number of People`) * 100)

# Read file for line graph
lineData <- select(read.csv("Synth_data_01-16.csv", check.names = FALSE), 
                   c("Year", "Visible minorities", "Sex", "Generation Status", "Age groups", "Average income"))

# Read file for dynamic scatter plot
spData <- read.csv("VM, ethnocultural, socioeconomic.csv", check.names = FALSE)

# Read the GeoJson file
CMA <- readOGR("~/social_indicatorsVis/TopoJSON/CMA_CA_2016_with_Residuals.json")

# Create a copy of the original CMA data (only do this once)
# Always use this to merge, since have to update CMA@data
ogCMAData <- CMA@data

# Read csv file
# Original file: 98-400-X2016214_English_CSV_data.csv
# To make the file, first select columns to keep, then pivot_longer Generation Status
employGen <- read.csv("98-400-X2016_reduced_pivoted_data.csv", check.names = FALSE)

# Data Cleaning
# Change Number of People to Population
setnames(employGen, colnames(employGen[8]), "Population")

# If value in Population column is "F" or "...", assign NA
employGen$Population[employGen$Population == "F" | employGen$Population == "..."] <- NA

# Change Population column to integer
employGen$Population <- as.integer(employGen$Population)

# Change Geography to CMANAME to be the key to merge by
setnames(employGen, colnames(employGen[2]), "CMANAME")

# Remove all rows for Population with NA
employGen <- employGen[complete.cases(employGen),]

# Read the GeoJson file and keep only FEDENAME as key to merge by
CAN <- readOGR("gfedTopo.json")

# Keep only FEDENAME as keys to merge by
CAN@data <- select(CAN@data, "FEDENAME")

# Rename it to match with VoterTurnout
names(CAN@data) <- "Electoral District Name"

# Create a copy of the original CMA data (only do this once)
# Always use this to merge, since have to update CMA@data
ogCANData <- CAN@data

# Read xlsx file
# Read the sheets separately and remove unnecessary columns
voterTurnout <- select(as.data.frame(read_excel("FED- vismin, income and voting data.xlsx", sheet = 1)),
                       -"Electoral District Number")
income <- select(as.data.frame(read_excel("FED- vismin, income and voting data.xlsx", sheet = 2)),
                 -"Geo_Code")
vismin <- select(as.data.frame(read_excel("FED- vismin, income and voting data.xlsx", sheet = 4)),
                 -"Geo_Code")

# Change names to match with voterTurnout
names(income) <- c("Province", "Electoral District Name", "Average after-tax household income ($)")
names(vismin) <- c("Province", "Electoral District Name", "Visible Minority Groups",
                   "Total", "Male", "Female", "Percentage Female", "Percentage Male")

# Calculate average for income
income <- income %>%
  group_by(Province, `Electoral District Name`) %>%
  summarise(`Average after-tax household income ($)` = mean(`Average after-tax household income ($)`),
            .groups = 'drop')

# Set color ramp 
my_colors <- colorRampPalette(brewer.pal(15, 'Dark2'))(15)


# To create a hierarchy of buttons, use conditional panels
# Define UI ---------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Social Inclusion Data Visualization Tool"),
  tabsetPanel(
    
    # Bar Graphs Tab
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
                                multiple = TRUE,
                                options = list(maxItems = 6,
                                               placeholder = 'Please select an option below',
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
                 
                 selectizeInput("VM", 
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
                                selected = "Total Visible Minority",
                                multiple = TRUE
                 ),
                 
                 selectizeInput("VM2", 
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
                                selected = "Total Visible Minority 2",
                                multiple = TRUE
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
                   Then select from either the Visible Minorities. This will produce 3 column graphs"),
                 plotlyOutput("sBar", inline = TRUE, width = 1000, height = 400),
                 br(),
                 plotlyOutput("ecplot", inline = TRUE, width = 1000, height = 400),
                 br(),
                 plotlyOutput("ecplot2", inline = TRUE, width = 1000, height = 400)
               )
             )
    ),
    
    # Scatter plot and Line Graph Tab
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
                 
                 # selectizeInput("g", 
                 #                label = "Geography",
                 #                choices = unique(degInc$Geography),
                 #                selected = "Canada"
                 #                
                 # ),
                 
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
                                multiple = TRUE,
                                options = list(maxItems = 6,
                                               placeholder = 'Please select an option below',
                                               onInitialize = I('function() { this.setValue(""); }')
                                )
                                    
                 ),
                 
                 h4("Indicators"),
                 
                 # helpText("For the example, only No certificate, diploma or degree is provided"),
                 # 
                 # selectizeInput("dos", 
                 #                label = "Degree of Study", 
                 #                choices = unique(degInc$Education),
                 #                selected = "No certificate, diploma or degree"
                 #                
                 # ),
                 
                 selectizeInput("gen",
                                label = "Generation Status",
                                choices = sort(unique(synData$`Generation Status`)),
                                selected = "First generation",
                                multiple = TRUE
                 ),
                 
                 helpText("This is for the scatter plot"),
                 
                 selectizeInput("VisM", 
                                label = "Visible Minority for Scatter plot",
                                choices = unique(synData$`Visible Minority`),
                                selected = "South Asian",
                                multiple = TRUE
                 ),
                 
                 helpText("This is for the line graph"),
                 
                 selectizeInput("VisMi", 
                                label = "Visible Minority for Line Graph",
                                choices = unique(lineData$`Visible minorities`),
                                selected = "South Asian",
                                multiple = TRUE
                 ),
                 
                 h4("Sub-populations"),
                 
                 selectizeInput("ag", 
                                label = "Age Group",
                                choices = unique(synData$`Age groups`),
                                selected = "15-29",
                                multiple = TRUE
                 ),
                 
                 selectizeInput("Sex", 
                                label = "Sex",
                                choices = sort(unique(synData$Sex), decreasing = TRUE),
                                selected = "Female",
                                multiple = TRUE
                 )
                 
                 # helpText("For the example, only Median total income ($) is provided"),
                 # 
                 # selectizeInput("Income",
                 #                label = "Income",
                 #                choices = unique(degInc$`Total Income Groups`),
                 #                selected = "Median total income ($)"
                 # )
                 
              ),
                 
             mainPanel(
               h1("Scatter Plot and Line Graph"),
               h4("To filter the data, please select Generation Status, Age, and Sex first.
                  Then select from either the Visible Minorities. This will produce a scatter plot"),
               plotlyOutput("splot", inline = TRUE, width = 1000, height = 600),
               br(),
               p("Note: Some combinations of filters will result in an error because that record does not exist in the data.
                 </br> The relationship for the trend lines is how percentage employed affects mean income"),
               br(),
               plotlyOutput("lgraph", inline = TRUE, width = 1000, height = 600)
             )
         )
    ),
    
    # Dynamic Scatter Plot Tab
    tabPanel("Dynamic Scatter Plot", fluid = TRUE,
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
                                multiple = TRUE,
                                options = list(maxItems = 6,
                                               placeholder = 'Please select an option below',
                                               onInitialize = I('function() { this.setValue(""); }')
                                )

                 ),

                 helpText("Select the X variable"),

                 selectizeInput("inputX",
                                label = "X Variable",
                                choices = list("Percentage employed Full-time",
                                               "Mean Income",
                                               "Percentage with a post-secondary degree",
                                               "Percentage home owners"),
                                selected = "Percentage with a post-secondary degree"

                 ),

                 helpText("Select the Y variable"),

                 selectizeInput("inputY",
                                label = "Y Variable",
                                choices = list("Percentage employed Full-time",
                                               "Mean Income",
                                               "Percentage with a post-secondary degree",
                                               "Percentage home owners"),
                                selected = "Mean Income"

                 ),

                 h4("Indicators"),

                 selectizeInput("genS",
                                label = "Generation Status",
                                choices = sort(unique(spData$`Generation Status`)),
                                selected = "First generation",
                                multiple =  TRUE
                 ),

                 selectizeInput("VisiM",
                                label = "Visible Minority",
                                choices = unique(spData$`Visible Minority`),
                                selected = "Not a visible minority",
                                multiple = TRUE
                 ),

                 h4("Sub-populations"),

                 selectizeInput("AgeG",
                                label = "Age Group",
                                choices = unique(spData$`Age group`),
                                selected = "15-29",
                                multiple = TRUE
                 ),

                 selectizeInput("spSex",
                                label = "Sex",
                                choices = unique(spData$Sex),
                                selected = "Female",
                                multiple = TRUE
                 )

               ),

               mainPanel(
                 h1("Dynamic Scatter Plot"),
                 h4("To filter the data, please select Visible Minority, Generation Status, Age, and Sex first.
                  Then select the X and Y variables. This will produce a scatter plot"),
                 plotlyOutput("dynSP", inline = TRUE, width = 1000, height = 600),
                 br(),
                 p("Note: Some combinations of filters will result in an error because that record does not exist in the data.")
               )
             )
    ),
    
    # Choropleth Tab
    tabPanel("Income by CMA", fluid = TRUE,
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
                 
                 h4("Indicators"),
                 
                 selectizeInput("Income",
                                label = "Income",
                                choices = unique(employGen$`Total income groups`),
                                selected = "Total - Total income"
                 ),
                 
                 selectizeInput("gs",
                                    label = "Generation Status",
                                    choices = sort(unique(employGen$`Generation Status`)),
                                    selected = "Total - Generation status"
                 ),
                 
                 selectizeInput("VisMin", 
                                    label = "Visible Minority",
                                    choices = unique(employGen$`Visible minority`),
                                    selected = "Total - Visible minority"
                 ),
                 
                 h4("Sub-populations"),
                 
                 selectizeInput("Age", 
                                    label = "Age Group",
                                    choices = unique(employGen$Age),
                                    selected = "Total - Age"
                 ),
                 
                 selectizeInput("s", 
                                    label = "Sex",
                                    choices = sort(unique(employGen$Sex), decreasing = TRUE),
                                    selected = "Total - Sex"
                 )
                 
               ),
               
               mainPanel(
                 h1("Choropleth"),
                 h4("To filter the data, please select Total Income Group, Generation Status, Age, and Sex first.
                  Then select the Visible Minorities."),
                 leafletOutput("map", width = 1000, height = 600),
                 br(),
                 p("Note: Some combinations of filters will result in an error because that record does not exist in the data.")
               )
             )
    ),
  
    # Choropleth Filled Tab
    tabPanel("Voter Turnout", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 
                 helpText("Changing this does nothing"),
                 
                 selectizeInput("yr", 
                                label = "Year",
                                choices = list("2011", 
                                               "2015",
                                               "2019"),
                                selected = "2011"
                 ),
                 
                 h4("Indicators"),
                 
                 selectizeInput("VisiMin", 
                                label = "Visible Minority",
                                choices = unique(vismin$`Visible Minority Groups`),
                                selected = "West Asian"
                 ),
                 
                 selectizeInput("fillBy",
                                label = "Choropleth choice",
                                choices = c("Population", "Electors", "Polling Stations", 
                                            "Valid Ballots", "Rejected Ballots", "Total Ballots
                                            Cast", "Average after-tax household income ($)", 
                                            "Total", "Male", "Female"),
                                selected = "Population"
                  )
                 
               ),
               
               mainPanel(
                 h1("Select indicators by Federal Electoral Districts and visible minority groups"),
                 leafletOutput("fed", width = 1000, height = 600)
                 )
             )
    ),
    
    # Debug Tab to check if tables are properly filtering
    tabPanel(
      "Debug", fluid = TRUE,
      h1("Filtered Data Tables"),
      br(),
      h2("Data frame for bar graph 1"),
      dataTableOutput("sDF"), # data frame for vismin*sex
      h2("Filtered data for bar graph 2"),
      dataTableOutput("df"), # filter_data()
      #h2("Data frame for bar graph 2"),
      #dataTableOutput("df2"), # data frame for ec()
      h2("Filtered data for bar graph 3"),
      dataTableOutput("df3"), # filter_data2(),
      #h2("Data frame for bar graph 3"),
      #dataTableOutput("df4"), # data frame for ec2()
      #dataTableOutput("DI"), # data frame for degIncome
      h2("Data frame for Employment Income Scatter plot"),
      dataTableOutput("EmI"), # data frame for employment income
      h2("Data frame for dynamic scatter plot"),
      dataTableOutput("spDF")
    )
  )
)

# Define server logic ---------------------------------------------------------------
server <- function(input, output) {
  
  # Reactive values ----------------------------------------------------------
  # This reactive filters for sex to build the first column graph on the first tab
  filtered_sex <- reactive({
    # Require geography, degree, field of study, and age inputs
    req(input$geo, input$deg, input$fos, input$age, input$VM)
    
    # Copy ogDT
    newDT <- ogDT
    
    # Filter values
    newDT <- filter(newDT, Geography == input$geo)
    newDT <- filter(newDT, Education == input$deg)
    newDT <- filter(newDT, `Field of Study` == input$fos)
    newDT <- filter(newDT, Age == input$age)
    
    # Remove rows where column 13 (Immigrant Status is NA)
    newDT <- newDT[complete.cases(newDT[ , 13]),]
    
    # Transform the data frame
    newDT <- newDT %>%
      select(c(1:22)) %>%
      pivot_longer(c(6:12, 15:22), names_to = "Visible Minority Groups", values_to = "Number of People") %>%
      pivot_wider(names_from = Sex, values_from = "Number of People", values_fn = sum) %>%
      filter(`Visible Minority Groups` %in% input$VM)
    
    return(newDT)
  })
  
  # This reactive will filter ogDT by inputs for Immigrant Status
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
       
      # # Remove rows where column 13 (Generation Status is NA)
      # newDT <- newDT[complete.cases(newDT[ , 13]),]
      
      # Transform the data
      newDT <- ogDT %>%
        select("Chinese", "Black", "Filipino", "Latin American", "Arab", "Korean", "Japanese", 
               "Total Visible Minority", "Total visible minority population", "South Asian", 
               "Southeast Asian", "West Asian", "Visible minority, n.i.e.", "Multiple visible minorities", 
               "Not a visible minority", "Immigrant Status") %>%
        pivot_longer(c(1:15), names_to = "Visible Minority Groups", values_to = "Number of People") %>%
        pivot_wider(names_from = `Immigrant Status`, values_from = "Number of People", values_fn = sum) %>%
        filter(`Visible Minority Groups` %in% input$VM) 
      
      return(newDT)
  })
  
  # Select Ethnocultural groups for the first bar plot
  # ec <- reactive ({
  #   
  #   # Require the filtered table and the Visible Minority input
  #   req(filtered_data(), input$VM)
  # 
  #   # From the filtered data select columns based on Visible Minority 2 Widget
  #   df <- filtered_data() %>%
  #     select(input$VM)
  #   
  #   # Transpose the data
  #   dft <- as.data.frame(t(as.matrix(df)))
  #   
  #   # Change row names and column names
  #   rownames(dft) <- colnames(df)
  #   colnames(dft) <- filtered_data()[,13]
  #   
  #   # Make the row name into a column
  #   dft <- cbind(rownames(dft), data.frame(dft, row.names=NULL))
  #   
  #   # Name the first column
  #   colnames(dft)[1] <- "Visible Minority Group"
  #   
  #   return(dft)
  # })
  
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
      
      # Transform the data
      newDT <- ogDT %>%
        select("Chinese", "Black", "Filipino", "Latin American", "Arab", "Korean", "Japanese", 
               "Total Visible Minority 2", "Total visible minority population 2", "South Asian 2", 
               "Southeast Asian 2", "West Asian 2", "Visible minority, n.i.e. 2", 
               "Multiple visible minorities 2", "Not a visible minority 2", "Generation Status") %>%
        pivot_longer(c(1:15), names_to = "Visible Minority Groups", values_to = "Number of People") %>%
        pivot_wider(names_from = `Generation Status`, values_from = "Number of People", values_fn = sum) %>%
        filter(`Visible Minority Groups` %in% input$VM2)
      
      return(newDT)
    }
  )
  
  # Select Ethnocultural groups for the first bar plot
  # ec2 <- reactive (
  #   {
  #     
  #     # Require the filtered table and the Visible Minority input
  #     req(filtered_data2(), input$VM2)
  #     
  #     # From the filtered data select columns based on Visible Minority 2 Widget
  #     df <- filtered_data2() %>%
  #       select(input$VM2)
  # 
  #     # Transpose the data
  #     dft <- as.data.frame(t(as.matrix(df)))
  #     
  #     # Change row names and column names
  #     rownames(dft) <- colnames(df)
  #     colnames(dft) <- filtered_data2()[,23]
  #     
  #     # Make the row name into a column
  #     dft <- cbind(rownames(dft), data.frame(dft, row.names=NULL))
  #     
  #     # Name the first column
  #     colnames(dft)[1] <- "Visible Minority Group 2"
  #     
  #     return(dft)
  #   }
  # )
  
  # This reactive will filter degInc
  # filtered_degInc <- reactive(
  #   {
  #     # Require Geography, sex, gen, and VisM inputs
  #     req(input$g, input$Sex, input$gen, input$VisM)
  #     
  #     # Note that degree and Income are static
  #     
  #     # Filter values
  #     newDT <- degInc
  #     
  #     newDT <- filter(newDT, Geography == input$g)
  #     #newDT <- filter(newDT, Age == input$ag)
  #     newDT <- filter(newDT, Sex == input$Sex)
  #     newDT <- filter(newDT, `Generation Status` == input$gen)
  #     newDT <- newDT %>% filter(`Visible Minority` %in% input$VisM)
  #     
  #     return(newDT)
  #   }
  # )
  
  # This reactive filters the synthetic data for the scatter plot
  filtered_synData <- reactive({
    
    # Require Sex, Age, Generation Status, VisMin
    req(input$Sex, input$ag, input$gen, input$VisM)
    
    # Filter values
    newDT <- synData %>%
      filter(`Age groups` %in% input$ag, 
             Sex %in% input$Sex, 
             `Generation Status` %in% input$gen,
             `Visible Minority` %in% input$VisM)

    return(newDT)
  })
  
  # This reactive filters the line plot data
  filtered_lineData <- reactive({
    
    # Require Sex, Age, Generation Status, VisMin
    req(input$Sex, input$ag, input$gen, input$VisMi)
    
    # Filter values
    newDT <- lineData %>%
      filter(`Age groups` %in% input$ag, 
             Sex %in% input$Sex, 
             `Generation Status` %in% input$gen,
             `Visible minorities` %in% input$VisMi)
    
    # If any of the inputs have multiple selections, sum Average income
    if (input$Sex > 1 | input$gen > 1 | input$ag > 1) {

      # Group by Year and VisMin, then calculate total average income
      newDT <- newDT %>%
        group_by(Year, `Visible minorities`) %>%
        summarise(`Average income` = sum(`Average income`), .groups = 'drop')

      # Convert to data frame
      newDT <- as.data.frame(newDT)
    }
    
    # Pivot by VisMin
    newDT <- pivot_wider(newDT, 
                         names_from = `Visible minorities`, 
                         values_from = `Average income`)
    
    return(newDT)
  })
  
  # This reactive filters spData for the dynamic scatter plot
  filtered_sp <- reactive({
    
    # Require Sex, Age, Generation Status, VisMin, inputX, and inputY
    req(input$spSex, input$AgeG, input$genS, input$VisiM, input$inputX, input$inputY)
    
    # Filter the data
    newDT <- filter(spData,
                    `Visible Minority` %in% input$VisiM,
                    Sex %in% input$spSex,
                    `Generation Status` %in% input$genS,
                    `Age group` %in% input$AgeG)
    
    # Select the columns to keep
    newDT <- select(newDT, c("Visible Minority", "Ethnic origin", "Place of birth", "Sex",
                             "Generation Status", "Age group", "Count", input$inputX, input$inputY))
    
    return(newDT)
  })
  
  # This reactive filters the employGen data for the choropleth
  filtered_EG <- reactive({
    
    # Require Sex, Age, Generation Status, VisMin, and Income
    req(input$s, input$Age, input$gs, input$VisMin, input$Income)
    
    # Filter employGen by user input
    EG <- employGen %>%
      filter(`Visible minority` == input$VisMin, Age == input$Age, 
             Sex == input$s, `Total income groups` == input$Income, 
             `Generation Status` == input$gs)
    
    return(EG)
  })
  
  filtered_VM <- reactive({
    
    # VisiMin
    req(input$VisiMin)
    
    # Filter vismin data to one group
    VM <- filter(vismin, `Visible Minority Groups` == input$VisiMin)
    
    return(VM)
  })
  
  # Output ---------------------------------------------------
  output$df <- renderDataTable({filtered_data()})
  
  #output$df2 <- renderDataTable({ec()})
  
  output$df3 <- renderDataTable({filtered_data2()})
  
  #output$df4 <- renderDataTable({ec2()})
  
  #output$DI <- renderDataTable({filtered_degInc()})
  
  output$EmI <- renderDataTable({filtered_synData()})
  
  output$sDF <- renderDataTable({filtered_sex()})
  
  output$spDF <- renderDataTable({filtered_sp()})
  
  output$sBar <- renderPlotly({
    req(filtered_sex())
    
    fig <- plot_ly(filtered_sex(), x = ~`Visible Minority Groups`, y = ~Female, type = 'bar', name = "Female",
                   width = 1000, height = 400) %>%
      add_trace(y = ~Male, name = "Male") %>%
      add_trace(y = ~`Total - Sex`, name = "Total - Sex") %>%
      layout(title = "Population of Visible Minority Groups by Education and Field of Study, 2016", 
             yaxis = list(title = 'Number of People'), barmode = 'group')
    
    fig
  })
  
  output$ecplot <- renderPlotly({
    
    # Require the first filtered ogDT data frame
    req(filtered_data())
  
    # df <- melt(data.table(ec()), id.vars = 'Visible Minority Group')
    # 
    # # Plot the column graph
    # ecp <- ggplot(df, aes(x = `Visible Minority Group`, y = value, fill = variable)) +
    #  geom_col(position = "dodge") + 
    #   scale_y_continuous(labels = scales::number) +
    #  labs(title = "Immigrant Status Indicator",
    #       x = "Visible Minority Group",
    #       y = "Number of People",
    #       fill = "Immigrant Status")
    # 
    # ggplotly(ecp)
    
    fig <- plot_ly(filtered_data(), x = ~`Visible Minority Groups`, y = ~Immigrants, type = 'bar', name = "Immigrants") %>%
      add_trace(y = ~`Non-permanent residents`, name = "Non-permanent residents") %>%
      add_trace(y = ~`Non-immigrants`, name = "Non-immigrants") %>%
      add_trace(y = ~`Total - Immigrant status`, name = "Total - Immigrant status") %>%
      layout(title = "Population of Visible Minority Groups by Education, Field of Study and Immigrant Status, 2016", 
             yaxis = list(title = 'Number of People'), barmode = 'group')
    
    fig
   })
  
  output$ecplot2 <- renderPlotly({
    
    # Require the second filtered data frame
    req(filtered_data2())
    
    # df <- melt(data.table(ec2()), id.vars = 'Visible Minority Group 2')
    # 
    # # Plot the column graph
    # ecp2 <- ggplot(df, aes(x = `Visible Minority Group 2`, y = value, fill = variable)) +
    #   geom_col(position = "dodge") + 
    #   scale_y_continuous(labels = scales::number) +
    #   labs(title = "Generation Status Indicator",
    #        x = "Visible Minority Group 2",
    #        y = "Number of People",
    #        fill = "Generation Status")
    # 
    # ggplotly(ecp2)
    
    fig <- plot_ly(filtered_data2(), x = ~`Visible Minority Groups`, y = ~`Third generation or more`, 
                   type = 'bar', name = "Third generation or more") %>%
      add_trace(y = ~`Second generation`, name = "Second generation") %>%
      add_trace(y = ~`First generation`, name = "First generation") %>%
      add_trace(y = ~`Total - Generation status`, name = "Total - Generation status") %>%
      layout(title = "Population of Visible Minority Groups by Education, Field of Study and Generation Status, 2016", 
             yaxis = list(title = 'Number of People'), barmode = 'group')
    
    fig
    
  })
  
  output$splot <- renderPlotly({
    
    # degree vs Income scatter plot by Generation
    # # Require filtered_degInc data frame
    # req(filtered_degInc())
    # 
    # fit <- lm(`Percentage by Total Income` ~ `Percentage by Generation`, data = filtered_degInc()) %>%
    #       fitted.values()
    # 
    # # Plot the scatter plot
    # sp <- plot_ly(data = filtered_degInc(), x = ~`Percentage by Generation`,
    #               y = ~`Percentage by Total Income`, type = 'scatter', mode = 'markers',
    #               text = ~paste('Age: ', Age,
    #                              '<br>Sex: ', Sex,
    #                              '<br>Level of Degree:', Education,
    #                              '<br>Number of People by Generation: ', `Number of People 2`,
    #                              '<br>Number of People by Total Income:', `Number of People`),
    #               color = ~`Visible Minority`,
    #               colors = my_colors,
    #               width = 1000,
    #               height = 600) %>%
    #   add_trace(x = ~`Percentage by Generation`, y = fit, mode = "lines") # Regression lines are overlapping
    # 
    # sp <- sp %>%
    #   layout(title = "Generation vs Employment Income",
    #          xaxis = list(title = "Generation status with No certificate, diploma, or degree (%)"),
    #          yaxis = list(title = "Number of People with \"Median Total income ($)\" (%)")
    #          )
    # 
    # sp
    
    # Fixed version
    # You can apply lm() with group_by() by using do()
    # Source: https://github.com/tidyverse/dplyr/issues/2177
    #fit <- filtered_degInc() %>%
    #  group_by(`Visible Minority`) %>%
    #  do(lm(`Percentage by Total Income` ~ `Percentage by Generation`, data = .) %>%
    #       coef() %>%
    #       bind_rows()) %>%
    #  ungroup()
    
    # The names from the lm() won't play nice with dplyr, so we rename them
    #names(fit) <- c("Visible Minority", "intercept", "slope")
    
    # In order to make sure that the graph has the same observations in the same
    # order as our fitted dataset. Without this join they're out of sync and
    # the regression lines will be scribbled.
    #fitted <- filtered_degInc() %>%
    #  inner_join(fit, by = "Visible Minority") %>%
    #  mutate(predicted = slope * `Percentage by Generation` + intercept)
    
    # Plot the scatter plot
    #sp <- plot_ly(data = fitted, x = ~`Percentage by Generation`,
    #              y = ~`Percentage by Total Income`, type = 'scatter', mode = 'markers',
    #              text = ~paste('Age: ', Age,
    #                             '<br>Sex: ', Sex,
    #                             '<br>Level of Degree:', Education,
    #                             '<br>Number of People by Generation: ', `Number of People 2`,
    #                             '<br>Number of People by Total Income:', `Number of People`),
    #               color = ~`Visible Minority`,
    #               colors = my_colors,
    #               width = 1000,
    #               height = 600) %>%
    #add_trace(x = ~`Percentage by Generation`, y = ~predicted, mode = "lines") # Regression lines are overlapping
    
    # Require filtered_synData()
    req(filtered_synData())

    # Linear regression
    fit <- filtered_synData() %>%
      group_by(`Visible Minority`) %>%
      do(lm(`Mean Income` ~ `Percentage employed Full-time`, data = .) %>%
           coef() %>%
           bind_rows()) %>%
      ungroup()

    # The names from the lm() won't play nice with dplyr, so we rename them
    names(fit) <- c("Visible Minority", "intercept", "slope")

    # In order to make sure that the graph has the same observations in the same
    # order as our fitted dataset. Without this join they're out of sync and
    # the regression lines will be scribbled.
    fitted <- filtered_synData() %>%
      inner_join(fit, by = "Visible Minority") %>%
      mutate(predicted = slope * `Percentage employed Full-time` + intercept)
    
    # Plot the scatter plot
    sp <- plot_ly(data = fitted, x = ~`Percentage employed Full-time`, y = ~`Mean Income`, 
                  type = 'scatter', mode = 'markers',
                  text = ~paste('Ethnic origin: ', `Ethnic origins`,
                                '<br> Sex: ', Sex,
                                '<br> Age Group: ', `Age groups`,
                                '<br> Generation: ', `Generation Status`,
                                '<br> Proportion with University degree:', `Proportion with univeristy degree`,
                                '<br> Number of People: ', Counts),
                  hovertemplate = paste('<b>Mean Income</b>: $%{y}',
                                        '<br><b>Percentage employed Full-time</b>: %{x}<br>',
                                        '%{text}'),
                  color = ~`Visible Minority`,
                  width = 1000,
                  height = 600) %>%
      add_trace(x = ~`Percentage employed Full-time`, y = ~predicted, mode = "lines") # Add Regression lines
    
    # Add title and axes titles
    sp <- sp %>%
      layout(title = "Percentage employed Full-time by Mean",
             xaxis = list(title = "Percentage employed Full-time"),
             yaxis = list(title = "Mean income ($)")
      )
    
    sp
    
  })
  
  # line graph
  output$lgraph <- renderPlotly({
    
    # Require filtered_lineData
    req(filtered_lineData())
    
    # Create the base graph
    lp <- plot_ly(data = filtered_lineData(), x = ~Year)
    
    # Add each series one-by-one as new traces
    for (i in 2:length(colnames(filtered_lineData()))) {
      lp <- lp %>%
        add_trace(x = filtered_lineData()$Year, y = filtered_lineData()[[i]], 
                  type = "scatter", mode = "lines+markers",
                  name = colnames(filtered_lineData())[i])
    }
    
    # Note hovermode = "x unified" is not working as it is supposed to
    # Best work-around was used in xaxis with spike layout
    lp <- lp %>% 
      layout(title = "Average Income for Visible Minority Groups by Census Years", 
             hovermode = "x unified",
             xaxis = list(title = "Year",
                          showspikes = TRUE,
                          spikecolor = "black",
                          spikethickness = 2,
                          spikemode  = 'toaxis+across',
                          spikesnap = 'data',
                          showline=TRUE),
             yaxis = list(title = "Average Income ($)")
      )
    
    lp
    
  })
  
  output$dynSP <-renderPlotly({
    
    # Require filtered_sp()
    req(filtered_sp())
    
    # Linear regression
    fit <- filtered_sp() %>%
      group_by(`Visible Minority`) %>%
      do(lm(.[[input$inputY]] ~ .[[input$inputX]], data = .) %>%
           coef() %>%
           bind_rows()) %>%
      ungroup()
    
    # The names from the lm() won't play nice with dplyr, so we rename them
    names(fit) <- c("Visible Minority", "intercept", "slope")
    
    # In order to make sure that the graph has the same observations in the same
    # order as our fitted dataset. Without this join they're out of sync and
    # the regression lines will be scribbled.
    fitted <- filtered_sp() %>%
      inner_join(fit, by = "Visible Minority") %>%
      mutate(predicted = slope * filtered_sp()[[input$inputX]] + intercept)
    
    # Create the base graph
    fig <- plot_ly(fitted, x = ~fitted[[input$inputX]], y = fitted[[input$inputY]], 
                   type = "scatter", mode = "markers",
                   text = ~paste('<b>',paste(input$inputX, ':', sep = ""), '</b>', fitted[[input$inputX]],
                                 '<br><b>',paste(input$inputY, ':', sep = ""), '</b>', fitted[[input$inputY]],
                                 '<br> Ethnic origin: ', `Ethnic origin`,
                                 '<br> Place of birth', `Place of birth`,
                                 '<br> Sex: ', Sex,
                                 '<br> Age Group: ', `Age group`,
                                 '<br> Generation: ', `Generation Status`,
                                 '<br> Number of People: ', Count),
                   hovertemplate = paste('%{text}'),
                   color = ~`Visible Minority`,
                   width = 1000,
                   height = 600) %>%
      add_trace(x = ~fitted[[input$inputX]], y = ~predicted, mode = "lines") # Add Regression lines
    
    # Add title and axes titles
    fig <- fig %>%
      layout(title = paste(input$inputX, "by", input$inputY),
             xaxis = list(title = input$inputX),
             yaxis = list(title = input$inputY)
      )
    
    fig
  })
  
  # Choropleth
  output$map <- renderLeaflet({
    
    # Require filtered_EG()
    req(filtered_EG())
    
    # When merging the data, there should only be 30 rows (The intersect of CMANAME)
    # Merge the data and select the columns to keep
    CMA@data <- select(left_join(ogCMAData, filtered_EG()), c("CMANAME", "Visible minority", "Age", "Sex", 
                                                           "Total income groups", "Generation Status",
                                                           "Population"))
    
    # Create the ranges for the bins (8 ranges to fit colour palette)
    # Change bin intervals to make fill more visible
    bins <- c(0, 8000, 16000, 24000, 32000, 40000, 44000, 48000, Inf)
    
    # Colour palette for the intervals
    pal <- colorBin("Purples", domain = CMA$Population, bins = bins, na.color = "#fcfbfd")
    
    # Customize the hovertext over the map
    hoverText <- sprintf(
      "<strong>Visible Minority: %s</strong>
      <br>Geography: <em>%s</em>
      <br>Number of People: <em>%d</em>
      <br>Age: %s
      <br>Sex: %s 
      <br>Total income groups: %s 
      <br>Generation Status: %s", 
      CMA@data$`Visible minority`, CMA@data$CMANAME, CMA@data$Population, CMA@data$Age,
      CMA@data$Sex, CMA@data$`Total income groups`, CMA@data$`Generation Status`
    ) %>% lapply(htmltools::HTML)
    
    # Create the choropleth
    m <- leaflet(CMA) %>%
      setView(-106.3, 56.1, zoom = 5) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(Population),
        weight = 2,
        opacity = 1,
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = hoverText,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(pal = pal, values = ~Population, opacity = 0.7, title = "Number of People", position = "topright")
    
    m
    
  })
  
  # Choropleth
  output$fed <- renderLeaflet({
    
    # Require filtered_VM(), fillBy
    req(filtered_VM(), input$fillBy)
    
    # Merge the data
    CAN@data <- left_join(ogCANData, voterTurnout)
    CAN@data <- left_join(CAN@data, income)
    CAN@data <- left_join(CAN@data, filtered_VM())
    
    # Create the ranges for the bins (8 ranges to fit colour palette)
    # Find the max value in the selected column and divide by 8 
    interval <- max(CAN@data[[input$fillBy]])/8
    
    # Count the number of significant digits
    # Subtract one and make it negative (since R counts 1's digit at 0)
    sd <- (ceiling(log10(interval))-1)*-1
    
    # Create the bins
    bins <- c(round(interval, sd) * 0:7, Inf)
    
    # Colour palette for the intervals
    pal <- colorBin("Blues", domain = CAN@data[[input$fillBy]], bins = bins)
    
    # Customize the hovertext over the map
    # Use a nested-if statement or switch-case statement 
    # to customize the hovertext to only show input$fillBy indicators
    hoverText <- sprintf(
       "<strong>Visible Minority: %s</strong>
        <br>Province: <em>%s</em>
        <br>Electoral District Name: <em>%s</em>
        <br>Population: %d
        <br>Electors: %d
        <br>Polling Stations: %d
        <br>Valid Ballots: %d
        <br>Valid Ballots %%: %f
        <br>Rejected Ballots: %d
        <br>Rejected Ballots %%: %f
        <br>Total Ballots Cast: %d
        <br>Voter Turnout %%: %f
        <br>Average after-tax household income ($): %f
        <br>Total: %d
        <br>Male: %d
        <br>Female: %d
        <br>Percentage Male: %f
        <br>Percentage Female: %f", 
      CAN@data$`Visible Minority Groups`, CAN@data$Province, CAN@data$`Electoral District Name`,
      CAN@data$Population, CAN@data$Electors, CAN@data$`Polling Stations`, CAN@data$`Valid Ballots`,
      CAN@data$`Valid Ballots %`, CAN@data$`Rejected Ballots`, CAN@data$`Rejected Ballots %`,
      CAN@data$`Total Ballots Cast`, CAN@data$`Voter Turnout %`, 
      CAN@data$`Average after-tax household income ($)`, CAN@data$Total, CAN@data$Male,
      CAN@data$Female, CAN@data$`Percentage Male`, CAN@data$`Percentage Female`
    ) %>% lapply(htmltools::HTML)
    
    # Create the choropleth
    map <- leaflet(CAN) %>%
      setView(-106.3, 56.1, zoom = 2.5) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(CAN@data[[input$fillBy]]),
        weight = 2,
        opacity = 1,
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = hoverText,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "12px",
          direction = "auto"
        )
      ) %>%
      addLegend(pal = pal, values = ~CAN@data[[input$fillBy]], opacity = 0.7, title = input$fillBy, position = "topright")
    
    map
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
