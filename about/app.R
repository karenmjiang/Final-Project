library(shiny)
library(shinythemes)
library(ggplot2)
library(readr)
library(tidyverse)
library(usmap)

aco <- readRDS(file = "data/aco.RDS")
county <- readRDS(file = "data/county.RDS")

# Define UI for application that draws a histogram
ui <- navbarPage(
    "2018 Accountable Care Organizations",

# Intro -------------------------------------------------------------------

    
    tabPanel("Intro",
        fluidRow(
            column(2, ""),
            column(8, 
                   includeHTML("intro.html")
                   ),
            column(2, "")
        )
        
    ),


# Results -----------------------------------------------------------------

    tabPanel("Results",
        fluidRow(
            column(2, ""),
            column(8,
                   includeHTML("results.html")
                   ),
            column(2, "")
        )
        
    ),
    
# Data Exploration --------------------------------------------------------

    tabPanel("Data Exploration",
             fluidPage(theme = shinytheme("paper"),
                 
                 titlePanel("Accountable Care Organizations"),
                 
                 p(""),
                 h3("What does the ACO landscape look like?"), 
                 p("Let's start by look at ACOs nationwide by counties."),
                
                 
                 sidebarLayout(
                     sidebarPanel(
                         
                         selectInput(inputId = "beneficiary_2",
                                     label = "Beneficiary Type",
                                     choices = county$Beneficiary),
                         
                         selectInput(inputId = "category_2",
                                     label = "Category", 
                                     choices = county$Category),
                     ),
                     
                     mainPanel(
                         plotOutput("usa")
                     )
                 ),
                 
                 h3("ACOs by State"),
                 p("Here, we can take a look state by state."),
                 
                 sidebarLayout(
                     
                     sidebarPanel(
                         
                         selectInput(inputId = "state",
                                     label = "State",
                                     choices = county$state_name), 
                         
                         selectInput(inputId = "beneficiary",
                                     label = "Beneficiary Type",
                                     choices = county$Beneficiary),
                         
                         selectInput(inputId = "category",
                                     label = "Category", 
                                     choices = county$Category)
                     ),
                     mainPanel(
                         plotOutput("state")
                         
                     )
                 )
             )
             ),

# Discussion --------------------------------------------------------------

    
    tabPanel(
        "Discussion",
        titlePanel("Discussion Title"),
        p(
            "What to say, what to say?"
        )
    ),
    

# About -------------------------------------------------------------------


    tabPanel(
        "About",
        titlePanel("About"),
        p(
            "This project looks to examine the ACO landscape in 2018. Can we identify predictors or find trends in successful ACOs?"
        ),
        h3("Project Background"),
        p(
            "In 2018, the National Health Expenditure accounted for 17.7% of GDP, which equates $3.6 trillion, or $11,172.00 per person. In order to control healthcare costs, the Center for Medicare and Medicaid Services (CMS) is shifting towards value-based payment systems through the Medicare Shared Saving Program (MSSP). Accountable Care Organizations (ACOs) represent one voluntary and emergent payment structure under MSSP."
        ),
        p(
            "Medicare traditionally paid providers and hospitals through a Fee For Service (FFS) payment system. That is, payments are made to providers and hospitals for each service provided. This creates the incentive to provide as many services as possible for patients. This has led to overutilization and increased costs for the health system and duplicative services and poor coordination of care for patients."
        ),
        p(
            "ACOs attempt to remedy some of these issues.
An ACO is a group of hospitals, physicians, other providers who come together voluntarily to provide better coordinated care for their Medicare Patients and limit unncessary spending. ACOs engage in financial risk, by accepting a capitated payment per patient per month. If the spending for care is less than the alloted amount, the ACO is able to keep the difference. However, if spending exceeds the amount, the ACO must pay back the difference to CMS. "
        ),
        p(
            "In order to prevent skimping on care for patients, ACOs must also provide outcome data in order to be eligible for savings."
        ),
        h3("Data Sources"),
        p(
            a(href = "https://data.cms.gov/Special-Programs-Initiatives-Medicare-Shared-Savin/2018-Number-of-ACO-Assigned-Beneficiaries-by-Count/qu23-5j39
", "2018 Number of ACO Assigned Beneficiares")
        ),
        p(
            "Assigned beneficiary person-year counts are based on certified ACO Participant Lists for the performance year (PY) 2018 and based on the assignment methodology and definition of dual eligible beneficiaries in effect for that performance year."
        ),
        p(
            a(href = "https://data.cms.gov/Special-Programs-Initiatives-Medicare-Shared-Savin/2018-County-level-FFS-Data-for-Shared-Savings-Prog/sju4-8k6c", "2018 County Level FFS Data")
        ),
        p(
            "Aggregate data consisting of per capita Parts A and B FFS expenditures, average CMS-HCC prospective risk scores and total person-years for assignable beneficiaries by Medicare enrollment type (End Stage Renal Disease (ESRD), disabled, aged/dual eligible, aged/non-dual eligible). It is the intent of CM to publish this PUF annually, in the summer following the conclusion of the calendar year to which it relates."
        ),
        p(
            a(
                href = "https://data.cms.gov/Special-Programs-Initiatives-Medicare-Shared-Savin/2018-Shared-Savings-Program-SSP-Accountable-Care-O/v47u-yq84",
                "2018 Shared Savings Program Accountable Care Organization"
            )
        ),
        p(
            "2018 SSP ACO PUF - To address the increasing number of requests for SSP ACO data, the Centers for Medicare (CM) has created a standard analytical file that CMS can use to efficiently satisfy these requests. It is the intent of CM to publish the ACO-level public-use file (PUF) that contains ACO-specific metrics as well as summarized beneficiary and provider information for each performance year of the SSP program."
        ),
        h3("About Me"),
        p(
            "My name is Karen Jiang. I am an MPH Candidate in Health Policy at Harvard T. H. Chan School of Public Health.
             You can reach me at karenjiang@hsph.harvard.edu"
        )
    )
)


# Server ------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$usa <- renderPlot({
        c <- county %>% 
            filter(Beneficiary == input$beneficiary_2,
                   Category == input$category_2)
        
        plot_usmap(data = c,
                   regions = "counties",
                   values = "Values",
                   color = "white") +
            scale_fill_continuous(low = "#DFF0EA", high = "#2A7886") +
            theme(legend.position = "bottom",
                  legend.direction = "horizontal")
        
    })
    
    output$state <- renderPlot({
        d <- county %>%
            filter(Beneficiary == input$beneficiary,
                   Category == input$category) 
        
        plot_usmap(data = d,
                   regions = "counties",
                   include = input$state,
                   values = "Values",
                   color = "white") +
            scale_fill_continuous(low = "#DFF0EA", high = "#2A7886") +
            theme(legend.position = "bottom",
                  legend.direction = "horizontal") + 
            labs(
                title = paste(input$beneficiary, "Beneficiaries in ", input$state),
                subtitle = paste("by ", input$category),
                fill = paste("Count of ", input$category)
            )
    },
    )
    
}

# Run the application
shinyApp(ui = ui, server = server)