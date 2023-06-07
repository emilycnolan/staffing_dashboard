library(shiny)

data <- read.csv("StaffingDataSet.csv")
data <- data[rowSums(is.na(data))==0,]
data$Median.Household.Income <- as.numeric(gsub("\\$","", gsub(",","", 
                                                               data$Median.Household.Income)))
data$X..change.18.23 <- as.numeric(gsub("%","",data$X..change.18.23))
data$X..change.20.23 <- as.numeric(gsub("%","",data$X..change.20.23))


ui <- fluidPage(
  tags$script(inactivity),
  
  # Application title
  navbarPage("Connecticut Staffing Data",
             theme = shinythemes::shinytheme("darkly"),
             
             tabPanel("About", icon = icon("circle-info"),
                      fluidRow(
                        column(6,
                               tags$h2("Objectives"),
                               tags$body("In this analysis I am looking to study the changes in staffing levels across a 5 year period, 2018
to 2023. Was there a change in staffing levels in 2020 due to Covid-19? Have staff quantities
increased or decreased over the past 5 years? What are the staffing trends in the larger cities
of Connecticut?"),
                               tags$h2("Methods"),
                               tags$body("The methods followed in this analysis include cleaning the data, creating some general descriptive statistics, performing a t-test, and creating visualizations."),
                               tags$h2("Data Description"),
                               tags$body("The dataset I’m using in this analysis is from the latest report on CT Public School staff from
EdSight (public-edsight.ct.gov), a government website with data on many different aspects of
the school system."),
                               
                               tags$h2("Interpretations and Conclusions"),
                               tags$body("From the analysis presented in this report, I can answer my original research questions. The first objective
was to see if Covid-19, 2020-2021, had an impact on staffing levels. The first indication that Covid-19 did
have an impact is from the t-test I performed which concluded that the mean change in staff for 2018-2023
was different from the mean change in staff for 2020-2023. That shows that there is a different in the rate
of change when the Covid-19 year is included. The next indication that it had an impact comes from the
visualizations created. The plot showing the average number of staff for each year shows a dip in the quantity
of staff in the 2020-2021 school year. This may mean that schools got rid of staff for the year Covid-19 had
it’s biggest impact. I also found that towns with a larger population (greater than 20,000), saw the largest
dip in staff in the 2020-21 year. This answered my next objective which was to see what the trend was across
the state and across larger cities specifically. In towns with large populations, cities, the trend continued
upwards but with the larger dip in 2020. For the state as a whole, staffing levels have been increasing at a
fairly steady rate over the past 5 years."),
                               column(width=6, tableOutput("vars"))
                        ))),
             navbarMenu("Data Analysis", icon = icon("magnifying-glass-chart"),
                        tabPanel("Summaries", icon = icon("chart-bar"),
                                 # Show a plot of the generated distribution
                                 mainPanel(
                                   tags$h3("Descriptive Statistics"),
                                   wellPanel(sidebarLayout(sidebarPanel(checkboxGroupInput("direction",
                                                                                           "Districts with Staff that:",
                                                                                           choices = c("increased", "decreased"), 
                                                                                           selected=NULL)),
                                                           mainPanel(tableOutput("sum")))),
                                   tags$h3("Frequency"),
                                   wellPanel(sidebarLayout(sidebarPanel(radioButtons("years",
                                                                                     "Districts that increased or decreased from:",
                                                                                     choices = list("2018 to 2023" = 1, 
                                                                                                    "2020 to 2023" = 2), 
                                                                                     selected=NULL)),
                                                           mainPanel(tableOutput("freq")))),
                                 )
                        ),
                        tabPanel("Visualizations", icon = icon("chart-line"),
                                 mainPanel(width = 12,
                                           wellPanel(plotOutput("box1")),
                                           wellPanel(sidebarLayout(sidebarPanel(radioButtons("category",
                                                                                             "Histogram of _______ Variable", choices = list(
                                                                                               "Population" = 1, "Median Household Income" = 2,
                                                                                               "Equalized Net Grand List Per Capita" = 3), selected = NULL)),
                                                                   mainPanel(plotOutput("hists")))),
                                           wellPanel(plotOutput("means"),
                                                     tags$body("This graph shows the average number of staff for each year across all districts. There has been an
upward trend over the past 5 years, dipping slightly in the 2020-21 school year. This is another indication
that Covid-19 may have had an impact on staffing levels.")),
                                           wellPanel(sidebarLayout(sidebarPanel(radioButtons("size", "Graphs by Population Size",
                                                                                             list("Less than 7,000" = 1, "7,000 to 20,000" = 2,
                                                                                                  "Greater than 20,000" = 3), selected = NULL)),
                                                                   mainPanel(tags$h3(textOutput("histsizetitle")),
                                                                             plotOutput("histsize"),
                                                                             tags$h3(textOutput("sizeyearstitle")),
                                                                             plotOutput("yearsbysize")
                                                                   )))
                                 )
                        )
             ))
)

