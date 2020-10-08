

fluidPage(
  titlePanel('Customer Segmentation'),
  tabsetPanel(
#---------- Location ------------------------------------------------------------------------------------            
    tabPanel("Locations", fluid = TRUE,
               mainPanel(
                 fluidRow(column(12, plotOutput('location'))))
             ),
#---------- Time ------------------------------------------------------------------------------------
    tabPanel("Time", fluid = TRUE,
             sidebarPanel(
               radioButtons("time", label = h3("Sales by time"),
                            choices = list("Weekdays" = 1, "Daily hours" = 2), 
                            selected = 1)),
             mainPanel(
               fluidRow(column(12, plotOutput('time'))))),
#---------- Sales overview ------------------------------------------------------------------------------------
    tabPanel("Sales overview", fluid = TRUE,
             sidebarPanel(
               selectInput("salesoverview", label = h3("Sales overview"),
                           choices = list('Total sales per gender' = 1, 'Total sales per type of product' = 2, "Total sales per credit rating" = 3, 'Total sales per membership' = 4),
                           selected = 1)),
             mainPanel(
               fluidRow(column(12, plotOutput('salesoverview'))))),
#---------- Gender ------------------------------------------------------------------------------------
    tabPanel("Gender", fluid = TRUE,
             sidebarPanel(
               selectInput("gender", label = h3("Gender"),
                           choices = list('Type of product' = 1, 'Membership' = 2),
                           selected = 1)),
             mainPanel(
               fluidRow(column(12, plotOutput('gender'))))),
#---------- Membership ------------------------------------------------------------------------------------
    tabPanel("Membership", fluid = TRUE,
               mainPanel(
                 fluidRow(column(12, plotOutput('membership')))) 
             ),
               
#---------- Rating ------------------------------------------------------------------------------------
    tabPanel("Rating", fluid = TRUE,
               mainPanel(
                 fluidRow(column(12, plotOutput('rating'))))
               )
  ))
