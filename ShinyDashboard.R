library(shiny)
library(dplyr)
library(ggplot2)


# Example data frame
df_grphl <- data.frame(
  region = c("Philadelphia", "Philadelphia", "Greater Philadelphia", "Greater Philadelphia",
             "Philadelphia", "Philadelphia", "Greater Philadelphia", "Greater Philadelphia",
             "Philadelphia", "Philadelphia", "Greater Philadelphia", "Greater Philadelphia"),
  awarding_agency_name = c("Department of Energy", "Department of Education", "Department of Education", "Department of Energy",
                           "Department of Energy", "Department of Education", "Department of Education", "Department of Energy",
                           "Department of Energy", "Department of Education", "Department of Education", "Department of Energy"),
  cfda_title = c("Grant A", "Grant B", "Grant C", "Grant D",
                 "Grant A", "Grant B", "Grant C", "Grant D",
                 "Grant A", "Grant B", "Grant C", "Grant D"),
  assistance_type_description = c("A", "B", "C", "D",
                                  "A", "B", "C", "D",
                                  "A", "B", "C", "D"),
  business_types_description = c("Business", "Local Governmnet", "State government", "Business",
                                 "Business", "Local Governmnet", "State government", "Business",
                                 "Business", "Local Governmnet", "State government", "Business"),
  total_obligated_amount = c(100, 120, 140, 160, 150, 170, 190, 210, 220, 240, 260, 280),
  year = c(2011, 2011, 2011, 2011,
           2012, 2012, 2012, 2012,
           2013, 2013, 2013, 2013),
  recipient_name = c("Haseeb","Alina","Annie","Mike",
                     'Alvin','Simon','Haseeb','Alina',
                     'Alexander','Haseeb','Alina','Phillip'),
  recipient_address = c("123 Main St.", "Elm St.", "Oak St.", " Pine St.",
                        " Maple St.", " Birch St.", " Cedar St.", " Spruce St.",
                        " Willow St.", " Cherry St.", " Walnut St.", " Chestnut St."),
  recipient_city = c("Philadelphia","Philadelphia","Philadelphia","Philadelphia",
                     'Philadelphia','Philadelphia','Philadelphia','Philadelphia',
                     'Philadelphia','Philadelphia','Philadelphia','Philadelphia')
)



# Define the UI
ui <- fluidPage(
  titlePanel("Federal Funds Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year", choices = unique(df_grphl$year)),
      selectInput("region", "Select Region", choices = unique(df_grphl$region)),
      selectInput("agency", "Select Agency", choices = unique(df_grphl$awarding_agency_name)),
      selectInput("business_type", "Select Business Type", choices = unique(df_grphl$business_types_description)),
      selectInput("cfda", "Select CFDA Title", choices = unique(df_grphl$cfda_title))
    ),
    mainPanel(
      h4("Total Funding Amount"),
      verbatimTextOutput("total_funding"),
      h4("Top 5 Recipients"),
      tableOutput("top_recipients"),
      h4("Funding Trend"),
      plotOutput("funding_trend")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  filtered_data <- reactive({
    df <- df_grphl %>%
      filter(year == input$year,
             region == input$region,
             awarding_agency_name == input$agency,
             business_types_description == input$business_type,
             cfda_title == input$cfda)
    df
  })
  
  output$total_funding <- renderText({
    sum(filtered_data()$total_obligated_amount)
  })
  
  output$top_recipients <- renderTable({
    top_recipients <- filtered_data() %>%
      group_by(recipient_name) %>%
      summarise(total_amount = sum(total_obligated_amount)) %>%
      arrange(desc(total_amount)) %>%
      head(5)
    top_recipients
  })
  
  output$funding_trend <- renderPlot({
    funding_trend <- filtered_data() %>%
      group_by(year) %>%
      summarise(total_amount = sum(total_obligated_amount))
    
    ggplot(data = funding_trend, aes(x = year, y = total_amount)) +
      geom_line() +
      labs(x = "Year", y = "Total Funding Amount", title = "Funding Trend")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
