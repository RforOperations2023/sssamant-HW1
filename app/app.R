library(shiny)
library(ggplot2)
library(DT)
library(stringr)
library(dplyr)
library(tools)
library(readr)
flights <- read_csv("flights4.csv")

names(flights) <- str_to_title(names(flights))
flights <- mutate(flights, TravelDate = as.Date(Date, format= "%d-%m-%Y"))
flights$Price<-as.integer(flights$Price)


# Define UI for application that plots features of tweets -----------
ui <- fluidPage(
  
    # Application title -----------------------------------------------
    titlePanel(title=div("Flight Analysis"), windowTitle = "myBrowserTitle"),
    # Horizontal line for visual separation -----------------------
          hr(),
          h4("Understanding Flight statistics:"),
          h4("Analysisng Price and Flight distributions for Service Providers"),
          br(),
    # Sidebar layout with a input and output definitions --------------
    sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
        sidebarPanel(width = 3,
                 
    # Select flight service to analyse ------------------------
        radioButtons(inputId = "Service",
                       label = "Flight Service",
                     choices = c("JetBlue","Spirit","American Airlines"),
                    selected = "JetBlue"),
    # Select location for flight source ------------------------
        selectInput(inputId = "from", 
                      label = "From",
                    choices = c("LGA","MIA","ORD","CAK","BOS","DAL","OMA"),
                   selected = "LGA"),
                 
    # select destination for flight destination ----------------------------
        selectInput(inputId = "to", 
                      label = "To",
                    choices = c("LGA","MIA","ORD","CAK","BOS","DAL","OMA"),
                    selected = "MIA"),
    
    # Horizontal line for visual separation -----------------------
        hr(),
    # Set Price Range ---------------------------------------------
                sliderInput(inputId = "price",
                            label = "Price Range", 
                            min = as.integer("50"), max = as.integer("400"), 
                            value = c(as.integer("50"),as.integer("400"))), 
                
    # Set Flight date Range ---------------------------------------------
                sliderInput(inputId = "date",
                            label = "Travelling Date", 
                            min = as.Date("2022-01-02"), max = as.Date("2022-01-27"), 
                            value = c(as.Date("2022-01-02"), as.Date("2022-01-27"))),
                 
                  
                 
    # Show data table ---------------------------------------------
            checkboxInput(inputId = "show_data",
                                label = "Show data table",
                                value = TRUE),
    # Horizontal line for visual separation -----------------------
                 hr(),
                  
    # Adding Download Button
          downloadButton("downloadData", "Download"),
          h6("Press the download button to save the dataset of flights and flight timings for your selected dates")
      
    
    ),
    
    
    # Output: -------------------------------------------------------
    mainPanel(
      
      tabsetPanel(
        type="tab",
        tabPanel("Total Flights/day for selected serviceProvider", plotOutput(outputId = "plot1", height = "350px", width = "900px")), #tab for bar chart
        tabPanel("Price/day for service provider", plotOutput(outputId = "plot2",  height = "350px", width = "900px")), #tab for pie chart
        tabPanel("Flight Service Share for Service Providers ", plotOutput(outputId = "plot3",  height = "350px", width = "900px")) #tab for scatter Plot
      ),
      
      
      # Horizontal line for visual separation -----------------------
      br(),
      
      
  
      # Show data table ---------------------------------------------
      DT::dataTableOutput(outputId = "flightstable")
    
      
    )
  )
)


# Define server function required to create the barchart ---------
server <- function(input, output) {
  
  # Create a subset of data filtering for selected title types ------
  #flights_subset will be used to plot bar plot and scatter plot. and will be returend as a csv file when the
  #user clicks on download
  flights_subset <- reactive({
    req(input$from) # ensure availablity of value before proceeding
    req(input$to)
    req(input$date)
    req(input$Service)
    req(input$price)
    filter(flights, From %in% input$from & To %in% input$to & TravelDate >= input$date[1] & TravelDate <=input$date[2] & Service==input$Service) #&To %#in% input$to) #input$to & Date >= input$date[1])
  })
  #flights_subset1 will be used to display the pie-plot
  flights_subset1 <- reactive({
    req(input$from) # ensure availablity of value before proceeding
    req(input$to)
    req(input$date)
    req(input$Service)
    req(input$price)
    filter(flights, From %in% input$from & To %in% input$to & Price >= input$price[1] & Price <= input$price[2] & TravelDate >= input$date[1] & TravelDate <=input$date[2])
    
  })
  # Create barchart object the plotOutput function is expecting --
  output$plot1 <- renderPlot({
    ggplot(data = flights_subset1(), aes(x = Date )) +
      geom_bar(color = 4, fill = "4") +
      ggtitle("Total Number of flights for Given Date and Price Range") +
      xlab("Date") +
      ylab("Total Flights")+
      theme_classic()+
      theme(axis.title = element_text(color = "black", size = 15, face = "bold"),
            axis.title.y = element_text(face = "bold"))
                              })
  # Create scatter plot object, object expects the following:
   output$plot2 <- renderPlot({
     ggplot(data = flights_subset(), aes_string(x = "Date", y = "Price")) +
    theme_classic()+
         theme(axis.title = element_text(color = "black", size = 15, face = "bold"),
               axis.title.y = element_text(face = "bold"))+
         geom_point(color = 4) 
     })
   #Creates Pi-plot object
    output$plot3 <- renderPlot({
    pie_data <- flights_subset1() %>%
       count(Service) %>%
       mutate(percent = n/sum(n))
      ggplot(data = pie_data, aes(x ="", y = percent,  fill = Service)) +
       geom_bar(position = "fill", width = 1, stat = "identity", color = "white") +
       geom_text(aes(x = 1.0, label = scales::percent(percent, accuracy = .1)), position = position_stack(vjust = .5)) +
       coord_polar(theta = "y")+
       theme_void() })
    
    # Print data table if checked -------------------------------------
    output$flightstable <- DT::renderDataTable(
       if(input$show_data){
         DT::datatable(data = flights_subset(), 
                       options = list(pageLength = 10), 
                       rownames = FALSE)
       })
    
      output$downloadData <- downloadHandler(
                      filename = function() 
                        {
                            paste("data-flights", Sys.Date(), ".csv", sep="")
                         },
                        content = function(file) 
                          {
                           write.csv(flights_subset(), file)
                          }
                            )  

    
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)

