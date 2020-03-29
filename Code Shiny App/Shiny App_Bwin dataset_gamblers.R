library(shiny)
library(plotly)
library(ggplot2)
library(colourpicker)
library(wordcloud2)
library(DT)
library(readr)
library(shinydashboard)

# Define UI for application that draws a histogram

datamart2 = read_csv("/srv/connect/apps/fix_data/datamart2.csv")

aggregation2 = read_csv("/srv/connect/apps/fix_data/aggregation2.csv")
demographics = read_csv("/srv/connect/apps/fix_data/demographics.csv")
activity = read_csv("/srv/connect/apps/fix_data/activity.csv")
conversions2 = read_csv("/srv/connect/apps/fix_data/conversions2.csv")

datamart2$Gender<- as.factor(datamart2$Gender)
datamart2$Country_name<- as.factor(datamart2$Country_name)

ui <- fluidPage(
    h1("Business analytics tools-open source"),
    # "Shiny use cases course" as a secondary header
    h2("visualization of the Data by Shiny app"),
    tabsetPanel(
        # Application title
        tabPanel("Inputs",
                 titlePanel(strong("Internet Sports Gambling ")),
                 
                 # Sidebar with a slider input for number of bins 
                 textInput("title", "Title", "Plot filters"),
                 
                 
                 #input to choose the size of your point for the graph
                 numericInput("size", "Point size", 1, 1),
                 #input to choose to have the best fit curve
                 checkboxInput("fit", "Add line of best fit", TRUE),
                 #colourinput to select the color
                 colourInput("color", "Point color", value = "blue"),
                 #select imput for country
                 selectInput("Country", "Country",
                             choices = c("All", levels(datamart2$Country_name)),
                             multiple = TRUE,
                             selected = "All"),
                 radioButtons("Gender", "Gender", choices = levels(datamart2$Gender)),
                 sliderInput("Age", "Age",
                             min(datamart2$Age), max(datamart2$Age),
                             value = c(14, 105)),
                 #  Input: Selector for choosing dataset ----
                 selectInput(inputId = "dataset",
                             label = "Choose a summary:",
                             choices = c("Marketing Metrics","Overall Picture on activity","Overall Picture on  Casino plays",
                                         "Overall Picture on transactions","Datamart")),
                 # Input: Numeric entry for number of obs to view ----
                 numericInput(inputId = "obs",
                              label = "Number of observations to view:",
                              value = 43851),
                 # Add a numeric input for the number of words
                 numericInput(inputId = "num", label = "Maximum words in wordcloud",
                              value = 10, min = 5),
                 # Add a color input for the background color 
                 colourInput("col", "Background color", value = "red")),
        
        #tabPanel(title =  "Wordcloud"),
        #wordcloud2Output("Wordcloud"),
        # Replace the `plotOutput()` with the plotly version
        tabPanel(title ="revenue",
                 plotlyOutput("plot")),
        tabPanel(title ="stacks",
                 plotlyOutput("plot2")),
        tabPanel(title ="winnings",
                 plotlyOutput("plot3")),
        tabPanel(title ="bets",
                 plotlyOutput("plot4")),
        # Output: Verbatim text for data summary ----
        tabPanel(title = "summary",
                 
                 verbatimTextOutput("summary")),
        
        # Output: HTML table with requested number of observations ----
        tabPanel(title = "table",
                 downloadButton("download_data"),
                 DT ::dataTableOutput("view")
        )
    )
)
# Define the server logic
server <- function(input, output) {
    #filtered data
    # Create a reactive variable named "filtered_data"
    filtered_data <- reactive({
        # Filter the data (copied from previous exercise)
        data<-datamart2
        if (input$Country != "All"){
            data<-subset(data, Country_name == input$Country)}
        data<-subset(data, Age >= input$Age[1] & Age <= input$Age[2])
        data<-subset(data, Gender == input$Gender)
    })
    output$plot <- renderPlotly({
        # Convert the existing ggplot2 to a plotly plot
        ggplotly({
            data <- filtered_data()
            
            p <- ggplot(data, aes(UserID, Revenue)) +
                geom_point(size = input$size, col = input$color) +
                scale_x_log10() +
                ggtitle("Costumer vs Revenue")
            if (input$fit) {
                p <- p + geom_smooth(method = "lm", col = "red")
            } 
            p
        })
    })
    output$plot2 <-renderPlotly({
        # Convert the existing ggplot2 to a plotly plot
        ggplotly({
            
            data<-filtered_data()   
            s <- ggplot(data, aes(UserID, Stakes_2005)) +
                geom_point(size = input$size, col = input$color) +
                scale_x_log10() +
                ggtitle("costumer vs stacks")
            if (input$fit) {
                s<- s + geom_smooth(method = "lm", col ="green")
            }
            s
        })
    })
    output$plot3 <-renderPlotly({
        # Convert the existing ggplot2 to a plotly plot
        ggplotly({
            
            data<-filtered_data()   
            w <- ggplot(data, aes(UserID, Winnings_2005)) +
                geom_point(size = input$size, col = input$color) +
                scale_x_log10() +
                ggtitle("costumer vs winnings")
            if (input$fit) {
                w<- w + geom_smooth(method = "lm", col ="green")
            }
            w
        })
    })
    output$plot4 <-renderPlotly({
        # Convert the existing ggplot2 to a plotly plot
        ggplotly({
            
            data<-filtered_data()   
            b <- ggplot(data, aes(UserID, Bets_2005)) +
                geom_point(size = input$size, col = input$color) +
                scale_x_log10() +
                ggtitle("costumer vs Bests")
            if (input$fit) {
                b<- b + geom_smooth(method = "lm", col ="green")
            }
            b
        })  
    })
    # Return the requested dataset ----
    datasetInput <- reactive({datamart2
        switch(input$dataset,
               "Marketing Metrics" = aggregation2,
               "Overall Picture on activity" = activity,
               "Overall Picture on  Casino plays" = demographics,
               "Overall Picture on transactions" = conversions2,
               "Datamart" = datamart2)
    })
    #download_data
    output$download_data <- downloadHandler(
        filename = "data.csv",
        content = function(file) {
            write.csv(datamart2, file, row.names = FALSE)
        }
    )
    
    # Generate a summary of the dataset ----
    output$summary <- renderPrint({
        dataset <- datasetInput()
        summary(dataset)
    })
    # Show the first "n" observations ----
    output$view <- DT:: renderDataTable({
        head(datasetInput(), n = input$obs)
    })
    #output$Wordcloud <- renderWordcloud2({
    #Use the values from the two inputs as
    # parameters to the word cloud
    
    # create_wordcloud(datamart2$LALastActiveDate, backgroundColor = input$col)
    #})
}
#run the application    
shinyApp(ui = ui, server = server)


