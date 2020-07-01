#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
try(devtools::install_github('dustinfife/flexplot'))
try(devtools::install_github('dustinfife/selection', force=TRUE))
require(flexplot)
require(selection)

myData <- NULL


calc_data = function(y,x, method, data){
    #browser()
    if (is.null(y) | is.null(x)){
        return(NULL)
    } else {
        f = flexplot::make.formula(y, x)
        a=selection::maximizeDV(f, data=data, method=method)
        a
    }
}

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Maximizing the Criterion"),
    sidebarPanel(
        fileInput("file", "Choose CSV File",
                  accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
        ),
        checkboxInput("header", "Header", TRUE),
        selectInput("method", "Estimation Method", c("regression", "logistic", "poisson")),
        
        selectInput(
            "y_input", 
            label = "Criterion",
            ""
        ),
        checkboxGroupInput("x_input", "Predictor Variable(s)")
    ),
    mainPanel(
        tableOutput("contents"),
        plotOutput("plot")
    )
)

# Define server logic
server <- function(input, output, session) {
    inFile <- reactive({
        if (is.null(input$file)) {
            return(NULL)
        } else {
            input$file
        }
    })
    
    ## changed from NULL to "" because a later if statement checks if it's equal to "".
    ## when it's NULL, it throws an error saying it doesn't exist
    yval = reactive({
        if (is.null(input$y_input)) return("") else input$y_input
    })
    xval = reactive({
        if (is.null(input$x_input)) return("") else input$x_input
    })    
 
    myData <- reactive({
        if (is.null(inFile())) {
            return(NULL)
        } else {
            read.csv(inFile()$datapath)
        }
    })
    
    returnResults = reactive({
        if (is.null(yval()) | is.null(xval())){
            return(NULL)
        } else {
            a = calc_data(yval(), xval(), input$method, read.csv(inFile()$datapath))
            a
        }
    })
    
    observe({
        updateSelectInput(
            session,
            "y_input",
            choices=names(myData()),
            selected = names(myData())[1])
        updateCheckboxGroupInput(
            session, 
            "x_input",
            label = "Predictor Variable(s)",
            choices = names(myData()),
            selected = names(myData())[2]
        )
    })

    output$contents <- renderTable({
        inFile <- input$file
        y = yval()
        x = xval()
        if (is.null(inFile)) {
            return(NULL)
        } else { 
            if (y=="" | is.null(x)| y==x) {
                return(NULL)
            } else {
                a = returnResults()
                d = data.frame(c("Current mean", "Estimated Mean (under optimal system)", 
                                 "Percent agreement (between the two systems)"),
                               Value = c(a$current_mean, a$optimal_mean, a$percent_agreement))
                names(d) = c("", "Value")
                d
            }
        }
    })
    
    output$plot <- renderPlot({
        #browser()
        inFile <- input$file
        y = yval()
        x = xval()
        if (is.null(inFile)) {
            return(NULL)
        } else {   
            if (y=="" | is.null(x)| y==x) {
                return(NULL)
            } else {
                a = returnResults()
                print(a$plot)
            }
        }
    })    
}

# Run the application 
shinyApp(ui = ui, server = server)
