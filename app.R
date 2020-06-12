#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(googlesheets4)
library(stringr)
library(rlist)

#yeast <- read_sheet("1RDIVbBy-YqDl3a5-onf5bY1OXbi7DbnWSGw_b2eCZ-Y")

# Define UI for application that filters a yeast spreadsheet
ui <- fluidPage(

    # Application title
    titlePanel("WDHY Yeast Search"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h4("Case Sensitive Search"),
            searchInput("sensitive", btnSearch = icon("search")),
            
            br(),
        
            h4("Non-case Sensitive Search"),
            searchInput("nonsensitive", btnSearch = icon("search")),
            
            br(),
            
            actionButton("reset", "Reset"),
            
            br(),
            
            h6("Instructions:"),
            h6("Hit the reset button. This should be the first thing you do at the start of each session"),
            h6("Resetting may take some time. The screen will flash briefly and jump back to the top when it is done."),
            h6("This app only searches through the markers column, so text in the comments column will not be used."),
            h6("In a case sensitive search, searching for ura is different from URA. In non-case sensitive, searching ura is the same as URA"),
            h6("If you have comments, suggestions, or bugs, email kevyan123@gmail.com"),
        ),

        mainPanel(
           dataTableOutput("yeastsearch")
        )
    )
)

server <- function(input, output, session) {
    output$yeastsearch <- renderDataTable({yeast})
    
      observeEvent(input$sensitive_search, {
      yeast <- dplyr::filter(yeast, str_detect(yeast$markers, input$sensitive))
      assign('yeast', yeast, envir = globalenv())
      updateTextInput(session, "sensitive", value = "")
      output$yeastsearch <- renderDataTable({yeast})
    })
      
      observeEvent(input$nonsensitive_search, {
        yeast <- dplyr::filter(yeast, str_detect(yeast$markers, regex(input$nonsensitive, ignore_case = TRUE)))
        assign('yeast', yeast, envir = globalenv())
        updateTextInput(session, "nonsensitive", value = "")
        output$yeastsearch <- renderDataTable({yeast})
      })   
      
    observeEvent(input$reset, {
      yeast <- read_sheet("https://docs.google.com/spreadsheets/d/1vrRv3FwRjn2epPMzvEhsOpEKbNsaX-AdUVam_p4WbCc/edit?usp=sharing")
      assign('yeast', yeast, envir = globalenv())
      output$yeastsearch <- renderDataTable({yeast})
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
