


library(shiny)
library(shinythemes)

shinyUI(fluidPage(
    theme = shinytheme("cerulean"),  # Apply a modern theme
    
    tags$head(
        tags$style(HTML("
      .nav-tabs {
        display: flex;
        justify-content: center;
      }
      .centered {
        text-align: center;
      }
      .inline-block {
        display: inline-block;
      }
    "))
    ),
    
    titlePanel(div("Next Word Prediction App (Prototype)", style = "text-align: center;")),
    
    tabsetPanel(
        tabPanel("Main App",
                 # Center the input panel
                 fluidRow(
                     column(6, offset = 3,
                            div(class = "centered", style = "margin-top: 20px;",
                                div(class = "inline-block", style = "margin-top: 5%;",
                                    textInput("input_text", "Enter text here:", "")
                                ),
                                p("Predicted next word(s):", class = "centered", style = "margin-top: 10px;"),
                                verbatimTextOutput("prediction_output")
                                ,
                                
                                div(class = "inline-block", style = "margin-top: 10px;", 
                                    numericInput("num_predictions", "Results required", min = 1, max = 10, value = 3, width = "100px")
                                )
                            )
                     )
                 ),
                 
                 fluidRow(
                     column(6, offset = 3,
                            div(class = "centered", style = "margin-top: 20px;",
                                h3("Instructions"),
                                p("Enter a sentence or a few words, select the number of predictions, and the app will dynamically predict the next word(s).")
                            )
                     )
                 )
        ),
        tabPanel("About",
                 fluidRow(
                     column(6, offset = 3,
                            div(class = "centered", style = "margin-top: 20px;",
                                h3("About This App"),
                                p("This app predicts the next word based on the text entered by the user. It uses a pre-trained model to make predictions. You can select the number of predictions to display.")
                            )
                     )
                 )
        )
    )
))
