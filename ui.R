library(shiny)
library(shinyIncubator)

shinyUI(fluidPage(
  
  headerPanel(em("OPinioned Tweet Implied Mining and Analysis (OPTIMA)")),

  # Getting User Inputs
  
  sidebarPanel(
    img(src = "optima.png", height = 100, width = 150),
    wellPanel(
      textInput("entity1", "Keyword: ","#Windows 10")
      )  ,
    wellPanel(
      sliderInput("maxTweets","Number of recent tweets to use for analysis:",min=50,max=1000,value=100,step=1), # The max can, of course, be increased
      actionButton(inputId='actb',icon =icon("twitter"), label="Hit it!")
    )),
  
  mainPanel(
    br(),
    br(),
    progressInit(),
    tabsetPanel(
      
      tabPanel("Raw tweets",tableOutput("tableentity1")),
      
      tabPanel("Word Clouds",h2(textOutput("entity1wc")),
               plotOutput("entity1wcplot"),
               h2(textOutput("entity2wc")),
               plotOutput("entity2wcplot")),
      
      tabPanel("Sentiment Scoring", 
               plotOutput("sentiboxplot"),
               #tableOutput('contents'),
               tableOutput("sentiheadtable"),
               tableOutput("sentitailtable"),id="test"),
      
      tabPanel("SentiPlots",
               br(),
               downloadButton('downloadData', 'Download'),
               br(),
               plotOutput("polarityplot"),
               plotOutput("emotionplot")),
      
      tabPanel("Machine Learning Classifier",
               br(),
               br(),
               fileInput('file', 'Upload file to classify tweets using Naive Bayes Classifier',
                         accept = c(
                           'text/csv',
                           'text/comma-separated-values',
                           'text/tab-separated-values',
                           'text/plain',
                           '.csv',
                           '.tsv'
                         )),
               tableOutput("Contents"),
               br(),
               br(),
               br(),
               br(),
               fileInput('file1', 'Upload file to classify tweets using Support Vector Machine Classifier)',
                         accept = c(
                           'text/csv',
                           'text/comma-separated-values',
                           'text/tab-separated-values',
                           'text/plain',
                           '.csv',
                           '.tsv'
                         )),
               tableOutput("Contents1") 
              )
     
    )
  )
  
))
