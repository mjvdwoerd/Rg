shinyUI(navbarPage("Plot Options",
       tabPanel("Upload Data",
                sidebarLayout(
                sidebarPanel(
                  fileInput('choice', h4('Upload your file here')
                            ),
                  tags$hr(),
                  checkboxInput('hdr', 'Header', FALSE),
                  radioButtons('sp', 'Separator',
                                c('Comma'=',',
                                'Semicolon'=';',
                                'Tab'='\t',
                                'Space'=' '),
                                selected=' '),
                  radioButtons('qt', 'Quote',
                               c('None'='',
                               'Double Quote'='"',
                               'Single Quote'="'"),
                                selected='')
                  ),
                  mainPanel(
                    h4("Data analysis for your file:"),
                    textOutput("Analysis1"),
                    textOutput("Analysis2"),
                    textOutput("Analysis3"),
                    textOutput("Analysis4")
                  )
               )),
       
       tabPanel("Regular Plot",
          sidebarLayout(
          sidebarPanel(
            radioButtons("plotType", "Plot type",
                         c("Scatter"="p", "Line"="l")
                        ),
            hr(),
            p("Save your plot"),
            textInput('filename',"Filename"),
            checkboxInput('saveplot',"Check to save"),
            downloadButton('downloadData', 'Download')
                                  ),
          
          mainPanel(
            plotOutput("plot")
                   )
          )
          ),
              
       tabPanel("Guinier Plot",
          sidebarLayout(
          sidebarPanel(
            sliderInput("slider1", label = h4("Restrict data input"),
                                            
                        min = 1, max = 25, 
                        value = 1),
            hr(),
            p("Rg value based on Guinier analysis:"),
            fluidRow(column(3, textOutput("value")))
            ),
          mainPanel(
            plotOutput("Guinier")
                  )
          )),

        tabPanel("Cody's Guinier",
                 sidebarLayout(
                   sidebarPanel(
                     sliderInput("lowAngle", label = h4("Restrict low angle"),
                                 min=1, max=50, value=1),
                     hr(),
                     sliderInput("RgRange", label = h4("Range of plot"),
                                 min=30, max=200, value=100),
                     hr(),
                     p('Save your plot:'),
                     textInput('filename',"Filename"),
                     checkboxInput('savePlot',"Check to save")
                     ),
                   mainPanel(
                     plotOutput("Cody"),
                     plotOutput("plotRg")
                     )
                   )),
       
        tabPanel("Summary",
           verbatimTextOutput("summary")
          ),     
       
        navbarMenu("More",
           tabPanel("Table",
           dataTableOutput("table")
                   ),
           tabPanel("About",
              fluidRow(
#                column(6,
#                includeMarkdown("about.md")
#                ),
                 column(3,
                 img(src=paste0("http://upload.wikimedia.org/",
                                "wikipedia/commons/9/92/",
                                "1919_Ford_Model_T_Highboy_Coupe.jpg")),
                 tags$small(
                    "Source: Photographed at the Bay State Antique ",
                    "Automobile Club's July 10, 2005 show at the ",
                    "Endicott Estate in Dedham, MA by ",
                 a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                        "User:Sfoskett")
                           )
                 )
            )
            )
          )
))

