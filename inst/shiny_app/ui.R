
library(shiny)

fluidPage(
  titlePanel(title=div(
     "finFindR",
     img(src='nmmf.png', height = '65px',align = "right"),
     img(src='west.png', height = '60px',align = "right")
      
    )),
  sidebarLayout(
    sidebarPanel(
        actionButton("saveRdata","Save Session Rdata"),
        actionButton("concatRdata","Concatenate Reference Rdata"),
        
        h1(" "),#just for space
  
        # all images in directory
        textInput(
          inputId = "queryDirectory",
          label = "Query Directory"
        ),
        
        radioButtons(
          inputId = "inputType",
          label = "Select Input Type",
          choices = c("Rdata","Image","Field","Label"),
          inline = T
        ),
        
        conditionalPanel(
          condition = "input.inputType == 'Image'",
          actionButton(
            inputId = "traceBatchQuery",
            label = "Trace"
          )
        ),
        conditionalPanel(
          condition = "input.inputType == 'Rdata'",
          actionButton(
            inputId = "loadRdataQuery",
            label = "Load Rdata"
          )
        ),
        conditionalPanel(
          condition = "input.inputType == 'Field'",
          fluidRow(
            column(width=3,h1(" "),#just for space,
                   actionButton(
              inputId = "cropRawImages",
              label = "Crop"
            )),
            column(width=9,sliderInput("Sensitivity",
                        "Threshold:",
                        min = .1,  max = .9, value = .4))
          ),
          radioButtons(
            inputId = "cropTarget",
            label = "Crop Type",
            choices = c("Body&Fin","Fin"),
            inline = T
          )
          
          
        ),
        conditionalPanel(
          condition = "input.inputType == 'Label'",
          fluidRow(
            column(width=8,
                   HTML("<div style='height: 45px;'>"),
                   fileInput(label=NULL,inputId="csvLabeler",accept=".csv"),
                   HTML("</div>")
            ),
            column(width=3,
              actionButton(
                inputId = "labelWithCSV",
                label = "Apply"
              )
            )
          ),
          checkboxInput(
            inputId = "removeForeign",
            label = "Remove Image if not Included in CSV",
            value = F
          )
        ),
        # actionButton(
        #   inputId = "clearQuery",
        #   label = "Clear Queries"
        # ),
        h1(" "),#just for space
        #get data in dir from specified form
        textInput(
          inputId = "referenceDirectory",
          label = "Reference Image Directory"
        ),
        
        # --- Set reference type for comparison
        radioButtons(
          inputId = "referenceType",
          label = "Select Reference Type",
          choices = c("Rdata")
        ),
        
        fluidRow(
          column(width=4,
        conditionalPanel(
          condition = "input.referenceType == 'Rdata'",
          actionButton(
            inputId = "loadRdataRef",
            label = "Load Rdata"
          )
        )),
        column(width=2,
        actionButton(
          inputId = "clearRef",
          label = "Clear References"
        ))),
    width = 3
    ),
    
    mainPanel(
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
        
      tabsetPanel(id = "mainTblPanel",
        tabPanel("Matches",
                           
          fluidRow(
            column(width = 6, class = "well",
              
              uiOutput("headerTableQuery"),
              
              plotOutput("imageTableQuery",click = clickOpts(id = "clickPointSet",clip = TRUE)),
              plotOutput("anglesTableQuery",height = 100)
              
              
            ),
            column(width = 6, class = "well",
                   verbatimTextOutput("imageNameTableRef"),
                   verbatimTextOutput("imageIDTableRef"),
                   checkboxInput(
                     inputId = "traceTableRef",
                     label = "Trace",
                     value = FALSE
                   ),
                   plotOutput("imageTableRef"),
                   plotOutput("anglesTableRef",height = 100)
                   # verbatimTextOutput("IDTableRef")
                   
            )
          ),
          #fluidRow(
            #column(width = 3,
                   radioButtons(
                     inputId = "rankLim",
                     label = "Rank",
                     inline = T,
                     choices = c(5,10,20,50),
                     selected = 10
                   #)
            ),
            #column(width = 2,
                   checkboxInput(
                     inputId = "topPerId",
                     label = "1 Per ID",
                     value = FALSE
                   #)
            ),
          #),
          tabsetPanel(id = "matchesTblPanel",
                      tabPanel("DistanceTab",DT::dataTableOutput("matchDistance"),downloadButton("DistanceTableDownload")),
                      tabPanel("IDTab",DT::dataTableOutput("matchID"),downloadButton("IDTableDownload")),
                      tabPanel("NameTab",DT::dataTableOutput("matchName"),downloadButton("NameTableDownload"))
                      )
        ),
      
        tabPanel("Clusters",
          uiOutput("displayWindows"),
          column(width = 12,
                 
          plotOutput("hashComparison",
                     click = clickOpts(id = "clickHashMap",clip = TRUE), 
                     dblclick = clickOpts(id = "dblclickHashMap",clip = TRUE), 
                     brush = brushOpts("brush",direction = "y",resetOnNew = TRUE,delay = 500),
                     hover = hoverOpts(id = "hover"))
          )
        )
      )
    )
  )
)