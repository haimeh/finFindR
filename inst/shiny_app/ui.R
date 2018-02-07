

library(shiny)

navbarPage("FinFindR",
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
          choices = c("Rdata","Image","Field")
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
          actionButton(
            inputId = "cropRawImages",
            label = "Crop"
          )
        ),
        actionButton(
          inputId = "clearQuery",
          label = "Clear Queries"
        ),
        
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
        
        conditionalPanel(
          condition = "input.referenceType == 'Rdata'",
          actionButton(
            inputId = "loadRdataRef",
            label = "Load Rdata"
          )
        ),
        actionButton(
          inputId = "clearRef",
          label = "Clear References"
        ),
    width = 3
    ),

    mainPanel(
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
                     value = TRUE
                   ),
                   plotOutput("imageTableRef"),
                   plotOutput("anglesTableRef",height = 100)
                   # verbatimTextOutput("IDTableRef")
                   
            )
          ),

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
                     brush = brushOpts("brush",direction = "y",resetOnNew = TRUE,delay = 1000),
                     hover = hoverOpts(id = "hover"))
          )
        )
      )
    )
  )
)