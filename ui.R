

library(shiny)

navbarPage("FinFindR",
  sidebarLayout(
    sidebarPanel(
      actionButton("saveRdata","Save Session Rdata"),
      # --- Set input type and processing
      checkboxInput(
        inputId = "trace",
        label = "Show Trace",
        value = TRUE
      ),
      
      radioButtons(
        inputId = "inputSize",
        label = "Select Input Size",
        choices = c("Batch")
      ),
      
      radioButtons(
        inputId = "refView",
        label = "Select View Type",
        choices = c("Match","Cluster")
      ),

      # all images in directory
      conditionalPanel(
        condition = "input.inputSize == 'Batch'",
        
        textInput(
          inputId = "queryDirectory",
          label = "Query Directory"
        ),
        
        conditionalPanel(
          condition = "input.inputSize == 'Batch'",
          radioButtons(
            inputId = "inputType",
            label = "Select Input Type",
            choices = c("Rdata","Image")
          )
        ),
        
        conditionalPanel(
          condition = "input.inputType == 'Image'",
          actionButton(
            inputId = "traceBatchQuery",
            label = "Start"
          )
        ),
        conditionalPanel(
          condition = "input.inputType == 'Rdata'",
          actionButton(
            inputId = "loadRdataQuery",
            label = "Load Rdata"
          )
        ),
        actionButton(
          inputId = "clearQuery",
          label = "Clear Queries"
        )
      ),
      h1(" "),
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
      
      width = 3),
    
    mainPanel(
      tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
        
      conditionalPanel(
        condition = "input.refView != 'Cluster'",
        
        fluidRow(
          column(width = 6, class = "well",h4("Matches"),
            plotOutput("refImage"),
            plotOutput("anglesRef",height = 100),
            DT::dataTableOutput("confidence")
          ),
          column(width = 6, class = "well",
            conditionalPanel(
              condition = "input.inputSize == 'Batch'",
              uiOutput("querySelector")
            ),
            conditionalPanel(
              condition = "input.inputSize == 'Single'",
              h4("Query")
            ),
            plotOutput("queryImage"),
            plotOutput("anglesQuery",height = 100),
            
            column(width = 12, h4(" "),
              verbatimTextOutput("singleID"),
              fluidRow(
                column(width = 6,
                  textInput(
                    inputId = "textIDSingle",
                    label = NULL
                  )
                ),
                column(width = 6,
                  actionButton("assignIDSingle","Assign ID")
                )
              )
            )
          )
        )
      ),
      
      conditionalPanel(
        condition = "input.refView == 'Cluster'",
        
        fluidRow(
          column(width = 6, class = "well",
            fluidRow(
              column(width = 2,
                     uiOutput("retraceLeft")
              ),
              column(width = 3,
                     uiOutput("removeLeft")
              ),
              
              column(width = 7,
                verbatimTextOutput("leftPhoto")
              )
              
            ),
            plotOutput("refImageLeft"),
            plotOutput("anglesLeft",height = 100),
            
            column(width = 12, h4(" "),
              verbatimTextOutput("leftID"),
              uiOutput("leftAssignButton")
            )
          ),
          column(width = 6, class = "well",
            fluidRow(
              column(width = 2,
                     uiOutput("retraceRight")
              ),
              column(width = 3,
                     uiOutput("removeRight")
              ),
              
              column(width = 7,
                     verbatimTextOutput("rightPhoto")
              )
            ),
            plotOutput("refImageRight"),
            plotOutput("anglesRight",height = 100),
            
            column(width = 12, h4(" "),
              verbatimTextOutput("rightID"),
              uiOutput("rightAssignButton")
            )
          )
        ),
        
        
        fluidRow(
          column(width = 12, class = "well",
            verbatimTextOutput("selectedPhoto"),
            actionButton("setLeft","Display on Left"),
            actionButton("setRight","Display on Right")
          )
        ),
        fluidRow(
          column(width = 12,
          plotOutput("hashComparison",
                     click = clickOpts(id = "click",clip = TRUE), 
                     dblclick = clickOpts(id = "dblclick",clip = TRUE), 
                     brush = brushOpts("brush",direction = "y",resetOnNew = TRUE,delay = 1000))
          )
        )
      )
    )
  )
)