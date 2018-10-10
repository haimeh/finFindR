
library(shiny)

fluidPage(
  titlePanel(title=div(
    "finFindR",
    img(src='nmmf.png', height = '65px',align = "right"),
    img(src='west.png', height = '60px',align = "right")
    
  )),
  sidebarLayout(
    sidebarPanel(
      #%%%%%%%%%%%%%%%%%%%%%%%%
      # save stuff
      #%%%%%%%%%%%%%%%%%%%%%%%%
      actionButton("saveRdata","Save Session Rdata"),
      actionButton("concatRdata","Concatenate Reference Rdata"),
      
      h1(" "),#just for space
      
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      # QUERY INPUT
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
      
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      # image vs rdata input
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
      
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      # Image cropping
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      # Label via csv
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
      
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      # REFERENCE INPUT
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

      
      tabsetPanel(id = "mainTblPanel",
                  
                  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                  # Matches tab - display ordered table of query to reference matches
                  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                  tabPanel("Matches",
                           
                           fluidRow(
                             column(width = 6, class = "well",
                                    
                                    uiOutput("headerTableQuery"),
                                    
                                    plotOutput("imageTableQuery",click = clickOpts(id = "clickPointSet",clip = TRUE))
                                    
                             ),
                             column(width = 6, class = "well",
                                    verbatimTextOutput("imageNameTableRef"),
                                    verbatimTextOutput("imageIDTableRef"),
                                    checkboxInput(
                                      inputId = "traceTableRef",
                                      label = "Trace",
                                      value = FALSE
                                    ),
                                    plotOutput("imageTableRef")
                                    
                             )
                           ),
                           radioButtons(
                             inputId = "rankLim",
                             label = "Rank",
                             inline = T,
                             choices = c(5,10,20,50),
                             selected = 10
                           ),
                           fluidRow(
                             column(width=6,
                                    checkboxInput(
                                      inputId = "topPerId",
                                      label = "1 Per ID",
                                      value = FALSE
                                    ),style="text-align: left"
                                    # ),
                                    # column(width=6,
                                    # radioButtons(
                                    #   inputId = "searchSet",
                                    #   label = NA,
                                    #   inline = T,
                                    #   choices = c("Search Queries","Search References", "Search Both"),
                                    #   selected = "Search Queries"
                                    # ),style="text-align: right"
                             )),
                           tabsetPanel(id = "matchesTblPanel",#,style = "height:500px; overflow-y: scroll;"
                                       tabPanel("IDTab",DT::dataTableOutput("matchID"),downloadButton("IDTableDownload")),
                                       tabPanel("DistanceTab",DT::dataTableOutput("matchDistance"),downloadButton("DistanceTableDownload")),
                                       tabPanel("NameTab",DT::dataTableOutput("matchName"),downloadButton("NameTableDownload"))
                           )
                  ),
                  
                  
                  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                  # Clusters tab - view dendrogram, representing matches as heirarchical cluster
                  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                  tabPanel("Clusters",
                           uiOutput("displayWindows"),
                           column(width = 12,
                                  
                                  DT::dataTableOutput("hashComparison",width = '1600px')# '1824px' '1280px'
                           )
                  )
      )
      )
      )
)