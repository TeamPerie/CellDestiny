library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycustomloader)

ui_myApp<-function() {
  dashboardPage(
    #https://stackoverflow.com/questions/54146804/how-to-prevent-code-for-adding-an-image-to-shinydashboard-header-appearing-in-br
    skin = "blue",
    dashboardHeader(title = "CellDestiny"),

    dashboardSidebar(

      sidebarUserPanel( name = "Institut Curie",
                        image="data:image/jpeg;base64,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"
      ),
      sidebarMenu(id = "sidebarmenu",
                  menuItem("QC",tabName = "QC", icon = icon('th'), selected = TRUE,
                           menuSubItem("Load Data", tabName = "load_QC"),
                           menuSubItem("Duplicates", tabName = "duplicats"),
                           menuSubItem("Repeat Use", tabName = "repeat_use")),

                  menuItem("Analysis", tabName = "Analysis" , icon = icon('chart-bar'),
                           menuSubItem("Load Data", tabName = "load_data"),
                           menuSubItem("Sample similarities", tabName = "sample_similaritiesMenu"),
                           menuSubItem("Clone size", tabName = "clone_sizeMenu"),
                           menuSubItem("Barcode sharing", tabName = "barcode_sharingMenu"),
                           menuSubItem("Diversity", tabName = "diversityMenu"),
                           menuSubItem("Categorisation", tabName = "categorisationMenu"))
      )
    ), # end of dashboardSidebar

    dashboardBody(
      # change title police
      tags$head(tags$style(HTML('
        .main-header .logo {
          font-family: "Garamond", sans-serif;
          font-size: 30px;
        }'
      ))),

      tags$head(
        tags$style(HTML(".main-sidebar {font-size: 15px}" ))
      ),

      fluidPage(

        #------------------------------#
        #------- Set input files ------#
        #------------------------------#

############### QC ******************************************************************************************************************
        tabItems(
          tabItem(tabName = "load_QC",
          fluidRow(
              ## Get matrix
              box(width = 12, title="Duplicate Matrix",status = "primary", solidHeader = TRUE,
                  radioButtons("replicats_matrix_extention", label = "Select your file extension : ",
                               inline = TRUE,
                               choices = c("csv", "csv2", "tsv"),
                               selected = NULL),
                  fileInput(inputId="replicats_matrix", label = 'Upload your barcode matrix composed of duplicated samples:',
                            accept = c(".csv", ".csv2",".tsv", ".csv.gz", ".csv2.gz",".tsv.gz"), multiple = FALSE),
                  dataTableOutput("contents_matQC"),
                  footer = "Accepted formarts: .csv (sep=','), .csv2 (sep=';') or .tsv (sep=tabulations) with corresponding separators"),
              ## Get metadata
              box(width = 12,title ="Duplicate Metadata",status = "primary", solidHeader = TRUE,
                  radioButtons("replicats_metadata_extention", label = "Select your file extension : ",
                               inline = TRUE,
                               choices = c("csv", "csv2", "tsv"),
                               selected = NULL),
                  fileInput(inputId="replicats_metadata",label = 'Upload the metadata corresponding to your barcode matrix with duplicates:',
                            accept = c(".csv", ".csv2",".tsv", ".csv.gz", ".csv2.gz",".tsv.gz"), multiple = FALSE),
                  dataTableOutput("contents_metQC"),
                  footer = "Accepted formarts: .csv (sep=','), .csv2 (sep=';') or .tsv (sep=tabulations) with corresponding separators"),
              ## Get duplicate variable name
              box(width = 4, title="Duplicate variable", status="primary", solidHeader=TRUE,
                  pickerInput(inputId = "replicats_var",
                              multiple = FALSE,
                              choices = NULL,
                              label = "Select the variable which defines your duplicates:",
                              options = pickerOptions(actionsBox = FALSE,
                                                      title = "Variable",
                                                      liveSearch = TRUE,
                                                      liveSearchStyle = "contains"))),
              # test data
              box(title ="Load test dataset",status = "warning", solidHeader = TRUE,
                  radioButtons(inputId ="QC_testdataLoder",
                               label = "Do you want to load test data ?",
                               choices = c("No", "Yes"),
                               selected = "No"),

                  tags$script(HTML("Shiny.addCustomMessageHandler('upload_txt_matQC', function(txt) {
  var target = $('#replicats_matrix').parent().parent().parent().find('input[type=text]');
  target.val(txt);
}); ")),
                  tags$script(HTML("Shiny.addCustomMessageHandler('upload_txt_metQC', function(txt) {
  var target = $('#replicats_metadata').parent().parent().parent().find('input[type=text]');
  target.val(txt);
}); "))
              ),

          ) # end of fluidRow
          #)
        ), # end of conditionalPannel QC

############### ANALYSIS ******************************************************************************************************************

        #tabItems(
          tabItem(tabName = "load_data",
          fluidRow(
           ## Get matrix
           box(width = 12, title="Filtred Matrix",status = "primary", solidHeader = TRUE,
               radioButtons("matrix_extention", label = "Select your file extension : ",
                            inline = TRUE,
                            choices = c("csv", "csv2", "tsv"),
                            selected = NULL),
               fileInput(inputId="matrix",label = "Upload your filtered matrix:",
                         accept = c(".csv", ".csv2",".tsv", ".csv.gz", ".csv2.gz",".tsv.gz"),
                         multiple = FALSE),
               dataTableOutput("contents_mat"),
               footer = "Accepted formarts: .csv (sep=','), .csv2 (sep=';') or .tsv (sep=tabulations) with corresponding separators"),
           ## Get metadata
           box(width = 12,title ="Metadata",status = "primary", solidHeader = TRUE,
               radioButtons("metadata_extention", label = "Select your file extension : ",
                            inline = TRUE,
                            choices = c("csv", "csv2", "tsv"),
                            selected = NULL),
               fileInput(inputId="metadata",label = "Upload the metadata corresponding to your filtred matrix:",
                         accept = c(".csv", ".csv2",".tsv", ".csv.gz", ".csv2.gz",".tsv.gz"),
                         multiple = FALSE),
               dataTableOutput("contents_met"),
               footer = "Accepted formarts: .csv (sep=','), .csv2 (sep=';') or .tsv (sep=tabulations) with corresponding separators"),

           box(width = 4, title="Individuals", status="primary", solidHeader=TRUE,
               pickerInput(inputId = "organism",
                           multiple = FALSE,
                           choices = NULL,
                           label = "Select the variable defining your individuals:",
                           options = pickerOptions(actionsBox = TRUE,
                                                   title = "ex: Mouse, Patients, etc.",
                                                   liveSearch = TRUE,
                                                   liveSearchStyle = "contains"))),
           # test data
           box(title ="Load test dataset",status = "warning", solidHeader = TRUE,
               radioButtons(inputId ="Analysis_testdataLoder",
                            label = "Do you want to load test data ?",
                            choices = c("No", "Yes"),
                            selected = "No"),
               tags$script(HTML(" Shiny.addCustomMessageHandler('upload_txt_mat', function(txt) {
  var target = $('#matrix').parent().parent().parent().find('input[type=text]');
  target.val(txt);
}); ")),
               tags$script(HTML("Shiny.addCustomMessageHandler('upload_txt_met', function(txt) {
  var target = $('#metadata').parent().parent().parent().find('input[type=text]');
  target.val(txt);
}); "))
           ),
         ) # end of fluidRow
          #)
        ), # end of conditionalPannel Analysis

############### Sub Menus ******************************************************************************************************************
#°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
        #tabItems(
         tabItem(tabName = "duplicats",

          fluidRow(
           box(status = "primary", solidHeader = FALSE, width = "100%",

           # Left column == questions box
           column(width=4,

            box(width = NULL, status ="primary",solidHeader = TRUE, title="Sample selections",
             pickerInput(inputId = "varRep",
                             label = "Which variable(s) do you want to select your samples on?",
                             choices = NULL,
                             multiple = TRUE,
                             options = pickerOptions(actionsBox = TRUE,
                                                     title = "Variables",
                                                     liveSearch = TRUE,
                                                     liveSearchStyle = "contains")),

             conditionalPanel("input.varRep!=''",
                pickerInput(inputId = "valRep",
                            label = "Select value(s) take by variable(s):",
                            choices = NULL,
                            multiple = TRUE,
                            options = pickerOptions(actionsBox = TRUE,
                                                    title = "Values",
                                                    liveSearch = TRUE,
                                                    liveSearchStyle = "contains"))
             )
            ), # end of sample selection box

            box(width = NULL, status ="primary",solidHeader = TRUE, title="Graph options",
              box(width = NULL, status ="primary",height = NULL,solidHeader = TRUE,
                radioButtons(inputId = "QCtransformation",
                             label = "Which transformation do you want to apply? ", inline = TRUE,
                             selected = "arcsin",
                             choices = c("arcsin", "log10(x+1)", "none"))
              ),

              box(width = NULL, status ="primary",height = NULL,solidHeader = TRUE,
                  radioButtons(inputId = "QCtransformation",
                               label = "Which transformation do you want to apply? ", inline = TRUE,
                               selected = "arcsin",
                               choices = c("arcsin", "log10(x+1)", "none"))
              ),
              box(width = NULL, status ="primary",height = NULL,solidHeader = TRUE,
                  radioButtons(inputId = "correlDup",
                               label = "Choose your correlation method :", inline = TRUE,
                               choices = c("spearman", "pearson"),
                               selected = "spearman")
              )
            )
           ), # end of column user questions

           column(width=8, align="center",
            box(align="center", width = "100%", height = "100%",

              conditionalPanel("input.valRep.length==0",
                                 imageOutput("QCDotEx"), width="100%", height="100%"),

              conditionalPanel("input.valRep.length>0",
                              uiOutput("plots"),
                              downloadButton("downloadImage_QC", "Plot"),
                              downloadButton("downloadTable_QC", "Matrix")
              )
            )

          ) # end of left column

          ) # end of box
         )# end of fluid row
        ), # end of tabItem duplicats

#°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
        tabItem(tabName = "repeat_use",

         fluidRow(

          box(status = "primary", solidHeader = FALSE, width = "100%",

            # Left column == questions box
            column(width=4,

             box(width = NULL, status ="primary",solidHeader = TRUE, title="Sample selections",
                 pickerInput(inputId = "indiv_varRU",
                             label = "Select the variable which defines your individuals:",
                             choices = NULL,
                             multiple = FALSE,
                             options = pickerOptions(actionsBox = TRUE,
                                                     title = "Individuals",
                                                     liveSearch = FALSE,
                                                     liveSearchStyle = "contains")),

                 conditionalPanel("input.indiv_varRU!=''",
                                  pickerInput(inputId = "indiv_valRU",
                                              label = "Which individuals do you want to plot ?",
                                              choices = NULL,
                                              multiple = TRUE,
                                              options = pickerOptions(actionsBox = TRUE,
                                                                      title = "Values",
                                                                      liveSearch = TRUE,
                                                                      liveSearchStyle = "contains"))
                 )
             ) # end of sample selection box

            ), # end of column user questions

            column(width=8, align="center",
                 box(align="center", width = "100%", height = "100%",
                     conditionalPanel("input.indiv_valRU.length==0",
                                      imageOutput("RUDotEx"), width="100%", height="100%"),

                     conditionalPanel("input.indiv_valRU.length>0",
                                      uiOutput("plotsRU"),
                                      downloadButton("downloadImage_RU", "Plot"),
                                      downloadButton("downloadTable_RU", "Matrix")
                     )
                 )
             ) # end of left column
          ) # end of box
        )# end of fluid row
      ),# end of tabItem repeat_use

#°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
  ##############################
  ####  Sample Similarities  ###
  ##############################
  tabItem(tabName = "sample_similaritiesMenu",
   # Row of 2 big columns
   fluidRow(
    box(status = "primary", solidHeader = FALSE, width = "100%",
     # Left column == questions box
     column(width=4,

       box(width = NULL, status ="primary", height = NULL, solidHeader = TRUE,
           radioButtons(inputId = "graph_sampleSim",
                        label = "Select the type of graph:",
                        inline = FALSE,
                        choices = c("None", "Heatmap", "Correlogram"),
                        selected = 'None')
        ),

   conditionalPanel(condition ="input.graph_sampleSim != 'None'",

     box(width = NULL, title="Sample selections", status ="primary",solidHeader = TRUE,
      box(width = NULL, status ="primary",solidHeader = TRUE,
        pickerInput(inputId = "organismSample",
                    label = "Which individual(s) do you want to display ?",
                    choices = NULL,
                    multiple = TRUE,
                    options = pickerOptions(actionsBox = TRUE,
                                            title = "Individuals",
                                            liveSearch = TRUE,
                                            liveSearchStyle = "contains"))

      ),
      box(width = NULL, status ="primary",solidHeader = TRUE,
        pickerInput(inputId = "variable",
                    label = "Which variable(s) do you want to display ?",
                    choices = NULL,
                    multiple = TRUE,
                    options = pickerOptions(actionsBox = TRUE,
                                            title = "Variables",
                                            liveSearch = TRUE,
                                            liveSearchStyle = "contains")),
        pickerInput(inputId = "value",
                    label= "Select at least two values to plot with:",
                    multiple = TRUE,
                    choices = NULL,
                    options = pickerOptions(actionsBox = TRUE,
                                            title = "Values",
                                            liveSearch = TRUE,
                                            liveSearchStyle = "contains"))

     )
    ) # end of Sample selections
   ), # end of none

    ####  Heatmap options ####
    ##########################

    conditionalPanel("input.value.length>0 && input.graph_sampleSim == 'Heatmap'",
      box(width = NULL, title="Graph options", status ="primary",solidHeader = TRUE,
        box(width = NULL, status ="primary",height = NULL,solidHeader = TRUE,
          radioButtons(inputId = "changeCalcul",
                       label = "Do you want to change default distance and clustering methods?", inline = TRUE,
                       choices = c("no", "yes"),
                       selected = "no")),

          conditionalPanel("input.changeCalcul=='yes'",
             box(width = NULL, status ="primary",height = NULL,solidHeader = TRUE,
                 radioButtons(inputId = "distance",
                              label = "Which distance method do you want?", inline = TRUE,
                              choices = c("euclidean", "maximum", "manhattan", "canberra"),
                              selected = "euclidean")),
             box(width = NULL, status ="primary",height = NULL,solidHeader = TRUE,
                 # https://support.minitab.com/en-us/minitab/18/help-and-how-to/modeling-statistics/multivariate/how-to/cluster-observations/methods-and-formulas/linkage-methods/
                 radioButtons(inputId = "clustering",
                              label = "Which clustering method do you want?", inline = TRUE,
                              choices = c("complete", "average", "mcquitty", "median", "centroid", "ward.D2"),
                              selected = "complete"))
             ), # end of changeCalcul=='yes'

          box(width = NULL, status ="primary",height = NULL,solidHeader = TRUE,
              radioButtons(inputId = "dendro",
                           label = "Do you want to display a column dendrogram?", inline = TRUE,
                           choices = c("no", "yes"),
                           selected = "no")),

          box(width = NULL, status ="primary",height = NULL,solidHeader = TRUE,
              uiOutput("nclustersUi")
              ),

          box(width = NULL, status ="primary",height = NULL,solidHeader = TRUE,
            radioButtons(inputId = "barcodes",
                         label = "Do you want to display a row barcodes?", inline = TRUE,
                         choices = c("no", "yes"),
                         selected = "no"))
      ) # end of graph options
     ) # end of conditionalPanel Heatmap
    ),
   #), # end of left column

    # Rigth column == Plot box
    column(width=8, align="center",

     ###################
     ####  Heatmap  ####
     ###################
     conditionalPanel("input.graph_sampleSim=='Heatmap'",
      tabBox(height="100%", width = "100%",
       title = "",
       id = "tabHeatmap",
       tabPanel(id="tabFig",
                title="Figure",
                conditionalPanel("input.value.length==0",
                                 imageOutput("heatmapEx", width="100%")),

                conditionalPanel("input.value.length>0",
                                 plotOutput(outputId="heatmap")),
                conditionalPanel("input.value.length>0",
                                 downloadButton("downloadImage_heatmap", "Plot"),
                                 downloadButton("downloadTable_heatmap", "Matrix"))
                ),
       tabPanel(id="tabInfo",
                title="Details",
                "Used distance: ",
                conditionalPanel("input.value.length==0 ",
                                 verbatimTextOutput("distanceEx")),
                conditionalPanel("input.value.length>0 ",
                                 verbatimTextOutput("distanceSelected")),
                "Used clustering: ",
                conditionalPanel("input.value.length==0 ",
                                 verbatimTextOutput("clusteringEx")),
                conditionalPanel("input.value.length>0 ",
                                 verbatimTextOutput("clusteringSelected")))
       ) # end of tabBox
      ), # end of conditionalPanel heatmap

     #######################
     ####  Correlogram  ####
     #######################
      conditionalPanel("input.graph_sampleSim=='Correlogram'",
      box(width = NULL, title="Graph options", status ="primary",solidHeader = TRUE,
        box(width = NULL, status ="primary",height = NULL,solidHeader = TRUE,
          radioButtons(inputId = "correlation",
                      label = "Choose your correlation method :", inline = TRUE,
                      choices = c("spearman", "pearson"),
                      selected = "spearman"))),
       box(align="center", width = "100%", height = "100%",
           #uiOutput("CloneSizeUI"),
           conditionalPanel("input.value.length==0 ",
                            imageOutput("correloEx")),

           conditionalPanel("input.value.length>0 ",
                            plotOutput(outputId="correlo"),
                            downloadButton("downloadImage_correlo", "Plot"),
                            downloadButton("downloadTable_correlo", "Matrix"))
       )
      ) # end of condit panel Correlogram
     )# end of right column

     ) # end of big box
    ) # end of fluidrow
   ), # end of tabItem sample_similaritiesMenu

#°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    #####################
    ####  Clone size  ###
    #####################

tabItem(tabName = "clone_sizeMenu",
  # Row of 2 big columns
  fluidRow(
    box(status = "primary", solidHeader = FALSE, width = "100%",
        # Left column == questions box
        column(width=4,

     box(width = NULL, status ="primary",height = NULL,solidHeader = TRUE,
       radioButtons(inputId = "graphType2",
                    label = "Select the type of graph:", inline = FALSE,
                    selected = "None",
                    choices = c("None", "Cumulative diagram", "Frequency distribution"))
      ),

     conditionalPanel("input.graphType2!='None'",

       box(width = NULL, title="Sample selections", status ="primary",solidHeader = TRUE,
        box(width = NULL, status ="primary",solidHeader = TRUE,
           pickerInput(inputId = "organismSampleCS",
                       label = "Which individual(s) do you want to display ?",
                       choices = NULL,
                       multiple = TRUE,
                       options = pickerOptions(actionsBox = TRUE,
                                               title = "Individuals",
                                               liveSearch = TRUE,
                                               liveSearchStyle = "contains"))
       ),
       box(width = NULL, status ="primary",height = 200,solidHeader = TRUE,
           pickerInput(inputId = "variableCS",
                       label = "Which variable(s) do you want to display ?",
                       choices = NULL,
                       multiple = TRUE,
                       options = pickerOptions(actionsBox = TRUE,
                                               title = "Variables",
                                               liveSearch = TRUE,
                                               liveSearchStyle = "contains")),
           pickerInput(inputId = "valueCS",
                       label= "Select the value(s) take by variable(s):",
                       multiple = TRUE,
                       choices = NULL,
                       options = pickerOptions(actionsBox = TRUE,
                                               title = "Values",
                                               liveSearch = TRUE,
                                               liveSearchStyle = "contains"))
       )), # end of sample selection

       box(width = NULL, title="Graph option", status ="primary",solidHeader = TRUE,

        conditionalPanel("input.graphType2=='Frequency distribution'",
          box(width = NULL, status ="primary",solidHeader = TRUE,
              radioButtons(inputId = "yCS",
                           label= "Select the type of diagram:",
                           choices = c("Density curve", "Histogarm"),
                           selected = "Density curve",
                           inline = FALSE))
       ),

       box(width = NULL, status ="primary",solidHeader = TRUE,
          radioButtons(inputId = "doColor",
                        label= "Do you want to add colors :",
                        choices = c("no", "yes"),
                        selected = "no",
                        inline = FALSE),

          conditionalPanel("input.doColor=='yes'",
           pickerInput(inputId = "colorCS",
                       label= "Select the variable for the color:",
                       multiple = FALSE,
                       choices = NULL,
                       options = pickerOptions(actionsBox = FALSE,
                                               title = "Color",
                                               liveSearch = TRUE,
                                               liveSearchStyle = "contains"))
        )
      ),

       # only freq
        conditionalPanel("input.graphType2=='Frequency distribution'",
         box(width = NULL, status ="primary",solidHeader = TRUE,
             sliderInput("nbins", label = "Select a number of bins", min = 0,
                         max = 100, value = 5))
        ),
       # only cum
        conditionalPanel("input.graphType2=='Cumulative diagram'",
         box(width = NULL, status ="primary",solidHeader = TRUE,
          radioButtons(inputId ="xProportion",
                       label = "Convert x axis in percentage :",
                       choices = c("no", "yes"),
                       selected = "no",
                       inline = FALSE))
       )
      )  # end of graph option
     )
    ), #end of left box

    column(width=8, align="center",
     box(align="center", width = "100%", height = "100%",

         ##  Clone Size  ##
         #°°°°°°°°°°°°°°°°°°°°°°°°#

         #uiOutput("CloneSizeUI"),
         conditionalPanel("input.graphType2=='Cumulative diagram' && input.valueCS.length==0 ",
                          imageOutput("cumulativeDiagramEx")),

         conditionalPanel("input.graphType2=='Cumulative diagram' && input.valueCS.length>0 ",
                          plotOutput(outputId="cumulativeDiagram"),
                          downloadButton("downloadImage_cumDiag", "Plot"),
                          downloadButton("downloadTable_cumDiag", "Matrix")
         ),

         conditionalPanel("input.graphType2=='Frequency distribution' && input.valueCS.length==0",
                          imageOutput("nonCumulativeHistEx")),

         conditionalPanel("input.graphType2=='Frequency distribution' && input.valueCS.length>0 ",
                          plotOutput(outputId="nonCumulativeHist"),
                          downloadButton("downloadImage_nonCumHist", "Plot"),
                          downloadButton("downloadTable_nonCumHist", "Matrix")
         )

     ) # end of right box
    ) # end of right column

   ) # end of big box
  ) # end of fluidrow
 ), # end of tabItem clone size


#°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    ##########################
    ####  Barcode sharing ###
    #########################

tabItem(tabName = "barcode_sharingMenu",
  # Row of 2 big columns
  fluidRow(
    box(status = "primary", solidHeader = FALSE, width = "100%",
        # Left column == questions box
        column(width=4,

   box(width = NULL, status ="primary",height = NULL,solidHeader = TRUE,
     radioButtons(inputId = "graphType1",
                  label = "Select the type of graph:", inline = FALSE,
                  selected = "None",
                  choices = c("None", "Dotplot", "Ternary plot"))
   ),

   conditionalPanel("input.graphType1!='None'",

     box(width = NULL, title="Sample selections", status ="primary",solidHeader = TRUE,

      box(width = NULL, status ="primary",solidHeader = TRUE,
       pickerInput(inputId = "organismSampleSB",
                   label = "Which individual(s) do you want to display ?",
                   choices = NULL,
                   multiple = TRUE,
                   options = pickerOptions(actionsBox = TRUE,
                                           title = "Individuals",
                                           liveSearch = TRUE,
                                           liveSearchStyle = "contains"))
      ),

      conditionalPanel("input.graphType1=='Dotplot'",
       box(width = NULL,title="x axis", status ="primary",solidHeader = TRUE,

          pickerInput(inputId = "x_var",
                     label = "Select x variable(s):",
                     choices = NULL,
                     multiple = TRUE,
                     options = pickerOptions(actionsBox = TRUE,
                                             title = "Variables",
                                             liveSearch = TRUE,
                                             liveSearchStyle = "contains")),
          pickerInput(inputId = "x_val",
                      label = "Select x value(s) to display:",
                      choices = NULL,
                      multiple = TRUE,
                      options = pickerOptions(actionsBox = TRUE,
                                              title = "Values",
                                              liveSearch = TRUE,
                                              liveSearchStyle = "contains"))
       ),

       box(width = NULL,title="y axis", status ="primary",solidHeader = TRUE,
          pickerInput(inputId = "y_var",
                      label = "Select y variable(s):",
                      choices = NULL,
                      multiple = TRUE,
                      options = pickerOptions(actionsBox = TRUE,
                                              title = "Variables",
                                              liveSearch = TRUE,
                                              liveSearchStyle = "contains")),
          pickerInput(inputId = "y_val",
                      label = "Select y value(s) to display:",
                      choices = NULL,
                      multiple = TRUE,
                      options = pickerOptions(actionsBox = TRUE,
                                              title = "Values",
                                              liveSearch = TRUE,
                                              liveSearchStyle = "contains"))
      )), # end of conditionalPanel Dotplot

    conditionalPanel("input.graphType1=='Ternary plot'",
       box(width = NULL,title="Value1", status ="primary",solidHeader = TRUE,
          pickerInput(inputId = "top_var",
                       label= "Select variable(s):",
                       multiple = TRUE,
                       choices = NULL,
                       options = pickerOptions(actionsBox = TRUE,
                                               title = "Variables",
                                               liveSearch = TRUE,
                                               liveSearchStyle = "contains")),
          pickerInput(inputId = "top_val",
                      label= "Select values(s) to display (left axis):",
                      multiple = TRUE,
                      choices = NULL,
                      options = pickerOptions(actionsBox = TRUE,
                                              title = "Values",
                                              liveSearch = TRUE,
                                              liveSearchStyle = "contains"))
       ),

       box(width = NULL,title="Value2", status ="primary",solidHeader = TRUE,
           pickerInput(inputId = "right_var",
                       label= "Select variable(s):",
                       multiple = TRUE,
                       choices = NULL,
                       options = pickerOptions(actionsBox = TRUE,
                                               title = "Variables",
                                               liveSearch = TRUE,
                                               liveSearchStyle = "contains")),
           pickerInput(inputId = "right_val",
                       label= "Select values(s) to display (right axis):",
                       multiple = TRUE,
                       choices = NULL,
                       options = pickerOptions(actionsBox = TRUE,
                                               title = "Values",
                                               liveSearch = TRUE,
                                               liveSearchStyle = "contains"))
       ),

       box(width = NULL,title="Value3", status ="primary",solidHeader = TRUE,
           pickerInput(inputId = "left_var",
                       label= "Select variable(s):",
                       multiple = TRUE,
                       choices = NULL,
                       options = pickerOptions(actionsBox = TRUE,
                                               title = "Variables",
                                               liveSearch = TRUE,
                                               liveSearchStyle = "contains")),
           pickerInput(inputId = "left_val",
                       label= "Select values(s) to display (bottom axis):",
                       multiple = TRUE,
                       choices = NULL,
                       options = pickerOptions(actionsBox = TRUE,
                                               title = "Values",
                                               liveSearch = TRUE,
                                               liveSearchStyle = "contains"))
       )

    ) # end of conditional panel Ternary
  ), # end of sample selection

     box(width = NULL, title="Graph options", status ="primary",solidHeader = TRUE,

      conditionalPanel("input.graphType1=='Dotplot'",
        box(width = NULL, status ="primary",height = NULL,solidHeader = TRUE,
             radioButtons(inputId = "SBtransformation",
                          label = "Which transformation do you want to apply? ", inline = TRUE,
                          selected = "arcsin",
                          choices = c("arcsin", "log10(x+1)", "none"))
        )
      ),
      conditionalPanel("input.graphType1=='Dotplot'",

        box(width = NULL, status ="primary",height = NULL,solidHeader = TRUE,
          radioButtons(inputId = "filledPlotSB",
                    label = "Do you want to color dots?", inline = TRUE,
                    selected = "no",
                    choices = c("no", "yes")),

          conditionalPanel("input.filledPlotSB=='yes'" ,
            pickerInput(inputId = "colorSB",
                       label= "Select the variable to color:",
                       multiple = FALSE,
                       choices = NULL,
                       options = pickerOptions(actionsBox = TRUE,
                                               title = "Color",
                                               liveSearch = FALSE))
          )
        )
      ),

       conditionalPanel("input.graphType1=='Ternary plot'" ,
            radioButtons(inputId = "colorSB_ternary",
                         label = "Do you want to color dots according individuals ?", inline = TRUE,
                         selected = "no",
                         choices = c("no", "yes")))

     ) # end of graph options
    ) #end of conditionalPanel not None
  ), # end of left column

  column(width=8, align="center",
   box(align="center", width = "100%", height = "100%",
    conditionalPanel("input.graphType1=='Dotplot' && (input.x_val.length==0 || input.y_val.length==0) ",
                     imageOutput("dotplotEx")),

    conditionalPanel("input.graphType1=='Dotplot' && input.x_val.length>0 && input.y_val.length>0 ",
                     # dotplot
                     plotOutput(outputId="dotplot"),
                     downloadButton("downloadImage_dotplot", "Plot"),
                     downloadButton("downloadTable_dotplot", "Matrix"),

                     # piechart plot
                     plotOutput(outputId="piechart"),
                     downloadButton("downloadImage_piechart", "Plot"),
                     downloadButton("downloadTable_piechart", "Matrix")),

    conditionalPanel("input.graphType1=='Ternary plot' && (input.top_val.length==0 || input.left_val.length==0 || input.right_val.length==0)",
                     imageOutput("ternaryPlotEx")),

    conditionalPanel("input.graphType1=='Ternary plot' && input.top_val.length>0 && input.left_val.length>0 && input.right_val.length>0",
                     plotOutput(outputId="ternaryPlot"),
                     downloadButton("downloadImage_ternary", "Plot"),
                     downloadButton("downloadTable_ternary", "Matrix"))

   ) # end of right box
  ) # end of right column

    ) # end of big box
   ) # end of fluidrow
  ), # end of tabItem barcode sharing

#°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
    ####################
    ####  Diversity  ###
    ####################

tabItem(tabName = "diversityMenu",
        # Row of 2 big columns
        fluidRow(
          box(status = "primary", solidHeader = FALSE, width = "100%",
              # Left column == questions box
              column(width=4,

     box(width = NULL, title="Sample selections", status ="primary",solidHeader = TRUE,
      # changer pour le dotplot, proposer uniquement un seul choix pour la variable
      box(width = NULL, status ="primary",solidHeader = TRUE,
       pickerInput(inputId = "organismSample_notPooled",
                   label = "Select individual(s): ",
                   choices = NULL,
                   multiple = TRUE,
                   options = pickerOptions(actionsBox = TRUE,
                                           title = "Individuals",
                                           liveSearch = TRUE,
                                           liveSearchStyle = "contains"))

      ),
      box(width = NULL, status ="primary",solidHeader = TRUE,
        pickerInput(inputId = "variable_notPooled",
                    label = "Select x axis variable(s):",
                    choices = NULL,
                    multiple = TRUE,
                    options = pickerOptions(actionsBox = TRUE,
                                            title = "Variables",
                                            liveSearch = TRUE,
                                            liveSearchStyle = "contains")),
        pickerInput(inputId = "value_notPooled",
                    label= "Select value(s) take by x axis variable(s):",
                    multiple = TRUE,
                    choices = NULL,
                    options = pickerOptions(actionsBox = TRUE,
                                            title = "Values",
                                            liveSearch = TRUE,
                                            liveSearchStyle = "contains"))
      ),
      box(width = NULL, status ="primary",solidHeader = TRUE,
          radioButtons(inputId = "yBoxplot",
                       label = "What do you want in y axis ?", inline = TRUE,
                       selected = "Number of barcodes",
                       choices = c("Number of barcodes", "Shannon index", "Simpson index"))
      )
     ), # end of box Sample selection

     box(width = NULL, title="Graph options", status ="primary",solidHeader = TRUE,
       box(width = NULL, status ="primary",solidHeader = TRUE,
           radioButtons(inputId = "boxplotCondition",
                        label = "Do you want to color your plot ?", inline = TRUE,
                        selected = "no",
                        choices = c("no", "yes")),

           conditionalPanel("input.boxplotCondition=='yes'",
             pickerInput(inputId = "boxplotColor_var",
                         label= "Select a variable: ",
                         multiple = FALSE,
                         choices = NULL,
                         options = pickerOptions(actionsBox = FALSE,
                                                 title = "Variable",
                                                 liveSearch = FALSE))
          )),

       box(width = NULL, status ="primary",solidHeader = TRUE,
          radioButtons(inputId = "boxplotDot",
                       label = "Do you want to display dots?", inline = TRUE,
                       selected = "no",
                       choices = c("no", "yes"))
       )
    ) # end of Graph options
   ), # end of left column

   column(width=8, align="center",
    box(align="center", width = "100%", height = "100%",

      conditionalPanel("input.value_notPooled.length<1",
                       imageOutput("boxplotEx")),

      conditionalPanel("input.value_notPooled.length>=1",
                       plotOutput(outputId="boxplot"),
                       downloadButton("downloadImage_boxplot", "Plot"),
                       downloadButton("downloadTable_boxplot", "Matrix"))

    ) # end of right box
   ) # end of right column

  ) # end of big box
 ) # end of fluidrow
), # end of tabItem diversityMenu

#°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
  #########################
  ####  Categorisation  ###
  #########################

tabItem(tabName = "categorisationMenu",
  # Row of 2 big columns
 fluidRow(
  box(status = "primary", solidHeader = FALSE, width = "100%",

    # Left column == questions box
    column(width=4,
     box(width = NULL, status ="primary",solidHeader = TRUE, title="Sample selections",
        pickerInput(inputId = "organismCat",
                     label = "Select individual(s):",
                     choices = NULL,
                     multiple = TRUE,
                     options = pickerOptions(actionsBox = TRUE,
                                             title = "Individuals",
                                             liveSearch = TRUE,
                                             liveSearchStyle = "contains")),
        pickerInput(inputId = "catVar",
                    label = "Select the variable used for cell types:",
                    choices = NULL,
                    multiple = FALSE,
                    options = pickerOptions(actionsBox = TRUE,
                                            title = "Categorgy",
                                            liveSearch = TRUE,
                                            liveSearchStyle = "contains")),
        conditionalPanel("input.catVar!=''",
          pickerInput(inputId = "catVal",
                    label = "Select its values:",
                    choices = NULL,
                    multiple = TRUE,
                    options = pickerOptions(actionsBox = TRUE,
                                            liveSearch = TRUE,
                                            liveSearchStyle = "contains")))
      ), # end of Sample selections

      box(width = NULL, status ="primary",solidHeader = TRUE,title="Graph option",

        box(width = NULL, status ="primary",solidHeader = TRUE,title="Threshold value",
         sliderInput("slider", label = "Select the threshold used for categorization:", min = 0,
                        max = 100, value = 20)),

         radioButtons(inputId = "condition",
                      label = "Do you want to add a condition (e.g: treatment, organ, etc.) ? ", inline = TRUE,
                      selected = "no",
                      choices = c("no","yes")),

         conditionalPanel("input.condition=='yes' && input.catVar!=''",
           pickerInput(inputId = "conditionVal",
                      label = "Select your condition value:",
                      choices = NULL,
                      multiple = FALSE,
                      options = pickerOptions(actionsBox = TRUE,
                                              title = "Condition",
                                              liveSearch = TRUE,
                                              liveSearchStyle = "contains")))
     ) # end of box Graph opt
    ), # end of left column

   column(width=8, align="center",
    box(align="center", width = "100%", height = "100%",
      conditionalPanel("input.catVal.length==0 && input.organismCat.length==0",
                       imageOutput("contrib2"),
                       imageOutput("contrib1")
      ),

      conditionalPanel("input.catVal.length>0 && input.organismCat.length>0 ",
                       # first plot
                       withLoader(plotOutput(outputId="bargraphCat_counts"), type = "html", loader = "dnaspin"),
                       downloadButton("downloadImage_counts", "Plot"),
                       downloadButton("downloadTable_counts", "Matrix"),

                       # second plot
                       plotOutput(outputId="bargraphCat_percent"),
                       downloadButton("downloadImage_percent", "Plot"),
                       downloadButton("downloadTable_percent", "Matrix")
      )
     ) # end of right box
    ) # end of right column

  ) # end of big box
 ) # end of fluidrow
) # end of tabItem categorisationMenu

      )# end of TabItems
    )# end of fluidPage
  )# end dashboardBody
 )# end dashboardPage
}

