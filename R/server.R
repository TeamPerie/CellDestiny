#' @export
#' @import shiny
#' @import shinyWidgets
#' @import plyr
#' @import dplyr
#' @import reshape2
#' @import gplots
#' @import ggplot2
#' @import ggtern
#' @import ggforce
#' @import vroom
#' @import stringr
#' @import tibble
#' @import vegan
#' @import scales
#' @import tidyr
#' @import ggpubr
#' @import gridExtra
#' @import stats
#' @import RColorBrewer
#' @import rlang
#' @import corrplot

#options(shiny.maxRequestSize = 35*1024^2)


server_myApp<-function(input, output, session) {

  #**********************************************************************
  #    QC part
  #**********************************************************************

  #------------------------------#
  #------- Get input files ------#
  #------------------------------#

  ## Get matrix
  rep_matrix <- reactive({
    if(input$QC_testdataLoder=="Yes"){
      tab=read.csv("testData/LentiviralBarcodingData/QC_data/QC_duplicate_matrix_Mouse_Lung_cDCs.csv.gz", check.names = FALSE)

    }else{
      req(input$replicats_matrix)
      # Check file extension
      ext <- input$replicats_matrix_extention
      tab = switch(ext,
                   csv =vroom(input$replicats_matrix$datapath, delim = ","),
                   csv2 =vroom(input$replicats_matrix$datapath, delim = ";"),
                   tsv =vroom(input$replicats_matrix$datapath, delim = "\t"),
                   validate("Invalid file; Upload a matrix as .csv (if separators ,) , .tsv (if separators \t) or .txt (if separators ;)"))
      tab<-as.data.frame(tab)
    }
    tab
  })

  ## Get metadata
  rep_metadata <- reactive({
    if(input$QC_testdataLoder=="Yes"){
      tab=read.csv("testData/LentiviralBarcodingData/QC_data/QC_duplicate_matrix_Mouse_Lung_cDCs_metadata.csv.gz", check.names = FALSE)
    } else {
      req(input$replicats_metadata)
      # Check metadata extension
      ext <- input$replicats_metadata_extention
      tab = switch(ext,
                   csv = vroom(input$replicats_metadata$datapath, delim = ","),
                   csv2 =vroom(input$replicats_metadata$datapath, delim = ";"),
                   tsv = vroom(input$replicats_metadata$datapath, delim = "\t"),
                   validate("Invalid file; Please upload a .csv or .tsv metadata"))
      tab<-as.data.frame(tab)
    }
    tab
  })

  ## Select duplicat varibale name
  observe({
    if(nrow(rep_matrix())!=0 && nrow(rep_metadata())!=0 && input$QC_testdataLoder=="No") {
      updatePickerInput(session,"replicats_var", choices=colnames(rep_metadata()))
    }
    if(input$QC_testdataLoder=="Yes"){
      updatePickerInput(session,"replicats_var",  choices=colnames(rep_metadata()), selected ="duplicates")
    }
  })


  # Toy dataset
  observeEvent(input$QC_testdataLoder, {
   if(input$QC_testdataLoder=="Yes"){
    # Count Matrix
    session$sendCustomMessage("upload_txt_matQC", "QC_duplicate_matrix_Mouse_Lung_cDCs.csv.gz")
     output$contents_matQC<-renderDataTable(expr = head(read.csv("testData/LentiviralBarcodingData/QC_data/QC_duplicate_matrix_Mouse_Lung_cDCs.csv.gz", check.names = FALSE)))
     # Count Matrix Metadata
     session$sendCustomMessage("upload_txt_metQC", "QC_duplicate_matrix_Mouse_Lung_cDCs_metadata.csv.gz")
     output$contents_metQC<-renderDataTable(expr = head(read.csv("testData/LentiviralBarcodingData/QC_data/QC_duplicate_matrix_Mouse_Lung_cDCs_metadata.csv.gz", check.names = FALSE)))
   }else{
     session$sendCustomMessage("upload_txt_matQC", "Load your count matrix")
     session$sendCustomMessage("upload_txt_metQC", "Load your metadata matrix")
     output$contents_matQC<-renderDataTable(expr = head(rep_matrix()))
     output$contents_metQC<-renderDataTable(expr = head(rep_metadata()))
   }
  })


  ############## Duplicates dotplots

  #----------------------------------#
  #------- Get user selections ------#
  #----------------------------------#

  ## Get duplicat values
  dup_val<-reactive({
    if( input$replicats_var!="" ){
      dup_val<-rep_metadata()[!is.na(select(rep_metadata(), input$replicats_var)),input$replicats_var]
      dup_val<-dup_val[which(dup_val!="")]
      dup_val
    }
  })

  observe({
    if (input$replicats_var!="") {
      updatePickerInput(session,
                      "varRep",
                      choices=colnames(select(rep_metadata(),-input$replicats_var)))
    }
  })

  observe({ if (length(input$varRep)>0) {
    updatePickerInput(session,
                      "valRep",
                      choices=lapply(select(rep_metadata(),input$varRep), na.omit))
  }
  })

  ### warning test if at least two
  answers_QC<-reactive({
    CheckValueLengths(input$varRep, input$valRep, rep_metadata())
  })

  #------------------------------#
  #------- Reformat inputs ------#
  #------------------------------#

  dupMatrix<-reactive({
    if(length(input$valRep)>0 && input$replicats_var!=""){
      qc_mat<-ReformatQCmatrix(rep_matrix(), rep_metadata(), input$replicats_var, dup_val(), transformation = input$QCtransformation, input$correlDup)
      dup_mat<-MakeDuplicatesMatrix(qc_mat, input$varRep,input$valRep, rep_metadata())
      dup_mat
    }
  })

  ### warning test
  number_samples<-reactive({
      nb_samples<-length(unique(dupMatrix()$Sample_names))
      nb_samples
  })

  #-------------------#
  #------- Plot ------#
  #-------------------#
  observe({
   if(length(input$varRep)==0 && length(input$valRep)==0){
     ## Example
     output$QCDotEx<-renderImage({list(src ="images/QCDotEx.png",
                                                  width="100%",
                                                  height="100%")
                                            },deleteFile=FALSE)
   }
  })

  # What will be output on the window
  observe({ if(length(input$varRep)>0 && length(input$valRep)>0){
    # dynamicly out put several plots
    nb_pages <- round(length(unique(dupMatrix()$Sample_names))/6)
    if(nb_pages==0){
      nb_pages <- 1
    }
    output$plots <- renderUI({
      validate(
        need(answers_QC()==length(input$varRep), "Please, select at least one value per variable."))
      validate(
        need(number_samples()>0, "Your selection correspond to any sample. Please, select more variables/values."))

      plot_output_list <- lapply( 1:nb_pages, function(i) {plotname <- paste("plot", i, sep="")
      plotOutput(plotname)})
      # Convert the list to a tagList to display properly.
      do.call(tagList, plot_output_list)
    })

    for (i in 1:nb_pages) {
      local({
        # Need local so that each item gets its own number. Without it, the value
        # of i in the renderPlot() will be the same across all instances, because
        # of when the expression is evaluated.
        my_i <- i
        plotname <- paste("plot", my_i, sep="")
        filtred_matrix<-dupMatrix()
        dup_val<-dup_val()
        if(length(unique(filtred_matrix$Sample_names))<4){ # if less than 2 rows to fill, bug with facet_wrap_paginate
          output[[plotname]]<-renderPlot({
            ggplot(filtred_matrix, aes(x=trans_dup1, y=trans_dup2))+
              facet_wrap_paginate(~paste(cor, Sample_names, sep = ": "), page=my_i) +
              geom_point(size=2.5, alpha=0.8, color="#7fdbbe") +
              theme_bw() +
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ # add right box border
              theme(text = element_text(size = 15)) + # Change font size
              theme(strip.text.x = element_text(face = "bold", size= 15)) + # Change
              xlab(paste0("Barcode abundances : ", dup_val[1], " (", input$QCtransformation , ")")) +
              ylab(paste0("Barcode abundances : ", dup_val[2], " (", input$QCtransformation , ")"))
          })


        }else{
          output[[plotname]]<-renderPlot({
            ggplot(filtred_matrix, aes(x=trans_dup1, y=trans_dup2))  +
              geom_point(size=2.5, alpha=0.8, color="#7fdbbe") +
              facet_wrap_paginate(~paste(cor, Sample_names, sep = ": "), ncol=2, nrow=3,page=my_i) +
              theme_bw() +
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ # add right box border
              theme(text = element_text(size = 15))  + # Change font size
              theme(strip.text.x = element_text(face = "bold", size= 15)) + # Change
              xlab(paste0("Barcode abundances : ", dup_val[1], " (", input$QCtransformation , ")")) +
              ylab(paste0("Barcode abundances : ", dup_val[2], " (", input$QCtransformation , ")"))
          })
        }
      }) # end of local
    } # end for
  }}) # end of observe



  # What the user will save
  dupPlot <- reactive({
    dup_mat<-dupMatrix()
    dup_val<-dup_val()
    nb_pages <- round(length(unique(dup_mat$Sample_names))/6)
    if(nb_pages==0){
      nb_pages <- 1
    }
    list_plot<-list()
    for(i in 1:nb_pages){
      if(length(unique(dup_mat$Sample_names))<4 ){ # if less than 2 rows to fill, bug with facet_wrap_paginate
        p <- ggplot(dup_mat, aes(x=trans_dup1, y=trans_dup2)) +
          geom_point(size=2.5, alpha=0.8, color="#7fdbbe") +
          facet_wrap_paginate(~paste(cor, Sample_names, sep = ": "), page=i) +
          theme_bw() +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ # add right box border
          theme(text = element_text(size = 15))  + # Change font size
          theme(strip.text.x = element_text(face = "bold", size= 15))+ # Change
          xlab(paste0("Barcode abundances : ", dup_val[1], " (", input$QCtransformation , ")")) +
          ylab(paste0("Barcode abundances : ", dup_val[2], " (", input$QCtransformation , ")"))
      }else{
        p<- ggplot(dup_mat, aes(x=trans_dup1, y=trans_dup2))  +
          geom_point(size=2.5, alpha=0.8, color="#7fdbbe") +
          facet_wrap_paginate(~paste(cor, Sample_names, sep = ": "),ncol=2, nrow=3,page=i) +
          theme_bw() +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ # add right box border
          theme(text = element_text(size = 15))  + # Change font size
          theme(strip.text.x = element_text(face = "bold", size= 15)) + # Change
          xlab(paste0("Barcode abundances : ", dup_val[1], " (", input$QCtransformation , ")")) +
          ylab(paste0("Barcode abundances : ", dup_val[2], " (", input$QCtransformation , ")"))
      }
      name<-paste("plot", i, sep = "_")
      list_plot[[name]]<-p
    }
    list_plot
  })

  observe({
    output$downloadImage_QC <- downloadHandler(filename = "QC_duplicates.pdf", # pdf because possibly several pages
                                               content = function(fname){
                                                 ggsave(fname, ggpubr::ggarrange(plotlist = c(dupPlot())), device = "pdf")
                                               }
                                )
  })

  observe({
    if(answers_QC()==length(input$varRep) && number_samples()>0){
      output$downloadTable_QC <- downloadHandler(filename = "QC_duplicates.csv",
                                                 content = function(fname){ write.csv(dupMatrix(), fname)})
    }
  })

  ################################### Individual dotplots   ###################################

  #----------------------------------#
  #------- Get user selections ------#
  #----------------------------------#

  ## Get duplicat values
  dup_valRU<-reactive({
    if(input$replicats_var!="" ){
      dup_valRU<-rep_metadata()[!is.na(select(rep_metadata(), input$replicats_var)),input$replicats_var]
      dup_valRU<-dup_valRU[which(dup_valRU!="")]
      dup_valRU
    }
  })

  observe({ if (input$replicats_var!="") {
    updatePickerInput(session,
                      "indiv_varRU",
                      choices=colnames(select(rep_metadata(),-input$replicats_var)))
  }
  })

  observe({ if (input$indiv_varRU!="") {
    updatePickerInput(session,
                      "indiv_valRU",
                      choices=lapply(select(rep_metadata(),input$indiv_varRU), na.omit))
  }
  })

  #------------------------------#
  #------- Reformat inputs ------#
  #------------------------------#

  repeatUseMatrix<-reactive({
    matxRu<-rep_matrix()
    metRU<-rep_metadata()
    dupVarRu<-input$replicats_var
    if(input$indiv_varRU!="" && length(input$indiv_valRU)>=2){
      qc_matRU<-ReformatQCmatrix(matxRu, metRU, dupVar = dupVarRu , dup_valRU(), transformation = input$QCtransformation, input$correlDup)
      ru_mat<-MakeRepeatUseMatrix(qc_matRU, input$indiv_varRU, input$indiv_valRU)
      ru_mat
    }
  })

  indiv_com<-reactive({
    print(repeatUseMatrix())
    indiv_com<-combn(colnames(repeatUseMatrix()[,-1]), 2, FUN = NULL)
    print(indiv_com)
    indiv_com
  })

  #-------------------#
  #------- Plot ------#
  #-------------------#
  ## Example
  observe({ if(input$indiv_varRU=="" && length(input$indiv_valRU)<2 ){
      output$RUDotEx<-renderImage({list(src ="images/RUDotEx.png",
                                        width="100%",
                                        height="100%")
      },deleteFile=FALSE)
    }
  })

  ## Dynamic plots
  observe({ if(input$indiv_varRU!="" && length(input$indiv_valRU)>=2){

      # dynamicly out put several plots
      nb_pagesRU <- round((ncol(indiv_com())/12))
      if(nb_pagesRU==0){
        nb_pagesRU <- 1
      }

      if(nb_pagesRU>1){
        output$plotsRU <- renderUI({

          validate(
            need(length(input$indiv_valRU)>1, "Please, select at least 2 individuals.")
          )

          plot_output_listRU <- lapply( 1:nb_pagesRU, function(i) {
            plotnameRU <- paste("plot", i, sep="")
            plotOutput(plotnameRU)
          })

          # Convert the list to a tagList to display properly.
          do.call(tagList, plot_output_listRU)
        })

        for (i in 1:nb_pagesRU) {
          local({
            # Need local so that each item gets its own number. Without it, the value
            # of i in the renderPlot() will be the same across all instances, because
            # of when the expression is evaluated.
            my_iRU <- i
            plotnameRU <- paste("plot", my_iRU, sep="")

            output[[plotnameRU]]<-renderPlot({PlotRepeatUse(repeatUseMatrix(), input$indiv_varRU)})
          }) # end of local
        } # end for

      }else{
        output$plotsRU <- renderUI({
          validate(
            need(length(input$indiv_valRU)>1, "Please, select at least 2 individuals.")
          )
          output$oneImage<-renderPlot({PlotRepeatUse(repeatUseMatrix(), input$indiv_varRU)})
          plotOutput("oneImage")
        })
      }

    } # end of if
  }) # end of observe

  QC_repeatUse <- reactive({
    nb_pagesRU <- round((ncol(indiv_com())/12)+1)
    if(nb_pagesRU==0){
      nb_pagesRU <- 1
    }
    list_plot<-list()
    for(i in 1:nb_pagesRU){
      p<-PlotRepeatUse(repeatUseMatrix(), input$indiv_varRU)
      name<-paste("plot", i, sep = "_")
      list_plot[[name]]<-p
    }
    list_plot
  })

  observe({
    output$downloadImage_RU <- downloadHandler(
      filename = "QC_repeatUse.pdf",
      content = function(fname){
        ggsave(fname, ggpubr::ggarrange(plotlist = c(QC_repeatUse())), device = "pdf")
      }
    )
  })

  observe({
      output$downloadTable_RU <- downloadHandler(filename = "QC_repeatUse.csv",
                                                 content = function(fname){ write.csv(repeatUseMatrix(), fname, row.names = F)}
      )
  })

  #**************************************************************************************************************************************************************
  #    Analysis part
  #**************************************************************************************************************************************************************
  #------------------------------#
  #------- Get input files ------#
  #------------------------------#

  ## Get matrix
  matrix <- reactive({
    if(input$Analysis_testdataLoder=="Yes"){
      tab=read.csv("testData/LentiviralBarcodingData/Analysis_data/Analysis_matrix_Mouse_Lung_cDCs.csv.gz")
    }else{
      req(input$matrix)
      # Check file extension
      ext <- input$matrix_extention
      tab = switch(ext,
                   csv =vroom(input$matrix$datapath, delim = ","),
                   csv2 =vroom(input$matrix$datapath, delim = ";"),
                   tsv =vroom(input$matrix$datapath, delim = "\t"),
                   validate("Invalid file; Upload a matrix as .csv (if separators ,) , .tsv (if separators \t) or .csv2 (if separators ;)"))
      tab<-as.data.frame(tab)
    }
    tab
  })
  ## Get metadata
  metadata <- reactive({
    if(input$Analysis_testdataLoder=="Yes"){
      tab=read.csv("testData/LentiviralBarcodingData/Analysis_data/Analysis_matrix_Mouse_Lung_cDCs_metadata.csv.gz", check.names = F)
    }else{
      req(input$metadata)
      # Check metadata extension
      ext <- input$metadata_extention
      tab = switch(ext,
                   csv =vroom(input$metadata$datapath, delim = ","),
                   csv2 =vroom(input$metadata$datapath, delim = ";"),
                   tsv =vroom(input$metadata$datapath, delim = "\t"),
                   validate("Invalid file; Upload a matrix as .csv (if separators ,) , .tsv (if separators \t) or .txt (if separators ;)"))
      tab<-as.data.frame(tab)
    }
    tab
  })

  observeEvent(input$Analysis_testdataLoder, {
    if(input$Analysis_testdataLoder=="Yes"){
      session$sendCustomMessage("upload_txt_mat", "Analysis_matrix_Mouse_Lung_cDCs.csv.gz")
      output$contents_mat<-renderDataTable(expr = head(read.csv("testData/LentiviralBarcodingData/Analysis_data/Analysis_matrix_Mouse_Lung_cDCs.csv.gz")))
      session$sendCustomMessage("upload_txt_met", "Analysis_matrix_Mouse_Lung_cDCs_metadata.csv.gz")
      output$contents_met<-renderDataTable(expr = head(read.csv("testData/LentiviralBarcodingData/Analysis_data/Analysis_matrix_Mouse_Lung_cDCs_metadata.csv.gz")))
    }else{
      session$sendCustomMessage("upload_txt_mat", "Load your count matrix")
      session$sendCustomMessage("upload_txt_met", "Load your metadata matrix")
      output$contents_mat<-renderDataTable(expr = head(matrix()) )
      output$contents_met<-renderDataTable(expr = head(metadata()) )
    }
  })

  ## Wide to long matrix
  longMatrix<-reactive({
    req(matrix())
    req(metadata())
    lgMtx<-WideToLong(matrix(), metadata())
    lgMtx
  })


  #-----------------------------------#
  #----- Get organism/individual -----#
  #-----------------------------------#



  ## Select Individual
  observe({
    if(nrow(matrix())!=0 && nrow(metadata())!=0 && input$Analysis_testdataLoder=="No"){
      updatePickerInput(session,"organism", choices=colnames(metadata()))
    }

    if(input$Analysis_testdataLoder=="Yes"){
      updatePickerInput(session,"organism",
                        choices=colnames(metadata()),
                        selected ="mouse")
    }
  })

  #------------------------------#
  #------- Reformat inputs ------#
  #------------------------------#


  #-----------------------------------------------#
  ##---- Conditions according analysis type -----##
  #-----------------------------------------------#
  ############################
  #   Sample Similarities    #
  ############################
    observe({ if (input$organism!="") {
      updatePickerInput(session,
                      "organismSample",
                      choices=c(lapply(select(metadata(),input$organism), na.omit),
                                "Pooled individuals"))
      }
    })

    observe({ if (input$organism!="") {
      updatePickerInput(session,
                        "variable",
                        choices=colnames(select(metadata(),
                                                -input$organism)))
      }
    })
      ## Select their Value(s) ##
    observe({ if(length(input$variable)>0) {
      updatePickerInput(session,
                        "value",
                        choices=lapply(select(metadata(),input$variable), na.omit))
      }
    })

    ### warning test
    answers<-reactive({
      CheckValueLengths(input$variable, input$value, metadata())
    })

    checkMice<-reactive({
      check<-0
      if(length(input$organismSample)>1){
        if(is.element("Pooled individuals", input$organismSample)){
          check<-1
        }
      }
      check
    })

  #-- B. Create sub matrix according user selections --#
  #----------------------------------------------------#

  sub_matrix<-reactive({
    if(length(input$organismSample)>0 && length(input$value)>0){
      if(input$organismSample=="Pooled individuals"){
        pool=TRUE
      }else{
        pool=FALSE
      }
      sub_matrix<-MakeHeatmapMatrix(matrix(), metadata(), input$organism, input$organismSample,
                                    input$variable, input$value, pool)
      sub_matrix
    }
  })

  # sub_matrix<-reactive({
  #   if(length(input$organismSample)>0 && length(input$value)>0){
  #     if(input$organismSample=="Pooled individuals"){
  #       sub_matx<-LongToWideSubMatrix_pooledIndiv(longMatrix(),
  #                                                 metadata = metadata(),
  #                                                 indivVar=input$organism,
  #                                                 listVar=input$variable,
  #                                                 listVal=input$value)
  #       sub_matx<-ColToRowNames(sub_matx)
  #     }else{
  #       sub_matx<-LongToWideSubMatrix(longMatrix = longMatrix(),
  #                              metadata(),
  #                              indivVar=input$organism,
  #                              indivVal=input$organismSample,
  #                              listVar=input$variable,
  #                              listVal=input$value)
  #
  #       # barcodes as row names
  #       sub_matx<-ColToRowNames(sub_matx)
  #     }
  #     sub_matx
  #   }
  # })

  max_clust<-reactive({
    if(length(input$organismSample)>0 && length(input$value)>0){
      ## calculate max number of column clusters
      # perform clustering on columns
      m <- as.matrix(asinh(sub_matrix()))
      dist_m<-dist(t(m), input$distance)
      cl.col <- hclust(dist_m, input$clustering)
      max_clusters<-max(cl.col$order)
      max_clusters
    }
  })

  #--- C. Create plots ---#
  #-----------------------#

  observe({ if(input$graph_sampleSim=="Heatmap") {

  ## button to output clusters by colors
  output$nclustersUi <- renderUI({
    # get the max
    numericInput(inputId = "nclusters",
                 label = "Do you want to display clusters for columns?",
                 min = 0,
                 max = max_clust(),
                 value = 0)
  })

  ## Example
  output$heatmapEx<-renderImage({list(src ="images/heatmap.png",
                                      width="100%",
                                      height="100%")
  },deleteFile=FALSE)
  ## Plot
  output$heatmap<-renderPlot({
    ## warning test
    validate(
      need(answers()==length(input$variable), "Please, select at least one value per variable"),
      need(checkMice()==0, "Please, select separated individuals OR 'Pooled individuals' but you can't select both")
    )
    ## end test
    PlotHeatmap(wideMatrix = sub_matrix(),
                distance = input$distance,
                clustering = input$clustering,
                showDendro = input$dendro,
                showBarcodes = input$barcodes,
                nClusters = input$nclusters)
  })

  ## Export
  heatmapImage <- function(){ PlotHeatmap(sub_matrix(),
                                          input$distance,
                                          input$clustering,
                                          input$dendro,
                                          input$barcodes,
                                          input$nclusters) }

  output$downloadImage_heatmap <- downloadHandler(filename = function() {paste0("heatmap_", input$organismSample,".pdf")},
                                                  content = function(fname){
                                                    pdf(fname)
                                                    heatmapImage()
                                                    dev.off()
                                                  }
  )
  output$downloadTable_heatmap <- downloadHandler(filename = function() {paste0("heatmap_matrix_", input$organismSample, ".csv")},
                                                  content = function(fname){ write.csv(sub_matrix(), fname)}
  )

  # Details tabBox
  output$distanceEx <- renderText({"default: euclidean"})
  output$clusteringEx <- renderText({"default: complete"})
  output$distanceSelected <- renderText({ input$distance })
  output$clusteringSelected <- renderText({ input$clustering })

  }
  })

  ############## Correlogram
  ## Example
  output$correloEx<-renderImage({list(src ="images/correlo.png",
                                      width="100%",
                                      height="100%")
  },deleteFile=FALSE)
  ## Plot
  output$correlo<-renderPlot({
    ## warning test
    validate(
      need(answers()==length(input$variable), "Please, select at least one value per variable"),
      need(checkMice()==0, "Please, select separated individuals OR 'Pooled individuals' but you can't select both")
    )
    ## end test
    PlotCorrelogram(sub_matrix(), input$correlation)
   })

  ## Export
  correloImage <- function(){PlotCorrelogram(sub_matrix(), input$correlation)}

  output$downloadImage_correlo <- downloadHandler(filename = function() {paste0("correlo_", input$organismSample,".pdf")},
                                                  content = function(fname){
                                                    pdf(fname)
                                                    correloImage()
                                                    dev.off()
                                                  }
  )
  output$downloadTable_correlo <- downloadHandler(filename = function() {paste0("correlo_matrix_", input$organismSample, ".csv")},
                                                  content = function(fname){ write.csv(sub_matrix(), fname)}
  )

  ##############
  # Clone size #
  ##############
  observe({ if (input$organism!="") {
     updatePickerInput(session,
                       "organismSampleCS",
                       choices=c(lapply(select(metadata(),input$organism), na.omit)))
    }
  })

    observe({ if (input$organism!="") {
      updatePickerInput(session,
                        "variableCS",
                        choices=colnames(select(metadata(),
                                                -input$organism)))
      }
    })

    observe({ if (length(input$variableCS)>0) {
        updatePickerInput(session,
                          "valueCS",
                          choices=lapply(select(metadata(),input$variableCS), na.omit))
      }
    })

      observe({ if (length(input$variableCS)>0) {
        if( input$doColor=="yes"){
          updatePickerInput(session,
                            "colorCS",
                            choices=colnames(select(metadata(), -input$variableCS) ) )
        }else{
          updatePickerInput(session,
                            "colorCS",
                            selected = NULL)
        }
      }
    })

    ####### calculations

    ### for frequencies
    sub_lgMtx<-reactive({
      if(length(input$organismSampleCS)>0 && length(input$variableCS)>0 && length(input$valueCS)>0){
        sub_matx<-LongToWideSubMatrix(longMatrix(),metadata(),
                               indivVar = input$organism,
                               indivVal=input$organismSampleCS,
                               listVar=input$variableCS,
                               listVal=input$valueCS)
        sub_lgMtx<-WideToLong(sub_matx, metadata())
        sub_lgMtx<-sub_lgMtx[which(sub_lgMtx$counts>0),]
        print(sub_lgMtx)
        sub_lgMtx
      }
    })

    ### for cum diag
    cum_mat<-reactive({
      if(length(input$organismSampleCS)>0 && length(input$variableCS)>0 && length(input$valueCS)>0){
        cum_mat<-MakeCumulativeDiagramMatrix(matrix(), metadata(), input$organism, input$organismSampleCS, input$variableCS, input$valueCS, input$colorCS, xProp=input$xProportion)
        cum_mat
      }
    })

    ### warning test
    answers_CS<-reactive({
      CheckValueLengths(input$variableCS, input$valueCS, metadata())
    })

    ## 2.1.Cumulative Diagram ##
    ##°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°##
    observe({ if(input$graphType2=="Cumulative diagram") {
        ## Example
        output$cumulativeDiagramEx<-renderImage({list(src ="images/cumulativeDiagram.png",
                                                      width="100%",
                                                      height="100%")
        },deleteFile=FALSE)

        ## Plot
        output$cumulativeDiagram<-renderPlot({
          ## warning test
          validate(
            need(answers_CS()==length(input$variableCS), "Please, select at least one value per variable")
          )
          print(PlotCumulativeDiagram(cum_mat(),input$organism, input$colorCS, xProp = input$xProportion))
        })

        ## Export
        cumDiagImage <- reactive({ PlotCumulativeDiagram(cum_mat(), input$organism, input$colorCS, xProp=input$xProportion) })
        output$downloadImage_cumDiag <- downloadHandler(filename = function() {paste0(input$graphType2, ".png")},
                                                           content = function(fname){
                                                             ggsave(fname, plot = cumDiagImage(), device = "png")})

        output$downloadTable_cumDiag <- downloadHandler(filename = function() {paste0(input$graphType2, ".csv")},
                                                           content = function(fname){ write.csv(cum_mat(), fname)})

      ## 2.2.Frequency distribution  ##
      ##°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°##
      #observe({
      }else if(input$graphType2=="Frequency distribution") {
        output$nonCumulativeHistEx<-renderImage({list(src ="images/nonCumulativeHist.png",
                                                      width="100%",
                                                      height="100%")
        },deleteFile=FALSE)

        ## Plot
        output$nonCumulativeHist<-renderPlot({
          ## warning test
          validate(
            need(answers_CS()==length(input$variableCS), "Please, select at least one value per variable")
          )
          ## end test
          print(PlotBarcodeFrequencies(sub_lgMtx(),input$colorCS,input$yCS, input$nbins))
        })
        ## Export
        nonCumHistImage <- reactive({ PlotBarcodeFrequencies(sub_lgMtx(),input$colorCS, input$yCS,input$nbins) })
        output$downloadImage_nonCumHist <- downloadHandler(filename = function() {paste0(input$graphType2, ".png")},
                                                        content = function(fname){
                                                          ggsave(fname, plot = nonCumHistImage(), device = "png")
                                                        }
        )
        output$downloadTable_nonCumHist <- downloadHandler(filename = function() {paste0(input$graphType2, ".csv")},
                                                        content = function(fname){ write.csv(sub_lgMtx(), fname)}
        )
      } else {
      }
    })

    ###################
    # Barcode sharing  #
    ###################

  observe({if (input$graphType1!="None") {

    observe({ if (input$organism!="") {
      updatePickerInput(session,
                        "organismSampleSB",
                        choices=lapply(select(metadata(),input$organism), na.omit))
      }
    })

    #_______ DOTPLOT

    #######
    #  X  #
    #######
    observe({ if (input$graphType1=="Dotplot" && input$organism!="") {
      # x var
      updatePickerInput(session,
                        "x_var",
                        choices=colnames(select(metadata(),-input$organism)))
      }
    })

    observe({ if(input$graphType1=="Dotplot" && length(input$x_var)>0){
        # x val
        updatePickerInput(session,
                          "x_val",
                          choices=lapply(select(metadata(),input$x_var), function(x) {x=na.omit(x)
                                         x=x[which(x!="")]
                                         return(x)}))
      }
    })

    #######
    #  Y  #
    #######
    observe({ if (input$graphType1=="Dotplot" && input$organism!="") {
      # y var
      updatePickerInput(session,
                        "y_var",
                        choices=colnames(select(metadata(),-input$organism)))
    }
    })

    observe({ if(input$graphType1=="Dotplot" && length(input$y_var)>0){
        # y val
        updatePickerInput(session,
                          "y_val",
                          choices=lapply(select(metadata(),input$y_var), function(x) {x=na.omit(x)
                          x=x[which(x!="")]
                          return(x)}))
      }
    })


    # COLOR
    observe({
      if (input$graphType1=="Dotplot" && input$organism!="" && input$filledPlotSB=="yes") {
        updatePickerInput(session,
                          "colorSB",
                          selected = NULL,
                          choices=colnames(select(metadata(), -c(input$x_var, input$y_var))))
      }else{
        updatePickerInput(session,
                          "colorSB",
                          selected = NULL,
                          choices=colnames(select(metadata(), -c(input$x_var, input$y_var))))
     }
    })

    #_______ TERNARY

    #########
    #  TOP  #
    #########
    observe({ if (input$graphType1=="Ternary plot" && input$organism!="") {
      updatePickerInput(session,
                        "top_var",
                        choices=colnames(select(metadata(),-input$organism)))
    }
    })

    observe({ if (input$graphType1=="Ternary plot" && input$organism!="") {
      updatePickerInput(session,
                        "top_val",
                        choices=lapply(select(metadata(),input$top_var), function(x) {x=na.omit(x)
                        x=x[which(x!="")]
                        return(x)}))
    }
    })

    ###########
    #  RIGHT  #
    ###########
    observe({ if (input$graphType1=="Ternary plot" && input$organism!="") {
      updatePickerInput(session,
                        "right_var",
                        choices=colnames(select(metadata(),-input$organism)))
    }
    })

    observe({ if (input$graphType1=="Ternary plot" && input$organism!="") {
      updatePickerInput(session,
                        "right_val",
                        choices=lapply(select(metadata(),input$right_var),function(x) {x=na.omit(x)
                        x=x[which(x!="")]
                        return(x)}))
    }
    })

    ##########
    #  LEFT  #
    ##########
    observe({ if (input$graphType1=="Ternary plot" && input$organism!="") {
      updatePickerInput(session,
                        "left_var",
                        choices=colnames(select(metadata(),-input$organism)))
    }
    })

    observe({ if (input$graphType1=="Ternary plot" && input$organism!="") {
      updatePickerInput(session,
                        "left_val",
                        choices=lapply(select(metadata(),input$left_var), function(x) {x=na.omit(x)
                        x=x[which(x!="")]
                        return(x)}))
    }
    })

    ### warning test
    answers_dotX<-reactive({
      CheckValueLengths(input$x_var, input$x_val, metadata())
    })

    answers_dotY<-reactive({
      CheckValueLengths(input$y_var, input$y_val, metadata())
    })

    answers_ternaryT<-reactive({
      CheckValueLengths(input$top_var, input$top_val, metadata())
    })

    answers_ternaryR<-reactive({
      CheckValueLengths(input$right_var, input$right_val, metadata())
    })

    answers_ternaryL<-reactive({
      CheckValueLengths(input$left_var, input$left_val, metadata())
    })

    #-------------------#
    #  Data generation  #
    #-------------------#
    # DOTPLOT
    abundance_matx_dotplot<-reactive({
      if(input$graphType1=="Dotplot"){
        abundance_matx_dotplot<-MakeDotPlotMatrix(matrix(), metadata(),
                                                  input$organism, input$organismSampleSB,
                                                  input$x_var, input$x_val,
                                                  input$y_var, input$y_val,
                                                  input$colorSB, input$SBtransformation)
        abundance_matx_dotplot
      }
    })

    piechart_mat<-reactive({
      if(input$graphType1=="Dotplot"){
        piechart_mat<-MakePieChartMatrix(abundance_matx_dotplot(), input$organism, input$colorSB )
        piechart_mat
      }
   })

    # TERNARY
    abundance_matx<-reactive({
      if(input$graphType1=="Ternary plot" && length(input$organismSampleSB)>0){

        sumAbundces<-MakeTernaryMatrix(matrix(), metadata(), input$organism, input$organismSampleSB,
                          input$top_var, input$top_val,
                          input$right_var, input$right_val,
                          input$left_var, input$left_val,
                          input$colorSB_ternary)
        sumAbundces
      }
    })

    #--- C. Create plots ---#
    #-----------------------#

    observe({
      if (input$graphType1=="Dotplot"){
        ## Example
        output$dotplotEx<-renderImage({list(src ="images/dotplot.png",
                                            width="100%",
                                            height="100%")
        },deleteFile=FALSE)

        if(length(input$y_val)>0 && length(input$x_val)>0){
          ## Plot
          output$dotplot<-renderPlot({
            validate(
              need(answers_dotX()==length(input$x_var), "x: Please, select at least one value per variable"),
              need(answers_dotY()==length(input$y_var), "y: Please, select at least one value per variable")
             )
            trans<-str_split(input$SBtransformation, " ")[[1]][1]
            PlotDotplot(abundance_matx_dotplot(), input$organism, input$colorSB, trans)
          })

          output$piechart<-renderPlot({ PlotPieChart(piechart_mat()) })

          ## Export

          # dotplot
          dotplotImage <- reactive({
            trans<-str_split(input$SBtransformation, " ")[[1]][1]
            PlotDotplot(abundance_matx_dotplot(), input$organism, input$colorSB, trans)
          })

          output$downloadImage_dotplot <- downloadHandler(filename = function() {paste0("dotplot_", input$x_val, "VS", input$y_val,".png")},
                                                          content = function(fname){ggsave(fname, plot = dotplotImage(), device = "png")})

          output$downloadTable_dotplot <- downloadHandler(filename = function() {paste0("dotplotMatrix_",
                                                                                        input$x_val, "VS",
                                                                                        input$y_val, ".csv")},
                                                          content = function(fname){ write.csv(abundance_matx_dotplot(), fname)})

          # piechart
          piechartImage<- reactive({ PlotPieChart(piechart_mat(),input$organism, input$colorSB ) })
          output$downloadImage_piechart <- downloadHandler(filename = function() {paste0("piechart_", input$x_val, "VS", input$y_val,".png")},
                                                          content = function(fname){ggsave(fname, plot = piechartImage(), device = "png")})

          output$downloadTable_piechart <- downloadHandler(filename = function() {paste0("piechartMatrix_",
                                                                                        input$x_val, "VS",
                                                                                        input$y_val, ".csv")},
                                                          content = function(fname){ write.csv(piechart_mat(), fname)})


        }

      } else if (input$graphType1=="Ternary plot"){
        ## Example
        output$ternaryPlotEx<-renderImage({list(src ="images/ternaryplot.png",
                                            width="300",
                                            height="350")
        },deleteFile=FALSE)

        ## Plot
        if(length(input$top_val)>0 && length(input$left_val)>0 && length(input$right_val)>0){
          output$ternaryPlot<-renderPlot({

            validate(
              need(answers_ternaryT()==length(input$top_var), "value1: Please, select at least one value per variable"),
              need(answers_ternaryR()==length(input$right_var), "value2: Please, select at least one value per variable"),
              need(answers_ternaryL()==length(input$left_var), "value3: Please, select at least one value per variable")
            )

            print(PlotTernaryPlot(abundance_matx(), input$organism, input$colorSB_ternary))
          })

          ternaryImage <- reactive({PlotTernaryPlot(abundance_matx(), input$organism, input$colorSB_ternary)})

          output$downloadImage_ternary <- downloadHandler(filename = function() {paste0("ternaryplot_",
                                                                                        input$top_val,"VS",
                                                                                        input$left_val,"VS",
                                                                                        input$right_val, ".png")},
                                                          content = function(fname){
                                                            ggsave(fname, plot = ternaryImage(), device = "png")
                                                          }
          )
          output$downloadTable_ternary <- downloadHandler(filename = function() {paste0("ternaryplotMatrix_",
                                                                                        input$top_val,"VS",
                                                                                        input$left_val,"VS",
                                                                                        input$right_val, ".csv")},
                                                          content = function(fname){ write.csv(abundance_matx(), fname)})
        }

      } else {}

    })
  } # end of not none
  })

    ###################
    #    Diversity    #
    ###################
    #-- A. Get user selections  --#
    #-----------------------------#
    ## Organism in samples ##
   observe({
    if (input$organism!="") {
    updatePickerInput(session,
                      "organismSample_notPooled",
                      choices=lapply(select(metadata(),input$organism), na.omit))

   updatePickerInput(session,
                     "variable_notPooled",
                     choices=colnames(select(metadata(),-input$organism)))
    }
   })

    ## Select their value(s) ##
    observe({if(length(input$variable_notPooled)>0){
      updatePickerInput(session,
                        "value_notPooled",
                        choices=lapply(select(metadata(),input$variable_notPooled), function(x) {x=na.omit(x)
                        x=x[which(x!="")]
                        return(x)}))
      }
    })

    observe({
      if(length(input$organismSample_notPooled)>0 && length(input$variable_notPooled)>0 && length(input$value_notPooled)>0 && input$boxplotCondition=="yes"){
        updatePickerInput(session,
                        "boxplotColor_var",
                        choices=colnames(select(metadata(),-c(input$variable_notPooled, input$organism))) )
      }
    })

    observe({
      if(input$boxplotCondition=="no"){
        updatePickerInput(session,
                          "boxplotColor_var",
                          choices=NULL )
      }
    })

    ### warning test
    answers_Div<-reactive({
      CheckValueLengths(input$variable_notPooled, input$value_notPooled, metadata())
    })

    #-- B. Create sub matrix according user selections --#
    #----------------------------------------------------#

    diversity_matx<-reactive({
      diversity_matx<-CalculDiversity(matrix(), metadata(),
                                      input$organism, input$organismSample_notPooled,
                                      input$variable_notPooled, input$value_notPooled,
                                      input$boxplotColor_var, input$yBoxplot)
    })

    #--- C. Create plots ---#
    #-----------------------#

    ## Example
    output$boxplotEx<-renderImage({list(src ="images/boxplot.png",
                                        width="300",
                                        height="350")
    },deleteFile=FALSE)

    ## Plot
    output$boxplot<-renderPlot({
        validate(
          need(answers_Div()==length(input$variable_notPooled), "Please, select at least one value per variable.")
        )
        print(PlotDiversity(diversity_matx(), input$yBoxplot, input$variable_notPooled, input$organism, input$boxplotColor_var, dots = input$boxplotDot))
    })

    #--- Export ---#
    #--------------#

    boxplotImage <- reactive({ PlotDiversity(diversity_matx(), input$yBoxplot, input$variable_notPooled, input$organism, input$boxplotColor_var, dots = input$boxplotDot) })
    output$downloadImage_boxplot <- downloadHandler(filename = function() {paste0(input$yBoxplot,"_", paste0(input$value_notPooled, collapse = "_"),".png")},
                                                    content = function(fname){
                                                      ggsave(fname, plot = boxplotImage(), device = "png")}
    )

    # Show the number of barcodes per individual (one dot = one indiv)
    observe({
        output$downloadTable_boxplot <- downloadHandler(filename = function() {paste0(input$yBoxplot,"_matrix_",paste0(input$value_notPooled, collapse = "_"), ".csv")},
                                                        content = function(fname){ write.csv(diversity_matx(), fname)}
        )
   })



    ########################
    #    Categorisation    #
    ########################
    #-- A. Get user selections  --#
    #-----------------------------#

    observe({ if (input$organism!="") {
      updatePickerInput(session,
                        "organismCat",
                        choices=c(lapply(select(metadata(),input$organism), function(x) {x=na.omit(x)
                        x=x[which(x!="")]
                        return(x)})))
    }
    })

    observe({ if (input$organism!="") {
      updatePickerInput(session,
                        "catVar",
                        choices=colnames(select(metadata(),-input$organism)))
    }
    })

    observe({ if (input$catVar!="") {
      updatePickerInput(session,
                        "catVal",
                        choices=lapply(c(select(metadata(),input$catVar)), function(x) {x=na.omit(x)
                        x=x[which(x!="")]
                        return(x)}))
    }
    })


    observe({ if (input$organism!="" && input$catVar!="") {
      updatePickerInput(session,
                        "conditionVal",
                        choices=lapply(select(metadata(),-c(input$organism, input$catVar)), function(x) {x=na.omit(x)
                        x=x[which(x!="")]
                        return(x)}))
      }
    })

    observe({updateSliderInput(session, "slider")})

    #-- B. Create sub matrix according user selections --#
    #----------------------------------------------------#


    categoryMatx<-reactive({
      if (length(input$catVal)>1 && length(input$organismCat)>0){

        MakeCategoryMatrices(matrix(), metadata(), input$organism, input$organismCat,
                                                  input$catVar, input$catVal,
                                                  input$slider,
                                                  input$condition, input$conditionVal)
      }
    })
    ##################

    observe({ if (length(input$catVal)==0 && length(input$organismCat)==0 ){
        output$contrib1<-renderImage({list(src ="images/contrib1.png", width="400")
                                     },deleteFile=FALSE)
        output$contrib2<-renderImage({list(src ="images/contrib2.png", width="400")
                                      },deleteFile=FALSE)

    }})

    observe({ if (length(input$catVal)>0 && length(input$organismCat)>0){
      print(length(input$catVal))
        output$bargraphCat_counts<-renderPlot({
          validate(
            need(length(input$catVal)>1, "Select at least two values to be compared.")
          )
          print(PlotCategoryCounts(data.frame(categoryMatx()[2]), input$slider, input$conditionVal))
        })
        output$bargraphCat_percent<-renderPlot({
          validate(
            need(length(input$catVal)>1, "Select at least two values to be compared.")
          )
          print(PlotCategories(data.frame(categoryMatx()[1]), input$slider, input$conditionVal))})
    }})

    bargraphCat_countsImage <- reactive({ PlotCategoryCounts(data.frame(categoryMatx()[2]), input$slider, input$conditionVal) })
    bargraphCat_percentImage <- reactive({ PlotCategories(data.frame(categoryMatx()[1]), input$slider, input$conditionVal) })

    ###########
    # Exports #
    ###########

    output$downloadTable_counts <- downloadHandler(filename = paste0("categorisation_BcCounts_", paste0(input$organismCat, collapse = "_"),".csv"),
                                      content = function(fname){ write.csv(data.frame(categoryMatx()[2]), fname)}
    )

    output$downloadTable_percent <- downloadHandler(filename = paste0("categorisation_abundancesMatrix_", input$slider,"%_", paste0(input$organismCat, collapse = "_"),".csv"),
                                                   content = function(fname){ write.csv(categoryMatx()[1], fname)}
    )

    output$downloadImage_counts <- downloadHandler(filename = paste0("categorisation_BcCounts_", paste0(input$organismCat, collapse = "_"),".png"),
                                            content = function(fname){ ggsave(fname, plot = bargraphCat_countsImage(), device = "png")}
    )

    output$downloadImage_percent <- downloadHandler(filename = paste0("categorisation_abundances_", input$slider,"%_", paste0(input$organismCat, collapse = "_"),".png"),
                                             content = function(fname){
                                               ggsave(fname, plot = bargraphCat_percentImage(), device = "png")
                                             }
    )


}






