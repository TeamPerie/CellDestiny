###############################
#         Package part        #
###############################

#' Delete levels in a list
#'
#' @param list, a list
#' @return a list without levels.
#'
#' @export
DeleteLevels<-function(list){
  list_without_levels<-levels(droplevels(unique(list)))
  return(list_without_levels)
}

#' Convert a list of several lists to a dataframe
#'
#' @param list, a list of lists
#'
#' @return a data-frame
#'
#' @export
ListToDf<-function(list){
  # length of the longest element of the list (will give the number of row of the df)
  maxLength<-max(unlist(lapply(list, function(x) length(x))))
  # get all sublists that will become columns
  for(i in 1:length(list)){
    # cbind each sub list with equal length
    if( i==1 ){
      length(list[[i]]) <- maxLength
      first<-as.data.frame(list[i])
    }else{
      length(list[[i]]) <- maxLength
      first<-cbind(first, as.data.frame(list[i]))
    }
  }
  return(first)
}

#' Generate your metadata
#'
#' @param matrix, a barcode count matrix (samples by columns and barcodes by rows)
#' @param separataor, fileds separator used to split sample names, default is "_"
#'
#' @return a data-frame
#'
#' @export
MakeMetadata<-function(matrix, separataor="_"){
  names<-data.frame("Sample_names"=colnames(matrix[-1]))
  names<-cbind(names,ldply(strsplit(as.character(names$Sample_names),separataor),identity))
  names<-names[,-1]
  list_met<-sapply(names,FUN = unique)
  # get length of the longer sub list that will give the number of rows in the matrix
  nRow<-max(laply(list_met, length))
  metadata<-ListToDf(list_met)
  return(metadata)
}

#####################################
#         Package & app part        #
#####################################

#' Check that each variable has at least one value
#'
#' @param listVar, list of selected variable(s)
#' @param listVal, list of selected value(s)
#' @param metadata, metadata tables with variable and value names
#'
#' @return an integer
#'
#' @export
CheckValueLengths<- function(listVar, listVal, metadata){
  # for warnings
  len=0
  for(var in listVar){
    if( length(intersect(listVal, metadata[,var]))>0 ){
      len=len+1 }}
  return(len)
}

#' First column as rownames
#'
#' @param matrix, a matrix with as first column, row names
#' @return a matrix with row names previously in th efirst column
#'
#' @export
ColToRowNames<-function(matrix){
  rownames(matrix)<-matrix[,1]
  matrix<-matrix[,-1]
  return(data.frame(matrix))
}
## ok
## ok

#' Transform a wide to long matrix
#'
#' @param wideMatrix, a wide format matrix with samples by columns and barcode by rows
#' @param metadata, the metadata of the matrix
#'
#' @return a long format matrix with sample names decomposed with metadata column names
#'
#' @export
WideToLong<-function(wideMatrix, metadata){
  longMat<-reshape2::melt(wideMatrix, variable.name ="samples", id.vars = names(wideMatrix[1]))
  longMat<-cbind(longMat,ldply(strsplit(as.character(longMat$samples),"_"),identity))
  colnames(longMat)<-c("Barcodes", "Sample_names","counts",colnames(metadata))
  longMat<-longMat<-longMat[which(longMat$counts>0),]
  return(longMat)
}
## ok
##ok

#' Create new columns with stitched value names and summed listVal
#'
#' @param matx, a long matrix with barcodes + individuals + value to stich together
#' @param listVal, list of values to stitch
#'
#' @return a matrix with stiched value as column names and summed counts
#'
#' @export
SumVars<-function(matx, listVal){
  # get variable columns
  colmns=names(matx[,-c(which(colnames(matx) %in% c("Barcodes","get(indivVar)","get(colorVar)")))])
  # sum their counts
  matx$new<-rowSums(matx[colmns])
  # stich their names
  colnames(matx)[ncol(matx)]<- paste(listVal, collapse = "_")
  # delete previous single unstiched columns
  matx<-select(matx, -c(colmns))
  return(matx)
}
## ok
## nom changé

####################### get sub matrix with selected values and individuals #######################

#' Select values and individuals
#'
#' @param longMatrix, a long matrix with sample names splited into columns
#' @param indivVar, individuals variable names
#' @param indivVal, individuals value names in same order than variables
#' @param listVar, a list of variables that correspond to metadata column names
#' @param listVal, a list of values that correspond to metadata values
#' @param metadata, a dataframe containing variables as columns names and their corresponding values
#'
#' @return a long df with samples containing values gave in listVal
#'
#' @export
LongSubMatrix<-function(longMatrix, indivVar, indivVal, listVar, listVal, metadata){
  # select individuals
  long_sub_Matrix<-longMatrix[longMatrix[,indivVar] %in% indivVal, ]
  nb_var<-length(listVar)
  # select variables
  for(i in 1:nb_var){
    # get the first variable
    var_name<-listVar[[i]]
    # get all its values from the metadata
    val_listRef<-metadata[,which(colnames(metadata)==var_name)]
    # get the selected ones
    val_wanted<-intersect(listVal, val_listRef)
    if(i==1){
      selected_var<-long_sub_Matrix[long_sub_Matrix[,var_name] %in% val_wanted, ]
    }else{
      selected_var<-selected_var[selected_var[,var_name] %in% val_wanted, ]
    }
  }
  return(selected_var)
}
## ok
## nom changé

#' Select samples according user selection on variables and individuals
#'
#' @param longMatrix, long format matrix splitted sample names in columns
#' @param metadata, a dataframe containing variables as columns names and their corresponding values
#' @param indivVar, individuals variable name
#' @param indivVal, individuals value name
#' @param listVar, a list of variables that correspond to metadata column names
#' @param listVal, a list of values that correspond to metadata values
#'
#' @return a long dataframe matrix with samples containing values gave in listVal
#'
#' @export
LongToWideSubMatrix <- function(longMatrix, metadata,indivVar, indivVal, listVar, listVal){
  # select variables
  selected_var<-LongSubMatrix(longMatrix, indivVar, indivVal, listVar, listVal, metadata)
  selected_var<-selected_var[which(selected_var$counts>0),]
  if(nrow(selected_var)>0){
    # long to wide format
    sub_matrix<-reshape2::dcast(selected_var, formula = Barcodes ~ Sample_names, value.var = "counts", fun.aggregate = sum)
    # Replace NA by zero
    sub_matrix[is.na(sub_matrix)] <- 0
    # Transform counts if they are separated by a comma and are not numeric
    sub_matrix[,-1]<-sapply(sub_matrix[,-1], function(x) {str_replace(x, ",", ".")})
    sub_matrix[,-1]<-sapply(sub_matrix[,-1],as.numeric)
    # if several conditions: select rows where at least one of the conditions are positive
    if(ncol(sub_matrix)>2) {
      sub_matrix<-sub_matrix[rowSums(sub_matrix[,-1])>0,]
    }
    names(sub_matrix) <- sub('^X', '', names(sub_matrix))
    return(sub_matrix)
  }else{
    names(selected_var) <- sub('^X', '', names(selected_var))
    return(selected_var)
  }
}
# ok
## nom changé

#' Count barcode abundances per individual and according conditions
#'
#' @param longMatrix, dataframe in long format
#' @param metadata, the metadata
#' @param indivVar, variable corresponding to indivuals variable
#' @param listVar,  a list of variables that correspond to metadata column names
#' @param colorVar, color variable name
#'
#' @return a long matrix with barcode abundances in each set of variable choose.
#'
#' @export
BcAbundance<-function(longMatrix, metadata, indivVar, listVar, colorVar=""){
  nb_var<-length(listVar)
  # if colors
  if (colorVar!="" && colorVar!=indivVar){
    if(nb_var>1){
      longMatrix$NewVar<-apply(select(longMatrix, listVar) , 1 , paste , collapse = "_" )
      abondance_matrix<-reshape2::dcast(longMatrix, formula = Barcodes+get(indivVar)+get(colorVar) ~ NewVar, value.var = "counts", fun.aggregate = sum, fill = 0)
    }else{
      abondance_matrix<-reshape2::dcast(longMatrix, formula = Barcodes+get(indivVar)+get(colorVar) ~ get(listVar), value.var = "counts", fun.aggregate = sum, fill = 0)
    }
    # if no color or colors == individual
  }else{
    if(nb_var>1){
      longMatrix$NewVar<-apply(select(longMatrix, listVar) , 1 , paste , collapse = "_" )
      abondance_matrix<-reshape2::dcast(longMatrix, formula = Barcodes+get(indivVar) ~ NewVar, value.var = "counts", fun.aggregate = sum, fill = 0)
    }else{
      # wide with values
      abondance_matrix<-reshape2::dcast(longMatrix, formula = Barcodes+get(indivVar) ~ get(listVar), value.var = "counts", fun.aggregate = sum, fill = 0)
    }
  }
  names(abondance_matrix) <- sub('^X', '', names(abondance_matrix))
  return(abondance_matrix) # long matrix
}

## ok
## nom changé

#' Normalize barcode abundances by their total abundance (by row)
#'
#' @param matrix, a dataframe
#' @param nbVal, number of values
#'
#' @return a matrix with normalized barcode abundances by their total abundance (in %)
#'
#' @export
NormByBcAbundance<-function(matrix, nbVal){
  col=1
  for(i in 1:nbVal){
    # add columns of normalised counts according abundance
    cell_type<-colnames(matrix[(ncol(matrix)-col)])
    matrix<-cbind(matrix,  matrix[,(ncol(matrix)-col)]/matrix$abondances*100)
    col=col+2
    # get cell
    colnames(matrix)[ncol(matrix)]<-paste("%",cell_type, sep = "_")
  }
  return(matrix)
}
## ok
## nom changé

####################### Plots ##################################################

################### QC ###############################

#' Reformat QC input table
#'
#' @param matrix, a wide dataframe
#' @param metadata, the metadata
#' @param dupVar, duplicate variable name
#' @param dupVal, duplicate values
#' @param sampleNameFieldsep, sample name variable separatorn default "_"
#' @param transformation, transformation to use, default is "arcsin". Other "log10(x+1)" or "none"
#' @param correlation, correlation method to use, "spearman" or "pearson" (default)
#'
#' @return a long format QC matrix
#'
#' @export
ReformatQCmatrix<-function(matrix, metadata, dupVar, dupVal, sampleNameFieldsep="_", transformation="arcsin", correlation="pearson"){
  # user selection
  qc_matrix<-WideToLong(matrix, metadata)
  # reformat to have one column per duplicate
  col<-qc_matrix[,which(colnames(qc_matrix) != dupVar)]
  col<-col[, which(colnames(col) %in% colnames(metadata))]
  qc_matrix<-select(qc_matrix, -c(Sample_names))
  qc_matrix$Sample_names<-apply(col,1, function(x) return(paste0(x, collapse = sampleNameFieldsep)))
  qc_matrix<-dcast(qc_matrix, formula = Barcodes+Sample_names ~ get(dupVar) , value.var = "counts",fun.aggregate = sum)
  names(qc_matrix) <- sub('^X', '', names(qc_matrix))
  qc_matrix$total_read<-apply(qc_matrix[,c(ncol(qc_matrix)-1, ncol(qc_matrix))], 1, sum)
  qc_matrix<-cbind(qc_matrix, setNames(ldply(strsplit(as.character(qc_matrix$Sample_names), "_"), identity), colnames(col)) )
  # add spearman correlation + pval informations
  cor_samples_ab <- qc_matrix %>%
    group_by(Sample_names) %>%
    mutate(cor=trunc(cor(get(dupVal[1]), get(dupVal[2]), use="na", method = correlation)*10^2)/10^2) %>%
    select(c(Sample_names, cor))
  qc_matrix<-merge(qc_matrix, data.frame(cor_samples_ab), by="Sample_names")
  if(transformation=="arcsin"){
    qc_matrix<-cbind(qc_matrix, data.frame(trans_dup1=asinh(qc_matrix[,3])))
    qc_matrix<-cbind(qc_matrix, data.frame(trans_dup2=asinh(qc_matrix[,4])))
  } else if (transformation=="log10(x+1)"){
    qc_matrix<-cbind(qc_matrix, data.frame(trans_dup1=log10(qc_matrix[,3]+1)))
    qc_matrix<-cbind(qc_matrix, data.frame(trans_dup2=log10(qc_matrix[,4]+1)))
  } else { # none
    qc_matrix$trans_dup1<-qc_matrix[,3]
    qc_matrix$trans_dup2<-qc_matrix[,4]
  }
  qc_matrix<-qc_matrix[which(qc_matrix$total_read>0),]
  return(qc_matrix)
}

#' Select values to create duplicates matrix
#'
#' @param matrix, a long matrix made with ReformatQCmatrix function
#' @param listVar, a list of variables that correspond to metadata column names
#' @param listVal, a list of values that correspond to metadata values
#' @param metadata, a dataframe containing variables as columns names and their corresponding values
#'
#' @return a long df with samples containing values gave in listVal
#'
#' @export
MakeDuplicatesMatrix<-function(matrix, listVar, listVal, metadata){
  nb_var<-length(listVar)
  # select variables
  for(i in 1:nb_var){
    # get the first variable
    var_name<-listVar[[i]]
    # get all its values from the metadata
    val_listRef<-metadata[,which(colnames(metadata)==var_name)]
    # get the selected ones
    val_wanted<-intersect(listVal, val_listRef)
    if(i==1){
      selected_var<-matrix[matrix[,var_name] %in% val_wanted, ]
    }else{
      selected_var<-selected_var[selected_var[,var_name] %in% val_wanted, ]
    }
  }
  names(selected_var) <- sub('^X', '', names(selected_var))
  return(selected_var)
}

#' Plot Duplicates
#'
#' @param matrix, a made by 1)ReformatQCmatrix --> 2) MakeDuplicatesMatrix functions
#' @param dupVal, duplicate value names as a list
#' @param transformation, transformation name, default is "arcsin". Other "log10(x+1)" or "none". It has to be the same as used for ReformatQCmatrix function
#' @param textSize, size of text, default is 15
#' @param correlation, the correlation used in ReformatQCmatrix() function. "spearman" or "pearson" (default)
#'
#' @return a Duplicates matrix
#'
#' @export
PlotDuplicates<-function(matrix, dupVal, transformation="arcsin", textSize=15, correlation ="pearson"){

  f_labels=matrix %>%
    group_by(Sample_names) %>%
    select(cor) %>%
    distinct()

  f_labels$var<-paste0(correlation, ": ",f_labels$cor)

  p<-ggplot(matrix, aes(x=trans_dup1, y=trans_dup2))  +
    geom_point(size=2.5, alpha=0.8, color="#7fdbbe") +
    facet_wrap(facets = ~Sample_names) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ # add right box border
    theme(text = element_text(size = textSize))  + # Change font size
    theme(strip.text.x = element_text(face = "bold", size= textSize)) + # Change
    xlab(paste0("Barcode abundances : ", dupVal[1], " (", transformation , ")")) +
    ylab(paste0("Barcode abundances : ", dupVal[2], " (", transformation , ")")) + # Change
    geom_text(mapping = aes(label = var, x = -Inf, y = Inf),  hjust= -0.1, vjust=  1, data = f_labels)

  return(p)
}

#' Make RepeatUse Matrix
#'
#' @param matrix, a wide dataframe created with ReformatQCmatrix function
#' @param indivVar, indiviudal variable name
#' @param indivVal, indiviudal value names
#'
#' @return matrix
#'
#' @export
MakeRepeatUseMatrix<-function(matrix, indivVar, indivVal){
  ru_matrix<-reshape2::dcast(matrix, formula = Barcodes ~ get(indivVar), value.var = "total_read", fun.aggregate = sum, fill = 0)
  names(ru_matrix) <- sub('^X', '', names(ru_matrix))
  ru_matrix<-ru_matrix[,which(colnames(ru_matrix) %in% c(indivVal, "Barcodes"))]
  return(ru_matrix)
}

#' Plot a dotplot of barcode abundances between individuals
#'
#' @param matrix, a wide dataframe with individuals by columns
#' @param indivVar, indiviudal variable name
#' @param indivCombinations, a dataframe of all 2D individual combinations
#' @param textSize, text size, default is 15
#'
#' @return one or several dotplots of barcode abundances in two individuals
#'
#' @export
PlotRepeatUse<-function(matrix, indivVar, textSize=15){
  indivCombinations<-combn(colnames(matrix[,-1]), 2, FUN = NULL)
  list_plot<-list()
  for(i in 1:ncol(indivCombinations)){
    p<-ggplot(matrix, aes_(x=as.name(indivCombinations[1,i]), y=as.name(indivCombinations[2,i])) ) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))+
      theme(text = element_text(size = textSize))  +
      ylab(paste("Abundances:", indivVar, indivCombinations[2,i], sep = " ")) +
      xlab(paste("Abundances:",indivVar, indivCombinations[1,i], sep = " ")) +
      geom_point(color = "#7fdbbe", alpha =0.8, size=4)
    name<-paste("plot", i, sep = "_")
    list_plot[[name]]<-p
  }
  combined_graphs<-ggarrange(plotlist=list_plot)
  return(combined_graphs)
}
## ok
## nom changé

############################### Analysis ##############################################################


########### Heatmap ###########
###############################

#' Make input matrix for heatmap
#'
#' @param matrix, a barcode count matrix
#' @param metadata, the metadata
#' @param indivVar, variable name defining individuals
#' @param indivVal, list of values, default is "" because of poolIndiv=TRUE
#' @param listVar, list of variables
#' @param listVal, list of values
#' @param poolIndiv, should individuals be pooled, default is TRUE
#'
#' @return a matrix to as input to PlotHeatmap function
#'
#' @export
MakeHeatmapMatrix <- function(matrix, metadata, indivVar, indivVal="", listVar, listVal, poolIndiv=TRUE){
  ### make heatmap matrix
  matx<-WideToLong(matrix, metadata)
  matx<-matx[which(matx$counts>0),]
  if(poolIndiv==FALSE){
    barcodesVar<-"Barcodes"
    # select values & individuales
    selected_var<-LongSubMatrix(matx, indivVar, indivVal, listVar, listVal, metadata)
    # collapse values to create new sample names with wanted values
    if(length(listVar)>1){
      selected_var$samples <- apply(selected_var[,listVar] , 1 , paste , collapse = "_" )
      sub_matrix<-reshape2::dcast(selected_var, formula = get(barcodesVar) ~ samples + get(indivVar), value.var = "counts", fun.aggregate = sum)
    }else{
      sub_matrix<-reshape2::dcast(selected_var, formula = get(barcodesVar) ~ get(indivVar)+get(listVar), value.var = "counts", fun.aggregate=sum)
    }
  }else{ # if pool
    barcodesVar<-"Bc_Ind"
    # select only values, as individuals are pooled
    allInd<-metadata[,which(colnames(metadata)==indivVar)]
    selected_var<-LongSubMatrix(matx, indivVar, allInd, listVar, listVal, metadata)
    selected_var$Bc_Ind<-apply(select(selected_var, c("Barcodes", indivVar)) , 1, function(x) paste0(x, collapse = "_" ))
    if(length(listVar)>1){
      # paste barcode and individual name to create pool
      selected_var$samples <- apply(selected_var[,listVar] , 1 , paste , collapse = "_" )
      sub_matrix<-reshape2::dcast(selected_var, formula = get(barcodesVar) ~ samples, value.var = "counts", fun.aggregate = sum)
    }else{
      sub_matrix<-reshape2::dcast(selected_var, formula = get(barcodesVar) ~ get(listVar), value.var = "counts", fun.aggregate = sum)
    }
  }
  sub_matx<-ColToRowNames(sub_matrix)
  names(sub_matx) <- sub('^X', '', names(sub_matx))
  return(sub_matx)
}

#' Plot a heatmap
#'
#' @param wideMatrix, a dataframe treated with LongToWideSubMatrix() or LongToWideSubMatrix_pooledIndiv() functions
#' @param distance, heatmap distance metrics
#' @param clustering, heatmap clustering method
#' @param showDendro, "yes" or "no". If yes, column dendrogram is shown
#' @param showBarcodes, "yes" or "no". If yes, row barcode names are shown
#' @param nClusters, number of clusters to be shown, minimum is 0
#' @param columnTextSize, column text size
#'
#' @return a heatmap of samples in {wideMatrix}
#'
#' @export
PlotHeatmap <- function(wideMatrix, distance="euclidean", clustering="complete", showDendro="no", showBarcodes="no", nClusters=0, columnTextSize=1){
  # transformation
  if(showDendro=="no"){
    dendro="none"
  }else{
    dendro="column"
  }
  if(showBarcodes=="yes"){
    rowLabel=NULL
  }else{
    rowLabel=FALSE
  }

  m <- as.matrix(asinh(wideMatrix))
  colors=brewer.pal(9,"GnBu")
  if(nClusters==0){
    myhm = function(y, distM, clustM) gplots::heatmap.2(y, distfun=function(x) dist(x,method = distM),
                                                        hclustfun=function(x) hclust(x, method=clustM),
                                                        margins = c(12, 12),
                                                        scale="none",
                                                        col=colors, # colors
                                                        cexCol=columnTextSize, cexRow=0.4, # font size
                                                        srtCol=45, # angle
                                                        key=TRUE, key.title = NA, density.info="none", # legend
                                                        labCol = NULL,
                                                        trace="none",
                                                        labRow = rowLabel, dendrogram = dendro) # user choice
  }else{ # if not null
    # perform clustering on columns
    cl.col <- hclust(dist(t(m), distance), clustering)
    # extract cluster assignments
    gr.col <- cutree(cl.col, nClusters)
    col2 <- brewer.pal(nClusters, "GnBu")

    myhm = function(y, distM, clustM) gplots::heatmap.2(y, distfun=function(x) dist(x,method = distM),
                                                        hclustfun=function(x) hclust(x, method=clustM),
                                                        margins = c(12, 12),
                                                        scale="none",
                                                        col= colors ,
                                                        cexCol=columnTextSize, cexRow=0.4, # font size
                                                        srtCol=45, # angle
                                                        key=TRUE, key.title = NA, density.info="none", # legend
                                                        labCol = NULL,
                                                        trace="none",
                                                        labRow = rowLabel, dendrogram = dendro, # user choice
                                                        ColSideColors=col2[gr.col]) # column clustering
  }
  # heatmap object
  heatMap <- myhm(m, distance, clustering)
  return(heatMap)
}
## ok
## nom changé

###############################

#' Plot and create matrix of a cumulative diagram
#'
#' @param matrix, a long matrix
#' @param  metadata, the metadta
#' @param indivVar, individuals variable name
#' @param indivVal, individuals values name
#' @param var, a variable
#' @param listVal, list of variable values wanted
#' @param colorVar, variable to split and color samples by
#' @param textSize, size of the plot text, deault is 15
#' @param xProp, should the x axis be in proportions, default is "no" Other is "yes"
#'
#' @return a matrix to generate the cumulative plot
#'
#' @export
MakeCumulativeDiagramMatrix <- function(matrix, metadata, indivVar, indivVal, var, listVal, colorVar="", textSize=15, xProp="no"){
  # select user choices
  sub_matx<-WideToLong(matrix, metadata)
  sub_matx<-LongToWideSubMatrix(sub_matx,metadata, indivVar, indivVal, var, listVal)
  sub_matx<-WideToLong(sub_matx, metadata)
  if(colorVar==""){
    new<-sub_matx[order(sub_matx$counts, decreasing = TRUE),]
    new$rank<-c(1:nrow(new)) # the first one is the bigest one
    new$counts_precent<-new$counts*100/sum(new$counts)
    new$cumsum<-cumsum(new$counts_precent)
    new<-select(new, c(cumsum, rank))
    new<-rbind(new, c(0,0))
    if(xProp=="yes"){
      new<-new %>%
        mutate(cumrank=cumsum(rank)) %>%
        mutate(percent=cumrank/sum(rank))
      # set 0 to 0 ranked values
      new[which(new$rank==0),c(ncol(new)-1, ncol(new))]<-0
    }
  }else if(colorVar!="" &&  colorVar!=indivVar) {
    new<-BcAbundance(sub_matx, metadata, indivVar, var, colorVar)
    new[,4:ncol(new)]<-apply(new[,4:ncol(new)], 2 , function(x) x*100/sum(x))
    new<-melt(new)
    new<-new[which(new$value>0),]
    new<-dplyr::arrange(new, desc(value), group_by = variable)
    new<-new %>%
      group_by(variable) %>%
      dplyr::mutate(cumsum = cumsum(value))
    new<-new %>%
      group_by(variable) %>%
      dplyr::mutate(rank = 1:n())
    new<-select(new, c(variable, cumsum, rank))
    add0<-data.frame(unique(new[,1]))
    add0$cumsum<-0
    add0$rank<-0
    colnames(add0)<-colnames(new)
    new<-data.frame(rbind(new, add0))
    if(xProp=="yes"){
      new<-new %>%
        group_by(variable) %>%
        dplyr::mutate(cumrank=cumsum(rank)) %>%
        dplyr::mutate(percent=cumrank/sum(rank))
      # set 0 to 0 ranked values
      new[which(new$rank==0),c(ncol(new)-1, ncol(new))]<-0
    }
  }else{
    new<-sub_matx
    new<-new %>%
      group_by_at(colorVar) %>%
      dplyr::mutate(norm = counts*100/sum(counts))
    #new<-dplyr::arrange(new, desc(norm), group_by_at = colorVar)
    new<- new %>%
      group_by_at(colorVar) %>%
      dplyr::arrange(desc(norm))
    new<-new %>%
      group_by_at(colorVar) %>%
      dplyr::mutate(cumsum = cumsum(norm))
    new<-new %>%
      group_by_at(colorVar) %>%
      dplyr::mutate(rank = 1:n())
    new<-select(new, c(colorVar, cumsum, rank))
    add0<-data.frame(unique(new[,1]))
    add0$cumsum<-0
    add0$rank<-0
    colnames(add0)<-colnames(new)
    new<-data.frame(rbind(new, add0))
    if(xProp=="yes"){
      new<-new %>%
        group_by_at(colorVar) %>%
        dplyr::mutate(cumrank=cumsum(rank)) %>%
        dplyr::mutate(percent=cumrank/sum(rank))
      # set 0 to 0 ranked values
      new[which(new$rank==0),c(ncol(new)-1, ncol(new))]<-0
    }
  }
  return(new)
}

#' Plot cumulative diagram
#'
#' @param matrix, matrix obtained with MakeCumulativeDiagramMatrix function
#' @param indivVar, individuals variable name
#' @param colorVar, variable to split and color samples by
#' @param xProp, should the x axis be in proportions, default is "no". Other is "yes"
#' @param textSize, size of the plot text, deault is 15
#'
#' @return a cumulative plot
#'
#' @export
PlotCumulativeDiagram <- function(matrix, indivVar, colorVar="", xProp="no", textSize=15){
  if(colorVar==""){
    if(xProp=="no"){
      p<-ggplot(matrix, aes(y=cumsum, x=rank)) +
        geom_line(color="#7fdbbe" ,size=3) +
        theme_classic() +
        xlab("Ranked number of clones (decreasing order)") +
        ylab("Cumulative clone size (%)") +
        theme(text = element_text(size = textSize))
    }else{
      p<-ggplot(matrix, aes(y=cumsum, x=percent)) +
        geom_line(color="#7fdbbe", alpha=0.7,size=2) +
        theme_classic()+
        xlab("Ranked number of clones (decreasing order)") +
        ylab("Cumulative clone size (%)") +
        theme(text = element_text(size = textSize))+
        scale_x_continuous(labels = percent)
    }
  }else if(colorVar!="" &&  colorVar!=indivVar){ #color is not indiv
    nbColors=nrow(data.frame(unique(matrix[,which(colnames(matrix)=="variable")])))
    if(nbColors<=8){
      mycolors=colorRampPalette(brewer.pal(8, "Set2"))(8)
    }else{
      mycolors=colorRampPalette(brewer.pal(8, "Set2"))(nbColors)
    }
    if(xProp=="no"){
      p<-ggplot(matrix, aes(y=cumsum, x=rank)) +
        geom_line(aes(colour=variable), alpha=0.7,size=2) +
        theme_classic()+
        scale_color_manual(values = mycolors) +
        xlab("Ranked number of clones (decreasing order)") +
        ylab("Cumulative clone size (%)") +
        theme(text = element_text(size = textSize))+
        labs(color = colorVar)
    }else{
      p<-ggplot(matrix, aes(y=cumsum, x=percent)) +
        geom_line(aes(colour=variable), alpha=0.7,size=2) +
        scale_color_manual(values = mycolors) +
        theme_classic()+
        xlab("Ranked number of clones (decreasing order)") +
        ylab("Cumulative clone size (%)") +
        theme(text = element_text(size = textSize))+
        labs(color = colorVar) +
        scale_x_continuous(labels = percent)
    }
  }else{ #color is indiv
    nbColors=nrow(data.frame(unique(matrix[,which(colnames(matrix)==colorVar)])))
    if(nbColors<=8){
      mycolors=colorRampPalette(brewer.pal(8, "Set2"))(8)
    }else{
      mycolors=colorRampPalette(brewer.pal(8, "Set2"))(nbColors)
    }
    if(xProp=="no"){
      p<-ggplot(matrix, aes(y=cumsum, x=rank)) +
        geom_line(aes_string(color=colorVar), alpha=0.7,size=2)+
        theme_classic()+
        xlab("Ranked number of clones (decreasing order)") +
        ylab("Cumulative clone size (%)") +
        theme(text = element_text(size = textSize)) +
        labs(color = colorVar)+
        scale_color_manual(values = c(mycolors))
    }else{
      p<-ggplot(matrix, aes(y=cumsum, x=percent)) +
        geom_line(aes_string(color=colorVar), alpha=0.7,size=2) +
        scale_color_manual(values = mycolors) +
        theme_classic()+
        xlab("Ranked number of clones (decreasing order)") +
        ylab("Cumulative clone size (%)") +
        theme(text = element_text(size = textSize))+
        labs(color = colorVar) +
        scale_x_continuous(labels = percent)
    }
  }
  return(p)
}
## ok

#' Plot cumulative diagram
#'
#' @param matrix, wide format barcode count matrix
#' @param metadata, a dataframe containing variables as columns names and their corresponding values
#' @param indivVar, individuals variable name
#' @param indivVal, individuals value name
#' @param listVar, a list of variables that correspond to metadata column names
#' @param listVal, a list of values that correspond to metadata values
#'
#' @return a cumulative plot
#'
#' @export
MakeBarcodeFrequenciesMatrix<-function(matrix, metadata, indivVar, indivVal, listVar, listVal){
  lgMtx<-WideToLong(matrix, metadata)
  sub_lgMtx<-LongSubMatrix(lgMtx, indivVar, indivVal, listVar, listVal, metadata)
  sub_lgMtx<-sub_lgMtx[which(sub_lgMtx$counts>0),]
  return(sub_lgMtx)
}


#' Plot a frequency diagram
#'
#' @param subLgMatrix, a long matrix
#' @param colorVar, variable to split and color samples by
#' @param y, should the frequencies by ploted in a density curve or an histogram, degault is "density". Other is "histogram"
#' @param nbins, number of bins, deauflt is 5
#' @param log, shoudl the x axis be in log scale, deault is "no". Other is "yes"
#' @param textSize, size of plot text, default is 15
#'
#' @return a frequency density curve or frequency histogram plot
#'
#' @export
PlotBarcodeFrequencies<- function(subLgMatrix, colorVar="", y="density", nbins=5, log="no", textSize=15){
  # plotNonCumHist
  # Calculating the Sturges bins
  breaks <- pretty(range(subLgMatrix$counts),
                   n = nclass.Sturges(subLgMatrix$counts),
                   min.n = 0)

  nbColors<-length(unique(subLgMatrix[,which(colnames(subLgMatrix)==colorVar)]))
  if(nbColors<8){
    mycolors=colorRampPalette(brewer.pal(8, "Set2"))(8)
  }else{
    mycolors=colorRampPalette(brewer.pal(8, "Set2"))(nbColors)
  }

  if(y=="Density curve"){
    y="density"
  }

  if(colorVar!=""){
    freq_plot<-ggplot(subLgMatrix, aes_string(x="counts", fill=colorVar)) +
      xlab("Barcode abundances") +
      labs(color=colorVar) +
      theme_classic() +
      scale_x_continuous(breaks = breaks)+
      scale_fill_manual(values = mycolors)+
      theme(axis.text.x = element_text(angle = 45, hjust=1), text = element_text(size = textSize))
  }else{
    freq_plot<-ggplot(subLgMatrix, aes(x=counts)) +
      xlab("Barcode abundances") +
      theme_classic()+
      scale_x_continuous(breaks = breaks)+
      theme(axis.text.x = element_text(angle = 45, hjust=1), text = element_text(size = textSize))
  }

  if(y=="density"){ # if density curves
    if (colorVar!=""){
      freq_plot<-freq_plot + geom_density(alpha = 0.4)
    }else{
      freq_plot<-freq_plot + geom_density(alpha = 1, fill = "#66c2a5", color="black")
    }
  }else{ # if histogram
    if(colorVar!=""){
      freq_plot<-freq_plot + geom_histogram(position="identity", bins = nbins, color="black", alpha = 0.4) + ylab("Number of barcodes")
    }else{
      freq_plot<-freq_plot + geom_histogram(position="identity", bins = nbins, fill = "#66c2a5", color="black") +ylab("Number of barcodes")
    }
  }

  if(log=="yes"){
    freq_plot<-freq_plot + scale_x_log10() + xlab("Barcode abundances (log10)")
  }

  return(freq_plot)
}

# ok
## nom changé

#' Plot a correlogram
#'
#' @param matrix, a matrix with barcode as rownames and values as columns
#'
#' @return a correlogram
#'
#' @export
PlotCorrelogram<-function(matrix, correlation="spearman"){
  corr_matx<-cor(matrix, method = correlation)
  corr<-corrplot(corr_matx,
                 col=brewer.pal(7,"GnBu"),
                 type = 'upper',
                 method = 'circle',
                 tl.col = "black")
  return(corr)
}

#' Make matrix for PlotDotPlot function
#'
#' @param matrixWide, a barcode count matrix
#' @param metadata, the metadata
#' @param indivVar, individual variable name
#' @param xVar, x axis variable
#' @param xVal, x axis values
#' @param yVar, y axis variable
#' @param yVal, y axis values
#' @param colorVar, color variable as column in the matrix
#'
#' @return a matrix
#'
#' @export
MakeDotPlotMatrix<-function(matrixWide, metadata, indivVar, indivVal, xVar, xVal, yVar, yVal, colorVar="", transformation="arcsin"){
  matrix<-WideToLong(matrixWide, metadata)
  if(colorVar!="" && colorVar!=indivVar){
    ### X axis
    x<-LongSubMatrix(matrix, indivVar, indivVal, xVar, xVal, metadata)
    ### Y axis
    y<-LongSubMatrix(matrix, indivVar, indivVal,yVar, yVal, metadata)
    ## Step1
    # get the matrix as if there were no colors to avoid duplicated dotes
    # sum by condition + indiv
    x_noColors<-BcAbundance(x, metadata, indivVar, xVar)
    y_noColors<-BcAbundance(y, metadata, indivVar, yVar)
    # concat variable columns
    mincol<-4
    ### X axis
    if(ncol(x_noColors)>mincol) x_noColors<-SumVars(x_noColors, xVal)
    ### Y axis
    if(ncol(y_noColors)>mincol) y_noColors<-SumVars(y_noColors, yVal)
    res_withoutcolor<-merge(x_noColors,y_noColors, all = TRUE, by = c("Barcodes", "get(indivVar)"))
    ## Step 2
    # get color information
    x_colors<-BcAbundance(x,  metadata, indivVar, xVar, colorVar)
    y_colors<-BcAbundance(y, metadata, indivVar, yVar, colorVar)
    # concat values columns
    ### X axis
    if(ncol(x_colors)>mincol) x_colors<-SumVars(x_colors, xVal)
    ### Y axis
    if(ncol(y_colors)>mincol) y_colors<-SumVars(y_colors, yVal)
    # reformat color variable
    res_color<-merge(x_colors,y_colors, all = TRUE, by = c("Barcodes", "get(indivVar)", "get(colorVar)"))
    res_color[is.na(res_color)]<-0
    res_color<-aggregate(x=list(color=res_color$`get(colorVar)`), by=list(res_color$Barcodes, res_color$`get(indivVar)`), paste, collapse="_")
    colnames(res_color)<-c("Barcodes", "get(indivVar)", "get(colorVar)")
    ## Step 3 - merge and transform
    res<-merge(x = res_withoutcolor, res_color, by =c("Barcodes", "get(indivVar)"), all.x=TRUE)
    colnme<-c("Barcodes", indivVar, names(res)[ncol(res)-2], names(res)[ncol(res)-1], colorVar)
    res[is.na(res)]<-0
    res<-res[which(rowSums(res[,c(ncol(res)-2,ncol(res)-1)])>0),]
    if(transformation=="arcsin (default)"){
      res[,c(ncol(res)-2,ncol(res)-1)]<-asinh(res[,c(ncol(res)-2,ncol(res)-1)])
    }else if(transformation=="log10(x+1)") {
      res[,c(ncol(res)-2,ncol(res)-1)]<-log10(1+res[,c(ncol(res)-2,ncol(res)-1)])
    }else{
    }
  }else{
    #no color
    ### X axis
    x<-LongSubMatrix(matrix, indivVar, indivVal,xVar, xVal, metadata)
    # sum by condition + indiv
    x<-BcAbundance(x,  metadata, indivVar, xVar, colorVar)
    ### Y axis
    y<-LongSubMatrix(matrix, indivVar, indivVal,yVar, yVal, metadata)
    y<-BcAbundance(y, metadata, indivVar, yVar, colorVar)
    mincol<-3
    ### X axis
    if(ncol(x)>mincol) x<-SumVars(x, xVal)
    ### Y axis
    if(ncol(y)>mincol) y<-SumVars(y, yVal)
    res<-merge(x,y, all = TRUE, by = c("Barcodes", "get(indivVar)"))
    colnme<-c("Barcodes", indivVar, names(res)[ncol(res)-1], names(res)[ncol(res)])
    res[is.na(res)]<-0
    res<-res[which(rowSums(res[,c(ncol(res)-1,ncol(res))])>0),] # delete null barcodes
    if(transformation=="arcsin"){
      res[,c(ncol(res)-1,ncol(res))]<-asinh(res[,c(ncol(res)-1,ncol(res))])
    }else if(transformation=="log10(x+1)") {
      res[,c(ncol(res)-1,ncol(res))]<-log10(1+res[,c(ncol(res)-1,ncol(res))])
    }else{
    }
  }
  colnames(res)<-colnme
  names(res) <- sub('^X', '', names(res))
  return(res)
}

#' Plot a dotplot
#'
#' @param longMatrix, a matrix made with MakeDotPlotMatrix function
#' @param indivVar, individual variable name
#' @param colorVar, color variable as column in the matrix
#' @param transformation, transformation to use, default is "arcsin". Other is "log10"
#' @param textSize, size of the text of the plot, deault is 15
#'
#' @return a dotplot of barcode abundances in two conditions
#'
#' @export
PlotDotplot<- function(longMatrix, indivVar, colorVar="", transformation="arcsin", textSize=15){
  # if color
  if (colorVar!="" && colorVar!=indivVar){
    x_name<-colnames(longMatrix)[ncol(longMatrix)-2]
    y_name<-colnames(longMatrix)[ncol(longMatrix)-1]
    colnames(longMatrix)<-c("Barcodes", indivVar,"xvar", "yvar", colorVar)
    dotplot<-ggplot(longMatrix, aes_string(x = "xvar",y = "yvar", colour = colorVar)) +
      labs(colour = colorVar)+
      xlab(paste0("BC abundances in ", x_name," (", transformation, ")")) +
      ylab(paste0("BC abundances in ", y_name," (", transformation, ")")) +
      scale_fill_brewer(palette="Set2")+
      geom_point(alpha = 0.8, size=3)
    # if color == indiv
  }else if (colorVar!="" && colorVar==indivVar) {
    x_name<-colnames(longMatrix)[ncol(longMatrix)-1]
    y_name<-colnames(longMatrix)[ncol(longMatrix)]
    colnames(longMatrix)<-c("Barcodes", colorVar, "xvar", "yvar")
    dotplot<-ggplot(longMatrix, aes_string(x = "xvar",y = "yvar", colour = colorVar)) +
      labs(colour = colorVar) +
      xlab(paste0("BC abundances in ", x_name," (", transformation, ")")) +
      ylab(paste0("BC abundances in ", y_name," (", transformation, ")")) +
      scale_colour_brewer(palette="Set2")+
      geom_point(alpha = 0.8, size=3)
  }else{
    x_name<-colnames(longMatrix)[ncol(longMatrix)-1]
    y_name<-colnames(longMatrix)[ncol(longMatrix)]
    columns<-colnames(longMatrix)[1:2]
    colnames(longMatrix)<-c(columns, "xvar", "yvar")
    dotplot<-ggplot(longMatrix, aes(x = xvar, y = yvar)) +
      xlab(paste0("BC abundances in ", x_name," (", transformation, ")")) +
      ylab(paste0("BC abundances in ", y_name," (", transformation, ")")) +
      geom_point(color = "#7fdbbe", alpha =0.8, size=3)
  }

  dotplot<-dotplot +
    theme_classic() +
    theme(plot.margin = unit(c(1,1,1,1), "cm"))+
    theme(text = element_text(size = textSize))

  return(dotplot)
}
# ok
## nom changé


#' Make input matrix for PlotPieChart function
#'
#' @param matrix, matrix made with MakeDotPlotMatrix function
#' @param indivVar, individual variable name
#' @param colorVar, color variable as column in the matrix
#' @param textSize, default is 8
#'
#' @return a matrix for PlotPieChart
#'
#' @export
MakePieChartMatrix<-function(matrix,indivVar, colorVar){
  # paste names that have positive values
  if (colorVar!="" && colorVar!=indivVar){
    matrix$type<-apply(matrix[,c(ncol(matrix)-2,ncol(matrix)-1)], 1, function(x) paste0(names(which(x>0)), collapse=" & "))
  }else{
    matrix$type<-apply(matrix[,c(ncol(matrix)-1,ncol(matrix))], 1, function(x) paste0(names(which(x>0)), collapse=" & "))
  }
  count_type <- melt(table(matrix$type))
  # Compute the position of labels
  count_type <- count_type %>%
    dplyr::arrange(desc(Var1)) %>%
    dplyr::mutate(prop = value / sum(count_type$value) *100) %>%
    dplyr::mutate(ypos = cumsum(prop)- 0.5*prop )
  return(count_type)
}

#' Plot a piechart of the number of shared or specific barcodes
#'
#' @param matrix, matrix made with MakePieChartMatrix function
#' @param textSize, default is 8
#'
#' @return a piechart
#'
#' @export
PlotPieChart<-function(matrix, textSize=8){
  # Piechart
  pieChart<-ggplot(matrix, aes(x="", y=prop, fill=Var1)) +
    geom_bar(stat="identity", width=1, color="black") +
    scale_fill_brewer(palette="Set2") +
    coord_polar("y", start=0) +
    theme_void() +
    geom_text(aes(y = ypos, label = paste0(round(prop), "%")), color = "black", size=textSize) + # proportions
    guides(fill = guide_legend(title = "Group")) +
    scale_y_continuous(breaks = matrix$ypos, labels =NULL) +
    theme(axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position="bottom", legend.direction = "vertical")

  return(pieChart)
}
## ok
## nom

#' Create matrix input for the PlotTernaryPlot function
#'
#' @param matrixWide, barcode count matrix
#' @param metadata, the metadata of the matrix
#' @param indivVar, individuals variable name
#' @param indivVal, individuals value names
#' @param topVar, top ternary variable
#' @param topVal, top ternary values
#' @param rightVar, right ternary variable
#' @param rightVal, right ternary values
#' @param leftVar, left ternary variable
#' @param leftVal, left ternary values
#' @param addColor, should the dots be colored by individuals, default is "no". Other is "yes"
#'
#' @return a ternary plot
#'
#' @export
MakeTernaryMatrix<-function(matrixWide, metadata, indivVar, indivVal,topVar, topVal, rightVar, rightVal, leftVar, leftVal, addColor="no"){

  if(addColor=="no"){
    color_var=""
  }else{
    color_var=indivVar
  }

  matrix<-WideToLong(matrixWide, metadata)

  top<-LongSubMatrix(matrix, indivVar, indivVal,topVar, topVal, metadata)
  top<-BcAbundance(top, metadata, indivVar, topVar, color_var)

  right<-LongSubMatrix(matrix, indivVar, indivVal,rightVar, rightVal, metadata)
  right<-BcAbundance(right, metadata, indivVar, rightVar, color_var)

  left<-LongSubMatrix(matrix, indivVar, indivVal,leftVar, leftVal, metadata)
  left<-BcAbundance(left, metadata, indivVar, leftVar, color_var)

  if(color_var!="" && color_var!=indivVar){
    mincol<-4
  }else{
    mincol<-3
  }
  if(ncol(top)>mincol){
    top<-SumVars(top, topVal)
  }
  if(ncol(right)>mincol){
    right<-SumVars(right, rightVal)
  }
  if(ncol(left)>mincol){
    left<-SumVars(left, leftVal)
  }

  if(color_var!="" && color_var!=indivVar){ # too many bugs
    sumAbundces<-merge(top, right, all = T, by=c("Barcodes","get(indivVar)", "get(color_var)"))
    sumAbundces<-merge(sumAbundces, left, all = T, by=c("Barcodes","get(indivVar)", "get(color_var)"))
    colnme<-c("Barcodes", indivVar, color_var , names(sumAbundces)[ncol(sumAbundces)-2], names(sumAbundces)[ncol(sumAbundces)-1], names(sumAbundces)[ncol(sumAbundces)], "Sum")
  }else{
    sumAbundces<-merge(top, right, all = T, by=c("Barcodes","get(indivVar)"))
    sumAbundces<-merge(sumAbundces, left, all = T, by=c("Barcodes","get(indivVar)"))
    colnme<-c("Barcodes", indivVar, names(sumAbundces)[ncol(sumAbundces)-2], names(sumAbundces)[ncol(sumAbundces)-1], names(sumAbundces)[ncol(sumAbundces)], "Sum")
  }
  sumAbundces[is.na(sumAbundces)]<-0
  colVars<-colnames(sumAbundces[,-c(which(colnames(sumAbundces) %in% c("Barcodes","get(indivVar)","get(color_var)")))])
  sumAbundces$Sum<-rowSums(select(sumAbundces, c(colVars)))
  sumAbundces<-sumAbundces[sumAbundces$Sum>0,]

  for(col in colVars){
    # to have percentage
    sumAbundces[,col]<-sumAbundces[,col]/sumAbundces$Sum*100
  }

  colnames(sumAbundces)<-colnme
  return(sumAbundces)
}

#' Plot a ternary plot
#'
#' @param matrix, a dataframe created by MakeTernaryMatrix function
#' @param indivVar, individuals variable name
#' @param colorVar, color variable name
#' @param addColor, should the dots be colored by individuals, default is "no". Other is "yes"
#'
#' @return a ternary plot
#'
#' @export
PlotTernaryPlot<-function(matrix, indivVar, addColor="no", textSize=15){

  if(addColor=="no"){
    color_var=""
  }else{
    color_var=indivVar
  }

  columnNames<-colnames(matrix[(ncol(matrix)-3):(ncol(matrix)-1)])
  # if colors
  if (color_var!=""){
    if(color_var!=indivVar){
      # 7 columns
      colnames(matrix)<-c("bc","indiv","color","varT", "varR","varL","Sum")
    }else{
      # 6 columns cause indivVar==colorVar
      colnames(matrix)<-c("bc","color","varT", "varR","varL","Sum")
    }
    ternary<-ggtern(matrix, aes(x=varL, y=varT, z=varR)) +
      geom_mask() +
      geom_point(aes(size=Sum, color=color), alpha=0.7) +
      labs(colour = color_var, size="Sum abundances" ) +
      scale_color_brewer(palette = "Set2")

    # if no colors
  }else{
    colnames(matrix)<-c("bc","indiv","varT", "varR", "varL","Sum")
    ternary<-ggtern(matrix, aes(x=varL, y=varT, z=varR)) +
      geom_mask() +
      geom_point(aes(size=Sum), alpha=0.7, color = "#66c2a5") +
      labs(size="Sum abundances")
  }

  ternary <- ternary +
    Tlab(label="", labelarrow = columnNames[1]) +
    Llab(label="", labelarrow = columnNames[3]) +
    Rlab(label="", labelarrow = columnNames[2]) +
    theme_classic() +
    theme_arrowlong() +
    theme_hidetitles() +
    theme(text = element_text(size = textSize))

  return(ternary)
}
## ok
## nom

#' Make catergory matrices
#'
#' @param matrix, barcode count matrix
#' @param metadata, the metadata corresponding to the matrix
#' @param indivVar, name of variable defining individuals
#' @param indivVal, list of selected individuals
#' @param cellTypeVar, list of sected variables
#' @param cellTypeVal, list of sected values
#' @param threshold, lineage bias minimum threshold (%), default is 10
#' @param condition, is there a condtion, default is "no"
#' @param conditionVal,  condition value (only one accepted), default is ""
#'
#' @return a list of two matrices, the category for PlotCategories function and barcode count matrix for PlotCategoryCount function
#'
#' @export
MakeCategoryMatrices<-function(matrixWide, metadata, indivVar, indivVal, cellTypeVar, cellTypeVal, threshold=10, condition="no", conditionVal=""){

  matrix<-WideToLong(matrixWide, metadata)

  ######## 1) for plotCategoryPlot
  if(condition=="no" ||  conditionVal==""){
    # Get remaining variables/values if no conditional one
    list_var<-cellTypeVar
    list_val<-metadata$type
    color=""
  }else{
    # Get conditional value
    list_val<-conditionVal
    # Get the variable from which come from the asking value
    list_var<-names(unlist(apply(metadata, 1, function(x) {which(x==list_val)})))
    color=list_var
  }

  # get the wide matrix with only wanted values
  sub_matx<-LongToWideSubMatrix(matrix, metadata, indivVar, indivVal, list_var, list_val)
  lgMtx<-WideToLong(sub_matx, metadata)

  # Get all barcodes from the condinional value (color)
  abdc<-BcAbundance(lgMtx, metadata, indivVar, cellTypeVar, color)

  nb_val<-length(which(cellTypeVal!=""))
  if(color==""){
    first<-2
    nb_col_other<-first+nb_val
  }else{
    first<-3
    nb_col_other<-first+nb_val
  }
  # Get abundances per column ranging from 0 to 100
  abdc<-abdc[,c(1,2, which(colnames(abdc) %in% c("get(colorVar)", cellTypeVal)))]

  abdc[,c((first+1):ncol(abdc))]<-apply(abdc[,c((first+1):ncol(abdc))], 2, function(x) x/sum(x)*100)
  abdc[is.na(abdc)]<-0
  # Sum all not wanted columns into one called "other"
  abdc_norm<-abdc

  # Delete barcodes with total abundance of 0
  abdc_norm<-abdc_norm[which(rowSums(abdc_norm[,(first+1):ncol(abdc_norm)])>0),]
  # tot abundances per barcode
  abdc_norm$abondances<-rowSums(abdc_norm[,(first+1):ncol(abdc_norm)])
  ## Convert barcode abundances in each cell type in % in each cell type:
  abdc_norm<-NormByBcAbundance(abdc_norm, nb_val)

  abdc_norm[is.na(abdc_norm)]<-0
  # Extract only  %age columns
  sub_cat<-abdc_norm[,c((nb_col_other+2):ncol(abdc_norm))] # +2 to exclude abondance column
  # Select ones higher than the threshold
  list_cat<-apply(sub_cat, 1, function(x){ if(any(x>threshold)){ res<-which(x>threshold)
  }else{ res<-which.max(x)}
    return(names(res))})

  abdc_norm$categories<-lapply(list_cat, function(x) paste0(x, collapse = "_"))

  # Reformat
  abdc_norm$categories<-str_remove_all(abdc_norm$categories, "%_")
  abdc_norm$threshold<-threshold

  ############ in shiny, different matrix
  #nb_val<-length(cellTypeVal)
  if(color==""){
    first<-2
  }else{
    first<-3
  }
  nb_col<-first+nb_val

  ##### Abundances
  # Sum of bc abundances by individuals & categories
  cellType_abundces<-abdc_norm[,c( (first+1): nb_col)]
  catSum<-apply(cellType_abundces, 2,
                function(abundance) aggregate(data = abdc_norm, abundance~categories+`get(indivVar)`, sum) )
  # transform catSum from list to df
  catSum<-do.call(rbind.data.frame, catSum)
  # extract initiale value of the category from rownames
  catSum$type_var<-rownames(catSum)

  # Reformat to have proper value names
  res<-lapply(rownames(catSum), function(x){str_extract(x, cellTypeVal)[which(!is.na(str_extract(x, cellTypeVal)))]})
  catSum$type_var<-unlist(res)
  catSum<-catSum[which(catSum$abundance>0),]
  # keep only row where the category is part of the type
  # find in the first column==category (== combinantions of all cell type), the variable (cell type) corresponding
  # because by default all the categories are calculated by cell type
  catSum$keep<-apply(catSum, 1, function(x) is.element(x[4], unlist(str_split(x[1], "_")) ) )
  catSum<-catSum[catSum$keep==TRUE,]
  #catSum$type_var<-as.vector(unlist(catSum$type_var))

  # mean of bc abundandces across individuals & categories
  mean_category_abundance<-aggregate(abundance~type_var+categories, data = catSum, mean)
  # sd of bc abundandces across individuals
  mean_category_abundance$sd_abundance<-aggregate(abundance~type_var+categories, data = catSum, sd)[,3]

  ##### Number of barcodes
  catSumBis<-apply(cellType_abundces, 2,
                   function(nbBc) aggregate(data = abdc_norm, nbBc~categories+`get(indivVar)`, length) )
  catSumBis<-do.call(rbind.data.frame, catSumBis)
  catSumBis$type_var<-rownames(catSumBis)

  # Reformat to have proper value names
  res<-lapply(rownames(catSumBis), function(x){str_extract(x, cellTypeVal)[which(!is.na(str_extract(x, cellTypeVal)))]})
  catSumBis$type_var<-unlist(res)
  # keep only row where the category is part of the type
  # find in the first column==category (== combinantions of all cell type), the variable (cell type) corresponding
  # because by default all the categories are calculated by cell type
  catSumBis$keep<-apply(catSumBis, 1, function(x) is.element(x[4], unlist(str_split(x[1], "_")) ) )
  catSumBis<-catSumBis[catSumBis$keep==TRUE,]
  #catSumBis$type_var<-as.vector(unlist(catSumBis$type_var))
  # mean of bc abundandces across individuals & categories
  mean_category_nbBc<-aggregate(nbBc~type_var+categories, data = catSumBis, mean)
  # sd of bc abundandces across individuals
  mean_category_nbBc$sd_nbBc<-aggregate(nbBc~type_var+categories, data = catSumBis, sd)[,3] # when NA == only one indiv has this category

  ### Merge abundance and number of barcodes matrices
  category<-merge(mean_category_nbBc, mean_category_abundance, by=c("type_var", "categories"), all = TRUE)
  # % per type_var==row of the number of barcode per category
  category$percent_nbBc<-signif(100*with(category, ave(nbBc, type_var, FUN=function(x) x/sum(x))),4)
  category$threshold<-threshold
  colnames(category)<-c("Variable","Categories", "Mean_nbBc", "sd_nbBc", "Mean_abundance", "sd_abundance", "percent_nbBc","Threshold")
  category[is.na(category)]<-0

  ######## for plotCategoryCounts

  # simple bargraph of categories counts (not cell types as previously)
  # count the number of barcodes per categories
  count <-ddply(abdc_norm, c("threshold","`get(indivVar)`", "categories"), summarize, count=length(categories))
  tot_bc <-ddply(count, "`get(indivVar)`", summarize, tot_bc=sum(count))
  count <- merge(count,tot_bc)
  count <-ddply(count, c("`get(indivVar)`", "categories", "count","tot_bc"), summarize, freq=count/tot_bc*100)
  countmean <- ddply(count,"categories", summarize,mean=mean(freq), sd=sd(freq))
  countmean$threshold<-threshold
  colnames(countmean)<-c("Categories", "Mean_percent", "Sd_percent", "Threshold")

  return(list(category, countmean))
}
## ok
## NOT ADDED

#' Plot a category bias plot
#'
#' @param catMatx , the first matrix of the list created with MakeCategoryMatrices function
#' @param threshold , the lineage bias threshold, default is 10
#' @param conditionVal , value of the condition, default is ""
#' @param textSize , the sier of the text, default is 15
#' @param legendPos , position of the legend arround the plot, default is right. Other options are "bottom" or "top" or "left".
#'
#' @return a bargraphe
#'
#' @export
PlotCategories<-function(catMatx, threshold=10, conditionVal="", textSize=15, legendPos="right"){
  plot<-ggplot(catMatx, aes(fill=Categories, x=Variable, y=percent_nbBc)) +
    geom_bar(stat="identity", position = "fill", color="black") +
    ylab("Contribution") +
    xlab("")  +
    ggtitle(paste0(threshold, "% bias")) +
    scale_y_continuous(labels=scales::percent)+
    scale_fill_brewer(palette="Set2") +
    theme_classic() +
    theme(legend.position=legendPos) +
    theme(text = element_text(size = textSize))

  if(conditionVal!=""){
    plot<-plot+labs(subtitle = paste0("condition : ", conditionVal))
  }
  return(plot)
}
## ok

#' Plot a category counts plot
#'
#' @param catCountMatx, the second matrix of the list created with MakeCategoryMatrices function
#' @param threshold , the lineage bias threshold, default is 10
#' @param textSize , the sier of the text, default is 15
#'
#' @return a bargraphe
#'
#' @export
PlotCategoryCounts<-function(catCountMatx, threshold=10,conditionVal="", textSize=15){
  #colnames(catCountMatx)<-c("Categories", "Mean_percent", "Sd_percent", "Threshold")
  plot<-ggplot(catCountMatx, aes(x=Categories, y=Mean_percent, fill=Categories)) +
    geom_bar(stat="identity", color="black") +
    scale_fill_brewer(palette = "Set2")+
    ylab("%BC across individuals") +
    xlab("") +
    geom_errorbar(aes(ymin=Mean_percent-Sd_percent, ymax=Mean_percent+Sd_percent), width = 0.3) +
    ggtitle(paste0(threshold, "% bias")) +
    scale_y_continuous(labels = function(bs) {paste0(bs, '%')}) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust=1))+
    theme(legend.position="none")+
    theme(text = element_text(size = textSize))

  if(conditionVal!=""){
    plot<-plot+labs(subtitle = paste0("condition : ", conditionVal))
  }

  return(plot)
}
## ok
## nom

#' Calucul barcodes diversity
#'
#' @param matrix, a matrix
#' @param metadata, the metadata
#' @param indivVar, individuals variable name
#' @param indivVal, individuals value names
#' @param listVar, a list of varaibles
#' @param listVal, a list of values
#' @param colorVar , the color variable
#' @param diversity, diversity index, default is "Number of barcodes". Others are "Simpson Index" or "Shannon Index"
#'
#' @return a matrix with diversity values
#'
#' @export
CalculDiversity<-function(matrix, metadata, indivVar, indivVal, listVar, listVal ,colorVar="", diversity="Number of barcodes"){
  longMtx<-WideToLong(matrix, metadata)
  lgSubMatx<-LongSubMatrix(longMtx, indivVar, indivVal, listVar, listVal, metadata)

  ## Hill formula
  Dq = function(x,q) {
    if(q==0){calcul=sum(x[x>0]^q)^(1/(1-q))} # Number of barcodes when 0
    return(calcul)
  }

  nb_var<-length(listVar)
  # 1<variable selected by user, merge them
  # + no color = calcul the diversity by individuals and merged variables
  if(nb_var>1 && colorVar==""){

      lgSubMatx$NewVar<-apply(select(lgSubMatx, c(listVar)) , 1 , paste , collapse = "_" )
    # Number of barcodes
    if(diversity=="Number of barcodes"){
      diversityMatrix<-aggregate(lgSubMatx$counts~lgSubMatx[,indivVar]+lgSubMatx$NewVar, data = lgSubMatx,
                                 function(x) Dq(x, 0))
    # Simpson
    }else if(diversity=="Simpson Index"){
      index="simpson"
      diversityMatrix<-aggregate(lgSubMatx$counts~lgSubMatx[,indivVar]+lgSubMatx$NewVar, lgSubMatx,
                                 function(x) diversity(x, index))
      # Shannon
    }else{
      index="shannon"
      diversityMatrix<-aggregate(lgSubMatx$counts~lgSubMatx[,indivVar]+lgSubMatx$NewVar, lgSubMatx,
                                 function(x) diversity(x, index))
    }
    colnames(diversityMatrix)<-c(indivVar, paste0(listVar, collapse= "_"), diversity)
    # 1<variable, merge them
    # + color = calcul the diversity by the color, individuals and merged variables
  }else if (nb_var>1 && colorVar!="") {
    lgSubMatx$NewVar<-apply(select(lgSubMatx, listVar) , 1 , paste , collapse = "_" )
    if(diversity=="Number of barcodes"){
      diversityMatrix<-aggregate(lgSubMatx$counts~lgSubMatx[,indivVar]+lgSubMatx$NewVar+lgSubMatx[,colorVar], lgSubMatx,
                                 function(x) Dq(x,0))
      # Simpson
    }else if(diversity=="Simpson Index"){
      index="simpson"
      ddiversityMatrix<-aggregate(lgSubMatx$counts~lgSubMatx[,indivVar]+lgSubMatx$NewVar+lgSubMatx[,colorVar], lgSubMatx,
                                  function(x) diversity(x, index))
      # Shannon
    }else{
      index="shannon"
      diversityMatrix<-aggregate(lgSubMatx$counts~lgSubMatx[,indivVar]+lgSubMatx$NewVar+lgSubMatx[,colorVar], lgSubMatx,
                                 function(x) diversity(x, index))
    }
    colnames(diversityMatrix)<-c(indivVar, paste(listVar, collapse= "_"), colorVar, diversity)
    # 1=variable, no need to merge them
    # + no colorVar = calcul the diversity by individuals and targeted variables
  }else if (nb_var==1 && colorVar=="") {
    if(diversity=="Number of barcodes"){
      diversityMatrix<-aggregate(lgSubMatx$counts~lgSubMatx[,indivVar]+lgSubMatx[,listVar], lgSubMatx,
                                 function(x) Dq(x,0))
      # Simpson
    }else if(diversity=="Simpson Index"){
      index="simpson"
      diversityMatrix<-aggregate(lgSubMatx$counts~lgSubMatx[,indivVar]+lgSubMatx[,listVar], lgSubMatx,
                                 function(x) diversity(x, index))
      # Shannon
    }else{
      index="shannon"
      diversityMatrix<-aggregate(lgSubMatx$counts~lgSubMatx[,indivVar]+lgSubMatx[,listVar], lgSubMatx,
                                 function(x) diversity(x, index))
    }
    colnames(diversityMatrix)<-c(indivVar, listVar, diversity)
    # 1=variable, no need to merge them
    # + color = calcul the diversity by the color variable, individuals and targeted variables
  }else if (nb_var==1 && colorVar!=""){
    if(diversity=="Number of barcodes"){
      diversityMatrix<-aggregate(lgSubMatx$counts~lgSubMatx[,indivVar]+lgSubMatx[,listVar]+lgSubMatx[,colorVar], lgSubMatx,
                                 function(x) Dq(x,0))
      # Simpson
    }else if(diversity=="Simpson Index"){
      index="simpson"
      diversityMatrix<-aggregate(lgSubMatx$counts~lgSubMatx[,indivVar]+lgSubMatx[,listVar]+lgSubMatx[,colorVar], lgSubMatx,
                                 function(x) diversity(x, index))
      # Shannon
    }else{
      index="shannon"
      diversityMatrix<-aggregate(lgSubMatx$counts~lgSubMatx[,indivVar]+lgSubMatx[,listVar]+lgSubMatx[,colorVar], lgSubMatx,
                                 function(x) diversity(x, index))
    }
    colnames(diversityMatrix)<-c(indivVar, listVar,colorVar, diversity)
  }else{
    print("else nothing")
  }
  return(diversityMatrix)
}
## ok
## nom

#' Plot a boxplot with diversity measures
#'
#' @param matrix, a long matrix containing diversity measures make with CalculDiversity function
#' @param diversity, diversity index, default is "Number of barcodes". Others are "Simpson Index" or "Shannon Index"
#' @param listVar , a list of varaibles
#' @param indivVar, individuals variable name
#' @param colorVar , the color variable
#' @param dots, reveal dots, default is "no". Other is "yes"
#' @param labels, show dots labels, default is "no". Other is "yes".
#' @param textSize, size of the text, default is 15
#'
#' @return a boxplot
#'
#' @export
PlotDiversity <- function(matrix, diversity, listVar, indivVar, colorVar="", dots="no", labels="no", textSize=15){
  mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(20)
  # If diversity in y :
  #}else{
  nb_var<-length(listVar)
  ## if more than one variable selected; take into account merged names of variables == NewVar
  # if no color
  if(nb_var>1 && colorVar==""){
    ncol_div<-which(colnames(matrix)==diversity)
    varName=paste(listVar, collapse= "_")
    boxplot<-ggplot(matrix, aes_string(x=varName, y=as.name(diversity), fill=varName)) +
      geom_boxplot(color="black") +
      theme_classic()+
      scale_fill_manual(values = brewer.pal(8,name = "Set2")) +
      theme(legend.position = 'none')
    # if color
  }else if (nb_var>1 && colorVar!=""){
    varName=paste(listVar, collapse= "_")
    ncol_div<-which(colnames(matrix)==diversity)
    ncol_color<-which(colnames(matrix)==colorVar)
    boxplot<-ggplot(matrix, aes_string(x=varName, y=as.name(diversity), fill=colorVar)) +
      geom_boxplot()+
      theme_classic() +
      labs(color = colorVar) +
      scale_fill_brewer(palette="Set2")
    ## if only one variable
    # if no color
  }else if (nb_var==1 && colorVar=="") {
    boxplot<-ggplot(matrix, aes_string(x=listVar, y=as.name(diversity), fill=listVar)) +
      geom_boxplot(color="black")+
      theme_classic() +
      xlab("")+
      scale_fill_manual(values = mycolors)+
      theme(legend.position = 'none')
    # if color
  }else if (nb_var==1 && colorVar!="") {
    boxplot<-ggplot(matrix, aes_string(x=listVar, y=as.name(diversity), fill=colorVar)) +
      geom_boxplot()+
      theme_classic() +
      labs(color = colorVar) +
      scale_fill_brewer(palette="Set2")
  }else{
    print("else nothing")
  }
  #}

  # add dots if yes
  if(dots=="yes"){
    boxplot <- boxplot + geom_point(position=position_dodge(width=0.75))
    if(labels=="yes"){
      boxplot <- boxplot + geom_text(aes_string(label = indivVar), check_overlap = TRUE,
                                     position=position_jitter(width = 0.15))
    }
  }

  boxplot<- boxplot  +
    theme(text = element_text(size = textSize))+
    ylab(diversity) +
    xlab("")

  return(boxplot)
}
## ok
## nom
