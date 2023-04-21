#' Creates display tracks for genes that overlap the selected gene
#' @param my_gene gene name
#' @param sql_database A list of sql_databases created by create_sql_database()
#' @return display tracks of all genes which overlap
#' @export
get_overlapping_genes<-function(my_gene,sql_database){
  my_loc<-get_model_from_gene(my_gene,sql_database)
  my_exons<-GenomicFeatures::exonsBy(sql_database[[my_loc]],by="gene")

  #get the exons which match the gene
  my_gene.selected<-my_exons[my_gene]
  #it returns a list - so unlist and give the transcript a name
  my_gene.selected<-unlist(my_gene.selected)
  elementMetadata(my_gene.selected)$transcript=names(my_gene.selected)
  #get the genes that overlap
  my_gene.overlap.genes<-lapply(sql_database,function(d) transcriptsByOverlaps(d,range(my_gene.selected)))
  suppressWarnings({my_exons<-lapply(sql_database,function(d) exonsBy(d,by="tx",use.names=TRUE))})

  #get the exons of these overlapping genes - we need to access lots of things from the list so go by index
  my_overlap<-function(i){
    exon_list<-NULL
    gr<-my_gene.overlap.genes[[i]] # these are overlapping genes
    sql_select<-names(my_gene.overlap.genes)[i] #get the database name
    sel_tx<-gr$tx_name # check there's something there to get
    if (!is.null(sel_tx)){
      exon_list<-unlist(my_exons[[sql_select]][sel_tx])
      elementMetadata(exon_list)$transcript=names(exon_list)
    }

    return(exon_list)
  }

  #get the overlapping exons - the unlists are a little complicated as we get lists of lists
  my_gene.overlap.exons<-(lapply(seq_along(my_gene.overlap.genes),function(x) (my_overlap(x))))
  #create tracks

  my_tracks<-function(x){
    gr=NULL
    sql_select<-names(my_gene.overlap.genes)[x]
    track_length<-(length(my_gene.overlap.exons[[x]]))
    if(track_length>0){
      gr<-Gviz::GeneRegionTrack(my_gene.overlap.exons[[x]],transcriptAnnotation="transcript",name =sql_select )
    }
    return(gr)
  }

  my_gene.overlap.grTracks<-unlist(lapply(seq_along(my_gene.overlap.exons), function(x) my_tracks(x)))
  return(my_gene.overlap.grTracks)
}

