#' Creates a gene region track of the selected gene using a model and sql database
#' @param my_gene gene name
#' @param my_model the sql database name in which the gene is found
#' @param sql_database A list of sql_databases created by create_sql_database()
#' @return a gene region track suitable for display with plotTracks
#' @export
gene_GeneRegionTrack<-function(my_gene,sql_database,my_model=NULL){
  if(is.null(my_model)){my_model=get_model_from_gene(my_gene,sql_database)}

  all_transcripts<-get_all_transcripts(sql_database)
  my_loc<-all_transcripts[grep(my_gene,all_transcripts$gene),]

  #get the exons which match the gene
  my_gene.selected<-GenomicFeatures::exonsBy(sql_database[[my_model]],by="tx",use.names=TRUE)[my_gene]
  #it returns a list - so unlist and give the transcript a name
  my_gene.selected<-unlist(my_gene.selected)
  elementMetadata(my_gene.selected)$transcript=names(my_gene.selected)
  #Create a track using the transcript metadata as the annotation
  grTrack.selected<-Gviz::GeneRegionTrack(my_gene.selected,name = my_model,transcriptAnnotation = "transcript",showId=TRUE)
  return(grTrack.selected)
}
