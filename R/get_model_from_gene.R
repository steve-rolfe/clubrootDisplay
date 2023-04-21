#' Gets the gene model name from a gene
#' @param my_gene gene name
#' @param sql_database A list of sql_databases created by create_sql_database()
#' @return the gene model containing this gene
#' @export
get_model_from_gene<-function(my_gene,sql_database){
  #get all of the transcript names from each database
  all_transcripts<-get_all_transcripts(sql_database)
  #my_model<-as.character(all_transcripts[grep(my_gene,all_transcripts$gene),]["model"])
  my_model<-as.character(all_transcripts[my_gene==all_transcripts$gene,]["model"])
  return(my_model)
}
