#' Creates a dataframe of all transcript names and corresponding databases
#' @param sql_database A list of sql_databases created by create_sql_database()
#' @return a dataframe of transcript names and databases
#' @export
get_all_transcripts<-function(sql_database){

  #get the transcripts
  my_transcripts<-lapply(sql_database,function(d) transcriptsBy(d,"gene"))
  #and the exons
  suppressWarnings({ my_exons<-lapply(sql_database,function(d) exonsBy(d,by="tx",use.names=TRUE))})

  #get the names of the transcripts
  my_transcript_names<-lapply(my_transcripts,function(tr) names(tr))
  names(my_transcript_names)<-names(sql_database)

  all_transcripts<-lapply(my_transcript_names,function(x) data.frame(gene=unlist(x)))
  names(all_transcripts)<-names(sql_database)
  #in a big list which we can search
  all_transcripts<-plyr::ldply(all_transcripts)
  colnames(all_transcripts)<-c("model","gene")
  return(all_transcripts)
}
