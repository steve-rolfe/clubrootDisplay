#' Map Interproscan tsv to a gene
#' Interproscan TSV output matched to proteins extracted from gff files
#' @param my_gene gene to map
#' @param ipro_list list of ipro tsv files from load_interproscan()
#' @param my_genome Pb314v2 genome from load_genome()
#' @param sql_database Sql databases from load_sql_database()
#' @return a dataframe of ipro motifs mapped to the gene
#' @export
map_interproscan<-function(my_gene,ipro_list,my_genome,sql_database,my_model=NULL){
  #get the model if NULL
  if(is.null(my_model)) {my_model<-get_model_from_gene(my_gene,sql_database)}
  #get the exons and translate into a protein
  my_gene.selected<-GenomicFeatures::exonsBy(sql_database[[my_model]],by="tx",use.names=TRUE)[my_gene]
  my_gene.selected<-unlist(my_gene.selected)
  my_gene_cds_seqs <- GenomicFeatures::extractTranscriptSeqs(my_genome, GRangesList(my_gene.selected))
  my_gene_protein <- Biostrings::translate(my_gene_cds_seqs)
  #create a list of gene positions
  df<-data.frame(ranges(my_gene.selected))
  my_seq<-unlist(lapply(seq_along(df$start),function(x) seq(df$start[x],df$end[x])))
  #get the strand and reverse list if -
  my_strand<-unique(strand(my_gene.selected))
  if(my_strand=='-'){my_seq<-rev(my_seq)}
  #load in the appropriate interproscan object
  ipro_tsv_filename<-ipro_list[grep(my_model,ipro_list)]
  if(!file.exists(ipro_tsv_filename)){stop(paste0("Cannot find ",ipro_tsv_filename))}
  ipro_tsv<-read.delim(ipro_tsv_filename,sep="\t",header=FALSE)
  colnames(ipro_tsv)<-c("Accession","MD5","Sequence_length","Analysis","Signature_accession","Signature_description","Start_location","Stop_location","Score","Status","Date","InterPro_annotations","InterPro_description")
  my_gene_ipro<-ipro_tsv[grep(my_gene,ipro_tsv$Accession),]
  my_gene_ipro$sstart=my_seq[(my_gene_ipro$Start_location-1)*3+1]
  my_gene_ipro$ssend=my_seq[(my_gene_ipro$Stop_location-1)*3+1]
  my_gene_ipro$strand=my_strand
  my_gene_ipro$chromosome=as.character(seqnames(my_gene.selected[1]))
  return(my_gene_ipro)
}
