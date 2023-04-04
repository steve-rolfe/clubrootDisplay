#' Load Interproscan files files
#' Interproscan TSV output matched to proteins extracted from gff files
#' @param location path to directory containing interproscan files or NULL to use package data (end in .ipro.tsv)
#' @return a list of tsv files
#' @export
load_interproscan<-function(location=NULL){
  #NULL means load interproscan files provided as data within the library package
  #A location will search the directory for ipro tsv files and load those instead
  if(is.null(location)){
    ipro_list=list.files(path=system.file("extdata", package = "clubrootDisplay"),pattern="*.ipro.tsv",full.names = TRUE)
  } else {
    ipro_list=list.files(path=location,pattern="*.ipro.tsv",full.names = TRUE)
  }
  return(ipro_list)
}


