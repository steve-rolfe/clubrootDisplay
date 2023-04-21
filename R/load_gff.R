#' Load GFF files
#' This function loads gff files
#' (which must have been mapped to the
#' GCA_900303365.2 reference genome).
#' @param location path to directory containing gff files or NULL to use package data
#' @return a list of gff files
#' @export
load_gff<-function(location=NULL){
  #NULL means load gff files provided as data within the library package
  #A location will search the directory for gff files and load those instead
  if(is.null(location)){
    gff_list=list.files(path=system.file("extdata", package = "clubrootDisplay"),pattern="*.gff$",full.names = TRUE)
  } else {
    gff_list=list.files(path=location,pattern="*.gff$",full.names = TRUE)
  }
  return(gff_list)
}
