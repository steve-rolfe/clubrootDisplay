#' Load Pb314v2 genome
#' This function loads the Pb314v2 genome
#' This is the GCA_900303365.2 reference genome.
#' @param location path to directory containing genome fasta file or NULL to use package data
#' @return a DNA string set of the fasta file to which everything has been mapped
#' @export
load_clubroot_genome<-function(location=NULL){
  #NULL means load genome fasta files provided as data within the library package
  #A location will search the directory for the genome fasta file and load that instead
  if(is.null(location)){
    Pb314v2_file=list.files(path=system.file("extdata", package = "clubrootDisplay"),
                            pattern="GCA_900303365.2_pb314_genomic.fna",
                            full.names = TRUE)
  } else {
    Pb314v2_file=location
    if(!file.exists(Pb314v2_file)){stop(paste0("Cannot load ",location))
                                  }
  }
  Pb314.chr<-Biostrings::readDNAStringSet(Pb314v2_file)
  return(Pb314.chr)
}

