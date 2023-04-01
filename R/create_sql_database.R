#' Create genome sql database
#' This function creates sql databases from provided gff files
#' (which must have been mapped to the GCA_900303365.2 reference genome).
#' If a genome is provided (as a DNAStringSet) then this information is added
#' If a directory is provided, the databases are saved in that location
#' @param gff_files a list of gff files from which to create the databases
#' @param genome a DNAStringSetcontaining the genome
#' @param save_dir An existing directory where the databases will be saved
#' @return a list of sql databases
#' @export
create_sql_database<-function(gff_files,genome=NULL,save_dir=NULL){
  if(!is.null(save_dir)){
    if(!dir.exists(save_dir)){
      stop(paste0("Save directory ",save_dir," does not exist"))
    }
  }
  if(!endsWith(save_dir,"/")){
    save_dir<-paste0(save_dir,"/")
  }
  suppressWarnings({ sql_database<-lapply(gff_files,
                     function(f) GenomicFeatures::makeTxDbFromGFF(f,
                      format="gff",
                      organism="Plasmodiophora brassicae",
                      chrominfo=GenomeInfoDb::Seqinfo(seqnames=names(my_genome),
                      seqlengths=lengths(my_genome),
                      genome="GCA_900303365.2")
                     )
                    )
  })
  names(sql_database)<-gsub("\\.gff","",basename(gff_files))

  if(!is.null(save_dir)){
    #save the databases
    sql_save<-lapply(names(sql_database),function(name) AnnotationDbi::saveDb(sql_database[[name]],paste0(save_dir,name,".sqLite")))
  }
  return(sql_database)
}


