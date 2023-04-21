#' Loads sql databases
#' This function loads sql databases from the provided location
#' @param location An existing directory where the databases are located
#' @return a list of sql databases
#' @export
load_sql_database<-function(location){
  if(!is.null(location)){
    if(!dir.exists(location)){
      stop(paste0("Directory ",location," does not exist"))
    }
  }
  sql_database_filenames<-list.files(path=location,pattern="*.sqLite$",full.names = TRUE)
  sql_database<-lapply(sql_database_filenames,function(f) AnnotationDbi::loadDb(f))

  #and name them
  sql_database_names<-basename(sql_database_filenames)
  sql_database_names<-gsub("\\.sqLite","",sql_database_names)
  names(sql_database)<-sql_database_names
  return(sql_database)
}
