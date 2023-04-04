setwd("G:/Shared drives/Clubroot_Leverhulme/communal_rcode")
library("GenomicFeatures")
library(Rsamtools)
library("bamsignals")
library(clubrootDisplay)

options(ucscChromosomeNames=FALSE)

sql_database<-load_sql_database("./sql_database")
my_genome<-load_clubroot_genome()

my_transcripts<-lapply(sql_database,function(d) transcriptsBy(d,"gene"))
my_transcript_names<-lapply(my_transcripts,function(tr) names(tr))
names(my_transcript_names)<-names(sql_database)

all_transcripts<-get_all_transcripts(sql_database)

bampath="Z:/rolfe_group1/Shared/Clubroot_Leverhulme/Pb_illumina/"
bamfiles=list.files(path=bampath,pattern="*.bam$",full.names = TRUE,recursive=TRUE)
#this is all - remove the ones which are split
bamfiles<-bamfiles[-grep("OVEO01",bamfiles)]
#and the merges
bamfiles<-bamfiles[-grep("bam_files",bamfiles)]
names(bamfiles)<-basename(bamfiles)

#It's a better idea to go through samples and models one at a time - if we add samples or models then we only need to run the new ones
#Lists can do this - but they get complicated - so we'll resort to for loops

for (d in seq_along(my_transcripts)){
  for (f in seq_along(bamfiles)){
    print(paste0(names(sql_database)[d],":",bamfiles[f]))
    out_file<-gsub(".bam",".count",basename(bamfiles[f]))
    out_file=paste0("./counts/",names(sql_database)[d],"/",names(sql_database)[d],"_",out_file)
    if (!file.exists(out_file)){
      my_counts<-bamCount(bamfiles[f], unlist(my_transcripts[[d]]), verbose=FALSE,paired.end="midpoint") #get the names
      my_counts<-data.frame(gene=names(unlist(my_transcripts[[d]])),counts=my_counts,sample=names(bamfiles[f]))
      print(out_file)
      write.table(my_counts,out_file,row.names = FALSE,sep="\t")
    }

  }
}




