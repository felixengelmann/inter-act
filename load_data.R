
library(tidyr)
library(dplyr)
library(stringr)


# ===========================
# Load experiment data
# =========================== 
experiments <- read.csv2("MetaAnalysisData.csv")
experiments <- experiments %>% separate(Method_Measure, c("Method", "Measure","WMC"), sep="_") %>% arrange(desc(DepType), Publication, VerbType, TargetType) %>% unite(ID_Pub, Id, Publication, remove=FALSE) %>% mutate(Effect=as.numeric(as.character(Effect)))

expnames <- c("---", as.character(unique(experiments$ID_Pub)))

getData <- function(expname, measures = c("FPRT","RT")){
  if(expname==expnames[1]) return(NA)
  experiments %>% filter(ID_Pub==expname & Measure%in%measures) %>% rename(Target = TargetType)
}


# ===========================
# Load simulation params
# =========================== 
simfit <- read.csv("simfit-batch-restr.csv")

getParams <- function(expname, decay = 0, measures = c("FPRT","RT")){
  if(expname==expnames[1]) return(NA)
  pub <- str_split(expname, '_')[[1]][2]
  simfit %>% filter(Publication==pub & Measure%in%measures & psc==1 & bll==decay)
}

