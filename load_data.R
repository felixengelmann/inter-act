
library(tidyr)
library(dplyr)
library(stringr)


# ===========================
# Load experiment data
# =========================== 
experiments <- read.csv("MetaAnalysisData.reindexed.csv")
experiments$Publication <- as.character(experiments$Publication)
experiments$Publication[experiments$Publication == "VanDykeE2LoSyn"] <- "VanDyke07E2LoSyn"
experiments <- experiments %>% separate(Method_Measure, c("Method", "Measure","WMC"), sep = "_") %>% arrange(ID, TargetType) %>% unite(ID_Pub, ID, Publication, remove = FALSE) %>% mutate(Effect = round(as.numeric(as.character(Effect))), SE = round(SE))

experiments$Prominence2 <- factor(experiments$Prominence2, levels = c("other", "subj_OR_topic", "subj_AND_topic"), labels = c("OTHER", "subject OR topic", "subject AND topic"))
experiments$DepType <- factor(experiments$DepType, labels = c("SV agreement", "SV nonagreement", "Reciprocals", "Reflexives"))

expnames <- c("---", as.character(unique(experiments$ID_Pub)))

getData <- function(expname, measures = c("FPRT","RT")){
  if(expname==expnames[1]) return(NA)
  experiments %>% filter(ID_Pub == expname & Measure%in%measures) %>% rename(Target = TargetType)
}


# ===========================
# Load simulation params
# =========================== 
simfit <- read.csv("simfit-batch-restr.csv")

getParams <- function(expname, decay = 0, measures = c("FPRT","RT")){
  if(expname == expnames[1]) return(NA)
  pub <- str_split(expname, '_')[[1]][2]
  simfit %>% filter(Publication == pub & Measure %in% measures & psc == 1 & bll == decay)
}

