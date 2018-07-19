
library(tidyr)
library(dplyr)
library(readr)
library(stringr)


# ===========================
# Load experiment data
# =========================== 
experiments <- read.csv2("MetaAnalysisData.csv")
experiments$Publication <- as.character(experiments$Publication)

experiments$Prominence2 <- factor(experiments$Prominence2, levels = c("other", "subj_OR_topic", "subj_AND_topic"), labels = c("OTHER", "subject OR topic", "subject AND topic"))
experiments$DepType <- factor(experiments$DepType, labels = c("SV agreement", "SV nonagreement", "Reciprocals", "Reflexives"))

experiments <- experiments %>% arrange(DepType, Prominence2, IntType, Publication, TargetType)

experiments <- experiments %>% separate(Method_Measure, c("Method", "Measure"), sep = "_") %>% mutate(ID = cumsum(ifelse(Publication != lag(Publication) | is.na(lag(Publication)), 1, 0))) %>% unite(ID_Pub, ID, Publication, remove = FALSE) %>% mutate(Effect = round(as.numeric(as.character(Effect))), SE = round(SE))


expnames <- c("---", as.character(unique(experiments$ID_Pub)))

getData <- function(expname, measures = c("FPRT","RT")){
  if(expname==expnames[1]) return(NA)
  experiments %>% filter(ID_Pub == expname & Measure%in%measures) %>% rename(Target = TargetType)
}


# ===========================
# Load simulation params
# =========================== 
simfit <- read.csv("simfit-batch-restr2.csv")
bestproms <- simfit %>% filter(psc==1) %>% group_by(bll, DepType, Prominence2, dprom) %>% summarise(fit = mean(Fit)) %>% filter(fit==min(fit)) %>% select(-fit) %>% rename(bestprom=dprom) %>% ungroup()
simfit <- left_join(simfit, bestproms) %>% filter(!(psc==1 & dprom!=bestprom))


getParams <- function(expname, decay = 0, measures = c("FPRT","RT")){
  if(expname == expnames[1]) return(NA)
  pub <- str_split(expname, '_')[[1]][2]
  simfit %>% filter(Publication == pub & Measure %in% measures & psc == 1 & bll == decay)
}

