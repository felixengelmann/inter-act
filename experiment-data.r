
# load("litdata.Rd")

library(tidyr)
library(dplyr)

# experiments <- read.csv2("JaegerEngelmannVasishthMetaAnalysisData.csv")
experiments <- read.csv2("MetaAnalysisData.csv")
experiments <- experiments %>% separate(Method_Measure, c("Method", "Measure","WMC"), sep="_") %>% arrange(desc(DepType), Publication, VerbType, TargetType) %>% unite(ID_Pub, Id, Publication, remove=FALSE) %>% mutate(Effect=as.numeric(as.character(Effect)))


# experiments <- subset(lit, !ID%in%c(3,4,5,10,13,27,54,55,61,63,65,69) & Method%in%c("SPR","ET") & Set=="data" & Type!="Number agreement")

expnames <- c("---", as.character(unique(experiments$ID_Pub)))


# e <- expnames[2]
getData <- function(expname, measures = c("FPRT","RT")){
	# M <- c(0,0)
	if(expname==expnames[1]) return(NA)
	experiments %>% filter(ID_Pub==expname & Measure%in%measures) %>% rename(Target = TargetType)
	# M <- c(subset(experiments, Experiment==e & Condition=="Match")$M, subset(experiments, Experiment==e & Condition=="Mismatch")$M)
	# data.frame(Target=c("Match","Mismatch"),M=M,CI.lower=NA, CI.upper=NA)
}


# experiments <<- c(
# 	"Default model prediction",
# 	"Sturt (2003) Exp. 2",
# 	"L. Jaeger et al. (2014) Exp. 1",
# 	"L. Jaeger et al. (2014) Exp. 2",
# 	"Badecker & Straub (2002) Exp. 3",
# 	"Cunnings & Felser (2013) Exp. 1",
# 	"Cunnings & Felser (2013) Exp. 2 [low-span]",
# 	"Patil et al. (subm.)",
# 	"King et al. (2012) [non-adj]",
# 	"Kush & Phillips (2014)",
# 	"Van Dyke (2007) Exp. 2 [subj]",
# 	"VanDyke & McElree (2011) Exp. 2b [retro]",
# 	"VanDyke & McElree (2011) Exp. 1b [retro]"
# 	)

# getData <- function(e){
# 	diff <- c(0,0)
# 	if(e==experiments[1]) return("DEFAULT")
# 	if(e==experiments[2]) diff <- c(0,0)
# 	if(e==experiments[3]) diff <- c(0,19)
# 	if(e==experiments[4]) diff <- c(15,NA)
# 	if(e==experiments[5]) diff <- c(42,NA)
# 	if(e==experiments[6]) diff <- c(0,-25)
# 	if(e==experiments[7]) diff <- c(-24,22)
# 	if(e==experiments[8]) diff <- c(20,-50)
# 	if(e==experiments[9]) diff <- c(0,-95)
# 	if(e==experiments[10]) diff <- c(0,30)
# 	if(e==experiments[11]) diff <- c(37,NA)
# 	if(e==experiments[12]) diff <- c(0,NA)
# 	if(e==experiments[13]) diff <- c(81,NA)
# 	data <- data.frame(Target=c("Match","Mismatch"),M=diff,CI.lower=NA, CI.upper=NA)
# 	return(data)
# }