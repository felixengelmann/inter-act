library(tidyr)
library(dplyr)
options(dplyr.width=Inf)
library(ggplot2)

source("interACT.R")


#  ===========================
#' Preparation
#' =========================== 

#' Prepare data
lit <- read.csv2("MetaAnalysisData.csv") %>% tbl_df
lit <- separate(lit, Method_Measure, c("Method","Measure","WM"), sep="_")
lit$Prominence <- factor(lit$Prominence2, levels=c("other", "subj_OR_topic", "subj_AND_topic"), labels=c("low", "medium", "high"))
lit <- lit %>% arrange(DepType, Prominence, IntType, Publication)
lit$Id <- 1:nrow(lit)
lit <- filter(lit, Measure!="RRT") %>% unite(ExpID, Id, Publication, DepType, Measure, WM, Cue, IntType, VerbType, remove=FALSE)
lit$Effect <- as.numeric(as.character(lit$Effect))
lit$SE <- as.numeric(as.character(lit$SE))
lit <- rename(lit, Target=TargetType)
(n_lit <- nrow(lit))
lit$Publication

# lit <- arrange(lit,Publication)
# print(distinct(lit[,3]),n=100)

#' Preset parameters
reset_params()
VERBOSE <<- FALSE

lf <<- 0.1
bll <<- 0.5
ans <<- 0.2
mas <<- 1.5
mp <<- 0.25
rth <<- -1.5
cuesim <<- -1
# when qcf>0, base-level influences match-quality (and thus the fan)
qcf <<- 0 
# when psc>0, prominence influences base-level activation
psc <<- 0 

#'  Meta-parameters
meta_recent <<- 0.7
meta_distant <<- 1.3
meta_lowprom <<- -0.5
meta_medprom <<- 0
meta_highprom <<- 2.5
meta_memory <<- 1
meta_deptype <<- 0
meta_method <<- 0
RESTRCONF = TRUE
confpubs <<- c("JaegerEtAl15E1", "JaegerEtAl15E2", "KushEtAl14")



#  ===========================
#' Fit models
#' =========================== 

#' Select publications
#' ---------------------------
pubselect <- unique(lit$Publication)

allparams <- NULL
alldata <- NULL


#' Run classic model
#' ---------------------------
qcf <<- 0
psc <<- 0
mas <<- 1.5
mp <<- 0.25
bll <<- 0.5
cuesim <<- -1
meta_recent <<- 0.7
meta_distant <<- 1.3
meta_lf <<- seq(.1,.25,.025)
meta_conflevel <<- -1
meta_lowprom <<- 0
meta_medprom <<- 0
meta_highprom <<- 0

pdf("fit-classic-decay.pdf")

#' Classic model, decay
for(pub in pubselect){
	dat <- filter(lit, Publication==pub)
	bll <<- 0.5
	dprom <<- 0
	source("fct.fit_model2.R")
	print(intPlot)
	params
	bestModel
	allparams <- bind_rows(allparams, params)
	alldata <- bind_rows(alldata, datapred)	
}

dev.off()


pdf("fit-classic-nodecay.pdf")

#' Classic model, no decay
for(pub in pubselect){
	dat <- filter(lit, Publication==pub)
	bll <<- 0
	dprom <<- 0
	source("fct.fit_model2.R")
	print(intPlot)
	params
	allparams <- bind_rows(allparams, params)
	alldata <- bind_rows(alldata, datapred)		
}

dev.off()



#' Run new model
#' ---------------------------
qcf <<- 10
psc <<- 1
mas <<- 1.5
mp <<- 0.25
meta_lf <<- seq(.1,.25,.025)
meta_conflevel <<- seq(10,100,10)
meta_lowprom <<- seq(-2.5,0,.1)
meta_medprom <<- seq(-1,2,.1)
meta_highprom <<- seq(1,4,.1)

pub_other <- filter(lit, Prominence1=="other") %>% with(unique(Publication))
pub_subj <- filter(lit, Prominence1=="subj") %>% with(unique(Publication))
pub_topic <- filter(lit, Prominence1=="topic") %>% with(unique(Publication))
pub_high <- filter(lit, Prominence1=="subj_topic") %>% with(unique(Publication))

# pdf("fit-new-decay-restr.pdf")

#' New model, decay

for(prompar in meta_lowprom){	
	print(prompar)
	for(pub in pub_other){
		dat <- filter(lit, Publication==pub)
		bll <<- 0.5
		dprom <<- prompar
		source("fct.fit_model2.R")
		print(intPlot)
		params
		allparams <- bind_rows(allparams, params)
		alldata <- bind_rows(alldata, datapred)		
	}
}

for(prompar in meta_medprom){	
	print(prompar)
	for(pub in pub_subj){
		dat <- filter(lit, Publication==pub)
		bll <<- 0.5
		dprom <<- prompar
		source("fct.fit_model2.R")
		print(intPlot)
		params
		allparams <- bind_rows(allparams, params)
		alldata <- bind_rows(alldata, datapred)		
	}
}

for(prompar in meta_medprom){	
	print(prompar)
	for(pub in pub_topic){
		dat <- filter(lit, Publication==pub)
		bll <<- 0.5
		dprom <<- prompar
		source("fct.fit_model2.R")
		print(intPlot)
		params
		allparams <- bind_rows(allparams, params)
		alldata <- bind_rows(alldata, datapred)		
	}
}

for(prompar in meta_highprom){	
	print(prompar)
	for(pub in pub_high){
		dat <- filter(lit, Publication==pub)
		bll <<- 0.5
		dprom <<- prompar
		source("fct.fit_model2.R")
		print(intPlot)
		params
		allparams <- bind_rows(allparams, params)
		alldata <- bind_rows(alldata, datapred)		
	}
}

# dev.off()

pdf("fit-new-nodecay-restr.pdf")

#' New model, no decay
for(prompar in meta_lowprom){	
	print(prompar)
	for(pub in pub_other){
		dat <- filter(lit, Publication==pub)
		bll <<- 0
		dprom <<- prompar
		source("fct.fit_model2.R")
		print(intPlot)
		params
		allparams <- bind_rows(allparams, params)
		alldata <- bind_rows(alldata, datapred)		
	}
}

for(prompar in meta_medprom){	
	print(prompar)
	for(pub in pub_subj){
		dat <- filter(lit, Publication==pub)
		bll <<- 0
		dprom <<- prompar
		source("fct.fit_model2.R")
		print(intPlot)
		params
		allparams <- bind_rows(allparams, params)
		alldata <- bind_rows(alldata, datapred)		
	}
}

for(prompar in meta_medprom){	
	print(prompar)
	for(pub in pub_topic){
		dat <- filter(lit, Publication==pub)
		bll <<- 0
		dprom <<- prompar
		source("fct.fit_model2.R")
		print(intPlot)
		params
		allparams <- bind_rows(allparams, params)
		alldata <- bind_rows(alldata, datapred)		
	}
}

for(prompar in meta_highprom){	
	print(prompar)
	for(pub in pub_high){
		dat <- filter(lit, Publication==pub)
		bll <<- 0
		dprom <<- prompar
		source("fct.fit_model2.R")
		print(intPlot)
		params
		allparams <- bind_rows(allparams, params)
		alldata <- bind_rows(alldata, datapred)		
	}
}

dev.off()

allparams

write.csv(allparams, "simfit-batch-restr2.csv",row.names=FALSE, fileEncoding="UTF-8")
write.csv(alldata, "simresults-batch-restr2.csv", row.names=FALSE, fileEncoding="UTF-8")




#  ===========================
#' Analyse
#' ===========================  

allparams <- read.csv("simfit-batch-restr2.csv")
alldata <- read.csv("simresults-batch-restr2.csv")
allparams$Prominence2 <- factor(allparams$Prominence2, levels=c("other","subj_OR_topic","subj_AND_topic"))
alldata$Prominence2 <- factor(alldata$Prominence2, levels=c("other","subj_OR_topic","subj_AND_topic"))

#
# Select best prom values
#

# decay
allparams %>% filter(psc==1 & bll==0.5) %>% group_by(DepType, Prominence, dprom) %>% summarise(fit = mean(Fit)) %>% filter(fit==min(fit))

allparams %>% filter(psc==1 & bll==0.5) %>% group_by(Prominence, dprom) %>% summarise(fit = mean(Fit)) %>% filter(fit==min(fit))

# no decay
allparams %>% filter(psc==1 & bll==0) %>% group_by(DepType, Prominence, dprom) %>% summarise(fit = mean(Fit)) %>% filter(fit==min(fit))


#' Model and decay
allparams %>% group_by(psc, bll) %>% summarise(mean(Fit), SE_Fit=sd(Fit)/sqrt(n()), mean(lf))

#' Prominence
allparams %>% filter(psc==1) %>% group_by(psc, bll, Prominence2) %>% summarise(mean(Fit), SE_Fit=sd(Fit)/sqrt(n()), mean(lf), mean(dprom))

allparams %>% filter(psc==1, bll==0.5) %>% group_by(psc, Prominence2, DistrPos) %>% summarise(mean(Fit), SE_Fit=sd(Fit)/sqrt(n()), mean(lf), mean(dprom), sd(dprom)/sqrt(n()))

#' Cuesim
allparams %>% filter(psc==1) %>% group_by(psc, bll, Lang) %>% summarise(mean(Fit), SE_Fit=sd(Fit)/sqrt(n()), mean(lf), mean(cuesim), mean(dprom))

#' DepType
allparams %>% group_by(psc, DepType) %>% summarise(mean(Fit), SE_Fit=sd(Fit)/sqrt(n()), mean(lf), mean(cuesim), mean(dprom))

#' Method
allparams %>% group_by(psc, Method) %>% summarise(mean(Fit), SE_Fit=sd(Fit)/sqrt(n()), mean(lf), mean(cuesim), mean(dprom))

#' IntType and prominence
allparams %>% group_by(psc, IntType) %>% summarise(mean(Fit), SE_Fit=sd(Fit)/sqrt(n()), mean(lf), mean(cuesim), mean(dprom))



#' Prominence
#' ---------------------------
subset(allparams, psc==1 & bll==0) %>% filter(DepType=="refl" & Prominence2=="subj_AND_topic") %>% with(hist(dprom))

ggplot(subset(allparams, psc==1 & bll==0), aes(dprom, col=Prominence2)) + geom_density() + facet_wrap(~DepType)
ggplot(subset(allparams, psc==1 & bll==0), aes(dprom, fill=Prominence2)) + geom_histogram(position=position_dodge(), binwidth=0.5) + facet_wrap(~DepType)

subset(allparams, psc==1 & bll==0) %>% filter(Prominence2=="subj_AND_topic")


#' Effects
#' ---------------------------

extdat <- filter(alldata, (psc==1 | is.na(psc)) & (bll==0 | is.na(bll)))

# ggplot(filter(alldata, psc!=0 & bll!=1 & DepType!="reci"), 
ggplot(filter(extdat, DepType!="reci"), 
	aes(Effect, col=Prominence2, linetype=Dataset)) + 
# geom_bar(stat="summary", position=position_dodge()) + 
	geom_density() +
	facet_wrap(~DepType+Target, scale="free_y")


ggplot(subset(alldata, psc==1 & bll==0 & Dataset=="Data" & DepType!="reci"), aes(Effect, col=Prominence2)) + 
# geom_bar(stat="summary", position=position_dodge()) + 
	geom_density() +
	facet_grid(DepType~Target, scale="free_y")
