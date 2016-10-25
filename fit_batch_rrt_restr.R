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
lit <- filter(lit, Measure=="RRT") %>% unite(ExpID, Id, Publication, DepType, Measure, WM, Cue, IntType, VerbType, remove=FALSE)
lit$Effect <- as.numeric(as.character(lit$Effect))
lit$SE <- as.numeric(as.character(lit$SE))
lit <- rename(lit, Target=TargetType) %>% select(-DataRequested, -DataReceived)
(n_lit <- nrow(lit))
lit$Publication


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
meta_lf <<- seq(.1,.25,.05)
meta_conflevel <<- -1
meta_lowprom <<- 0
meta_medprom <<- 0
meta_highprom <<- 0

pdf("fit-classic-decay-rrt.pdf")

#' Classic model, decay
for(pub in pubselect){
	dat <- filter(lit, Publication==pub)
	bll <<- 0.5
	source("fct.fit_model.R")
	print(intPlot)
	params
	bestModel
	allparams <- bind_rows(allparams, params)
	alldata <- bind_rows(alldata, datapred)	
}

dev.off()


pdf("fit-classic-nodecay-rrt.pdf")

#' Classic model, no decay
for(pub in pubselect){
	dat <- filter(lit, Publication==pub)
	bll <<- 0
	source("fct.fit_model.R")
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
meta_lf <<- seq(.1,.25,.05)
meta_conflevel <<- seq(10,100,10)
# meta_lowprom <<- seq(-2.5,2.5,.5)
# meta_medprom <<- seq(-2.5,2.5,.5)
# meta_highprom <<- seq(-2.5,2.5,.5)
meta_lowprom <<- seq(-3,-.5,.5)
meta_medprom <<- seq(-1,1,.5)
meta_highprom <<- seq(.5,3,.5)


pdf("fit-new-decay-restr-rrt.pdf")

#' New model, decay
for(pub in pubselect){
	dat <- filter(lit, Publication==pub)
	bll <<- 0.5
	source("fct.fit_model.R")
	print(intPlot)
	params
	allparams <- bind_rows(allparams, params)
	alldata <- bind_rows(alldata, datapred)		
}

dev.off()

pdf("fit-new-nodecay-restr-rrt.pdf")

#' New model, no decay
for(pub in pubselect){
	dat <- filter(lit, Publication==pub)
	bll <<- 0
	source("fct.fit_model.R")
	print(intPlot)
	params
	allparams <- bind_rows(allparams, params)
	alldata <- bind_rows(alldata, datapred)		
}

dev.off()

allparams

write.csv(allparams, "simfit-batch-rrt-restr.csv")
write.csv(alldata, "simresults-batch-rrt-restr.csv")




#  ===========================
#' Analyse
#' ===========================  

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


