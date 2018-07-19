


#  ===========================
#' Prepare 
#' =========================== 

#' Assign meta parameter values according to design
#' ---------------------------
# assign_meta_parameters <- function(dat){
	design <- dat %>% select(-ExpID, -(Effect:Target), -Id) %>% slice(1)
# Interference type (linear order)
	if(design$IntType=="pro") {lp <<- meta_recent; ldp <<- meta_distant}
	if(design$IntType=="retro") {lp <<- meta_distant; ldp <<- meta_recent}
# Latency factor
	lf <<- meta_lf
	# if(psc==1) lf <<- 0.2
	# if(psc==1 & design$DepType=="refl") lf <<- 0.15

# Prominence
	# dprom = 0
	# if(design$Prominence2=="subj_OR_topic") dprom <<- meta_medprom
	# if(design$Prominence2=="subj_AND_topic") dprom <<- meta_highprom
	# if(design$Prominence2=="other") dprom <<- meta_lowprom
# Number of distractors
	if(design$DistrPos=="memory_3x") ndistr <<- 3*meta_memory else ndistr <<- 1
# Cue confusion
	if(nrow(dat) > 1) {
		if(!RESTRCONF & dat$Effect[dat$Target=="Mismatch"]>0) {cuesim <<- cl2cuesim(meta_conflevel)} else {cuesim <<- -1}
		if(RESTRCONF & design$Publication%in%confpubs) {cuesim <<- cl2cuesim(meta_conflevel)}
	} else {cuesim <<- -1}
# }



#  ===========================
#' Run simulations
#' =========================== 
#' Prepare simulation matrix

# sims <- NULL

# for(dprom in meta_lowprom){
# 	print(dprom)
# 	sims_tmp <- create_param_matrix(model_4cond, 5000)
# 	if(!is.null(sims)) sims_tmp$Set = sims_tmp$Set + max(sims$Set)
# 	sims <- bind_rows(sims, sims_tmp)
# }

sims <- create_param_matrix(model_4cond, 5000)
(n_sims <- nrow(sims))

#' Run 
results <- run(sims)

#' Get means
intMeans <- compute_int_means(results)
intMeans <- dat %>% select(Target, Data=Effect) %>% right_join(intMeans) 




#  ===========================
#' Compute fit
#' ===========================  

#' Numerical fit
intMeans <- mutate(intMeans, d=abs(Data-Effect))

#' Summarise numerical fit and correlation
fit <- intMeans %>% group_by(Set) %>% 
  summarise(
    rmsd=mean(d, na.rm=TRUE), 
    SE=sd(d)/sqrt(n()), 
    R=cor(Effect, Data))

fitByTarget <- intMeans %>% group_by(Set, Target, Data, SE, cuesim, dprom) %>% 
  summarise(rmsd=mean(d, na.rm=TRUE)) %>% ungroup()

fitControl <- fitByTarget


# ---------------------------
#' Reduce impact of unreachable or unimportant data points
fitControl <- mutate(fitByTarget, rmsd = ifelse(Target=="Mismatch" & Data>0 & cuesim == -1, rmsd/2, rmsd))
fitControl <- mutate(fitControl, rmsd = ifelse(Target=="Match" & Data<0 & dprom <= 0, rmsd/2, rmsd))
fitControl <- mutate(fitControl, rmsd = ifelse(SE > abs(Data), rmsd/2, rmsd))
# ---------------------------


fitControl <- fitControl %>% group_by(Set) %>% summarise(rmsd=mean(rmsd, na.rm=TRUE))

#' Select best fit
fit$R[is.na(fit$R)] <- 1
fit <- mutate(fit, score=R-(rmsd/100))
# (bestdist <- min(fit$rmsd))
(bestdist <- min(fitControl$rmsd))
(bestcor <- max(fit$R))
(bestfit <- max(fit$score))
# (bestset <- fit$Set[fit$score==bestfit])
# (bestset <- fit$Set[fit$rmsd==bestdist])
(bestset <- fitControl$Set[fitControl$rmsd==bestdist])
bestsetfit <- filter(fit, Set==bestset)
bestfitByTarget <- filter(fitByTarget, Set==bestset) %>% select(Set, Target, rmsd) %>% spread(Target, rmsd)

#' Best model
(bestModel <- filter(intMeans, Set==bestset) %>% mutate(Dataset="Model"))
dat1 <- select(dat, Target, Effect, SE) %>% mutate(Dataset="Data")
bestModel <- bind_rows(bestModel, dat1) %>% mutate(Dataset = factor(Dataset, levels=c("Model","Data")))

#' Parameters
params <- select(bestModel, Set, lf, rth, mas, mp, bll, lp, ldp, psc, qcf, cuesim, dprom, cueweighting, ndistr) %>% slice(1) %>% bind_cols(design) %>% mutate(Fit = bestdist, Fit_Match = bestfitByTarget$Match, Fit_Mismatch = bestfitByTarget$Mismatch, R = bestsetfit$R)

#' Data and predictions
datapred <- select(bestModel, Target, Effect, SE, Dataset, bll, psc, qcf, dprom) %>% cbind(design)



#  ===========================
#' Plot
#' =========================== 

#' Interference plot
dodge <- position_dodge(0.9)
(intPlot <- ggplot(bestModel, aes(Target, Effect, fill=Dataset))
	+ geom_hline(aes(yintercept=0), colour="gray10")
	+ geom_bar(stat="identity", position=dodge)
	# + stat_summary(fun.y="mean", geom="bar", position=dodge)
	+ geom_errorbar(aes(ymin=Effect-SE, ymax=Effect+SE), position=dodge, width=0)
	# + stat_summary(fun.data="mean_cl_normal", width=0, geom="errorbar", show.legend=FALSE, position=dodge)
	+ theme_grey(base_size=18)
	+ theme(legend.title=element_blank())
	+ scale_fill_brewer(palette="Set1")
	+ ylab("Distractor Match - Mismatch (ms)")
	+ ggtitle(as.character(params$Publication))
	)

