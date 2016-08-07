# TODO: show activation of antecedent and distractor
# TODO: Add effect size in numbers
# TODO: Explanation


library(shiny)
library(ggplot2)
# library(ggthemes)
library(tidyr)
library(dplyr)
library(Hmisc)
options(dplyr.width = Inf)

# source("helpers.r")
# experiments <<- NULL
source("experiment-data.r")
source("inter-act.r")


# Fitted meta-parameters
fit_distant <<- 1.3
fit_recent <<- 0.7
fit_lowprom <<- -1
fit_medprom <<- 0
fit_highprom <<- 2.5
fit_confRec <<- -0.5
fit_confCN <<- -0.5
fit_memory <<- 1

# Others
expname <<- "---"
useDecay <<- TRUE

# Parameters 
reset_params()
VERBOSE <<- FALSE
iterations <- 5000
# qcf <<- 1 # when qcf>0, base-level influences match-quality (and thus the fan)
# psc <<- 1 # when psc>0, prominence influences base-level activation
# rth <<- -4.5
# lp <<- fit_distant
# ldp <<- fit_recent
cl <- cuesim2cl()
# lf <<- 0.2
# bll <<- 0.5
# ans <<- 0.2
# mas <<- 2
# mp <<- 0
# ndistr <<- 1
# cuesim <<- -1
# dprom <<- 0



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

values <- reactiveValues(p1=1, p2=2, p3=3, studyProperties="")

  observe({
    if(input$reset){
      reset_params()
      qcf <<- 10
      psc <<- 1
      rth <<- -4.5
      lp <<- 1
      ldp <<- 1
      cl <- cuesim2cl()
      useDecay <<- TRUE
      iterations=5000
      expname <- "---"

      values$studyProperties <- ""

      # updateSliderInput(session, "qcf", value = qcf)
      updateSliderInput(session, "cl", value = cuesim2cl())
      updateSliderInput(session, "cueweighting", value = cueweighting)
      updateSliderInput(session, "dprom", value = dprom)
      updateSliderInput(session, "ldp", value = ldp)
      updateSliderInput(session, "lp", value = lp)
      updateSliderInput(session, "ndistr", value = ndistr)
      updateSliderInput(session, "lf", value = lf)
      updateSliderInput(session, "rth", value = rth)
      updateSliderInput(session, "ans", value = ans)
      updateSliderInput(session, "mp", value = mp)
      updateSliderInput(session, "mas", value = mas)
      updateSliderInput(session, "ga", value = ga)
      updateSliderInput(session, "lp", value = lp)
      updateSliderInput(session, "ldp", value = ldp)
      updateSliderInput(session, "iterations", value = iterations)
      updateSelectInput(session, "data", selected = expname)
      updateSelectInput(session, "setparams", selected = expname)
      updateRadioButtons(session, "useDecay", selected = useDecay)
    }
  })

  observe({
    if(input$setparams!="---"){
      reset_params()
      lf <<- 0.1
      ans <<- 0.2
      mas <<- 1.5
      mp <<- 0.25
      cuesim <<- -1
      qcf <<- 10
      psc <<- 1
      rth <<- -1.5
      # lp <<- fit_distant
      # ldp <<- fit_recent
      cl <- cuesim2cl()
      useDecay <<- FALSE
      iterations=5000
      #
      expname <- input$setparams
      params <- getData(expname)[1,]
      # Interference type
      if(params$IntType=="pro") {lp <<- fit_recent; ldp <<- fit_distant}
      if(params$IntType=="retro") {lp <<- fit_distant; ldp <<- fit_recent}
      # Prominence
      if(params$Prominence2=="subj_OR_topic") dprom <<- fit_medprom
      if(params$Prominence2=="subj_AND_topic") dprom <<- fit_highprom
      if(params$Prominence2=="other") dprom <<- fit_lowprom
      # Cue confusion
      if(params$DepType=="reci") cuesim <<- fit_confRec
      if(params$DepType=="refl" & params$Lang=="CN") cuesim <<- fit_confCN
      # Number of distractors
      if(params$DistrPos=="memory_3x") ndistr <<- 3*fit_memory

      # Study property text
      values$studyProperties <- paste(
        params$Lang, " ", params$DepType, ", ", params$VerbType, " 
", params$IntType, "active, ", params$Cue, " 
", ndistr, " distr. in ", params$DistrPos,
        sep="")
    
      # updateSliderInput(session, "qcf", value = qcf)
      updateSliderInput(session, "cl", value = cuesim2cl())
      updateSliderInput(session, "cueweighting", value = cueweighting)
      updateSliderInput(session, "dprom", value = dprom)
      updateSliderInput(session, "ldp", value = ldp)
      updateSliderInput(session, "lp", value = lp)
      updateSliderInput(session, "ndistr", value = ndistr)
      updateSliderInput(session, "lf", value = lf)
      updateSliderInput(session, "rth", value = rth)
      updateSliderInput(session, "ans", value = ans)
      updateSliderInput(session, "mp", value = mp)
      updateSliderInput(session, "mas", value = mas)
      updateSliderInput(session, "ga", value = ga)
      updateSliderInput(session, "lp", value = lp)
      updateSliderInput(session, "ldp", value = ldp)
      updateSliderInput(session, "iterations", value = iterations)
      updateSelectInput(session, "data", selected = expname)
      updateSelectInput(session, "setparams", selected = expname)
      updateRadioButtons(session, "useDecay", selected = useDecay)
    }

  })

  # observe({
  #   if(input$setext){
  #     reset_params()
  #     pc <<- 5
  #     cuesim <<- -0.45
  #     lf <<- 0.15
  #     rth <<- -1.5
  #     ans <<- 0.15
  #     mp <<- 1.5
  #     ndistr <<- 1
  #   }
  #   updateSliderInput(session, "pc", value = pc)
  #   updateSliderInput(session, "cl", value = cuesim2cl())
  #   updateSliderInput(session, "cueweighting", value = cueweighting)
  #   updateSliderInput(session, "dbl", value = dbl)
  #   updateSliderInput(session, "ldp", value = ldp)
  #   updateSliderInput(session, "lp", value = lp)
  #   updateSliderInput(session, "nd", value = ndistr)
  #   updateSliderInput(session, "lf", value = lf)
  #   updateSliderInput(session, "rth", value = rth)
  #   updateSliderInput(session, "ans", value = ans)
  #   updateSliderInput(session, "mp", value = mp)
  #   updateSliderInput(session, "mas", value = mas)
  #   updateSliderInput(session, "ga", value = ga)
  # })

  observe({
    if(input$intRet) {
      updateSliderInput(session, "lp", value = fit_distant)
      updateSliderInput(session, "ldp", value = fit_recent)
    }
  #   })
  # observe({
    if(input$intPro) {
      updateSliderInput(session, "lp", value = fit_recent)
      updateSliderInput(session, "ldp", value = fit_distant)
    }
    })

  output$littable = renderDataTable({
      # lit$i <- NULL
      # lit$Set <- NULL
      # lit$Mnum <- NULL
      # lit$TypeClass <- NULL
      # lit$Type2 <- NULL
      # lit$PredOrig <- NULL
      # lit$PromClass <- NULL
      # lit$Subject <- NULL
      experiments %>% select(Id, Publication, DepType, TargetType, Effect, SE, VerbType, Lang, Method, Measure, WMC, Cue, IntType, Prominence=Prominence1)
    })

  update <- observe({
    runmodel <- input$runmodel
    iterations <- input$iterations
    # qcf <<- input$qcf
    cuesim <<- cl2cuesim(input$cl)
    cueweighting <<- input$cueweighting
    dprom <<- input$dprom
    ldp <<- input$ldp
    lp <<- input$lp
    lf <<- input$lf
    rth <<- input$rth
    ans <<- input$ans
    mp <<- input$mp
    mas <<- input$mas
    ga <<- input$ga
    ndistr <<- input$ndistr
    useDecay <<- input$useDecay
    if(!useDecay) bll <<- 0 else bll <<- 0.5

    values$studyProperties <- ""

    sets <- create_param_matrix(model_4cond, iterations=iterations)
    results <- run(sets)
    # means_int <- simMeans <- compute_int_means(results)


    # Latencies
    # ---------------------------
    # meanResults <- select(results, Target, Distractor, latency

    (condPlot <- ggplot(results, aes(Distractor, latency, col=Target, group=Target))
      + stat_summary(fun.y="mean", geom="line")
      + stat_summary(fun.data="mean_cl_normal", size=.4, show.legend=FALSE)
      + xlab("Distractor")
      + ylab("Latency in ms")
      # + scale_color_hc()
      # + theme_bw()
      + scale_color_brewer(palette="Set1")
      + theme(legend.position="right",legend.key=element_blank())
      + ggtitle("Retrieval Latency")
      )

    values$condPlot <- condPlot

    # Interference effect
    # ---------------------------
    intResults <- results %>% select(Set, Iteration, Target, Distractor, latency) %>% spread(Distractor, latency) %>% mutate(Effect = Match-Mismatch, Dataset = "Model")

    # Add data
    if(input$data!="---"){
      expname1 <- input$data
      expdata <- getData(expname1)
      expdata2 <- expdata%>% select(Target, Effect) %>% mutate(Dataset = "Data")
      intResults <- bind_rows(intResults, expdata2) %>% mutate(Dataset = factor(Dataset, levels=c("Model","Data")))

      # Study property text
      params <- expdata[1,]
      values$studyProperties <- paste(
        params$Lang, " ", params$DepType, ", ", params$VerbType, " 
", params$IntType, "active, ", params$Cue, " 
", ndistr, " distr. in ", params$DistrPos,
        sep="")
    }

    # Interference plot
    dodge <- position_dodge(0.9)
    (intPlot <- ggplot(intResults, aes(Target, Effect, fill=Dataset))
      + geom_hline(aes(yintercept=0), colour="gray10")
      + stat_summary(fun.y="mean", geom="bar", position=dodge)
      + stat_summary(fun.data="mean_cl_normal", width=0, geom="errorbar", show.legend=FALSE, position=dodge)
      + theme(legend.title=element_blank())
      + scale_fill_brewer(palette="Set1")
      + ylab("Distractor Match - Mismatch (ms)")
      + ggtitle("Interference Effect")
      )

    values$intPlot <- intPlot


    # Retrieval Accuracy
    # ---------------------------
    accResults <- results %>% 
      gather(Retrieval, Proportion, acc, miss, fail) %>% 
      select(Target, Distractor, Retrieval, Proportion) %>% 
      mutate(Retrieval = factor(Retrieval, levels=c("acc","miss","fail"), labels=c("Target","Distractor","Failed")), Target = factor(Target, labels=c("Target-Match", "Target-Mismatch"))) %>% group_by(Target, Distractor, Retrieval) %>% summarise_each(funs("mean"))

    # Accuracy plot
    dodge <- position_dodge(0.9)
    (accPlot <- ggplot(accResults, aes(Distractor, Proportion, fill=Retrieval))
      + geom_hline(aes(yintercept=0), colour="gray10")
      + geom_bar(stat="identity")
      + scale_fill_brewer(palette="Set1", direction=-1)
      + ggtitle("Retrieval Accuracy")
      + facet_wrap(~Target)
      )

    values$accPlot <- accPlot

  })



  output$studyProperties <- renderText({
    values$studyProperties
    })


  output$plot1 <- renderPlot({
      values$intPlot
    })

  output$plot2 <- renderPlot({
      values$condPlot
    })

  output$plot3 <- renderPlot({
    values$accPlot
  })


 
  })
