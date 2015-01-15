
source("act-s.r")


interference_model_simple <- function(iterations=1000, print=0){
  ta <- NULL  ## collects target activations
  l <- NULL   ## collects latencies
  r <- NULL   ## collects retrievals
  ##
  for(i in 1:iterations){
    a <- matrix(rep(NA,4),nrow=2)
    ##
    ##
    ## DEFINE CHUNK ACTIVATIONS AT RETRIEVAL POINT BY CONDITION
    ## fan: for each chunk slot, add the fan values it receives from all cues
    ##      (0 = no match, 1 = single chunk, >2 = fan)
    ## match: for each slot, define the match with its corresponding cue
    ##        (by default, a match (maximum similarity) is 0, a mismatch (maximum difference) is -1,
    ##         but it can be defined as desired)
    ## times: a list of times passed since references of the chunk
    ## weights: a value for each cue, weighting its influence on the chunk activation
    ##
    ##
    ## -- Retrieval cues are +subj +sing
    ##
    ## a) Interference: full match with fan from 1 partially matching chunk
    # Chunk 1 (target) with slot values: +subj +sing
    a[1,1] <- activation(fan=c(1+0, 0+2), match=c(0,0), times=c(lp), weights=cueweights)
    # Chunk 2 (distractor) with slot values: -subj +sing 
    a[1,2] <- activation(fan=c(0+0, 0+2), match=c(-1,0), times=c(ldp), weights=cueweights)    
    ##
    ## b) No interference: full match, no fan
    # 1: +subj +sing
    a[2,1] <- activation(fan=c(1+0, 0+1), match=c(0,0), times=c(lp), weights=cueweights)
    # 2: -subj -sing       
    a[2,2] <- activation(fan=c(0+0, 0+0), match=c(-1,-1), times=c(ldp), weights=cueweights)     
    ##
    ##
    ## COMPUTE RETRIEVAL RESULT AND LATENCY
    ##
    r <- rbind(r, apply(a,1,retrieve))  ## retrieved chunk index
    maxacts <- apply(a,1,max)           ## maximum activation per condition
    l <- rbind(l, latency(maxacts))     ## latencies
    ##
    ta <- rbind(ta, a[,1])              ## target activations
  }
  ##
  ##
  ## MEANS
  ##
  ## correct retrievals:
  c <- r           
  c[r!=1] <- 0
  ## retrieval failures:
  f <- r           
  f[r==0] <- 1; f[r!=0] <- 0
  ## misretrievals:
  m <- 1-c         
  m[f==1] <- 0
  ##
  if(print!=0) print(colMeans(l))
  if(print!=0) print(colMeans(c))
  if(print!=0) print(colMeans(m))
  if(print!=0) print(colMeans(f))
  ##
  ## return l=latencies, c=correct retrievals, m=misretrievals, f=failures, a=target-chunk activations
  return(list(l=l, c=c, m=m, f=f, a=ta))
}



## RUN MODEL
reset_params()
#
lf <<- 1
rth <<- -1.5
lp <<- 1
ldp <<- 1
w <<- 1
cueweights <<- c(1,1)
#
result <- interference_model_simple(1000)
#
## By condition: l=latencies, c=correct retrievals, m=misretrievals, f=failures, a=target-chunk activations:
lapply(result, colMeans)
#
## Interference effect:
mean(result$l[,1]-result$l[,2])


## PLOTS
l <- colMeans(result$l)
names(l) <- c("Interference", "No interference")
barplot(l, ylab="Retrieval latency (ms)")

m <- colMeans(result$m)
names(m) <- c("Interference", "No interference")
barplot(m, ylab="Prob. of misretrievals", ylim=c(0,1))

f <- colMeans(result$f)
names(f) <- c("Interference", "No interference")
barplot(f, ylab="Retrieval failure prob.", ylim=c(0,1))

