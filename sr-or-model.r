
source("act-s.r")

## Model:
sr_or_model <- function(iterations=1000, print=0){
  latencies <- NULL         ## collects latencies
  correct <- NULL         	## collects accuracies
  for(i in 1:iterations){
    # a <- matrix(rep(NA,8),nrow=4)   ## Chunk activations
    l <- NULL
    c <- NULL
    ##
    ## SRC Embedded verb:
    # Subj-filler
    a <- NULL
    a[1] <- activation(fan=c(1,1), match=c(0,0), times=1)  			# +DP +specCP
    a[2] <- activation(fan=c(NA,NA), match=c(-1,-1))
    r <- retrieve(a)
    c <- c(c,ifelse(r==1,1,0))
    l <- c(l,latency(max(a)))
    ##
    ## ORC Embedded verb:
    # Subj
    a <- NULL
    a[1] <- activation(fan=c(2,1), match=c(0,0), times=1)  			# +NP +specI-bar
    a[2] <- activation(fan=c(2,NA), match=c(0,-1), times=c(3,2))	# +NP
    r <- retrieve(a)
    c1 <- ifelse(r==1,1,0)
    l1 <- latency(max(a))
    # Obj-filler
    a[1] <- activation(fan=c(1,1), match=c(0,0), times=2)  			# +DP +specCP
    a[2] <- activation(fan=c(NA,NA), match=c(-1,-1))
    r <- retrieve(a)
    c2 <- ifelse(r==1,1,0)
    l2 <- latency(max(a))
    #
    c <- c(c, (c1+c2)/2)
    l <- c(l, l1+l2)
    ##
    ## SRC Main verb:
    # Subj
    a <- NULL
    a[1] <- activation(fan=c(3,1), match=c(0,0), times=c(5,4))     	# +NP +specIP
    a[2] <- activation(fan=c(3,NA), match=c(0,-1), times=c(2,1))	# +NP
    r <- retrieve(a)
    c <- c(c,ifelse(r==1,1,0))
    l <- c(l,latency(max(a)))
    ##
    ## ORC Main verb:
    # Subj
    a <- NULL
    a[1] <- activation(fan=c(3,1), match=c(0,0), times=c(5,4))     			# +NP +specIP
    a[2] <- activation(fan=c(3,NA), match=c(0,-1), times=c(3,2,1))
    r <- retrieve(a)
    c <- c(c,ifelse(r==1,1,0))
    l <- c(l,latency(max(a)))
    ##
    ##
    latencies <- rbind(latencies, l)
    correct <- rbind(correct, c)
  }
  if(print!=0) print(colMeans(latencies))
  if(print!=0) print(colMeans(correct))
  return(list(l=latencies,c=correct))
}



## RUN MODEL
reset_params()
#
lf        <<- .14       # latency factor
ans       <<- 0.15      # activation noise
w         <<- 1         # source activation
mas       <<- 1.5       # maximum associative strength 
mp        <<- 1.5       # mismatch penalty
cueweights <<- c(1,1)
#
result <- sr_or_model(1000)
#
## By condition: l=latencies, c=correct retrievals:
lapply(result, colMeans)
#
## Effects of ORC on emb V and main V:
c(mean(result$l[,2]-result$l[,1]), mean(result$l[,4]-result$l[,3]))

l <- colMeans(result$l)
names(l) <- c("embV-SRC", "embV-ORC", "mainV-SRC", "mainV-ORC")
barplot(l, ylab="Retrieval latency (ms)")

c <- colMeans(result$c)
names(c) <- c("embV-SRC", "embV-ORC", "mainV-SRC", "mainV-ORC")
barplot(c, ylab="Correct retrieval prob.", ylim=c(0,1))
