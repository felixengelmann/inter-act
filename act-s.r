

##
## Global parameters
##

reset_params <- function(){
  lp        <<- 1       # default time since last presentation (sec)
  blc       <<- 0       # base-level activation
  bll       <<- 0.5     # decay parameter
  lf        <<- 1       # latency factor
  le        <<- 1       # latency exponent
  rth       <<- -1.5       # retrieval threshold
  rand_time <<- 3       # latency variability
  ans       <<- 0.1     # activation noise
  w         <<- 1       # source activation
  mas       <<- 1       # maximum associative strength 
  mp        <<- 1       # mismatch penalty
  ##
  ## Distractor control:
  ldp       <<- 1       # last distractor presentation (sec)
  dbl       <<- 0       # distractor base-level
  ##
  ## Cue weighting, cue confusion, and activation-sensitivity:
  cueweights <<- c(1,1) # vector of cue weightings
  cuesim     <<- -1      # cue confusion 
  asf        <<- 0       # activation-sensitive-fan factor
}
reset_params()



##
## ACT-R equations
##

## Activation equation ##
activation <- function(fan=1, weights=NULL, match=0, times=lp, bl=blc){
  if(is.null(weights)) Wkj <- w/length(fan) else Wkj <- w*weights/sum(weights)
  base_act <- log(sum(times^(-bll))) + bl
  Sji <- mas-log(fan)
  Sji[Sji==Inf | is.na(Sji)] <- 0
  Pi <- sum(mp*match)
  act <- base_act + sum(Wkj*Sji) + Pi + act_r_noise(ans)
  return(act)
}

## Determine retrieved chunk ##
retrieve <- function(a, tau=rth){
  retrieved <- ifelse(max(a)>tau, which(a==max(a), a), 0)
  return(retrieved)
}

## Retrieval latency ##
latency <- function(A, F=lf, f=le, tau=rth){
  t <- ifelse(A>=tau, F*exp(-f*A)*1000, F*exp(-f*tau)*1000)
  round(sapply(t, randomize_time))
}

## Noise ##
act_r_noise <- function(s=ans){
  var <- pi^2/3*s^2
  rnorm(1, 0, sqrt(var))
}

## Random component ##
randomize_time <- function(time, n=rand_time){
  ifelse(n>0, runif(1, time*(n-1)/n, time*(n+1)/n), time)
}

## Activation-sensitive fan ##
fan_strength <- function(a1, a2, f=asf){
  adiff <- a1-a2
  ifelse(adiff > 0, 1/(1+adiff*f), 1-(adiff*f))
}

noise_off <- function(){
  ans <<- 0
  rand_time <<- 0
}

reset_params()
