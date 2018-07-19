
source("interACT.R")
reset_params()


# Standard interference model with 4 conditions
# ---------------------------------------------
model_4cond <- list(
	target_match = list(c(1,1), c(1,1), c(1,0), c(1,0)),
	distractor_match = list(c(0,1), c(0,0), c(0,1), c(0,0)),
	Target = c("Match","Match","Mismatch","Mismatch"),
	Distractor = c("Match","Mismatch","Match","Mismatch"),
	weights = list(c(strWeight(),semWeight()))
	)


# Set parameters
# ---------------------------
lf <<- c(0.1,0.2)
mas <<- c(1,1.5)
mp <<- 0.5
# when psc > 0, prominence influences base-level activation
psc <<- 1 
# match quality correction factor
qcf <<- 1
# cue-feature similarity [-1..0]
cuesim <<- -1
# Strength of structural cue as ratio
cueweighting <<- 1     


# Run model
# ---------------------------
sims <- create_param_matrix(model_4cond, iterations=5000)
results <- run(sims)
(simMeans <- compute_int_means(results))
(condMeans <- compute_cond_means(results))

