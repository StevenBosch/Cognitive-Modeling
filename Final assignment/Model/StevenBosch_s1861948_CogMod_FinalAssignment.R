### Final assignment CogMod: modelling Los et. al's temporal preparation experiment. ###

# To implement:
# Memory system like assignment 4 (previous encounters have influence on preparation)
# Something with expectance of the interval given the memory, plus higher expectance over time.
# So a subject increases its expectance over time during an interval, making him more prepared for the signal every ms.
# But if he has many long intervals in memory, he will be even worse for short intervals now and vice versa. 

# ________________________________________________________________________________________________________________________________#


# Experiment setup parameters
nrGroups = 3     # The number of groups
nrSubjects = 20   # The number of 'subjects' per group
nrBlocks = 8     # Number of blocks
nrTrials = 120   # Number of trials per block

# Foreperiod distributions
foreperiods = c(400, 800, 1200, 1600)
uniform = c(30, 30, 30, 30)
exp = c(64, 32, 16, 8)
antiExp = c(8, 16, 32, 64)
distributions = array(NA, c(nrGroups, nrBlocks))
distributions[1,] = c("uni", "exp,", "exp", "uni", "anti-exp", "uni", "anti_exp", "uni")
distributions[2,] = c("uni", "anti-exp", "anti-exp", "uni", "exp", "uni", "exp", "uni")
distributions[3,] = c("uni", "uni", "uni", "uni", "uni", "uni", "uni", "uni")

# Data storage
subjectsData = data.frame(group = integer(nrGroups*nrSubjects*nrBlocks*nrTrials),
                          sub = integer(nrGroups*nrSubjects*nrBlocks*nrTrials),
                          block = integer(nrGroups*nrSubjects*nrBlocks*nrTrials),
                          forePeriod = integer(nrGroups*nrSubjects*nrBlocks*nrTrials),
                          reactionTime =double(nrGroups*nrSubjects*nrBlocks*nrTrials))

# The Los et al. experiment
for(group in 1:nrGroups){
  for(subject in 1:nrSubjects){
    for(block in nrBlocks){
      distribution = sample.int(nrTrials, nrTrials)
      switch(distributions[group, block],
        "uni"= {
          lim400 = nrTrials/length(foreperiods)
          lim800 = lim400*2
          lim1200 = lim400*3
        },
        "exp"= {
          lim400 = 64
          lim800 = 64 + 32
          lim1200 = 64 + 32 + 16
        },
        "anti-exp"= {
          lim400 = 8
          lim800 = 8 + 16
          lim1200 = 8 + 16 + 32
        }
      )
      distribution = replace(distribution, distribution<lim400, 400)
      distribution = replace(distribution, (distribution>lim400) & (distribution<lim800), 800)
      distribution = replace(distribution, (distribution>lim800) & (distribution<lim1200), 1200)
      distribution = replace(distribution, (distribution>lim400) & (distribution<nrTrials), 1600)
      
      
    }
  }
}