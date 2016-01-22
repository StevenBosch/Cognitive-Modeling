### Final assignment CogMod: modelling Los et. al's temporal preparation experiment. ###

# To implement:
# Memory system like assignment 4 (previous encounters have influence on preparation)
# Something with expectance of the interval given the memory, plus higher expectance over time.
# So a subject increases its expectance over time during an interval, making him more prepared for the signal every ms.
# But if he has many long intervals in memory, he will be even worse for short intervals now and vice versa. 

# -------------------------------------------------------------------------------

source("DM-module.R", TRUE)
source("time.R", TRUE)

# Experiment setup parameters
nrGroups = 3     # The number of groups
nrSubjects = 3   # The number of 'subjects' per group
nrBlocks = 8     # Number of blocks
nrTrials = 120   # Number of trials per block

# Subject properties
basePenalty = 500

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
                          reactionTime = integer(nrGroups*nrSubjects*nrBlocks*nrTrials))

#--------------- The Los et al. experiment---------------#

for(group in 1:nrGroups){
  for(subject in 1:nrSubjects){
    
    # Create the declarative memory for this subject
    num.chunks = 35 # The max interval is 1600 ms, which amounts to 28-31 ticks, so 35 should be enough
    max.num.encounters = 8 * 64 # If all blocks were exponential, this is the max
    DM = create.dm(num.chunks,max.num.encounters)
    
    for(block in 1:nrBlocks){
      # Create the right disribution of trials for this block
      distribution = sample.int(nrTrials, nrTrials)
      switch(distributions[group, block],
        "uni"= {
          lim400 = nrTrials/length(foreperiods)
          lim800 = lim400*2
          lim1200 = lim400*3
        },
        "exp"= {
          lim400 = 64
          lim800 = 96
          lim1200 = 112
        },
        "anti-exp"= {
          lim400 = 8
          lim800 = 24
          lim1200 = 56
        }
      )
      distribution = replace(distribution, distribution<=lim400, 400)
      distribution = replace(distribution, (distribution>lim400) & (distribution<=lim800), 800)
      distribution = replace(distribution, (distribution>lim800) & (distribution<=lim1200), 1200)
      distribution = replace(distribution, (distribution>lim400) & (distribution<=nrTrials), 1600)
      
      for(trial in 1:nrTrials){
        ticks = timeToTicks(distribution[trial])
        reactionTime = basePenalty - exp(-10/((ticks-10)^2))* 200
        
        # Store the trial
        index = (group-1)*nrSubjects*nrBlocks*nrTrials + (subject-1)*nrBlocks*nrTrials + (block-1)*nrTrials + trial
        subjectsData$group[index] = group
        subjectsData$sub[index] = subject
        subjectsData$block[index] = block
        subjectsData$forePeriod[index] = distribution[trial]
        subjectsData$reactionTime[index] = reactionTime
      }
    }
  }
}

data = subjectsData

## Plot the data

brown <- "#8b4513";
red <- "#ff1100";
black <- "#000000";
brownT <- "#8b451322";
redT <- "#ff110022";
blackT <- "#00000022";

## ---

par(mfrow=c(2,4))

plotDat <- with(data,aggregate(list(reactionTime=reactionTime),list(forePeriod=forePeriod, group=group, block=block),mean))

yrange <- range(plotDat$reactionTime)*c(.95,1.05)
xrange <- range(300, 1700)

for(block in 1:nrBlocks){
  title = paste("Block ", block)
  plotDatTemp = plotDat[plotDat$block==block,]
  with(plotDatTemp[plotDatTemp$group==1,],plot(forePeriod,reactionTime,type="b",col=red,lwd=2,ylim=yrange,xlim=xrange,main=paste(title)))
  with(plotDatTemp[plotDatTemp$group==2,],lines(forePeriod,reactionTime,type="b",col=brown,lwd=2,ylim=yrange,xlim=xrange))
  with(plotDatTemp[plotDatTemp$group==3,],lines(forePeriod,reactionTime,type="b",col=black,lwd=2,ylim=yrange,xlim=xrange))
}