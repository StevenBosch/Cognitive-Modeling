source("DM-module.R", TRUE)
source("time.R", TRUE)

# Experiment setup parameters
nrSubjects = 10   # The 'number of subjects'
trainingTrials = 500 # The number of learning trials
trials = 1000 # The number of test trials

# Experiment durations, for calculating the activation of the chunks
FixPointDuration = 1000
delayLowerBound = 250
delayUpperBound = 850

# Create the prior distributions
priorDist1 = seq(from = 494, to = 847, by = (847-494)/10)
priorDist2 = seq(from = 671, to = 1023, by = (1023-671)/10)
priorDist3 = seq(from = 847, to = 1200, by = (1200-847)/10)
priorDists = list(priorDist1, priorDist2, priorDist3)

# 
subjectsPriors = array(NA, dim = c(nrSubjects, length(priorDists), 2)) # per prior distribution a stored mean and sd
subjectsTrainData = array(NA, dim = c(nrSubjects, length(priorDists), trainingTrials))
subjectsTestData = array(NA, dim = c(nrSubjects, length(priorDists), trials))

readySetGo = function() {
  for(subject in 1:nrSubjects) {
    # Create the declarative memory of the current subject
    num.chunks <- 3 # One chunk for every prior condition (short, medium or long duration)
    max.num.encounters <- 500
    DM = create.dm(num.chunks,max.num.encounters)
    
    for(priorDist in 1:length(priorDists)) {
      curtime = 0 # Keep track of the time during the experiment, to account for activation decrease
      for(lTrial in 1:trainingTrials) {
        sample = sample(1:11, 1) # Randomly choose a sample from the distribution
        t_s = priorDists[[priorDist]][sample] # The current sample time
        t_m = ticksToTime(timeToTicks(sample))
        delay = sample(250:850, 1)
        curtime = curtime + delay + FixPointDuration + t_m
        
        # Add the time of the sample to the chunk of the current condition (measured with timeToTicks and ticksToTime)
        DM = add.encounter(DM, priorDist, curtime)
        
        # Store t_m
        estimatedTimes[lTrial] = t_m
      }
      # Calculate activation
      
      
      curtime = 0
      for(trial in 1:trials){
        sample = sample(1:11, 1) # Randomly choose a sample from the distribution
        t_s = priorDists[[priorDist]][sample] # The current sample time
        
        # Measure the interval in ticks
        timeToTicks(sample)
        
        # Using the previously defined prior determine the estimated time and produced time
      }
    }
  }
}

readySetGo()

# First stage: determine measured time from the sample time:
# Gaussian: t_measured -> p(t_measured|t_sample) (sd grows linearly with mean)

# Second stage: determine estimated time from the measured time:
# t_estimated = f(t_measured)

# Third stage: produce interval from the estimated time:
# Gaussian: t_produced -> p(t_produced|t_estimated) (sd grows linearly with mean)
