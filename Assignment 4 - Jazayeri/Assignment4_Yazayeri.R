source("DM-module.R", TRUE)
source("time.R", TRUE)

# Experiment setup parameters
nrSubjects = 1   # The 'number of subjects'
trials = 500 # The total number of trials, it depends on which trials you plot, how many of these you consider as `training' trials

# Experiment durations, for calculating the activation of the chunks
FixPointDuration = 1000
delayLowerBound = 250
delayUpperBound = 850

# Create the prior distributions
priorDist1 = seq(from = 494, to = 847, by = (847-494)/10)
priorDist2 = seq(from = 671, to = 1023, by = (1023-671)/10)
priorDist3 = seq(from = 847, to = 1200, by = (1200-847)/10)
priorDists = list(priorDist1, priorDist2, priorDist3)

# Data storage
subjectsPriors = array(NA, dim = c(nrSubjects, length(priorDists), 2)) # per prior distribution a stored mean and sd
subjectsData = array(NA, dim = c(nrSubjects, length(priorDists), trials))

readySetGo = function(nrSubjects, trials) {
  for(subject in 1:nrSubjects) {
    # Create the declarative memory of the current subject
    num.chunks = 1
    max.num.encounters = 2000
    DM = create.dm(num.chunks,max.num.encounters)
    
    # Temporary storage stuff
    measuredTimes = array(NA, dim=c(trials))
    activation = array(NA, dim=c(trials))
    priors = array(NA, dim=c(trials))
    
    # For every condition (short, medium, long)
    for(priorDist in 1:length(priorDists)) {
      curtime = 0 # Keep track of the time during the experiment, to account for activation decrease
      
      # Perform the trials
      for(trial in 1:trials) {
        # The current sample time
        t_s = priorDists[[priorDist]][sample(1:11, 1)]
        
        # Measure the sample time
        t_m = ticksToTime(timeToTicks(t_s)) 
        
        # Determine the current time with the values used in the paper for delay time and fixation point duration
        delay = sample(250:850, 1)
        curtime = curtime + delay + FixPointDuration + t_m # The current time, given that the first trial was at time == 0 ms
        
        # Add the time the trial takes place as an encounter
        DM = add.encounter(DM, 1, curtime)
        
        # Store the measured duration time on the same index as the encounter is added to the DM to link the two
        measuredTimes[trial] = t_m
        
        # Calculate activation values for all encounters so far using the current time and the time of the encounter
        for(encounter in 1:sum(!is.na(measuredTimes))) {
          activation[encounter] = actr.B(DM[encounter], curtime)
        }
        
        # Calculate the priors with the activations
        activationSum = sum(activation)
        for(encounter in 1:sum(!is.na(measuredTimes))) {
          priors[encounter] = activation[encounter] / activationSum
        }
        
        # Determine the estimated time by multiplying the priors of all encounters with their measured duration
        subjectsData[subject][priorDist][trial] = sum(na.omit(priors)*na.omit(measuredTimes))
      }
    }
  }
}

readySetGo(nrSubjects, trials)

# First stage: determine measured time from the sample time:
# Gaussian: t_measured -> p(t_measured|t_sample) (sd grows linearly with mean)

# Second stage: determine estimated time from the measured time:
# t_estimated = f(t_measured)

# Third stage: produce interval from the estimated time:
# Gaussian: t_produced -> p(t_produced|t_estimated) (sd grows linearly with mean)
