# Initial parameters
nrSubjects = 1000   # The 'number of subjects' the bisection model tests on
t_0 = 0.011        # The starting length of a pulse
a = 1.1             # The growth factor of the pulses
b = 0.015           # Noise influence parameter
  
# Act-R's noise function
actr.noise <- function(s,n=1) {
  rand <- runif(n,min=0.0001,max=0.9999)
  s * log((1 - rand ) / rand)
}

# Convert ticks to time and return time (not used in the bisection model but added for completeness sake)
ticksToTime = function(ticks) {
  # The starting values, the first pulse length is also subject to noise here
  pulseLength = t_0 + actr.noise(b*a*t_0)
  measuredTime = 0
  # Add the time from every tick and return the total measured time
  for(tick in 1:ticks) {
    pulseLength = a * pulseLength + actr.noise(b*a*pulseLength)
    measuredTime = measuredTime + pulseLength
  }
  measuredTime
}

# Convert time to ticks and return ticks
timeToTicks = function(time) {
  # The starting values, the first pulse length is also subject to noise here
  ticks = 0
  pulseLength = t_0 + actr.noise(b*a*t_0)
  measuredTime = 0
  # Until the targetTime is reached, continue to count ticks and return the final count
  while(measuredTime < time) {
    pulseLength = a * pulseLength + actr.noise(b*a*pulseLength)
    measuredTime = measuredTime + pulseLength
    ticks = ticks + 1
  }
  ticks
}

# This function runs the bisection experiment for a specified interval (min, max) and stepsize
bisection = function(min, max, stepsize) {
  # Initialize the long proportion vector and index variable
  propLong = seq(min, max, stepsize)
  i = 1
  # For every step between the interval, let every 'subject' return their estimation of the interval.
  # If the 'subject' estimates long, increase the amount of counted longs
  for(time in seq(min, max, stepsize)) {
    long = 0
    for(subject in 1:nrSubjects) {
      if(timeToTicks(time) >= (timeToTicks(min)+timeToTicks(max))/2) {
        long = long + 1
      }
    }
    # Calculate the proportion of reported longs over all subjects and increase the index
    propLong[i] = long/nrSubjects
    i = i + 1
  }
  return(propLong)
}

# Plot the experiment data and model data for the interval 3-6
# The experiment data
expTime = c(3, 3.37, 3.78, 4.24, 4.76, 5.34,6)
expPropLong = c(0.08, 0.1, 0.2, 0.45, 0.74, 0.86, 0.95)
# The model data
modelPropLong = bisection(3,6,0.5)
# Plots
plot(seq(3,6,0.5), modelPropLong, xlab="time", ylab="Proportion long", type = "o",lwd=2, main="3-6 sec discrimination", col="red", xlim=c(3,6), ylim=c(0,1))
lines(expTime, expPropLong, type = "o",lwd=2, col="blue")
legend("bottomright", c("Model", "Experiment"), lty=c(1,1), col=c("red", "blue"))

# Plot the experiment data and model data for the interval 2-8
# The experiment data
expTime = c(2, 2.52, 3.18, 4, 5.04, 6.35, 8)            
expPropLong = c(0.02, 0, 0.12, 0.5, 0.84, 0.91, 1)     
# The model data
modelPropLong = bisection(2,8,1)
# Plots
plot(seq(2,8,1), modelPropLong, xlab="time", ylab="Proportion long", type = "o",lwd=2, main="2-8 sec discrimination", col="red", xlim=c(2,8), ylim=c(0,1))
lines(expTime, expPropLong, type = "o",lwd=2, col="blue")
legend("bottomright", c("Model", "Experiment"), lty=c(1,1), col=c("red", "blue"))

# Plot the experiment data and model data for the interval 4-12
# The experiment data
expTime = c(4, 4.8, 5.77, 6.93, 8.32, 9.99, 12)
expPropLong = c(0, 0.07, 0.22, 0.46, 0.69, 0.86, 0.92)
# The model data
modelPropLong = bisection(4,12,1)
# Plots
plot(seq(4,12,1), modelPropLong, xlab="time", ylab="Proportion long", type = "o",lwd=2, main="4-12 sec discrimination", col="red", xlim=c(4,12), ylim=c(0,1))
lines(expTime, expPropLong, type = "o",lwd=2, col="blue")
legend("bottomright", c("Model", "Experiment"), lty=c(1,1), col=c("red", "blue"))