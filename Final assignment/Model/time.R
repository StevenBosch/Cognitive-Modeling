# Time measurement parameters
t_0 = 11            # The starting length of a pulse in ms
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