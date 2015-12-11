# Initial parameters
t_0 = 0.0011
a = 1.1
b = 0.015
  
# Act-R's noise function
actr.noise <- function(s,n=1) {
  rand <- runif(n,min=0.0001,max=0.9999)
  s * log((1 - rand ) / rand)
}

# Convert ticks to time and return time
ticksToTime = function(ticks) {
  # Define the starting values, the first pulse length is also subject to noise
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
  # Define the starting values, the first pulse length is also subject to noise
  ticks = 0
  pulseLength = t_0 + actr.noise(b*a*t_0)
  measuredTime = 0
  # Until the targetTime is reached continue to count ticks
  while(measuredTime < time) {
    pulseLength = a * pulseLength + actr.noise(b*a*pulseLength)
    measuredTime = measuredTime + pulseLength
    ticks = ticks + 1
  }
  # Return ticks - 1 to prevent overshooting
  ticks - 1
}

# Run the conversion 100 times for every target time and plot the normal distributions of the results
x = c()
plot.new()
for(targetTime in seq(6, 18, by=6)) {
  print(targetTime)
  for(i in 1:100){
    measuredTicks = timeToTicks(targetTime)
    x[i] = ticksToTime(measuredTicks)
  }
  x = sort(x)
  plot(x, dnorm(x,mean(x),sd(x)), xlab="Measured time (s)", ylab="Cumulative Probability",type = "l",lwd=2, cex=2, main="Measured time density functions", cex.axis=.8)
  grid(col="blue")
}
