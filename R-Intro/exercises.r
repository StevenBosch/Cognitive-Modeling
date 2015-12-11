##
## Exercises for An Introduction to R
##
## Hedderik van Rijn, Cognitive Modeling Course 05/06
##
## Hand in the answers to Step 10, both figure and text.
## 

## ---------------------------------------------------------------------------
##
## Step 1. Browse through Appendix A, see R-Intro-AppA.r
##
## This should have given you a feel of what is possible with R.
##

## ---------------------------------------------------------------------------
##
## Step 2. Read Chapter 2, Simple Manipulations; Numbers and Vectors

## E.2.1. Before continuing, make sure that you can work out what the
##        following statement does. (I know, the effect of the statement
##        itself kind of silly.)

## Note: %% is the modulo operator

x <- 1:40; x[sum((seq(1,10,by=.5)%%1)==0)] <- -(sum(1:40)-(2+2^3)); sum(x)

## ---------------------------------------------------------------------------
##
## Step 3: Skip Chapter 3, but do note that it is there and what it is 
##         about, so that you know where to return to if you have a
##         question about its topic.

## ---------------------------------------------------------------------------
##
## Step 4: Read Chapter 4.

## To save you some typing, copy and paste the following lines:

state <- c("tas", "sa", "qld", "nsw", "nsw", "nt", "wa", "wa", "qld", "vic",
"nsw", "vic", "qld", "qld", "sa", "tas", "sa", "nt", "wa", "vic", "qld",
"nsw", "nsw", "wa", "sa", "act", "nsw", "vic", "vic", "act")

incomes <- c(60, 49, 40, 61, 64, 60, 59, 54, 62, 69, 70, 42, 56, 61, 61, 61,
58, 51, 48, 65, 49, 49, 41, 48, 52, 46, 59, 46, 58, 43)

## E.4.1 Calculate the income standard deviation per state. My answers are:

act        nsw         nt        qld         sa        tas        vic 
 2.1213203 10.5577775  6.3639610  9.1815031  5.4772256  0.7071068 11.7260394 
        wa 
 5.3150729 

## If you feel particulary ambitious today, you can also try to come up
## with the standard errors, see the paragraph starting with "As
## an exercise...".

## ---------------------------------------------------------------------------
##
## Step 5: Read Chapter 5, paragraphs 1, 2, 3, 8, 9, 10

## After reading 5.1, 5.2, 5.3:

## E.5.1 Read the manual pages for runif and for trunc

## E.5.2 Make sure that you understand that this line implements ten
##       rolls of an almost honest die. 

trunc(runif(10,min=0,max=5.999))+1

## E.5.3 Create a matrix called simDice in which you store the results
##       of ten replications (10 rows) of a throw of six dice (6 columns). 

> simDice

      [,1] [,2] [,3] [,4] [,5] [,6]
 [1,]    3    5    3    1    1    2
 [2,]    5    5    1    4    2    6
 [3,]    3    1    3    1    1    5
 [4,]    1    1    5    6    3    3
 [5,]    4    2    5    6    3    4
 [6,]    1    3    3    5    5    5
 [7,]    2    2    1    1    4    5
 [8,]    6    4    5    5    1    6
 [9,]    5    6    5    2    4    6
[10,]    3    2    1    1    1    3

## After reading the other paragraphs:

## E.5.4 Add a column to the simDice matrix with the mean number of dots.
##
##  Step 1: Calculate the mean per row (hint: use apply)
##  Step 2: "Bind" these values to the matrix (hint: use cbind)
##

## ---------------------------------------------------------------------------
##
## Step 6. Read Chapter 6

## E.6.1 Given the following ACT-R chunk:
##
## Amsterdam isa cityfact
## slot1 "Amsterdam"
## slot2 "capital"
## slot3 "Netherlands"
##
## One can create a R-representation of this chunk using a list-structure:
##

Amsterdam <- list(isa="cityfact",slot1="Amsterdam",slot2="capital",slot3="Netherlands")

## Create a similar chunk for Groningen.

## E.6.2 Create a data structure that resembles the declarative memory, so that typing
##       DM$Amsterdam$slot1 returns "Amsterdam" and typing DM$Groningen returns the information
##       for Groningen. 

## E.6.3 Add a component reflecting the prior usage for all chunks in DM.
## Make sure that this component reflects (at least) 10 prior uses of this
## chunk, ordered in time. (Hint: use runif, sort)

## E.6.4 Study the data.frame below. Why can't we use a single data.frame to represent all
##       information available in declarative memory? 

DM.as.df <- data.frame(name=c("Amsterdam","Groningen"),
                       number=1:2,
                       slot1=c("Amsterdam","Groningen"),
                       slot2=c("capital","er_gaat_niets_boven"),
                       slot3=c("Netherlands","Netherlands"),
                       Ai=sort(rnorm(2,0,10)))

## ---------------------------------------------------------------------------
##
## Step 7. Read Chapter 7.1, 7.3

## E.7.1 Create a small data file and read it in using read.table. Make sure that
##       (1) one line contains the sting:
##
##                           # is called hekje in Dutch
##
##       (2) the last line indicates the current time, without having this time
##       read in by R, and
##       (3) that you don't specify row names in the data file.

## ---------------------------------------------------------------------------
##
## Step 8. Glance over Chapter 8, and realize how useful it would have been to
##         had access to R during "statistiek". :-)
##
##         Read 8.1

## No exercises.

## ---------------------------------------------------------------------------
##
## Step 9. Read 9.
##

## E.9.1 Evaluate the following code, and make sure you understand what is
##       going on. 

x <- rnorm(100,1000,100)
ind <- trunc(runif(100,0,4))*5
y <- x + ind*rnorm(100,0,10)


xc <- split(x,ind)
yc <- split(y,ind)

## Why are x & y splitted into 4 groups?

par(ask=T)
for(i in 1:length(yc)) {
  plot(xc[[i]], yc[[i]], main=paste("Added some noise *",i-1))
  abline(lsfit(xc[[i]],yc[[i]]))
}

## ---------------------------------------------------------------------------
##
## Step 10. Read Chapter 10, paragraph 1:5.
##
## Optional: read paragraph 7:10

## E.10.1 Create a function called actr.B that given a vector of times of prior
##        usage, the current time (all in seconds) and an optional value for d
##        returns the baselevel activation of a chunk. If d is not specified, the
##        function should use the default, .5. See included PDF file for formulas. 

> actr.B(c(1,25,35,100),110)
[1] -0.4526421

> actr.B(c(1,25,35,100),110,.4)
[1] -0.1073861

## E.10.2 Create a function (actr.B.optN) that calculates the base level
##        activation based the optimized formula.

> actr.B.optN(4,110)
[1] -0.2707986

## E.10.3 Create a function (actr.B.opt) that calculates the base level
##        activation using the optimized equation but that takes the same
##        arguments as the function actr.B.

> actr.B.opt(c(1,25,35,100),110)
[1] -0.2707986

## E.10.3 Create a plot where the x-axis denotes the number of seconds (0:200),
##        and the y-axis denotes the activation of a chunk (-1.5,1.5). Plot the
##        activation over time for a chunk that is used at 20 randomly distributed
##        times during these 200 seconds. (See plot on the website)

## Use the following lines for plotting:

plot(x=0,y=0,ylab="Act",xlab="Sec",type="n",xlim=c(0,200),ylim=c(-1.5,1.5))
lines(...,lty=1) ## for normal version

## Hint: use the following code:
references <- sort(trunc(runif(20,0,200)))
actr.B.plot <- function(time) {
  ## Can you explain the rationale behind the next line?
  actr.B(references[references<time],time)
}

## E.10.4 Add the activation estimation using the optimized baselevel equation. Note
##        that the optimized version predicts for each "t" an "n" of (N/T)*t given 
##        N references to a chunk in T seconds.

lines(...,lty=2) ## for optimized version

## E.10.5 Given a set of references as shown below, explain why the
##        optimized version does not work well in this situation. 

references <- c(sort(runif(10,1,180)),180+(1:10)*2)
