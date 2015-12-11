## Commands from An Introduction to R, Appendix A

help.start() 

x <- rnorm(50) 
y <- rnorm(x) 


plot(x, y) 

## If no graphics window appeared, enter: x11(":0.0"), and then try the plot command again.


ls() 

rm(x, y) 

x <- 1:20 
w <- 1 + sqrt(x)/2 

dummy <- data.frame(x=x, y= x + rnorm(x)*w) 
dummy 

fm <- lm(y ~ x, data=dummy) 
summary(fm) 


fm1 <- lm(y ~ x, data=dummy, weight=1/w^2) 
summary(fm1) 

attach(dummy) 

lrf <- lowess(x, y) 

plot(x, y) 
lines(x, lrf$y) 
abline(0, 1, lty=3) 
abline(coef(fm)) 
abline(coef(fm1), col = "red") 

detach() 

plot(fitted(fm), resid(fm), xlab="Fitted values", ylab="Residuals", main="Residuals vs Fitted") 

qqnorm(resid(fm), main="Residuals Rankit Plot") 

rm(fm, fm1, lrf, x, dummy) 

file.show("morley.tab")
mm <- read.table("morley.tab")
mm

mm$Expt <- factor(mm$Expt)
mm$Run <- factor(mm$Run)

attach(mm)

plot(Expt, Speed, main="Speed of Light Data", xlab="Experiment No.") 

fm <- aov(Speed ~ Run + Expt, data=mm)
summary(fm)


fm0 <- update(fm, . ~ . - Run)
anova(fm0, fm)
detach()
rm(fm, fm0)

x <- seq(-pi, pi, len=50)
y <- x

f <- outer(x, y, function(x, y) cos(y)/(1 + x^2))

oldpar <- par(no.readonly = TRUE)
par(pty="s")

contour(x, y, f)
contour(x, y, f, nlevels=15, add=TRUE)

fa <- (f-t(f))/2

contour(x, y, fa, nlevels=15)
par(oldpar)

image(x, y, f)
image(x, y, fa)

objects(); rm(x, y, f, fa) 


q()
