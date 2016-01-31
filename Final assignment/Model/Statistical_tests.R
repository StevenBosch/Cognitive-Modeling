# Perform statistical tests
summary(aov(reactionTime[block==1] ~ forePeriod[block==1], data=data))
summary(aov(reactionTime[block==2] ~ forePeriod[block==2], data=data))
summary(aov(reactionTime[block==3] ~ forePeriod[block==3], data=data))
summary(aov(reactionTime[block==4] ~ forePeriod[block==4], data=data))
summary(aov(reactionTime[block==5] ~ forePeriod[block==5], data=data))

summary(aov(reactionTime[block==1] ~ group[block==1], data=data))
summary(aov(reactionTime[block==2] ~ group[block==2], data=data))
summary(aov(reactionTime[block==3] ~ group[block==3], data=data))
summary(aov(reactionTime[block==4] ~ group[block==4], data=data))
summary(aov(reactionTime[block==5] ~ group[block==5], data=data))

summary(aov(reactionTime[block==1] ~ group[block==1] * forePeriod[block==1], data=data))
summary(aov(reactionTime[block==2] ~ group[block==2] * forePeriod[block==2], data=data))
summary(aov(reactionTime[block==3] ~ group[block==3] * forePeriod[block==3], data=data))
summary(aov(reactionTime[block==4] ~ group[block==4] * forePeriod[block==4], data=data))
summary(aov(reactionTime[block==5] ~ group[block==5] * forePeriod[block==5], data=data))