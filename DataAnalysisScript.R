tp <- read.table("tad.csv", header=TRUE, sep= ',')

head(tp)
summary(tp)
str(tp)

#Summarise the data and use 1-way ANOVA:
library(doBy)

#Summarising the data:
summary.raw <- summaryBy(time ~ experiment + tadpole, data = tp, FUN = c(mean))

summary.raw

#Running model:
tp.summary.raw <- lm(time.mean ~ experiment, data = summary.raw)
#Checking assumptions
plot(tp.summary.raw)
#Violates assumptions
anova(tp.summary.raw)
#Suggests an effect

library(emmeans)
#Calculating treatment means:
tp.summary.raw.emmeans <- emmeans(tp.summary.raw, "experiment")
#Conduct Tukey test:
pairs(tp.summary.raw.emmeans)

#NMDA > Control
#Control == Spinalized
#NMDA >  spinalized

#Repeating the analysis using log-tramsformed means:
tp.summary.raw.log <- lm(log(time.mean) ~ experiment, data = summary.raw)
plot(tp.summary.raw.log)
anova(tp.summary.raw.log)


#Taking the log before finding the mean:
summary.log <- summaryBy(log(time+1) ~ experiment + tadpole, data = tp, FUN = c(mean))
summary.log
names(summary.log) <- c("experiment", "tadpole","mean.of.log.time")
tp.summary.log.early <- lm(mean.of.log.time ~ experiment, data = summary.log)
plot(tp.summary.log.early)
#Still violates assumptions
anova(tp.summary.log.early)
#Suggests weak evidence for an effect (p = 0.0703)
