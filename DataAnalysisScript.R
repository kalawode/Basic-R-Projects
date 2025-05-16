tp <- read.table("tad.csv", header=TRUE, sep= ',')

head(tp)
summary(tp)
str(tp)

boxplot(log(time+1)~experiment, data = tp)
stripchart(log(time+1)~experiment, data = tp, method = "jitter", vertical = TRUE, pch = 21, col = "maroon", bg = "bisque", add = TRUE)

log(tp$time)

#Analyze the data using mixed effects model:
library(lme4)
library(lmerTest)

tp.lmer <- lmer(log(time+1)~experiment + (1|tadpole), data = tp)
plot(tp.lmer)
hist(residuals(tp.lmer))

summary(tp.lmer)

library(emmeans)
#Calculate mean values per treatment:
tp.emmeans <- emmeans(tp.lmer, "experiment")
tp.emmeans
#Perform Tukey test to compare means among treatments:
pairs(tp.emmeans)
# NMDA > Control
#Control == Spinalized
#NMDA >  spinalized

########################################

#METHOD 2:  Summarize the data and use 1-way ANOVA:
library(doBy)

#Summarizing the data:
summary.raw <- summaryBy(time ~ experiment + tadpole, data = tp, FUN = c(mean))

summary.raw

#Running our model:
tp.summary.raw <- lm(time.mean ~ experiment, data = summary.raw)
#Checking assumptions
plot(tp.summary.raw)
#Violates assumptions
anova(tp.summary.raw)
#Suggests an effect

library(emmeans)
#Calculate treatment means:
tp.summary.raw.emmeans <- emmeans(tp.summary.raw, "experiment")
#Conduct Tukey test:
pairs(tp.summary.raw.emmeans)
# NMDA > Control
#Control == Spinalized
#NMDA >  spinalized

#Repeating the analysis using log-tramsformed means:
tp.summary.raw.log <- lm(log(time.mean) ~ experiment, data = summary.raw)
plot(tp.summary.raw.log)
#Also violates assumption of equal variance
anova(tp.summary.raw.log)
#Suggests no overall effect!

#Now, try taking the log before finding the mean:
summary.log <- summaryBy(log(time+1) ~ experiment + tadpole, data = tp, FUN = c(mean))
summary.log
names(summary.log) <- c("experiment", "tadpole","mean.of.log.time")
tp.summary.log.early <- lm(mean.of.log.time ~ experiment, data = summary.log)
plot(tp.summary.log.early)
#Still violates assumptions
anova(tp.summary.log.early)
#Suggests weak evidence for an effect (p = 0.0703)
