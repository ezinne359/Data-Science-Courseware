install.packages('knitr')

data(InsectSprays)
dim(InsectSprays)

table(InsectSprays[,"spray"])
pie(table(InsectSprays[,"spray"]))

summary(InsectSprays[,"count"])
hist(InsectSprays[,"count"], xlab="Insect Counts", main="")

plot(count~spray, data=InsectSprays,
     xlab="Types of Insecticide",
     ylab="Counts of Insects")

anova(lm(count~spray, data=InsectSprays))
