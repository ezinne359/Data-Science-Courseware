data(state.x77)
statedata=as.data.frame(state.x77)

##Modifying colomn names
colnames(statedata)=c("popu", "inc", "illit", "life.exp", "murder", "hs.grad", "frost", "area")

##Plot of life expectancy v income 
##There seems to be a positive correlation between the two
plot(life.exp~inc, data=statedata)

##Correlation
cor(statedata[,"life.exp"], statedata[,"inc"])

plot(life.exp~inc, data=statedata, type="n")
text(life.exp~inc, data=statedata, state.abb)

##Simple Linear Regression 
model1=lm(life.exp~inc, data=statedata)
model1

plot(life.exp~inc, data=statedata,
     xlab="Income", ylab="Life Expectancy")
abline(model1)

par(mfrow=c(2,2)) # create a panel of four plotting areas

for(i in 1:4){
  ## Plot the population
  plot(life.exp~inc, data=statedata,
       xlab="Income", ylab="Life Expectancy",
       main=paste("Random sample", format(i)),
       ylim=c(min(life.exp), max(life.exp)+0.3))
  abline(model1)
  if(i==1){
    legend(3030, 74.2, 
           pch=c(NA, NA, NA, 1, 16), 
           lty=c(1, 1, 2, NA, NA),
           col=c(1, 2, 2, 1, 2),
           c("population truth", "sample estimate",
             "sample confidence band", 
             "states", "sampled"),
           cex=0.7,
           bty="n"
    )
  }
  ## Select the sample
  selected.states=sample(1:50, 10)
  points(statedata[selected.states,"inc"], 
         statedata[selected.states,"life.exp"], pch=16, col=2)
  ## Fit a regression line using the sample
  model.sel = lm(life.exp~inc, data=statedata[selected.states,])
  abline(model.sel, col=2)
  ## Make a confidence band. 
  #### first calculate the width of the band, W.
  ww=sqrt(2*qf(0.95, 2, nrow(statedata)-2))
  #### generate plotting X values. 
  plot.x<-data.frame(inc=seq(3000, 7000, 1))
  #### se.fit=T is an option to save 
  #### the standard error of the fitted values. 
  plot.fit<-predict(model.sel, plot.x, 
                    level=0.95, interval="confidence", 
                    se.fit=T)
  
  #### lines is a function to add connected lines 
  #### to an existing plot.
  lines(plot.x$inc, plot.fit$fit[,1]+ww*plot.fit$se.fit, 
        col=2, lty=2)
  lines(plot.x$inc, plot.fit$fit[,1]-ww*plot.fit$se.fit, 
        col=2, lty=2)
}
