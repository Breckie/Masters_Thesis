library(ggplot2)
library(tidyr)
library(dplyr)
library(car)
install.packages("car",dependencies=TRUE)
install.packages(pbkrtest)

#car wouldn't load because dependency package pbkrtest wouldn't run on my version of R
#SO recomended this:
install.packages("lme4")
packageurl <- "https://cran.r-project.org/src/contrib/Archive/pbkrtest/pbkrtest_0.4-4.tar.gz"
install.packages(packageurl, repos=NULL, type="source")
#And it finally worked!

#DATA
natural <- read.csv('Desktop/benthics_2016_natural.csv', stringsAsFactors = FALSE)
natural <- mutate(natural, total = mac + other)
natural$Date <- factor(natural$Date, levels = c('July', 'August', 'September', 'January'))
______________________________________________________
#Jarrett's example
#simulation for multiple linear regression
my_data <- data.frame(x1 = runif(100,0,10),
                      x2 = runif(100,0,10))
my_data$y <- rnorm(100, my_data$x1 + my_data$x2, 5)


mod <- lm(y ~ x1 + x2, data=my_data)
Anova(mod)
summary(mod)


#view partial correlations - you
#can add all sorts of things to the axes
crPlots(mod, smoother=FALSE)
__________________________________________________________
?lm
  
MODEL1 <- lm(formula = mac ~ Date + total, data = natural)
Anova(MODEL1)
summary(MODEL1)

head(natural)
str(natural)

#THIS MAY HAVE WORKED BUT i DON'T KNOW WHAT IT MEANS
_________________________________________________________
MODEL2 <- lm(formula = mac ~ Date + mac, data = natural)
Anova(MODEL2)
summary(MODEL2)

_________________________
MODEL3 <- lm(formula = mac ~ mac + total, data = natural)
Anova(MODEL3)
summary(MODEL3)
_____________________________________
#try to figure out how to graphically examine residuals
> eruption.lm = lm(eruptions ~ waiting, data=faithful) 
> eruption.res = resid(eruption.lm)

> plot(faithful$waiting, eruption.res, 
       +     ylab="Residuals", xlab="Waiting Time", 
       +     main="Old Faithful Eruptions") 
> abline(0, 0)                  
# From http://www.r-tutor.com/elementary-statistics/simple-linear-regression/residual-plot
residual3 <-resid(MODEL3)
summary(residual3)
plot(natural$mac, residual3,
     ylab = "Residuals", xlab = "Mac Counts",
     main = "Who Knows")

?resid
?plot
head(residual3)
__________________________________________
#moving on to levene's test for equality of variances

#Quick boxplots of these data sets:
  
  #plot(count ~ spray, data = InsectSprays)

plot(mac ~ total, data = natural)
plot(mac ~ Date, data = natural)
plot(total ~ Date, data = natural)
plot(other ~ Date, data = natural)

#The leveneTest function is part of the car package.
#With one independent variable:

leveneTest(count ~ spray, data=InsectSprays)

leveneTest(mac ~ total, data=natural)
?leveneTest
## load leveneTest function
#library(car)
# run the levene test centered around the mean
leveneTest(data$height, data$sex, center=mean)
leveneTest(natural$mac, natural$total, center=mean)

#results because it probably worked?
#Levene's Test for Homogeneity of Variance (center = mean)
#      Df F value    Pr(>F)    
#group 60  3.5699 0.0004223 ***
#      25                      
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Warning message:
#In leveneTest.default(natural$mac, natural$total, center = mean) :
#natural$total coerced to factor.

#From Wikipedia: 
#"If the resulting p-value of Levene's test is less than some significance 
#level (typically 0.05), the obtained differences in sample variances are 
#unlikely to have occurred based on random sampling from a population with 
#equal variances. Thus, the null hypothesis of equal variances is rejected 
#and it is concluded that there is a difference between the variances in the 
#population.

?log
____________________________________
#THAT WASN'T GETTING ME ANYWHERE, TAKE 2
natlognat <- read.csv('Desktop/natlognat.csv')
head(natlognat)
plot(logmac ~ logtotal, data = natlognat)
#huh, that looks promising, now try the lm on these data?

#MODEL1 <- lm(formula = mac ~ Date + total, data = natural)
#Anova(MODEL1)
#summary(MODEL1)

MODEL4 <- lm(formula = logmac ~ Date, data = natlognat)
Anova(MODEL4)
summary(MODEL4)

MODEL5 <- lm(formula = logmac ~ logtotal, data = natlognat)
Anova(MODEL5)
summary(MODEL5)

abline(lm(formula = logmac ~ Date, data = natlognat))

abline(lm(formula = logmac ~ logtotal, data = natlognat))

#okay i don't understand something...
plot(logmac ~ logtotal, data = natlognat)
abline(lm(formula = logmac ~ logtotal, data = natlognat))

#better
plot(logmac ~ Date, data = natlognat)

#Okay, I need to figure out how to compare things to just the densities in the 
#first sample month (July)

initialtotdens <- filter(natural, Date == 'July')
plot(natural$mac, initialtotaldens$mac)

#nope, try these options from SO:
#ggplot(data=data.frame(x=df1$x, y=df2$x), aes(x,y)) + geom_point()

#ggplot() + geom_point(aes(x=df1$x, y=df2$x))

#ggplot(data=NULL, aes(x=df1$x, y=df2$x)) + geom_point()

#ggplot(data=df1, aes(x=x)) + geom_point(aes(y=df2$x))

ggplot() + geom_point(aes(x=initialtotdens$mac, y=natural$mac))
#nope
ggplot(data=initialtotdens, aes(x=mac)) + geom_point(aes(y=natural$mac))
#nope, something is wrong related to different number of data points

natfinalcomp <- read.csv('Desktop/natfinalcomp.csv', stringsAsFactors = FALSE)

head(natfinalcomp)

initialdens <- filter(natfinalcomp, Date == 'July')
finaldens <- filter(natfinalcomp, Date == 'January')

figb <- ggplot() + geom_point(aes(x=initialdens$Mac.Density, y=finaldens$Mac.Density))

figa <- ggplot() + geom_point(aes(x=initialdens$Mac.Density, y=finaldens$Mac.Surv..))
figa

#you are onto something with the code, but these are not regressions....
#remember:
lm(formula = logmac ~ Date, data = natlognat)

figa <- lm(formula = finaldens$Mac.Surv ~ initialdens$Mac.Density)
plot(figa)

abline(lm(formula = finaldens$Mac.Surv ~ initialdens$Mac.Density))
?lm

figa <- lm(formula = finaldens$Mac.Surv ~ initialdens$Mac.Density)

head(initialdens)
head(finaldens)

figa <- lm(formula = finaldens$Mac.Surv ~ initialdens$logmac)
Anova(figa)
summary(figa)

?lm

backwardsfiga <- lm(formula = initialdens$logmac ~ finaldens$Mac.Surv)
Anova(backwardsfiga)
summary(backwardsfiga)

figc <- lm(formula = finaldens$Mac.Surv ~ initialdens$logtotal)
Anova(figc)
summary(figc)


#Ugh, I no longer know where I am going with this. These figures look nothing 
#like what I was expecting, and I actually have no idea what any of them mean
