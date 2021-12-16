#Inspection of data to check the variables have been correctly identified#
str(B.Longum.Data.Fig.1.)
#Changing the explanatory variable 'time after final STZ injection (weeks)' from a factor to a continuous variable#
B.Longum.Data.Fig.1.$ï..time.after.final.STZ.injection..week.<-as.numeric(B.Longum.Data.Fig.1.$ï..time.after.final.STZ.injection..week.)
#Checking change has been implemented# 
str(B.Longum.Data.Fig.1.)

#Summary statistics#
summary(B.Longum.Data.Fig.1.)
#Time after injection & blood glucose conc. both have very similar medians and means, indicating normally distributed data.#

#plotting a histogram of the response vatiable, fasting blood glucose conc.#
hist(B.Longum.Data.Fig.1.$fasting.blood.glucose..mmol.per.l.)
#the resultant plot shows a very low blood glucose conc. is the most prevalent, following a right-skewed distribution#
#plotting whether time after STZ injections affects fasting blood glucose conc.#
boxplot(B.Longum.Data.Fig.1.$fasting.blood.glucose..mmol.per.l.~B.Longum.Data.Fig.1.$ï..time.after.final.STZ.injection..week.)
#resultant 9 boxplots show the average rising gradually,as expected with rising diabetes, and varied standard deviations and ranges. Outliers are present at -1 weeks also.#
#plotting whether treatment affects fasting blood glucose conc.#
boxplot(B.Longum.Data.Fig.1.$fasting.blood.glucose..mmol.per.l.~B.Longum.Data.Fig.1.$Mouse.group)
#boxplots give expected trend. Nano-se-b.longum is the most effective at reducing fasting blood glucose conc. towards a normal conc..Normal data has 4 outliers.#
#plotting how time affects blood glucode conc.#
plot(B.Longum.Data.Fig.1.$fasting.blood.glucose..mmol.per.l.~B.Longum.Data.Fig.1.$ï..time.after.final.STZ.injection..week.)
#both the boxplot of mouse group vs blood glucose conc. and the plot of time after injection vs blood glucose conc. show heteroscedasity is present even if just slightly# 


install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
install.packages("e1071")
library(e1071)

#ggplot to confirm right-skewed nature of response variable#
ggplot(data = B.Longum.Data.Fig.1., aes (x=fasting.blood.glucose..mmol.per.l.)) + geom_histogram()
#check using the skewness function#
??skewness
skewness(B.Longum.Data.Fig.1.$fasting.blood.glucose..mmol.per.l.)
skewness(B.Longum.Data.Fig.1.$ï..time.after.final.STZ.injection..week.)
#skewness values are actually relatively low, between -0.5 and +0.5, thus a transformation is not necessary at this time#

ggplot(data = B.Longum.Data.Fig.1., aes(x=ï..time.after.final.STZ.injection..week., y = fasting.blood.glucose..mmol.per.l.)) + geom_point() + scale_x_log10() + scale_y_log10()
#a log transform of the data does eliminate heteroscedascisity but the data is still right-skewed#

#basic model to start off with, a simple regression model between to two covariate variables# 
model0 <- lm (B.Longum.Data.Fig.1.$fasting.blood.glucose..mmol.per.l.~B.Longum.Data.Fig.1.$ï..time.after.final.STZ.injection..week.)
#summary output to gain coefficients for algebraic structure#
summary(model0)
#anova output to gain basic statistics#
anova(model0)
#plotting residuals to check they conform to a normal distributon#
hist(rstandard(model0))
#a q-q plot to double-check the normal distribution of residuals# 
plot(model0, 2)
plot(rstandard(model0), type="l")
points(rstandard(model0), pch=1, col="darkred")
logLik(model0)

model1 <- lm (B.Longum.Data.Fig.1.$fasting.blood.glucose..mmol.per.l.~B.Longum.Data.Fig.1.$Mouse.group)
summary(model1)
anova(model1)
hist(rstandard(model1))
plot(model1,2)
plot(rstandard(model1), type="l")
points(rstandard(model1), pch=1, col="darkblue")
logLik(model1)


model2 <- lm (B.Longum.Data.Fig.1.$fasting.blood.glucose..mmol.per.l.~B.Longum.Data.Fig.1.$Mouse.group+B.Longum.Data.Fig.1.$ï..time.after.final.STZ.injection..week.)
summary(model2)
anova(model2)
hist(rstandard(model2))
plot(model2,2)
plot(rstandard(model2), type="l")
points(rstandard(model2), pch=1, col="darkblue")
logLik(model2)

model3 <- lm (B.Longum.Data.Fig.1.$fasting.blood.glucose..mmol.per.l.~B.Longum.Data.Fig.1.$Mouse.group*B.Longum.Data.Fig.1.$ï..time.after.final.STZ.injection..week.)
summary(model3)
anova(model3)
hist(rstandard(model3))
plot(model3,2)
plot(rstandard(model3), type="l")
points(rstandard(model3), pch=1, col="darkblue")
logLik(model3)


install.packages("lme4")
library(lme4)

model4<-lmer(fasting.blood.glucose..mmol.per.l.~ï..time.after.final.STZ.injection..week.+ (1|Mouse.group), data=B.Longum.Data.Fig.1.)
summary(model4)
anova(model4)
logLik(model4)

#LRT for model3 and model1#
1-pchisq(310,5)

#LRT for model3 and model0#
1-pchisq(396,8)

ggplot(B.Longum.Data.Fig.1., aes(x =ï..time.after.final.STZ.injection..week., y =fasting.blood.glucose..mmol.per.l., colour= Mouse.group)) + geom_point()+ geom_smooth(method=lm, aes(group=Mouse.group, color=Mouse.group), se=FALSE)
