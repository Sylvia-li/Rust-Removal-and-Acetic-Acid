library(reshape2)
library(ggplot2)
library(tidyr)
library(BsMD)
library(car)

Alldata <- read.csv("data.csv", header = T, fill = T, stringsAsFactors = T)

########Section data by amount of salt
nosalt <- Alldata[1:8,]
salt2 <- Alldata[9:16,]
salt5 <- Alldata[17:24,]
salt75 <- Alldata[25:32,]
salt100 <- Alldata[33:40,]

#######Section data by time
minute1 <- Alldata[c(1,9,17,25,33),]
minute2 <- Alldata[c(2,10,18,26,34),]
minute5 <- Alldata[c(3,11,19,27,35),]
minute10 <- Alldata[c(4,12,20,28,36),]
minute15 <- Alldata[c(5,13,21,29,37),]
minute20 <- Alldata[c(6,14,22,30,38),]
minute30 <- Alldata[c(7,15,23,31,39),]
minute45 <- Alldata[c(8,16,24,32,40),]

########Simple plots to visualize data
####Evaluate time+salt and concentration of vinegar
#########BARPLOT VARYING SALT
df <- melt(minute30, id.vars = "Treatment")
ggplot(df, aes(x=Treatment, y = value, fill = variable)) + geom_bar(stat = 'identity', position = 'dodge') + labs(title = "Pennies cleaned vs Salt amount for each concentration of vinegar", x = "Salt amount and Time spent", y = "amount of pennies cleaned")
##########BARPLOT VARYING TIME WITH CONSTANT SALT
df <- melt(salt100, id.vars = "Treatment")
ggplot(df, aes(x=Treatment, y = value, fill = variable)) + geom_bar(stat = 'identity', position = 'dodge') + labs(title = "Pennies cleaned vs time spent for each concentration of vinegar", x = "Time spent in constant salt amount", y = "amount of pennies cleaned")

#####BOXPLOT
###########VARYing TIME WITH CONSTANT SALT
df <- melt(salt5)
plt <- ggplot(data = df, aes(x = variable, y = value, color = variable))
plt + geom_boxplot() + theme_minimal() + labs(x = "0.5g of salt", y = "Pennies cleaned")
###########VARYING SALT
df <- melt(minute45)
plt <- ggplot(data = df, aes(x = variable, y= value, color = variable))
plt + geom_boxplot() + theme_minimal() + labs(x = "45 minute", y = "Pennies cleaned")


############MODELS (EFFECT MODELS OR LINEAR MODELS OR INTERACTION PLOTS)###################
interactionData <- read.csv("intplotdata.csv", header = T, stringsAsFactors = T)
Treatment <- factor(interactionData$Treatment, levels = rev(unique(interactionData$Treatment)))
Concentration <- interactionData$Concentration
Cleaned <- interactionData$Cleaned
Time <- factor(interactionData$Minute, levels = rev(unique(interactionData$Minute)))
Salt <- factor(interactionData$Salt..g., levels = rev(unique(interactionData$Salt)))
#Interaction plot based on time AND salt
interaction.plot(Treatment, Concentration, Cleaned, col = 1:6, ylim = range(Cleaned),cex.axis = 0.7, cex = 0.5)
points(Treatment, Cleaned, pch = as.numeric(Cleaned))
#interaction based on time only
interaction.plot(Time, Concentration, Cleaned, col = 1:6, ylim = range(Cleaned))
points(Time, Cleaned, pch = as.numeric(Cleaned))
#Interaction based on salt only
interaction.plot(Salt, Concentration, Cleaned,  col = 1:6, ylim = range(Cleaned))
points(Salt, Cleaned, pch = as.numeric(Cleaned))

#####EFFECT MODELS: TREATMENT DIFFERENCE MODELS########
#####SALT INTERACTION
g <- lm(Cleaned ~ Salt + Concentration, contrasts = list(Salt = contr.sum, Concentration = contr.sum), data = interactionData)
summary(g)
par(mfrow = c(1,2))
with (interactionData, interaction.plot(Salt, Concentration, Cleaned, type = "b", main = "Original Data"))
with(interactionData, interaction.plot(Salt, Concentration, g$fitted.values, type = "b", main = "Predicted/Fitted Values"))
######TIME INTERACTION WITH CONCEN MODELS
g2 <- lm(Cleaned ~ Time + Concentration, contrasts = list(Time = contr.sum, Concentration = contr.sum), data = interactionData)
summary(g2)
par(mfrow = c(1,2))
with (interactionData, interaction.plot(Time, Concentration, Cleaned, type = "b", main = "Original Data"))
with(interactionData, interaction.plot(Time, Concentration, g2$fitted.values, type = "b", main = "Predicted/Fitted Values"))
#####CONCENTRATION WITH TIME INTERACTION MODELS
ga <-lm(Cleaned ~ Concentration + Time, contrasts = list(Concentration = contr.sum, Time = contr.sum), data = interactionData)
summary(ga)
par(mfrow = c(1,2))
with (interactionData, interaction.plot(Concentration, Time, Cleaned, type = "b", main = "Original Data"))
with(interactionData, interaction.plot(Concentration, Time, ga$fitted.values, type = "b", main = "Predicted/Fitted Values"))
######CONCENTRATION WITH SALT INTERACTION MODEL
gb <-lm(Cleaned ~ Concentration + Salt, contrasts = list(Concentration = contr.sum, Salt= contr.sum), data = interactionData)
summary(gb)
par(mfrow = c(1,2))
with (interactionData, interaction.plot(Concentration, Salt, Cleaned, type = "b", main = "Original Data"))
with(interactionData, interaction.plot(Concentration, Salt, gb$fitted.values, type = "b", main = "Predicted/Fitted Values", cex = 0.5))



#######SIGNIFICANT EFFECTS######
qqnorm.lenth <- function (effects)
{
  abseffects<- 2*abs(effects)
  m <- length(abseffects)
  # Psuedo sd based all abs(effect)
  s0 <- 1.5*median(abseffects) 
  non.sig <- abseffects < 2.5*s0
  # Refined estimate of sd 
  pse <- 1.5*median(abseffects[non.sig]) 
  
  sme<-qt(1-(1-(1+0.95^(1/m))/2), m/3)*pse;
  sig <- abseffects>sme
  
  hqq<-qqnorm(effects, type = "n")
  text(hqq$x, hqq$y, labels = names (effects), col = sig + 1)
  qqline(effects)
  if(sum(sig) == 0) {cat("No Significant Effects Found by Lenth's Method. \n \n")}
  if(sum(sig) > 0) 
  {cat ("Significant Factors Selected by Lenth's Method:\n",
        names(abseffects)[sig], "\n") }
  return(list(pse=pse, sme=sme))
}
#####SIGNIFICANT CONCENTRATIONS
g3 <- lm(Cleaned~(Concentration), data = interactionData)
effects <- g3$coefficients[-1]
qq<-qqnorm(effects, type = "n")
qqline(effects)
text(qq$x, qq$y, labels = names(effects))
qqnorm.lenth(effects)
par(mfrow=c(1,1))
LenthPlot(g3, las=3)
#####SIGNIFICANT CONCENTRATION AND SALT
g4 <- lm(Cleaned~(Salt +  Concentration)^2, data = interactionData)
effects2 <- g4$coefficients[-1]
qq2<-qqnorm(effects2, type = "n")
qqline(effects2)
text(qq2$x, qq2$y, labels = names(effects2))
qqnorm.lenth(effects2)
par(mfrow=c(1,1))
LenthPlot(g4, las=3)
######SIGNIFICANT CONCENTRATION AND TIME
g5 <- lm(Cleaned~(Concentration + Time)^2, data = interactionData)
effects3 <- g5$coefficients[-1]
qq3<-qqnorm(effects3, type = "n")
qqline(effects3)
text(qq3$x, qq3$y, labels = names(effects3))
qqnorm.lenth(effects3)
par(mfrow=c(1,1))
LenthPlot(g5, las=3)


#######ANOVA#############
leveneTest(Cleaned ~ Concentration, data = interactionData)
leveneTest(Cleaned ~ Salt)
leveneTest(Cleaned~Time)



#######Effect Comparison with Tukey Method##############
###TIME
TukeyHSD(aov(Cleaned ~ Time, interactionData))
plot(TukeyHSD(aov(Cleaned ~ Time, interactionData)))
###SALT
TukeyHSD(aov(Cleaned ~ Salt, interactionData))
plot(TukeyHSD(aov(Cleaned ~ Salt, interactionData)))
###General Treatment
TukeyHSD(aov(Cleaned ~ Treatment, interactionData))
plot(TukeyHSD(aov(Cleaned ~ Treatment, interactionData)))
####CONCENTRATION
TukeyHSD(aov(Cleaned ~ Concentration, interactionData))
plot(TukeyHSD(aov(Cleaned ~ Concentration, interactionData)))

####MODEL CHECKING##########
##SALT 
par(mfrow = c(1,2))
qqnorm(g$residuals)
qqline(g$residuals)
shapiro.test(g$residuals)
plot(g$fitted.values, g$residuals, xlab = "fitted value", ylab = "residuals", col = Salt, pch = as.numeric(Concentration), main = "Residuals vs FItted in Interaction Model")
abline(h=0)
##TIME
par(mfrow = c(1,2))
qqnorm(g2$residuals)
qqline(g2$residuals)
shapiro.test(g2$residuals)
plot(g2$fitted.values, g2$residuals, xlab = "fitted value", ylab = "residuals", col = Time, pch = as.numeric(Concentration), main = "Residuals vs FItted in Interaction Model")
abline(h=0)
##Concentration with time
par(mfrow = c(1,2))
qqnorm(ga$residuals)
qqline(ga$residuals)
shapiro.test(ga$residuals)
plot(ga$fitted.values, ga$residuals, xlab = "fitted value", ylab = "residuals", col = Time, pch = as.numeric(Concentration), main = "Residuals vs FItted in Interaction Model")
abline(h=0)
####CONCENTRATION WITH SALT
par(mfrow = c(1,2))
qqnorm(gb$residuals)
qqline(gb$residuals)
shapiro.test(gb$residuals)
plot(gb$fitted.values, ga$residuals, xlab = "fitted value", ylab = "residuals", col = Time, pch = as.numeric(Concentration), main = "Residuals vs FItted in Interaction Model")
abline(h=0)
