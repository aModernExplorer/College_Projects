###############################################################################################
# Packages
###############################################################################################

install.packages("tictoc")
install.packages("caret")
install.packages("e1071")
library(e1071)
library(tictoc)
library(caret)
library(MASS)
library(plyr)
library(ggplot2)

###############################################################################################
# Cleaning and Manipulating
###############################################################################################

# Cleaning Ethnicity column because I found "" rows that were randomly added and removed them and dropped the level
AdmDat <- read.csv("./Classification_Project_GitHub/Admit_Data.csv", header = TRUE)
AdmDat <- AdmDat[, -c(11)] # took out column SportRating because of correlation issues
levels(AdmDat$Race)
# AdmDat$Race <- AdmDat$Race[!AdmDat$Race=""] # created a AdmDat$Race column without level ""
# AdmDat$Race <- factor(AdmDat$Race) # factored it to get 4 levels
summary(AdmDat)

# Making FinancialAid a factor
AdmDat$FinancialAid <- as.factor(AdmDat$FinancialAid)
class(AdmDat$FinancialAid) # checking to make sure it came out to be a factor

# Filling NA of AcdmcIndex by substituting it with the median of AcdmcIndex (In Excel)
# AdmDat$AcdmcIndex <- as.factor(AdmDat$AcdmcIndex) 
# scatter.smooth(AdmDat$AcdmcIndex, AdmDat$GPARecalc)
# lm <- lm(AdmDat$GPARecalc~AdmDat$AcdmcIndex, na.action = na.omit) # Attempted to predict by linear model and substitute NAs with equation
# summary(lm)

AdmDat$AcdmcInt1 <- revalue(AdmDat$AcdmcInt1, c("AstroAndPhys"="PhysAndAstro")) # Changed a mislabeled level

# Change AcmcInd to an ordered factor to accurately reflect what data conveys and to be able to use it appropriately
AdmDat$AcdmcIndex <- factor(AdmDat$AcdmcIndex, levels = c(1, 2, 3, 4, 5),
                            ordered = TRUE)
class(AdmDat$AcdmcIndex)

# Create a .csv of cleaned dataset
?write.csv
write.csv(AdmDat, file = "CleanedAdmDat")

###############################################################################################
# Descriptive Statistics
###############################################################################################

# Density plots of quantitative variables
ggplot(AdmDat, aes(x = AdmDat$ACTComp)) + geom_density(alpha = .20, fill = "Maroon") + ggtitle("Density Plot of ACTComp") + 
  theme(plot.title = element_text(hjust = .5))

ggplot(AdmDat, aes(x = AdmDat$GPARecalc)) + geom_density(alpha = .20, fill = "Maroon") + ggtitle("Density Plot of GPARecalc") + 
  theme(plot.title = element_text(hjust = .5))

# Barplots of qualtiative variables
count1 <- table(AdmDat$AcdmcIndex) # Counted the occurences of each level
barplot(count1, main="AcdmcInd Distribution", horiz=TRUE,
        names.arg=c("1", "2", "3", "4", "5"), col = "Maroon4") # Created horizontal barplot

count2 <- table(AdmDat$DecisionPlan)# Counted the occurences of each level of DecisionPlan
par(las=2) # Make labels horizontal Ccourtesy of Quick-R)
par(mar=c(5,8,4,2)) # Make labels fit (Courtesy of Quick-R)
barplot(count2, main="DecisionPlan Distribution", horiz=TRUE, 
        names.arg=c("EarlyAction1", "EarlyAction2", "EarlyDecision1", "EarlyDecision2", "RegularDecision"), 
        cex.names=.8, col = "Maroon4") # Created horizontal barplot
levels(AdmDat$DecisionPlan)

count3 <- table(AdmDat$MeritAward)
barplot(count3, main="MeritAward Distribution", horiz=TRUE,
        col = "Maroon4")

###############################################################################################
# Modeling
###############################################################################################

# Splitting into training and test dataset
smpl <- floor(.7*nrow(AdmDat)) # Making a sample with half of the data
set.seed(123) # set.seed to make reproducible work
train_ind <- sample(seq_len(nrow(AdmDat)), size = smpl) # random sample half amount of rows in AdmDat
trn <- AdmDat[train_ind, ] # Create training dataset 
tst <- AdmDat[-train_ind, ] # Create testing dataset

# Create a baseline model to compare to later models
lgmB <- glm(Decision~.-ID, family = binomial(link = 'logit'), data=trn) # Create baseline logistic model
summary(lgmB) # AIC = 1594.6

# Predict on test set with baseline model
fitted.results <- predict(lgmB,newdata=tst,type='response') # find probablitlity of attending the university

# Make results binary
fitted.results <- ifelse(fitted.results > .5, 1, 0)

# Predict on test set
accuracy <- mean(as.matrix(fitted.results) == as.matrix(tst$Decision),na.rm=TRUE)
print(paste('Accuracy =',accuracy)) # Accuracy = .8393

# Feature Selection
modl <- glm(formula = Decision ~ .-ID, data=AdmDat)
summary(modl)
step <- stepAIC(modl, direction="backward", na.action=na.remove)
step$anova # display results

# Fnding optimal model
lgm1 <- glm(Decision~DecisionPlan+Legacy+Athlete+EventParticipation+CampusVisits+ACTComp+AcdmcIndex+PrmntGeom+Religion, family = binomial(link = 'logit'), data=trn) # Create baseline logistic model
summary(lgm1) # The models AIC = 1564.8
fit.results1 <- predict(lgm1,newdata=tst,type='response')
fit.results1 <- ifelse(fit.results1 > .5, 1, 0)
accuracy1 <- mean(as.matrix(fit.results1) == as.matrix(tst$Decision),na.rm=TRUE)
print(paste('Accuracy =',accuracy1)) # Accuracy = .8484

lgm2 <- glm(Decision~DecisionPlan+Legacy+Athlete+EventParticipation+CampusVisits+ACTComp+AcdmcIndex+GPARecalc, family = binomial(link = 'logit'), data=trn)
summary(lgm2) # AIC = 1582
fit.results2 <- predict(lgm2,newdata=tst,type='response')
fit.results2 <- ifelse(fit.results2 > .5, 1, 0)
accuracy2 <- mean(as.matrix(fit.results2) == as.matrix(tst$Decision),na.rm=TRUE)
print(paste('Accuracy =',accuracy2)) # Accuracy = .8394

lgm3 <- glm(Decision~DecisionPlan+Legacy+Athlete+EventParticipation+CampusVisits, family = binomial(link = 'logit'), data=trn)
summary(lgm3) # AIC = 1675.5
fit.results3 <- predict(lgm3,newdata=tst,type='response')
fit.results3 <- ifelse(fit.results3 > .5, 1, 0)
accuracy3 <- mean(as.matrix(fit.results3) == as.matrix(tst$Decision),na.rm=TRUE)
print(paste('Accuracy =',accuracy3)) # Accuracy = .8269

lgm4 <- glm(Decision~DecisionPlan+Legacy+Athlete+PrmntGeom+ClassRank, family = binomial(link = 'logit'), data=trn)
summary(lgm4) # AIC = 2074.8
fit.results4 <- predict(lgm4,newdata=tst,type='response')
fit.results4 <- ifelse(fit.results4 > .5, 1, 0)
accuracy4 <- mean(as.matrix(fit.results4) == as.matrix(tst$Decision),na.rm=TRUE)
print(paste('Accuracy =',accuracy4)) # Accuracy = .8099

lgm5 <- glm(Decision ~ PermntCountry + Ethnicity + Religion + Inquired + 
              DecisionPlan + Legacy + Athlete + SportRating + EventParticipation + 
              CampusVisits + ClassRank + ACTComp + AcdmcIndex + MeritAward, 
            family = binomial(link = 'logit'), data = trn) # AIC = 1539
summary(lgm5)
tic()
fit.results5 <- predict(lgm5,newdata=tst,type='response')
fit.results5 <- ifelse(fit.results5 > .5, 1, 0)
accuracy5 <- mean(as.matrix(fit.results5) == as.matrix(tst$Decision),na.rm=TRUE)
print(paste('Accuracy =',accuracy5)) # Accuracy = .8507
toc() # Time to run on test set = .02

# confusionMatrix(fit.results5, tst$Decision, positive = NULL)
?confusionMatrix
