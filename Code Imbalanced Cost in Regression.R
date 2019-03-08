#Workspace einrichten
getwd()
setwd("/Users/Constanze/Desktop/uni/se wirtschaftsinformatik/seminararbeit")

#Computing the misprediction errors and giving them the right "direction"
errorCalculate <- function(predicted, actual){
  error <- numeric()
  new_error <- 0
  for (i in 1:length(predicted)) {
    #overprediction, "Richtung des Fehlers ist 1
    if ((predicted[i]<0 && predicted[i]<actual[i])|(predicted[i]>0 && predicted[i]>actual[i])){
      new_error = (1)*abs(predicted[i] - actual[i])
    } else {
      #underprediction, "Richtung des Fehlers ist -1)
      new_error = (-1)*abs(predicted[i] - actual[i])
    }
    error <- append(error, new_error, after = length(error))  
  }
  return(error)
}

#Calculating the cost due to a specific error, depending on over- or underprediction
#punishment as a measure for the asymmetric cost of underprediction 
costCalculate <- function(predicted, actual, punishment){
  error <- numeric()
  cost <- numeric()
  for (i in 1:length(predicted)) {
    #overprediction
    if ((predicted[i]<0 & predicted[i]<actual[i])|(predicted[i]>0 & predicted[i]>actual[i])){
      new_error = predicted[i] - actual[i]
      new_cost = ((abs(new_error)/totalProfit)) * 1
    } else {
      #underprediction
      new_error = predicted[i] - actual[i]
      new_cost = ((abs(new_error)/totalProfit)) * punishment
    }
    cost <- append(cost, new_cost, after = length(cost))  
  }
  return(cost)
}

#Calculating the Average Misprediction Cost, based on Delta
#This function will be optimised
amcCalculate <- function(delta, predicted, actual, punishment){
  cost <- costCalculate(predicted, actual+delta, punishment)
  amc <- mean(cost)
  return(amc)
}

#Example for optimizing
optimise(amcCalculate, c(-1000, 1000), predictedLR, actualLR, 100)

##LR
#Loading the data
datenLR <- read.csv("SelbstLR.csv", numerals = "no.loss", colClasses = "numeric")
predictedLR <- as.numeric(datenLR$prediction.PROFIT.)
actualLR <- as.numeric(datenLR$PROFIT)

totalProfit <- sum(abs(actualLR))

#Plot Lin Lin Cost Function
errorLR <- errorCalculate(predictedLR, actualLR)
costLR <- costCalculate(predictedLR, actualLR, 50)
plot(errorLR, costLR, xlab = "Error", ylab = "Cost", main = "Lin-lin cost function")

#Optimizing the AMC
#with different cost ratios
costRatio <- c(1, 10, 20, 50, 100)
amcCostRatios <- sapply(costRatio, optimise(amcCalculate, c(-1000, 1000), predictedLR, actualLR, punishment=costRatio))
deltaLR100 <- optimise(amcCalculate, c(-1000, 1000), predictedLR, actualLR, 100)
deltaLR50 <- optimise(amcCalculate, c(-1000, 1000), predictedLR, actualLR, 50)
deltaLR10 <- optimise(amcCalculate, c(-1000, 1000), predictedLR, actualLR, 10)
deltaLR20 <- optimise(amcCalculate, c(-1000, 1000), predictedLR, actualLR, 20)
deltaLR1 <- optimise(amcCalculate, c(-1000, 1000), predictedLR, actualLR, 1)
LR1 <- amcCalculate(0, predictedLR, actualLR, 1)
LR10 <- amcCalculate(0, predictedLR, actualLR, 10)
LR20 <- amcCalculate(0, predictedLR, actualLR, 20)
LR50 <- amcCalculate(0, predictedLR, actualLR, 50)
LR100 <- amcCalculate(0, predictedLR, actualLR, 100)

##NN
#Loading the data
datenNN <- read.csv("SelbstNN.csv", numerals = "no.loss", colClasses = "numeric")
predictedNN <- as.numeric(datenNN$prediction.PROFIT.)
actualNN <- as.numeric(datenNN$PROFIT)

totalProfit <- sum(abs(actualNN))

#Plot Lin Lin Cost Function
errorNN <- errorCalculate(predictedNN, actualNN)
costNN <- costCalculate(predictedNN, actualNN, 50)
plot(errorNN, costNN, xlab = "Error", ylab = "Cost", main = "Lin-lin cost function")

#Plotting the adjustment curve
bspDelta <- seq(from =-900, to = -500, by = 1)
bspAMC <- sapply(bspDelta, amcCalculate, predictedNN, actualNN, 50)
plot(bspDelta, bspAMC, xlab = "Adjustment Delta", ylab = "Average Misprediction Cost")

#Optimizing the AMC
#with different cost ratios
costRatio <- c(1, 10, 20, 50, 100)
amcCostRatiosNN <- sapply(costRatio, optimise(amcCalculate, c(-1000, 1000), predictedNN, actualNN, punishment=costRatio))
deltaNN100 <- optimise(amcCalculate, c(-1000, 1000), predictedNN, actualNN, 100)
deltaNN50 <- optimise(amcCalculate, c(-1000, 1000), predictedNN, actualNN, 50)
deltaNN10 <- optimise(amcCalculate, c(-1000, 1000), predictedNN, actualNN, 10)
deltaNN20 <- optimise(amcCalculate, c(-1000, 1000), predictedNN, actualNN, 20)
deltaNN1 <- optimise(amcCalculate, c(-1000, 1000), predictedNN, actualNN, 1)
NN1 <- amcCalculate(0, predictedNN, actualNN, 1)
NN10 <- amcCalculate(0, predictedNN, actualNN, 10)
NN20 <- amcCalculate(0, predictedNN, actualNN, 20)
NN50 <- amcCalculate(0, predictedNN, actualNN, 50)
NN100 <- amcCalculate(0, predictedNN, actualNN, 100)

##M5
#Loading the data
datenM5 <- read.csv("SelbstM5.csv", numerals = "no.loss", colClasses = "numeric")
predictedM5 <- as.numeric(datenM5$prediction.PROFIT.)
actualM5 <- as.numeric(datenM5$PROFIT)

totalProfit <- sum(abs(actualM5))

#Plot Lin Lin Cost Function
errorM5 <- errorCalculate(predictedM5, actualM5)
costM5 <- costCalculate(predictedM5, actualM5, 50)
plot(errorM5, costM5, xlab = "Error", ylab = "Cost", main = "Lin-lin cost function")

#Optimizing the AMC
#with different cost ratios
costRatio <- c(1, 10, 20, 50, 100)
amcCostRatiosM5 <- sapply(costRatio, optimise(amcCalculate, c(-1000, 1000), predictedM5, actualM5, punishment=costRatio))
deltaM5100 <- optimise(amcCalculate, c(-1000, 1000), predictedM5, actualM5, 100)
deltaM550 <- optimise(amcCalculate, c(-1000, 1000), predictedM5, actualM5, 50)
deltaM510 <- optimise(amcCalculate, c(-1000, 1000), predictedM5, actualM5, 10)
deltaM520 <- optimise(amcCalculate, c(-1000, 1000), predictedM5, actualM5, 20)
deltaM51 <- optimise(amcCalculate, c(-1000, 1000), predictedM5, actualM5, 1)
M51 <- amcCalculate(0, predictedM5, actualM5, 1)
M510 <- amcCalculate(0, predictedM5, actualM5, 10)
M520 <- amcCalculate(0, predictedM5, actualM5, 20)
M550 <- amcCalculate(0, predictedM5, actualM5, 50)
M5100 <- amcCalculate(0, predictedM5, actualM5, 100)

#Tabellen
valuesLRTuned <- c(3.64, 20.5, 39.1, 95.2, 189)
valuesLRUntuned <- c(5.14, 29.07, 55.67, 135.42, 268.14)

valuesNNTuned <- c(3.34, 6.26, 8.64, 15.35, 26.31)
valuesNNUntuned <- c(3.51, 10.65, 18.57, 42.35, 81.98)

valuesM5Tuned <- c(4.4, 19.03, 35.16, 83.25, 163.38)
valuesM5Untuned <- c(4.76, 24.00, 45.38, 109.52, 216.41)

values <- list(valuesLRTuned, valuesLRUntuned, valuesNNTuned, valuesNNUntuned, valuesM5Tuned, valuesM5Untuned)
valuesMean <- sapply(values, mean)
valuesSD <- sapply(values, sd)

#Test for significance - anova
tuned <- c(0,1,0,1,0,1)
tuned <- factor(tuned, levels=c(0,1), labels=c("tuned","untuned"))
tuned

method <- c(1, 1, 2, 2, 3, 3)
method <- factor(method, levels = c(1, 2, 3), labels = c("LR", "NN", "M5"))
amcs <- c(69.49, 98.69, 11.98, 31.41, 61.04, 80.01)

#Anova for means over all cost ratios (table 2)
newMeanMatrix <- read.csv2("anova klein.csv")
newMeanMatrix$cost <- as.numeric(levels(newMeanMatrix$cost))[newMeanMatrix$cost]
results <- aov(cost ~ method+tuning, data = newMeanMatrix)#Interaktionseffekte (mit*) ging nicht, zu wenig Werte, zu wenig degrees of freedom
summary(results)
#main effect method F(2,2)=129.03 with p=0.00769, main effect of tuning F(1,2), p=0.02122
TukeyHSD(results)

#ANOVA for each cost ratio
#1:1
sign1to1 <- read.csv2("sign 1to1.csv")
sign1to1$cost <- as.numeric(levels(sign1to1$cost))[sign1to1$cost]
sign1to1$tuned <- factor(sign1to1$tuned, levels = c(0,1), labels = c("tuned", "untuned"))
AOV1zu1 <- aov(cost ~ tuned+method, data = sign1to1)
summary(AOV1zu1)
# not significant 

#Rest is significant 
#1:10
sign1to10 <- read.csv2("sign1to10.csv")
sign1to10$cost <- as.numeric(levels(sign1to10$cost))[sign1to10$cost]
sign1to10$tuned <- factor(sign1to10$tuned, levels = c(0,1), labels = c("tuned", "untuned"))
AOV1zu10 <- aov(cost ~ tuned+method, data = sign1to10)
summary(AOV1zu10)

#1:20
sign1to20 <- read.csv2("sign1to20.csv")
sign1to20$cost <- as.numeric(levels(sign1to20$cost))[sign1to20$cost]
sign1to20$tuned <- factor(sign1to20$tuned, levels = c(0,1), labels = c("tuned", "untuned"))
AOV1zu20 <- aov(cost ~ tuned+method, data = sign1to20)
summary(AOV1zu20)

#1:50
sign1to50 <- read.csv2("sign1to20.csv")
sign1to50$cost <- as.numeric(levels(sign1to50$cost))[sign1to50$cost]
sign1to50$tuned <- factor(sign1to50$tuned, levels = c(0,1), labels = c("tuned", "untuned"))
AOV1zu50 <- aov(cost ~ tuned+method, data = sign1to50)
summary(AOV1zu50)

#1:100
sign1to100 <- read.csv2("sign1to100.csv")
sign1to100$cost <- as.numeric(levels(sign1to100$cost))[sign1to100$cost]
sign1to100$tuned <- factor(sign1to100$tuned, levels = c(0,1), labels = c("tuned", "untuned"))
AOV1zu100 <- aov(cost ~ tuned+method, data = sign1to100)
summary(AOV1zu100)
