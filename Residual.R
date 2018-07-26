#1. To use "redlist" package
library(reldist)

#2. Read my data
Data <- read.csv("Desktop/Yona/Eco_Eco/Data.csv", as.is = TRUE, na.strings = c("*", "**"))

#3. Replacing # with 208000
Data$A_MEAN[Data$A_MEAN == "#"] <- 208000

#4. Remove commas from my data
Population <- gsub(",", "", Data$TOT_EMP)
Income <- gsub(",", "", Data$A_MEAN)

#5. Convert into numeric vector
Population <- as.numeric(Population)
head(Population)
Income <- as.numeric(Income)
head(Income)

#6. Make column for total income per occupation (TIPC)
Total <- Population*Income
head(Total)

#7. One more step before making new dataframe
Area <- Data$AREA_NAME
Code <- Data$OCC_CODE
OCC <- Data$OCC_TITLE

#8. Make a simplified dataframe for inequality analysis
Data <- data.frame(Area, Code, OCC, Population, Income, Total)
Data <- Data[-grep("-0000", Data$Code), ]
Data <- Data[!(is.na(Data$Population) | is.na(Data$Income)), ]

#9. Export this dataframe into a csv
write.csv(Data, "Desktop/Yona//Eco_Eco/Plot.csv")

#10. Use this dataframe
Data <- read.csv("Desktop/Yona//Eco_Eco//Plot.csv")

#11. Preparation for loop
MSAs <- unique(Data$Area)
head(MSAs)

#12. Make dummy vectors for Loop
giniVect <- numeric()
entVect <- numeric()
popVect <- numeric()

#13. Loop for Plot
for(MSA in MSAs) {
  X <- Data[Data$Area == MSA,]
  popVect <- c(popVect, sum(X$Population))
  denPop <- X$Population/sum(X$Population)
  income <- X$Income
  giniVect <- c(giniVect, gini(income, denPop))
  entVect <- c(entVect, -sum(denPop*log(denPop)))
}

#14. Draw plot of Diversity against Inequality
plot(giniVect, entVect)

#15. Find best fit linear line
mod <- lm(entVect ~ giniVect)
abline(mod, col="red", lty=1)
summary(mod)

#16. Plot for diversity against population
plot(popVect, entVect)
lines(lowess(popVect, entVect), col="red", lty=1)

#17. Plot for inequality against population
plot(popVect, giniVect)
lines(lowess(popVect, giniVect), col="red", lty=1)

#18. Plot for residual against population, inequality, diversity
Y <- resid(mod)
plot(popVect, Y)
lines(lowess(popVect, Y), col="red", lty=1)
plot(giniVect, Y)
plot(entVect, Y)
abline(lm(Y~entVect), col="red", lty=1)

#19. Make virtual MSAs (in process)
for(MSA in MSAs) {
  X1 <- Data[Data$Area == MSA,]
  X2 <- Data[Data$Area == MSA,], X1 != X2
  X1[sample(nrow(X1), 100)]
  X3 <- X1$Population+X2$Population
}
