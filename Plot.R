library(reldist)

#1. Read my data
Data <- read.csv("Desktop/Yona//Eco_Eco//Data.csv", as.is = TRUE, na.strings = c("*", "**"))

#2. Replacing # with 208000
Data$A_MEAN[Data$A_MEAN == "#"] <- 208000

#3. Remove commas from my data
Population <- gsub(",", "", Data$TOT_EMP)
Income <- gsub(",", "", Data$A_MEAN)

#4. Convert into numeric vector
Population <- as.numeric(Population)
head(Population)
Income <- as.numeric(Income)
head(Income)

#5. Make column for total income per occupation (TIPC)
Total <- Population*Income
head(Total)

#6. One more step before making new dataframe
Area <- Data$AREA_NAME
Code <- Data$OCC_CODE
OCC <- Data$OCC_TITLE

#7. Make a simplified dataframe for inequality analysis
Data <- data.frame(Area, Code, OCC, Population, Income, Total)
Data <- Data[-grep("-0000", Data$Code), ]
Data <- Data[!(is.na(Data$Population) | is.na(Data$Income)), ]

#8. Export this dataframe into a csv
write.csv(Data, "Desktop/Yona//Eco_Eco/Plot.csv")

#9. Use this dataframe
Data <- read.csv("Desktop/Yona//Eco_Eco//Plot.csv")

#10. Preparation for loop
MSAs <- unique(Data$Area)
head(MSAs)

#11. Make dummy vectors
giniVect <- numeric()
entVect <- numeric()

#12. Loop for Plot
for(MSA in MSAs) {
  X <- Data[Data$Area == MSA,]
  denPop <- X$Population/sum(X$Population)
  income <- X$Income
  giniVect <- c(giniVect, gini(income, denPop))
  entVect <- c(entVect, -sum(denPop*log(denPop)))
}

plot(giniVect,entVect)