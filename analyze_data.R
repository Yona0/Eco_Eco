library(reldist)
# Install using these directions:
# http://bioconductor.org/packages/release/bioc/html/BioCor.html
#library(BioCor)

selectRandomPairs <- function(numCities,numPairsSamp) {
    numPairsTot <- choose(numCities,2)
    sampIndexVect <- sample.int(numPairsTot,numPairsSamp)
    pairMatrix <- matrix(NA,numPairsSamp,2)
    fastCounter <- 1
    slowCounter <- 1
    for(n1 in 1:(numCities-1)) {
      for(n2 in (n1+1):numCities) {
        if(fastCounter %in% sampIndexVect) {
          # Then add the pair to matrix
          pairMatrix[slowCounter,] <- c(n1,n2)
          slowCounter <- slowCounter + 1
	}
        fastCounter <- fastCounter + 1
      }
    }
    return(pairMatrix)
}


#10. Use this dataframe
Data <- read.csv("intermediate_data.csv",stringsAsFactors=F)

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

numPairsTot <- choose(length(MSAs),2)
numPairsSamp <- length(MSAs)
#sampIndexVect <- sample.int(numPairsTot,numPairsSamp)
pairMatrix <- selectRandomPairs(length(MSAs),numPairsSamp)

entVectVirtual <- rep(NA,numPairsSamp)
giniVectVirtual <- rep(NA,numPairsSamp)
for(ii in 1:numPairsSamp) {
  #pair <- combinadic(1:length(MSAs),2,sampIndexVect[ii])
  pair <- pairMatrix[ii,]
  print(pair)
  MSA1 <- MSAs[pair[1]]
  MSA2 <- MSAs[pair[2]]
  X1 <- Data[Data$Area == MSA1,]
  X2 <- Data[Data$Area == MSA2,]
  jobs <- sort(unique(c(X1$OCC,X2$OCC)))
  N1 <- rep(0,length(jobs))
  N2 <- N1
  M1 <- N1
  M2 <- N1
  for(jj in 1:length(jobs)) {
    if(sum(X1$OCC == jobs[jj])) {
      # In first city
      N1[jj] <- X1[X1$OCC == jobs[jj],"Population"]
      M1[jj] <- X1[X1$OCC == jobs[jj],"Income"]
    }
    if(sum(X2$OCC == jobs[jj])) {
      # In second city
      N2[jj] <- X2[X2$OCC == jobs[jj],"Population"]
      M2[jj] <- X2[X2$OCC == jobs[jj],"Income"]
    }
  }
  N <- N1 + N2
  M <- (N1*M1 + N2*M2)/(N1+N2)
  p <- N/sum(N)
  giniVectVirtual[ii] <- gini(M,p)
  entVectVirtual[ii] <- -sum(p*log(p))
}

xMin <- min(giniVect,giniVectVirtual)
yMin <- min(entVect,entVectVirtual)
xMax <- max(giniVect,giniVectVirtual)
yMax <- max(entVect,entVectVirtual)

plot(giniVect, entVect,xlim=c(xMin,xMax),ylim=c(yMin,yMax))
points(giniVectVirtual,entVectVirtual,col="red")

##19. Make virtual MSAs (in process)
#for(MSA in MSAs) {
#  X1 <- Data[Data$Area == MSA,]
#  X2 <- Data[Data$Area == MSA,], X1 != X2
#  X1[sample(nrow(X1), 100)]
#  X3 <- X1$Population+X2$Population
#}
