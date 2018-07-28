
data(iris)
head(iris)

iris.pca <- prcomp(iris[,1:4],
                 center = TRUE,
                 scale. = TRUE) 
print(iris.pca)
summary(iris.pca)

# plot method
plot(iris.pca, type = "l")

library(quantmod)
tckrs <- c("SPY", "QQQ", "AMZN", "GOOG", "WMT","LMT")
getSymbols(tckrs, from = "2015-01-01")

SPY.Close <- SPY[,4]
QQQ.Close <- QQQ[,4]
AMZN.Close <- AMZN[,4]
GOOG.Close <- GOOG[,4]
WMT.Close <- WMT[,4]
LMT.Close <- LMT[,4]

SPY1 <- as.numeric(SPY.Close[1])
QQQ1 <- as.numeric(QQQ.Close[1])
AMZN1 <- as.numeric(AMZN.Close[1])
WMT1 <- as.numeric(WMT.Close[1])
LMT1 <- as.numeric(LMT.Close[1])

SPY <- SPY.Close/SPY1
QQQ <- QQQ.Close/QQQ1
AMZN <- AMZN.Close/AMZN1
WMT <- WMT.Close/WMT1
LMT <- LMT.Close/LMT1

all <- cbind(SPY, QQQ, AMZN, WMT,LMT)
head(all)

# The xts package is an extension of the zoo package, so coercing our xts object basket to a zoo object is a simple task:

all.z <- as.zoo(all)

# Set a color scheme:
ts <- rainbow(ncol(all.z))
# Plot the overlayed series
plot(x = all.z, ylab = "Cumulative Return", main = "Cumulative Returns",
        col = ts, screens = 1)
# Set a legend in the upper left hand corner to match color to return series
legend(x = "topleft", legend = c("SPY", "QQQ", "AMZN", "WMT","LMT"), 
       lty = 1,col = ts)

all.pca <- prcomp(all.z,
                 center = TRUE,
                 scale. = TRUE) 
print(all.pca)
summary(all.pca)
