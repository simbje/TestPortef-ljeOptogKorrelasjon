

###### Test portefølje optimaliserer og Korrelasjon-------------------

#Det du trenger for scriptet-------------

#install.packages("PerformanceAnalytics")
#install.packages("PortfolioAnalytics")
#install.packages("quantmod")
#install.packages("data.table")
#install.packages("tseries")
#install.packages("scales")
#install.packages("ggplot2")
#install.packages("corrplot")

library(quantmod)
library(ggplot2)
library(PerformanceAnalytics)
library(data.table)
library(PortfolioAnalytics)
library(quantmod)
library(ggplot2)
library(corrplot)
library(data.table)
library(scales)

library(tseries)

######

symbols <- c("ATEA.OL","VEI.OL","GOOG","THIN.OL","TSLA","ATVI","FB",
             "OSEBX.OL","BWLPG.OL","YAR.OL","AMZN", "TOM.OL","BRG.OL", "XLV")

getSymbols(symbols, from = "2014-01-01", to= "2016-01-01")
prices <- list()
for(i in 1:length(symbols)) {
  prices[[i]] <- Ad(get(symbols[i]))  
}
prices <- do.call(cbind, prices)
colnames(prices) <- gsub("\\.[A-z]*", "", colnames(prices))
View(prices)

getSymbols("SPY", from="2014-01-01", to= "2016-01-01")
getSymbols("BRNT.L", from="2014-01-01")

SPY <- na.omit(Return.calculate(Ad(SPY)))
BRNT <-na.omit(Return.calculate(Ad(BRNT.L)))

#df<- data.frame(Return.calculate(Ad(BRNT.L))) tull

Tech <- Return.calculate(prices)
Tech <- na.omit(Tech)
charts.PerformanceSummary(Tech, main = "Tech portefølje test")
charts.PerformanceSummary(Tech$ATEA, main = "Tech portefølje test")

stats <- rbind(table.AnnualizedReturns(Tech),
               maxDrawdown(Tech),
               CalmarRatio(Tech),
               SortinoRatio(Tech) * sqrt(252),
               AverageRecovery(Tech),
               VaR(Tech, p=.95, method="historical"))
              # CAPM.beta(Tech, prices$SPY, Rf = 0)) # NOE ER FEIL
round(stats, 3)

#weights<- c(1/7,1/7,1/14,1/14,1/14,1/14,1/14,1/7,1/14,1/7)
weights <- c(1/14,1/14,1/28,1/28,1/28,1/28,1/28,1/14,1/28,1/14, 1/28, 1/14,1/14,1/14)
sum(weights)

TechP <- Return.portfolio(Tech) #weights = weights)


charts.PerformanceSummary(TechP, main = "Tech portefølje test")

stats <- rbind(table.AnnualizedReturns(TechP),
               maxDrawdown(TechP),
               CalmarRatio(TechP),
               SortinoRatio(TechP) * sqrt(252),
               AverageRecovery(TechP),
               VaR(TechP, p=.95, method="historical"))
# CAPM.beta(Tech, prices$SPY, Rf = 0)) # NOE ER FEIL
round(stats, 3)

table.Stats(TechP)
table.CalendarReturns(TechP)

# opTIMAL vekter tseries, 

opt <- portfolio.optim(Tech)
str(opt)

# pw = p weights, pm = expected return,
optim <- rbind(opt$pw,opt$pm,opt$ps)
optim <- colnames("Tech")
round(optim,3)
optW <- c(opt$pw)
optW 

Ret <- apply(Tech,2,"mean")
Vol <- apply(Tech,2,"sd")
plot(Vol, Ret)
text(Vol, Ret, labels = colnames(Tech), cex = 0.7)
abline(h = 0, lty = 3)


Port_optW <- Return.portfolio(Tech,weights = optW)

stats <- rbind(table.AnnualizedReturns(Port_optW),
               maxDrawdown(Port_optW),
               CalmarRatio(Port_optW),
               SortinoRatio(Port_optW) * sqrt(252),
               AverageRecovery(Port_optW),
               VaR(Port_optW, p=.95, method="historical"))
# CAPM.beta(Tech, prices$SPY, Rf = 0)) # NOE ER FEIL
round(stats, 3)

#correlation 
p <- na.omit(prices)
corl <- cor(p) 
corrp <- corrplot(corl, method = "circle", order = "AOE") 








