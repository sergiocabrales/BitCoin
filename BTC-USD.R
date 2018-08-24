# INGENIERÍA FINANCIERA
# Sergio Cabrales s-cabral@uniandes.edu.co
#################################################

install.packages('quantmod')
library("quantmod")

# BITCOIN
BITCOIN <- getSymbols("BTC-USD",from="2017-01-01", warnings = FALSE, auto.assign=FALSE)

# Omitir Missing Data

BITCOIN <- na.omit(BITCOIN)

# daily,weekly,monthly,quarterly, and yearly 
dailyReturn(BITCOIN) # returns by day 
weeklyReturn(BITCOIN) # returns by week 
monthlyReturn(BITCOIN) # returns by month, indexed by yearmon 

plot(dailyReturn(BITCOIN))
plot(monthlyReturn(BITCOIN))

allReturns(BITCOIN) # note the plural

#Charting
barChart(BITCOIN)
candleChart(BITCOIN,theme='white', type='candles') 

addBBands()
addRSI()
addMACD()
addCMF()

# Return and Volatility I
ret_BITI <- dailyReturn(BITCOIN,type='log')
vol_BI <- sd(ret_BITI)*sqrt(252)
vol_BI

# Black-Scholes Option Value
blackscholes <- function(S, K, rf, T, sigma) {
  values <- c(2)
  d1 <- (log(S/K)+(rf+sigma^2/2)*T)/(sigma*sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  values[1] <- S*pnorm(d1) - K*exp(-rf*T)*pnorm(d2)
  values[2] <- K*exp(-rf*T) * pnorm(-d2) - S*pnorm(-d1)
  values
}

# Black-Scholes
blackscholes(16.36,16.50,0.0227,1,vol/100)
