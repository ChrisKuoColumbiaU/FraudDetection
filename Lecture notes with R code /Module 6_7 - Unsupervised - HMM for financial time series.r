
#install.packages('depmixS4')
#install.packages('quantmod')
#install.packages("Metrics")
library('depmixS4')
library('quantmod')
set.seed(1)

# bear market returns distributions
bear_mean <- -0.05
bear_var <- 0.2

# bear market returns distributions
bull_mean <- 0.1
bull_var <- 0.1

# Assume the lengths of days can vary from 30 days to 200 days.
days <- replicate(4, sample(30:200, 1))

# Create the various bull and bear markets returns
market_bull_1 <- rnorm( days[1], bull_mean, bull_var )
market_bear_2 <- rnorm( days[2], bear_mean, bear_var )
market_bull_3 <- rnorm( days[3], bull_mean, bull_var )
market_bear_4 <- rnorm( days[4], bear_mean, bear_var )

# Create the list of true regime states and full returns list
regime_index <- c( rep(1,days[1]), rep(2,days[2]), rep(1,days[3]), rep(2,days[4]))
stock_rtns <- c( market_bull_1, market_bear_2, market_bull_3, market_bear_4)
plot(stock_rtns, type="l", xlab='', ylab="Returns")

# Specify the model
hmm <- depmix(data=data.frame(stock_rtns), 
              stock_rtns ~ 1, 
              family = gaussian(), 
              nstates = 2,
             )
# Fit the model by calling the "fit" function
hmmfit <- fit(hmm, verbose = FALSE)
hmmfit

# plot posterior state sequence for the 2-state model
post_probs <- posterior(hmmfit)
head(post_probs)

layout(1:2)
# Plot the true regimes
matplot(post_probs$state, type='l', main='True Regimes',xlab='', ylab='Regime')
# Plot the probabilities of the regimes
matplot(post_probs[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
legend(x='right', c('Regime #1','Regime #2'), fill=1:2, bty='n')

# Obtain AAPL data from 2001 onwards and
# create the returns stream from this
getSymbols( "AAPL", from="2014-01-01" )
Rtns = diff( log( Cl( AAPL ) ) )
Rtns = as.numeric(Rtns)
plot(Rtns, type="l", xlab='', ylab="Returns")

stock_hmm <- depmix(data=data.frame(Rtns), 
                Rtns ~ 1, 
                family = gaussian(), 
                nstates = 2,
             )
stock_hmmfit <- fit(stock_hmm, verbose = FALSE)
post_probs <- posterior(stock_hmmfit)

# Plot 
layout(1:2)
plot(Rtns, type='l', main='Regimes', xlab='', ylab='Returns')
matplot(post_probs[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
legend(x='right', c('Regime #1','Regime #2'), fill=1:2, bty='n')
