# Black Sholes Model of option pricing
# To call this program, use:
# source(paste0('C:/Users/', Sys.getenv('USERNAME'), '/OneDrive - Rock Creek Group/Research/R Programs/Quant Models/', sep = ''))

# S = Stock Price
# K = Strike Price at Expiration 
# r = Risk-free Interest Rate
# T = Time to Expiration
# sig = Volatility of the Underlying asset

#Function
BlackScholes <- function(S, K, r, T, sig, type){
  
  if(type=="C"){
    d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    
    value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
    return(value)}
  
  if(type=="P"){
    d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    
    value <-  (K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1))
    return(value)}
}



#Example - random numbers

call <- BlackScholes(110,100,0.04,1,0.2,"C")
put <- BlackScholes(110,100,0.04,1,0.2,"P")

print(call)
print(put)


#Example - AAPL using Quandl

data <- Quandl("WIKI/AAPL.4")

# Retrieve the first 50 values
recent_data <- data[1:50,]

# Use the arrange function from dplyr package to get old values at top. This would guarantee that the calculations are performed with the oldest values of the series at first.
recent_data <- arrange(recent_data, -row_number())

# Add a new column with the returns of prices. Input NA to the first value of the column in order to fit the column in the recent_data data frame.
recent_data$returns <- c('NA',round(diff(log(recent_data$Close)),3))

# Convert the column to numeric
recent_data$returns <- as.numeric(recent_data$returns)

# Calculate the standard deviation of the log returns
Standard_deviation <- sd(recent_data$returns,na.rm=TRUE)
print(Standard_deviation)

#Convert Std. Dev. measure to year values using 250 trading days in a year
annual_sigma <- standard_deviation * sqrt(250)


#Calculate Implied Vol 

S <- 50
K <- 70
C <- 1.5 
r <- 0
Time <- 1
sigma <- seq(0.01,0.5,0.005)

# The variable CallValueperSigma would store all the option prices that are 
# calculated with the different values of sigma along the sigma vector. 
CallValueperSigma <- BlackScholes(S,K,r,Time,sigma, "C") 


# IV is the the min value of the difference between each value of the  #CallValueperSigma  vector and the call price. 
IV <- which.min(abs(CallValueperSigma-C))/2 

print(IV)

#The “which” command would return the index of the value in the sigma vector that #correspond to the option price derived from the BlackScholes  function  that is #nearest respect  the call market price. 
#This value is divided by two in order to get the specific Implied Volatility value from the sigma vector, because sigma starts from 0 to 0.5.
