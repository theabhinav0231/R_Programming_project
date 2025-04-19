library(quantmod)

# Fetch stock data for Apple Inc.
getSymbols("AAPL", src = "yahoo", from = "2022-01-01", to = Sys.Date())

# Display first few rows
head(AAPL)

# Calculate 50-day Simple Moving Average
sma50 <- SMA(Cl(AAPL), n = 50)

# Calculate 200-day Simple Moving Average
sma200 <- SMA(Cl(AAPL), n = 200)

# Plot the stock price chart with moving averages
chartSeries(AAPL, theme = chartTheme("white"), TA = NULL)
addSMA(n = 50, col = "blue")
addSMA(n = 200, col = "red")