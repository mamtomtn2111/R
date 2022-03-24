install.packages("quantmod")
library(quantmod)
getSymbols("AAPL")

head(AAPL)
tail(AAPL)

Open <- Op(AAPL)
Close <- Cl(AAPL)

Volume <- Vo(AAPL)
AAPL <- last(AAPL, "1 year")
head(AAPL)

AAPL <- first(AAPL, '3 years')
head(AAPL)

getSymbols(c("AAPL","GOOG"))
stocklist <- c("AAPL","GOOG","VIC")
getSymbols(stocklist)
chartSeries(VIC, type = "line")
chartSeries(VIC, type = "bar", subset = "2012-05::2012-06")
