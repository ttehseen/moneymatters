library(lubridate)

#Import the data into R

rawData<-rawData[-c(59:64),] #remove the unnecessary rows.
#Remove the unnecessary rows at the start of the data.



#let's add the column heads
names(rawData) <- rawData[1,]
rawData <- rawData[-1,]

# Let's convert the data type.
rawData[,c(3,4)] <- sapply(rawData[,c(3,4)], as.numeric)

# We also have the date type values in there that we need to covert.
rawData$`Transaction Date` <- dmy(rawData$`Transaction Date`)

# Let's find the most expensive transaction in this month.

biggestPurchase<-max(rawData$`Debit Amount`, na.rm = T)

rawData[which(rawData$`Debit Amount` %in% biggestPurchase),]

# Not surprisingly, it is an airline ticket.

# Now we can also chart how much the person has spent. The data is already sorted by date. 
# We like that. A first step could be to get the cumulative expenditure at the end of each day.

aggregate(rawData$`Debit.Amount`, by=list(Transaction.Date=rawData$`Transaction.Date`),
          FUN=sum)
newdata <- aggregate(onlyexpenditure$Debit.Amount, 
                     by=list(Date=onlyexpenditure$Transaction.Date), FUN=sum)

names(newdata) <- c("Date", "Cost")

View(newdata)

newdata$CumSum <- cumsum(newdata$Cost)

newdata

ggplot(data = newdata , aes(Date, CumSum)) + 
  geom_point(col="blue") + 
  geom_line(col="light blue") + 
  geom_smooth(se = F, color="dark green", alpha = 0.1) +
  ylab("$") + 
  ggtitle("Daily Expenditure")

  newdata$CumSum[,21]
  
