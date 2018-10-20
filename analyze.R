#Tiffany Tsang
#Two Six Capital Data Challenge
#October 19, 2018

library(zoo)

datadir <- "/Users/tiffanyimovie/Desktop/coding-challenges/two-six/"
setwd(datadir)

data <- read.csv(file="data_challenge_transactions.csv", header=TRUE, sep=",")

#Part 1

#total new users per year
total.new.users.per.year <- table(format(as.Date(data[,4], "%m/%d/%y"),"%Y"))
plot(total.new.users.per.year,type="o",xlab = "Year",ylab = "New Users")

#total sales amount per year
total.sales.per.year <- aggregate(sales_amount~format(as.Date(data[,2], "%m/%d/%y"),"%y"),data,sum)
barplot(total.sales.per.year$sales_amount,
        xlab = "Year", ylab = "Total sales amount, USD ($)",
        names.arg=total.sales.per.year[,1])

#Part 2

#total transactions per region
total.transactions.per.region <- table(data$region)
barplot(total.transactions.per.region,
        xlab = "Region",
        ylab = "Total transactions",
        names.arg=total.sales.per.region[,1])
total.transactions.per.region.sorted <- sort(table(data$region),decreasing=T)

#average sales amount per region
average.sales.per.region <- aggregate(sales_amount~region,data,mean)
barplot(average.sales.per.region$sales_amount,
        xlab = "Region",
        ylab = "Average sale amount, USD ($)",
        names.arg=total.sales.per.region[,1])

#total sales amount per region
total.sales.per.region <- aggregate(sales_amount~region,data,sum)
barplot(total.sales.per.region$sales_amount,
        xlab = "Region",
        ylab = "Total sales",
        names.arg=total.sales.per.region[,1])

#Part 3
quarter <- as.Date(as.yearqtr(as.Date(data[,2], "%m/%d/%y")))

#total transactions per season
total.transactions.per.season <- table(format(quarter,"%m"))
barplot(total.transactions.per.season,
        xlab = "Season",
        ylab = "Total transactions",
        names.arg=c("Jan-Mar","Apr-June","July-Aug","Oct-Dec"))

#total sales amount per transaction per season
total.sales.per.season <- aggregate(sales_amount~format(quarter,"%m"),data,sum)
barplot(total.sales.per.season$sales_amount,
        xlab = "Season",
        ylab = "Total sales amount, USD ($)",
        names.arg=c("Jan-Mar","Apr-June","July-Aug","Oct-Dec"))

#average sales amount per transaction per season
average.sales.per.season <- aggregate(sales_amount~format(quarter,"%m"),data,mean)
barplot(average.sales.per.season$sales_amount,
        xlab = "Season",
        ylab = "Average sales amount, USD ($)",
        names.arg=c("Jan-Mar","Apr-June","July-Aug","Oct-Dec"))

#Part 4

#split all data by year
y2013 <- data[format(as.Date(data[,2], "%m/%d/%y"),"%y") == 13, ]
y2014 <- data[format(as.Date(data[,2], "%m/%d/%y"),"%y") == 14, ]
y2015 <- data[format(as.Date(data[,2], "%m/%d/%y"),"%y") == 15, ]
y2016 <- data[format(as.Date(data[,2], "%m/%d/%y"),"%y") == 16, ]

#find total transactions and average sales for 2013
users2013.transactions <- table(y2013$user)
users2013.transactions <- mean(users2013)
users2013.average.sales <- aggregate(sales_amount~user,y2013,mean)
users2013.average.sales <- mean(users2013.average.sales[,2])

#find total transactions and average sales for 2014
users2014.transactions <- table(y2014$user)
users2014.transactions <- mean(users2014)
users2014.average.sales <- aggregate(sales_amount~user,y2014,mean)
users2014.average.sales <- mean(users2014.average.sales[,2])

#find total transactions and average sales for 2015
users2015.transactions <- table(y2015$user)
users2015.transactions <- mean(users2015)
users2015.average.sales <- aggregate(sales_amount~user,y2015,mean)
users2015.average.sales <- mean(users2015.average.sales[,2])

#find total transactions and average sales for 2016
users2016.transactions <- table(y2016$user)
users2016.transactions <- mean(users2016)
users2016.average.sales <- aggregate(sales_amount~user,y2016,mean)
users2016.average.sales <- mean(users2016.average.sales[,2])

#create list that associates transactions and average sales to a given year
customer.quality.transactions <- c(users2013.transactions,users2014.transactions,users2015.transactions,users2016.transactions)
customer.quality.average.sales <- c(users2013.average.sales,users2014.average.sales,users2015.average.sales,users2016.average.sales)
year.vector <- c(2013,2014,2015,2016)
customer.quality <- cbind(year.vector, customer.quality.transactions, customer.quality.average.sales)
write.csv(customer.quality, "customer.quality.csv")




#START RANDOM CODE
barplot(total.new.users.per.year,
        las = 2, xlab = "Year", ylab = "Total new users")
total.new.users.per.year.sorted <- sort(table(format(as.Date(data[,4], "%m/%d/%y"),"%Y")),decreasing=T)
write.csv(total.new.users.per.year.sorted, "total_new_users.csv")

ggplot(data=as.data.frame(total.new.users.per.year), aes(x=total.new.users.per.year[,1], y=total.new.users.per.year[,2], group=1)) +
  geom_line()+
  geom_point()

#average sales amount per transaction per month
total.sales.per.month <- aggregate(sales_amount~format(as.Date(data[,2], "%m/%d/%y"),"%m/%y"),data,sum)
barplot(total.sales.per.month$sales_amount,
        las = 2,
        ylab = "Total sales amount",
        names.arg=as.yearqtr(as.Date(total.sales.per.month[,1],"%y%m/%d"),"%q%Y"))

#data$user to get list of users. equivalen to data[,1]
#total.sales.per.year <- aggregate(sales_amount~format(as.Date(data[,2], "%m/%d/%y"),"%m/%y"),data,sum)
#small.data <- data[1:10,]

#average sales amount per transaction per year
average.sales.per.transaction.per.year <- aggregate(sales_amount~format(as.Date(data[,2], "%m/%d/%y"),"%Y"),data,mean)
barplot(total.new.users.per.year,
        las = 2, xlab = "Year", ylab = "Total new users")
total.new.users.per.year.sorted <- sort(table(format(as.Date(data[,4], "%m/%d/%y"),"%Y")),decreasing=T)
write.csv(total.new.users.per.year.sorted, "total_new_users.csv")

ggplot(data=as.data.frame(total.new.users.per.year), aes(x=total.new.users.per.year[,1], y=total.new.users.per.year[,2], group=1)) +
  geom_line()+
  geom_point()
barplot(average.sales.per.transaction.per.year$sales_amount, 
        xlab = "Year",
        ylab = "Average transaction amount",
        names.arg=average.sales.per.transaction.per.year[,1])

#average sales amount per user
average.sales.per.user <- aggregate(sales_amount~user,data,mean)
average.sales.per.user <- mean(average.sales.per.user$sales_amount)


#total sales amount per user
total.sales.per.user <- aggregate(sales_amount~user,data,sum)
total.sales.per.user <- mean(total.sales.per.user$sales_amount)

#average number of transactions per user

total.sales.per.month <- aggregate(sales_amount~format(as.Date(data[,2], "%m/%d/%y"),"%m/%y"),data,sum)
format(quarter,"%m") == "01"
#total sales amount per quarter
total.sales.per.quarter <- aggregate(sales_amount~quarter,data,sum)
barplot(total.sales.per.quarter$sales_amount,
        las = 2,
        ylab = "Total sales amount",
        names.arg=as.yearqtr(as.Date(total.sales.per.quarter[,1],"%y%m/%d"),"%q%Y"))

#END RANDOM