# 
# Project Objectives:
# 
# Historical sales data for 45 Walmart stores located in different regions are available. There
# are certain events and holidays which impact sales on each day. The business is facing a
# challenge due to unforeseen demands and runs out of stock some times, due to
# inappropriate machine learning algorithm. Walmart would like to predict the sales and
# demand accurately. An ideal ML algorithm will predict demand accurately and ingest
# factors like economic conditions including CPI, Unemployment Index, etc. The objective is
# to determine the factors affecting the sales and to analyze the impact of markdowns
# around holidays on the sales.
# 
# Holiday Events
# Super Bowl: 12-Feb-10, 11-Feb-11, 10-Feb-12, 8-Feb-13
# Labour Day: 10-Sep-10, 9-Sep-11, 7-Sep-12, 6-Sep-13
# Thanksgiving: 26-Nov-10, 25-Nov-11, 23-Nov-12, 29-Nov-13
# Christmas: 31-Dec-10, 30-Dec-11, 28-Dec-12, 27-Dec-13
# 
# Analysis Tasks
# 
# Basic Statistics tasks
# 1) Which store has maximum sales
# 
# 2) Which store has maximum standard deviation i.e., the sales vary a lot. Also, find out the coefficient of mean to standard deviation
# 
# 3) Which store/s has good quarterly growth rate in Q3'2012
# 
# 4) Some holidays have a negative impact on sales. Find out holidays which have higher sales than the mean sales in non-holiday season for all stores together
# 
# 5) Provide a monthly and semester view of sales in units and give insights
# 
# Statistical Model
# For Store 1 - Build prediction models to forecast demand (Linear Regression - Utilize variables like date and restructure dates as 1 for 5 Feb 2010 (starting from the earliest date in order). Hypothesize if CPI, unemployment, and fuel price have any impact on sales.) Change dates into days by creating new variable.
# Select the model which gives best accuracy.

library(ggplot2)
library(dplyr)
library(ggrepel)
library(tis)
library(scales)
library(tidyverse)
library(lubridate)
library(zoo)
library(GGally)
library(ggpubr)
#-- READ DATA ---

current_loc = setwd('C:\\Users\\PriyaSharma\\Downloads\\Practice\\R')

getwd()

raw_data = read.csv('WALMART_SALES_DATA.csv', header = TRUE, sep = ',')


raw_data$Weekly_Sales = as.integer(raw_data$Weekly_Sales)
raw_data$Holiday_Flag = as.factor(raw_data$Holiday_Flag)
raw_data$Date = as.Date(raw_data$Date, format = "%d-%m-%Y")

head(raw_data, 6)

summary(raw_data)
str(raw_data)

#formatC(raw_data$Weekly_Sales, big.mark = ",")
#raw_data$Weekly_Sales = prettyNum(raw_data$Weekly_Sales, format = "d", big.mark = ",") Turned int to char

head(raw_data, 6)

sum(is.na(raw_data))
#1. Which store has maximum sales? Ans: Store 20

salesperstore = raw_data %>% group_by(Store) %>% summarise(TotalSalesPerStore = sum(Weekly_Sales)) %>% arrange(desc(TotalSalesPerStore))
salesperstore

#VISUALIZATION

# sales_plot = ggplot(salesperstore, aes(x = Store, y = TotalSalesPerStore)) +
#   geom_line(colour = "purple", linejoin = "round", size = 1, alpha = 0.7) +
#   xlab("Store Number") + ylab("Sales") + ggtitle("Sales Per Store") +
#   geom_text(aes(label = TotalSalesPerStore), hjust = 0, vjust = 0)

sales_plot = ggplot(salesperstore, aes(x = Store, y = TotalSalesPerStore)) +
  geom_point(color = "black", alpha = 1, shape = 2) +
  geom_line(colour = "purple", linejoin = "round", size = 1, alpha = 0.7) +
  xlab("Store Number") + ylab("Sales") + ggtitle("Sales Per Store") +
  geom_label_repel(aes(label = ifelse(TotalSalesPerStore> 200000000, TotalSalesPerStore, "")), label.size = 0.25, label.r = 0.15, label.padding = 0.5, arrow = arrow(length = unit(0.02, "npc")))
geom_text_repel(aes(), hjust = 0, vjust = 0, box.padding = 0.5, arrow = arrow(length = unit(0.02, "npc")))

sales_plot
#geom_text_repel(nudge_x = ifelse(salesperstore$TotalSalesPerStore > 200000000, TotalSalesPerStore, ""), nudge_y = ifelse(salesperstore$TotalSalesPerStore > 200000000, TotalSalesPerStore, ""))

#2. Maximum standard deviation. Also, find out the coefficient of mean to standard deviation
#Ans: Store 14 has max SD. Store 35 has highest coefficient of mean to SD.

SD_Coeff_perStore = raw_data %>% group_by(Store) %>% 
  summarise(SD = sd(Weekly_Sales), CoeffofVar = sd(Weekly_Sales/mean(Weekly_Sales, na.rm = TRUE))) %>%
  arrange(desc(SD))
SD_Coeff_perStore

SD_perStore = raw_data %>% group_by(Store) %>% 
  summarise(SD = sd(Weekly_Sales)) %>% arrange(desc(SD))

Coeff_perStore = raw_data %>% group_by(Store) %>% 
  summarise(CoeffofVar = sd(Weekly_Sales)/mean(Weekly_Sales, na.rm = TRUE)) %>%
  arrange(desc(CoeffofVar))

#VISUALIZATION

SD_Coeff_Plot = ggplot(SD_Coeff_perStore, aes(x = Store, y = SD)) + 
  geom_line(aes(color = "purple"), size = 1.5, alpha = 0.5, linetype = "solid") +
  theme(legend.position = "none") +
  geom_point(color = "black", alpha = 1, shape = 8) +
  ggtitle("SD Per Store") +
  geom_label_repel(aes(label =round(CoeffofVar, digits = 2)))


SD_Hist_Plot = ggplot(SD_perStore, aes(x = SD)) +
  geom_histogram(aes(y = ..density..), binwidth = 25000, color = "black", fill = "darkmagenta") +
  geom_density(alpha = 0.2, fill = "yellow") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(name = "SD", labels = comma, breaks = pretty_breaks(n = 14))
  #scale_x_continuous(name = "SD", labels = comma, breaks = seq(min(SD_perStore$SD), max(SD_perStore$SD), by = 25000)) #another way to add axis ticks

Coeff_Plot = ggplot(Coeff_perStore, aes(x = CoeffofVar)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "darkmagenta") +
  geom_density(alpha = 0.2, fill = "yellow") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(name = "Coefficient of Mean to SD", labels = comma, breaks = pretty_breaks(n = 14))


# 3) Which store/s has good quarterly growth rate in Q3 2012? Ans:Store 7

#equation = ((Q3 2012 weekly sales - Q2 2012 Weekly Sales)/ Q2 Weekly Sales)*100
# Q3 2012 includes = July 2012, August 2012, September 2012
# Q2 2012 includes = April 2012, May 2012, June 2012

Q3_2012_Sales = filter(raw_data, between(Date, as.Date("2012-07-01"), as.Date("2012-09-30")))
Q2_2012_Sales = filter(raw_data, between(Date, as.Date("2012-04-01"), as.Date("2012-06-30")))

Q3_agg_Sales = aggregate(Weekly_Sales ~ Store, data = Q3_2012_Sales, sum)
Q2_agg_sales = aggregate(Weekly_Sales ~ Store, data = Q2_2012_Sales, sum)

Growth_Rate_perStore = data.frame("Store" = Q3_agg_Sales$Store, "Growth_Rate" = round(((Q3_agg_Sales$Weekly_Sales - Q2_agg_sales$Weekly_Sales)/Q2_agg_sales$Weekly_Sales)*100, 2))

Growth_Rate_perStore[order(-Growth_Rate_perStore$Growth_Rate),]

#VISUALIZATION

Growth_visualize = ggplot(Growth_Rate_perStore, aes(x = Store, y = Growth_Rate)) +
  geom_bar(stat = "identity", color = "steelblue", fill = "darkgreen") +
  scale_x_continuous(name = "Store Number", breaks = breaks_pretty(45)) +
  scale_y_continuous(name = "Growth Rate in %", breaks = breaks_pretty((10))) +
  geom_text(aes(label = paste(Growth_Rate, "%")), vjust = ifelse(Growth_Rate_perStore$Growth_Rate >= 0, -0.5, 2.0), color = "black", size = 2.7) +
  ggtitle("Growth % per Store")

#4) Some holidays have a negative impact on sales. Find out holidays which have higher sales than the mean sales in non-holiday season for all stores together #Super Bowl and Thanksgiving
#Holiday Events
# Super Bowl: 12-Feb-10, 11-Feb-11, 10-Feb-12, 8-Feb-13
# Labor Day: 10-Sep-10, 9-Sep-11, 7-Sep-12, 6-Sep-13
# Thanksgiving: 26-Nov-10, 25-Nov-11, 23-Nov-12, 29-Nov-13
# Christmas: 31-Dec-10, 30-Dec-11, 28-Dec-12, 27-Dec-13
 
Holiday_Sales = raw_data %>% group_by(Store) %>% summarise(Weekly_Sales_Sum = sum(Weekly_Sales[Holiday_Flag==1]))

Holiday_Sales[order(-Holiday_Sales$Weekly_Sales_Sum),] #highest sales during holidays by Store 20.

SuperBowl = data.frame("Holiday" = "Super Bowl", "Date" = as.Date(c("2010-02-12", "2011-02-11", "2012-02-10", "2013-02-08")))
LaborDay = data.frame("Holiday" = "Labor Day", "Date" = as.Date(c("2010-09-10", "2011-09-09", "2012-09-07", "2013-09-06")))
Thanksgiving = data.frame("Holiday" = "Thanksgiving", "Date" = as.Date(c("2010-11-26", "2011-11-25", "2012-11-23", "2013-11-29")))
Christmas = data.frame("Holiday" = "Christmas", "Date" = as.Date(c("2010-12-31", "2011-12-30", "2012-12-28", "2013-12-27")))

Holiday_List = rbind(SuperBowl, LaborDay, Thanksgiving, Christmas)
#HolidayList_wSales = transform(Holiday_List, Sales = ifelse(raw_data$Holiday_Flag == 1, raw_data$Weekly_Sales, "NA"))

HolidayList_wSales = merge(raw_data, Holiday_List, all.y = TRUE)

sum(is.na(HolidayList_wSales)) #42 null values

Holiday_Sales = HolidayList_wSales %>% group_by(Holiday) %>% summarize(WeeklySales = sum(Weekly_Sales, na.rm = TRUE)) %>% arrange(desc(WeeklySales))


ggplot(Holiday_Sales, aes(y = WeeklySales, x = Holiday)) +
  geom_bar(stat = "identity", position = "dodge", fill = "SteelBlue") +
  scale_x_discrete() +
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 10)) +
  ggtitle("Sales During Holidays")

#Super Bowl has the highest sales out of all holidays.

PositiveImpactonSales_bHolidays = HolidayList_wSales %>% group_by(Holiday) %>% 
  summarize(WeeklySales_Holiday = mean(Weekly_Sales, na.rm = TRUE), WeeklySales_NotHoliday_AllStores = ifelse(raw_data$Holiday_Flag == 0, mean(raw_data$Weekly_Sales, na.rm = TRUE), "NA")) %>%
  distinct(Holiday, .keep_all = TRUE) %>% mutate(WeeklySales_NotHoliday_AllStores = as.numeric(WeeklySales_NotHoliday_AllStores))

Holidays_PosImpact = select(PositiveImpactonSales_bHolidays[PositiveImpactonSales_bHolidays$WeeklySales_Holiday >= PositiveImpactonSales_bHolidays$WeeklySales_NotHoliday_AllStores,])


# 5) Provide a monthly and semester (quarterly) view of sales in units and give insights

Monthly_Quarterly_Sales = transform(raw_data, Date = format(as.Date(raw_data$Date), "%m-%Y"), Month = format(as.Date(raw_data$Date), "%B"), 
                                    Quarter = quarters(as.Date(raw_data$Date)) , Year = format(as.Date(raw_data$Date), "%Y")) 

#you can use the format portion of this statement in the aggregate function but couldn't rename it which is why I broke this down in two parts
#aggregate(Weekly_Sales ~ format(as.Date(raw_data$Date), "%m-%Y")+Store, Monthly_Sales, sum)

Monthly_Sales_View = aggregate(Weekly_Sales ~ Date+Month+Year+Store, Monthly_Quarterly_Sales, sum)


ggplot(Monthly_Sales_View, aes(fill = Year, y = Weekly_Sales, x = Month)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(limits = month.name) +
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 10)) +
  ggtitle("Monthly View of Sales") +
  scale_fill_brewer(palette = "Paired")

#December Month has the most sales. January the least probably because of high sales in December. Sales/holidays end by January.

Quarterly_Sales_View = aggregate(Weekly_Sales ~ Quarter+Year+Store, Monthly_Quarterly_Sales, sum)

ggplot(Quarterly_Sales_View, aes(fill = Year, y = Weekly_Sales, x = Quarter)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = comma, breaks = pretty_breaks(n = 10)) +
  ggtitle("Quarterly View of Sales") +
  scale_fill_brewer(palette = "Paired")

#Q4 had the highest sales in 2010 and 2011 but the lowest sales in 2012. Q2 and Q3 had no significant difference
#in their sales from one year to the next. Q1's sales increased each year from 2010 to 2012.

#Statistical Model
# For Store 1 - Build prediction models to forecast demand (Linear Regression - Utilize variables like date and restructure dates as 1 for 5 Feb 2010 (starting from the earliest date in order). Hypothesize if CPI, unemployment, and fuel price have any impact on sales.) Change dates into days by creating new variable.
# Select the model which gives best accuracy.

pairs(raw_data[,3:8], col = raw_data$Store == 1)

ggpairs(raw_data, columns = 3:8)

Test_Data = subset(raw_data, Store == 1)
Train_Data = subset(raw_data, Store != 1)

#Null Hypothesis 1: There is positive relationship between unemployment and sales.

#Null Hypothesis 2: Positive relationship between CPI and sales.

#Null Hypothesis 3: There is a positive relationship between Fuel Price and sales.

#Null Hypothesis 1 --


fit_unemployment <- lm(Weekly_Sales ~ Unemployment, data = Train_Data)


ggplot(data = Train_Data, aes(x = Unemployment, y = Weekly_Sales)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")

summary(fit_unemployment)

cor(Train_Data$Weekly_Sales, Train_Data$Unemployment)

#We reject null hypothesis 1 because there seems to be a negative relationship between 
#sales and unemployment. Also, there is a negative correlation between the two: -0.103.

#Null Hypothesis 2 --

fit_CPI <- lm(Weekly_Sales ~ CPI, data = Train_Data)


ggplot(data = Train_Data, aes(x = CPI, y = Weekly_Sales)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")

summary(fit_CPI)

cor(Train_Data$Weekly_Sales, Train_Data$CPI)

#We reject null hypothesis 2 because there seems to be a negative relationship between 
#sales and CPI Also, there is a negative correlation between the two: -0.099.

#Null Hypothesis 3 --

fit_FuelPrice <- lm(Weekly_Sales ~ Fuel_Price, data = Train_Data)


ggplot(data = Train_Data, aes(x = Fuel_Price, y = Weekly_Sales)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data")

summary(fit_FuelPrice)

cor(Train_Data$Weekly_Sales, Train_Data$Fuel_Price)

#We reject null hypothesis 3 because there seems to be no relationship between 
#sales and fuel price. Also, there is a no correlation between the two: 0.0153

#Testing prediction models on the test data.

#Predicting via fit_unemployment

predict(fit_unemployment, data.frame(Unemployment = 8)) #sales = 1,035,657.

filter(Test_Data, between(Test_Data$Unemployment, 8.0, 9.0)) %>% summarise(mean_salary = mean(Weekly_Sales)) #1.522,329

#Difference between actual and predicted = 1.522,329 - 1,035,657 = 486,672.

#Predicting via fit_CPI

predict(fit_CPI, data.frame(CPI = 210)) #sales = 978,143.

filter(Test_Data, between(Test_Data$CPI, 210, 211)) %>% summarise(mean_salary = mean(Weekly_Sales)) #1,526,701

#Difference between actual and predicted = 1,526,701 - 978,143 = 548,558.

#Predicting via fit_FuelPrice

predict(fit_FuelPrice, data.frame(Fuel_Price = 3.5)) #sales = 1,038,008

filter(Test_Data, between(Test_Data$Fuel_Price, 3.5, 4.0)) %>% summarise(mean_salary = mean(Weekly_Sales)) #1,561,592

#Difference between actual and predicted = 1,561,592 - 1,038,008 = 523,584

#fit_unemployment gives the best accuracy.

