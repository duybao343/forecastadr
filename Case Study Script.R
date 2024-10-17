library(dplyr)
library(ggplot2)
library(caret)
library(superml)
df <- read.csv("G:/My Drive/Agoda/Case Study/Data Splitting/Case_Study_Urgency_Message_Data_Mod6.csv")

#Data Checks:
#Check if there is any missing value
any(is.na(df))
#Count missing value from the entire dataframe
sum(is.na(df))
#Overview of null value from each column
colSums(is.na(df)) 

#Remove Missing Values:
#Typically, there are 3 most common techniques to deal with missing values, i.e removing it, replacing with constants, or predicting using algorithms. In this case, we only have 4 missing values for children attribute, which is a very small value, thus we can simply remove them.
df <- na.omit(df)
#Recheck on missing value
any(is.na(df))

#Duped Records:
#Check if there is any duplicate record
any(duplicated(df))
#Count duplicated rows
sum(duplicated(df))
#Identify number of rows before handling the duplicated rows
nrow(df)
# Handle the duplicated rows by removing it
df <- distinct(df)
#Check number of rows after removing duplicated rows
nrow(df) 
#Recheck on duplicated rows
any(duplicated(df)) 

#Data Validation:
#Number of stays in weekend and weekdays also cannot be 0, considering hotel booking is count per night
nrow(df[df$length_stay== 0 ,])
#Chain or Non-chain only
nrow(df[df$chain_hotel== "chain"|df$chain_hotel=="non-chain",])-nrow(df[])
#lead_time and length_stay > or = to 0
nrow(df[df$lead_time<0,])
nrow(df[df$length_stay<0,])
#count rows before removal
nrow(df)
#remove invalid lead_time rows
df <- df[df$lead_time>=0,]
df <- df[df$length_stay>=0,]
#Check number of rows after removing invalid rows
nrow(df)

df_basic_info <- subset(df, select=c(hotel_id,city_id,star_rating,accommadation_type_name,chain_hotel))
dim(df)
dim(df_basic_info)
df_basic_info_dist <- distinct(df_basic_info)
dim(df_basic_info_dist)

df_hotel <- subset(df, select=c(hotel_id))
df_hotel_dist <- distinct(df_hotel)
dim(df_hotel_dist)

# Convert some of the numeric variables into factors
df$star_rating <- as.factor(df$star_rating)
df$accommadation_type_name <- as.factor(df$accommadation_type_name)
df$city_name <- as.factor(df$city_name)
df$chain_hotel <- as.factor(df$chain_hotel)
#Rearrange order of the levels of the arrival_date_month factor so they are  in chronological order when plotting them
df$arrival_month <- factor(df$arrival_month, levels=c("Oct", "Nov", "Dec"))
df$booking_month <- factor(df$booking_month, levels=c("Aug", "Sep", "Oct", "Nov", "Dec"))


# remove unused  data
df_reg <- subset(df, select= -c(hotel_id,booking_ID,city_id,ID))
dim(df_reg)
summary(df_reg)

##skip to regression if desired





#Exploratory Data Analysis
city_df <- df[]
city_df <- head(city_df %>% count(city_name,sort=TRUE),n=5)
city_df
# Plot the bar chart 
barplot(city_df$n,names.arg=city_df$city_name,xlab="City",ylab="Number of Reservation",col="yellow",
        main="Top 5 Reservation based on City",border="red")

city_adr <- df[]
city_adr <- city_adr %>% group_by(city_name) %>% 
  summarise(RevCity = sum(ADR_USD))
city_adr
# Plot the bar chart 
barplot(city_adr$RevCity,names.arg=city_adr$city_name,xlab="City",ylab="Revenue",col="green",
        main="Revenue based on City",border="blue")

city_lead <- df[]
city_lead <- city_lead %>% group_by(city_name) %>% 
  summarise(lead = mean(lead_time))
city_lead
# Plot the bar chart 
barplot(city_lead$lead,names.arg=city_adr$city_name,xlab="City",ylab="Average Lead Time",col="red",
        main="Average Lead Time based on City",border="black")

city_adrv <- df[]
city_adrv <- city_adrv %>% group_by(city_name) %>% 
  summarise(adrv = mean(ADR_USD))
city_adrv
# Plot the bar chart 
barplot(city_adrv$adrv,names.arg=city_adr$city_name,xlab="City",ylab="Average ADR",col="green",
        main="Average ADR based on City",border="black")

# Extract data where guest completed their stay in hotel
ar <- df[]
ar <- select(df, hotel_id, arrival_month, accommadation_type_name, ADR_USD)

ar <- ar %>%
  group_by(accommadation_type_name) %>%
  summarise(Revenue = sum(ADR_USD))
ar <- ar[order(ar$Revenue, decreasing=TRUE),]
ar <- head(ar, 5)

# Get the library.
library(plotrix)

# Plot the chart.
pie(ar$Revenue,labels = ar$accommadation_type_name, main = "Hotel Revenue By Type")

# Extract data where guest completed their stay in hotel
cr <- df[]
cr <- select(df, hotel_id, arrival_month, accommadation_type_name, ADR_USD)

cr <- cr %>%
  group_by(accommadation_type_name) %>%
  summarise(AVGADR = mean(ADR_USD))
cr <- cr[order(cr$AVGADR, decreasing=TRUE),]
cr <- head(cr, 5)

# Get the library.
library(plotrix)

# Plot the chart.
pie(cr$AVGADR,labels = cr$accommadation_type_name, main = "Average ADR By Type")


#Regression:
# Convert some of the numeric variables into factors
df$star_rating <- as.factor(df$star_rating)
df$accommadation_type_name <- as.factor(df$accommadation_type_name)
df$city_name <- as.factor(df$city_name)
df$chain_hotel <- as.factor(df$chain_hotel)
#Rearrange order of the levels of the arrival_date_month factor so they are  in chronological order when plotting them
df$arrival_month <- factor(df$arrival_month, levels=c( "Oct", "Nov", "Dec"))
df$booking_month <- factor(df$booking_month, levels=c( "Aug", "Sep", "Oct", "Nov", "Dec"))

library(gridExtra)
ADR_plot <- ggplot(df_reg, aes(x=accommadation_type_name, y=ADR_USD)) + 
  geom_boxplot() + 
  theme_light() + 
  ggtitle("Average daily rate vs. Type") + 
  xlab("Type") + 
  ylab("Average daily rate (adr)")
ADR_plot

df_reg_type <- read.csv("G:/My Drive/Agoda/Case Study/Data Splitting/Case_Study_Urgency_Message_Data_vType.csv")
library(gridExtra)
ADR_plot <- ggplot(df_reg_type, aes(x=accommadation_type_name, y=ADR_USD)) + 
  geom_boxplot() + 
  theme_light() + 
  ggtitle("Average daily rate vs. Type") + 
  xlab("Type") + 
  ylab("Average daily rate (adr)")
ADR_plot

library(ggplot2)
ggplot(df_reg, aes(x=ADR_USD)) + 
  geom_histogram(aes(y=after_stat(density)), binwidth=5, colour="black", fill="lightgray") + 
  geom_density(alpha=.1, fill="#FF6666") + 
  theme_light() + 
  ggtitle("Histogram of average daily rate (adr)") + 
  xlab("average daily rate (adr)")

nrow(df[df$ADR_USD>1000,])

#Explore associations among the different variables and adr.
library(PerformanceAnalytics) 
# Plot correlations among numeric variables
num_df_reg <- select_if(df_reg, is.numeric) #Pick numeric variables
# dim(num_df_resort) #10 numeric variables

# Plot correlations and histograms using PerformanceAnalytics library
chart.Correlation(num_df_reg, histogram=TRUE, pch=19)
#observations: ADR & length of stay have a non-linear trend where adr is highest for a medium term stay; ADR and Lead Time exhibit a significant positive correlation (r=0.11).
#Lead time and length of stay also exhibit a significant positive correlation (r=0.19)

# Number of adults, number of children, total of special requests
ggplot(df_reg, aes(x=lead_time, y=ADR_USD)) + geom_point() + theme_light()
ggplot(df_reg, aes(x=length_stay, y=ADR_USD)) + geom_point() + theme_light()

ggplot(df_reg, aes(x=star_rating, y=ADR_USD)) + geom_point() + theme_light()

# Explore full arrival date trends
ggplot(df_reg, aes(x=checkin_date, y=ADR_USD)) + 
  geom_point() + 
  theme_light() + 
  ggtitle("Average daily rate vs. Checkin date") + 
  xlab("Checkin date") + 
  ylab("Average daily rate (adr)")

#Categorical Exploration
ggplot(df_reg, aes(x=accommadation_type_name, y=ADR_USD, colour=accommadation_type_name)) + 
  geom_boxplot() + 
  geom_jitter(position=position_jitter(0.2)) + 
  theme_light() + 
  ggtitle("Average daily rate vs. Accommodation Type") + 
  xlab("Type") + 
  ylab("Average daily rate (adr)") +
  theme(legend.position = "none") 

ggplot(df_reg, aes(x=chain_hotel, y=ADR_USD, colour=chain_hotel)) + 
  geom_boxplot() + 
  geom_jitter(position=position_jitter(0.2)) + 
  theme_light() + 
  ggtitle("Average daily rate vs. Chain") + 
  xlab("Chain") + 
  ylab("Average daily rate (adr)") +
  theme(legend.position = "none") 

ggplot(df_reg, aes(x=city_name, y=ADR_USD, colour=city_name)) + 
  geom_boxplot() + 
  geom_jitter(position=position_jitter(0.2)) + 
  theme_light() + 
  ggtitle("Average daily rate vs. City") + 
  xlab("City") + 
  ylab("Average daily rate (adr)") +
  theme(legend.position = "none") 

star <- ggplot(df_reg, aes(x=star_rating, y=ADR_USD)) + 
  geom_point() + 
  geom_smooth() + 
  theme_light() + 
  ggtitle("Average daily rate vs. Star Rating") + 
  xlab("Star") + 
  ylab("Average daily rate (adr)")

# Explore monthly trends
df_reg %>% group_by(star_rating) %>% summarise(mean_adr=mean(ADR_USD, na.rm=TRUE)) %>% arrange(desc(mean_adr))
star

ggplot(df_reg, aes(x=star_rating, y=ADR_USD, colour=star_rating)) + 
  geom_boxplot() + 
  geom_jitter(position=position_jitter(0.2)) + 
  theme_light() + 
  ggtitle("Average daily rate vs. Star") + 
  xlab("Star") + 
  ylab("Average daily rate (adr)") +
  theme(legend.position = "none") 

ggplot(df_reg, aes(x=accommadation_type_name, y=star_rating, colour=accommadation_type_name)) + 
  geom_boxplot() + 
  geom_jitter(position=position_jitter(0.2)) + 
  theme_light() + 
  ggtitle("City vs. Star") + 
  xlab("City") + 
  ylab("Star") +
  theme(legend.position = "none") 

ggplot(df_reg, aes(x=city_name, y=accommadation_type_name, colour=city_name)) + 
  geom_boxplot() + 
  geom_jitter(position=position_jitter(0.2)) + 
  theme_light() + 
  ggtitle("City vs. Type") + 
  xlab("City") + 
  ylab("Type") +
  theme(legend.position = "none") 

par(mfrow=c(2,1))

interaction.plot(df_reg$arrival_month, df_reg$city_name, df_reg$ADR_USD, xlab="Month", ylab="Mean of adr", legend=TRUE, trace.label="City")

interaction.plot(df_reg$booking_month, df_reg$city_name, df_reg$ADR_USD, xlab="Month", ylab="Mean of adr", legend=TRUE, trace.label="City")

month <- ggplot(df_reg, aes(x=arrival_month, y=ADR_USD)) + 
  geom_boxplot() + 
  geom_smooth() + 
  theme_light() + 
  ggtitle("Average daily rate vs. Arrival month") + 
  xlab("Arrival month") + 
  ylab("Average daily rate (adr)")

month2 <- ggplot(df_reg, aes(x=booking_month, y=ADR_USD)) + 
  geom_boxplot() + 
  geom_smooth() + 
  theme_light() + 
  ggtitle("Average daily rate vs. Booking month") + 
  xlab("Booking month") + 
  ylab("Average daily rate (adr)")

library(gridExtra)
grid.arrange(month, month2, nrow=2, ncol=1)


##REGRESSION: RUN

# Explore yearly trends
df_reg %>% group_by(arrival_month) %>% summarise(mean_adr=mean(ADR_USD, na.rm=TRUE)) %>% arrange(desc(mean_adr))

# Choose the variables that will be used for modeling (all variables except the newly created formatted arrival dates)
dim(df_reg)
df_reg_short <- subset(df_reg, select=c(ADR_USD,city_name,accommadation_type_name,star_rating,chain_hotel,lead_time,length_stay,arrival_month,booking_month,arrival_day))
dim(df_reg_short)
# Pick rows to be used for training
set.seed(10)
train <- sample(1:nrow(df_reg_short), round(nrow(df_reg_short)*0.75, 0)) #Pick rows to be used for training (75% of the data)
length(train) 
# Values used for testing
y <- df_reg_short[-train, "ADR_USD"] # adr observed values used for evaluating model performance in test dataset

# Train model
linear.reg <- lm(ADR_USD ~ ., data=df_reg_short, subset=train)
summary(linear.reg)

#apartment base
#cityA base
#star rating 0 base

##REGRESSION: STOP


# Check for homoscedasticity
par(mfrow=c(1,1))
plot(linear.reg)

library(lmtest)
bptest(linear.reg) #Constant variance (from lmtest library)
hist(linear.reg$res) #the residuals seem to follow a normal distribution except for some extreme values on the left

linear.reg.step <- step(linear.reg, direction="both") 
summary(linear.reg.step)

# Check for homoscedasticity
par(mfrow=c(1,1))
plot(linear.reg.step)

library(lmtest)
bptest(linear.reg.step) #Constant variance (from lmtest library)
hist(linear.reg.step$res) #the residuals seem to follow a normal distribution except for some extreme values on the left


predict.linear.reg <- predict(linear.reg, newdata=df_reg_short[-train, ])
MSE.linear.reg <- mean((predict.linear.reg - y)^2) 
RMSE.linear.reg <- sqrt(mean((predict.linear.reg - y)^2)) 
MAE.linear.reg <- mean(abs(predict.linear.reg - y)) 


# Create table with MSE, RMSE, MAE for each model
results <- data.frame(Model = c( "Linear regression"), 
                      MSE = MSE.linear.reg, 
                      RMSE = RMSE.linear.reg, 
                      MAE = MAE.linear.reg)

library(knitr)
kable(results, format="pandoc", digits=2, caption = "Summary results for each model performance") #Nicer table using kable