#Base: IQR Outliers Exclusion
summary(df_reg)
Q <- quantile(df_reg$ADR_USD, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(df_reg$ADR_USD)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
df_reg <- subset(df_reg, df_reg$ADR_USD > (Q[1] - 1.5*iqr) & df_reg$ADR_USD < (Q[2]+1.5*iqr))
summary(df_reg)

#Case Analysis 1: IQR Outliers Exclusion
summary(df_reg_type)
Q <- quantile(df_reg_type$ADR_USD, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(df_reg_type$ADR_USD)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
df_reg_type <- subset(df_reg_type, df_reg_type$ADR_USD > (Q[1] - 1.5*iqr) & df_reg_type$ADR_USD < (Q[2]+1.5*iqr))
summary(df_reg_type)

#Case 2: mean ADR case
df_reg <- df_reg %>% group_by(hotel_id, city_name, star_rating,accommadation_type_name,chain_hotel,booking_date,checkin_date,checkout_date,lead_time,length_stay,arrival_month,arrival_day,arrival_week_number,booking_month,booking_day,booking_week_number) %>% summarise(ADR_USD = mean(ADR_USD))
dim(df_reg)
summary(df_reg)
df_reg <- subset(df_reg, select= -c(hotel_id))

# Check for homoscedasticity
par(mfrow=c(1,1))
plot(linear.reg)
# Check for homoscedasticity
par(mfrow=c(1,1))
plot(linear.reg.step)

#Case 3: Other Outliers Ex
outliers <- boxplot(df_reg$ADR_USD, plot=FALSE)$out
df_reg_xout <- df_reg[-which(df_reg$ADR_USD %in% outliers),]
summary(df_reg_xout)
summary(df_reg)

#Case 4: 5% outliers exclusion
df_reg_xout2 <- df_reg[df_reg$ADR_USD > quantile(df_reg$ADR_USD, 0.05) & df_reg$ADR_USD < quantile(df_reg$ADR_USD, 0.95),]
summary(df_reg_xout2)

#Case 5: Hotel Exclusion
df_reg <- df_reg[df_reg$hotel_id!=102982,]
summary(df_reg)

# Number of adults, number of children, total of special requests
ggplot(df_reg_xout, aes(x=lead_time, y=ADR_USD)) + geom_point() + theme_light()
ggplot(df_reg_xout, aes(x=length_stay, y=ADR_USD)) + geom_point() + theme_light()

ggplot(df_reg, aes(x=hotel_id, y=ADR_USD, colour=hotel_id)) + 
  geom_boxplot() + 
  geom_jitter(position=position_jitter(0.2)) + 
  theme_light() + 
  ggtitle("Average daily rate vs. Hotel ID") + 
  xlab("Hotel") + 
  ylab("Average daily rate (adr)") +
  theme(legend.position = "none") 

# Check for large leverage points
n=36763; #Number of observations
p=32; #Number of predictors, including the intercept
lev=influence(linear.reg)$hat
halfnorm(lev, nlab=10, labs=row.names(df_resort_short), ylab="Leverages") #134 and 154 have high leverage
# Check for outliers
jack=rstudent(linear.reg);
t=qt(.05/(2*n), n-p-1) # Bonferroni correction. # -3.856416
jack[which(abs(jack) > abs(t))] # 445 and 173 outliers

df_resort_short[c(134,154,173,145), ]# Make predictions on the test dataset