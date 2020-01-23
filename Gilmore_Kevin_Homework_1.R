install.packages("dplyr")
library("dplyr")
library(ggplot2)
library(plyr)
library("tidyverse")
library(MASS)
install.packages("LaTex")

#1
FAA1 <- read.csv("C:/Users/14408/Desktop/Stat Modeling/FAA1.csv")
FAA2 <- read.csv("C:/Users/14408/Desktop/Stat Modeling/FAA2_New.csv")

#2
str(FAA1)
str(FAA2)

#FAA1 contains 800 observations and 8 variables, while FAA2 contains 
#only 150 observations and 7 variables. FAA also does not contain duration

#3
# I have used the full_join function in the dplyr package. I found the merge function to 
# be difficult with the extra variable in the "FAA2" dataset. The full_join function removes duplicates,
# thus showing there were 100 duplicates. There is no need to have duplicates. 

#4
New_Combined_FAA <- full_join(FAA1,FAA2)
str(New_Combined_FAA)

#In the combined dataset there is 850 obsevations and 8 variables 
summary(New_Combined_FAA)

#5
# There were 100 duplicate values between the two datasets when merged, which have now been eliminated
# In the summary statistics, the minimum duration is 14.76 minutes, however it should always be greater than 40m.
# There is a -3.546 value in height, however it is requested to be at least 6 meters high
# The length of the airport runway is typically less than 6000 feet, however the maxmimum distance is 6533.05 feet
# The speed_air category contains majority NA's, thus you might want to consider deleting

#6
Duration <- New_Combined_FAA %>% filter(duration >=1&duration<=40)
Speed_Ground_Less <- New_Combined_FAA %>% filter(speed_ground < 30)
Speed_Ground_Greater <- New_Combined_FAA %>% filter(speed_ground > 140)
Speed_Air_Less <- New_Combined_FAA %>% filter(speed_air < 30)
Speed_Air_Greater <- New_Combined_FAA %>% filter(speed_air > 140)
Height <- New_Combined_FAA %>% filter(height <6)
Distance <- New_Combined_FAA %>% filter(distance > 6000)

# I first checked all the rows individually for abnormal values and found 21. I then combined them 
# all into the merged dataset. 


FAA1_filtered <- FAA1 %>%
  filter(duration >40,height>=6, speed_ground >=30, speed_ground <=140,
         distance < 6000)
FAA2_filtered <- FAA2 %>%
  filter(height>=6, speed_ground >=30, speed_ground <=140,
         distance < 6000)
filtered <- full_join(FAA1_filtered,FAA2_filtered)

#7
# There are 832 observations in the clean data and 8 variables 
str(filtered)
summary(filtered)

#8
par(mfrow = c(3,3))
hist(filtered$duration,main = "Duration")
hist(filtered$no_pasg, main = "Numer of Passengers")
hist(filtered$speed_ground,main="Ground Speed")
hist(filtered$speed_air,main = "Air Speed")
hist(filtered$height,main = "Height")
hist(filtered$pitch,main = "Pitch")
hist(filtered$distance, main = "Distance")

#9
# The minimum duration is no longer 14.76, instead it is 41.95 meeting the normal value
# There is no longer a -3 in height, the minimum is 6.228, thus being a normal value
# With the clean data the distance is no longer over the runway length of 6000 feet, instead it is 5,381.96
# The observation number dropped from 850 to 832 

#10 
cor(filtered$duration,filtered$distance)
cor(filtered$no_pasg,filtered$distance)
cor(filtered$speed_ground,filtered$distance)
cor(filtered$height,filtered$distance)
cor(filtered$pitch,filtered$distance)


table1 <- matrix(c('Speed_Ground','0.08662701','Positive','Pitch','.08709602','Positive'
                   ,'Height','.09952859','Positive','No_pasg','-.01801031','Negative'),ncol = 3,byrow = TRUE)
colnames(table1) <- c('Name','Correlation Size','Direction')
table1

#11

Passenger_plot <- ggplot(data=filtered,aes(x=filtered$no_pasg,y=filtered$distance))+
  geom_point()+
  geom_point(data=filtered,col='blue',size=3)

Speed_ground_plot <- ggplot(data=filtered,aes(x=filtered$speed_ground,y=filtered$distance))+
  geom_point()+
  geom_point(data=filtered,col='blue',size=3)

Height_plot <- ggplot(data=filtered,aes(x=filtered$height,y=filtered$distance))+
  geom_point()+
  geom_point(data=filtered,col='blue',size=3)

Pitch_plot <- ggplot(data=filtered,aes(x=filtered$pitch,y=filtered$distance))+
  geom_point()+
  geom_point(data=filtered,col='blue',size=3)

air_speed_plot <- ggplot(data = filtered,aes(x=filtered$speed_air,y=filtered$distance)) +
  geom_point()+
  geom_point(data = filtered,col='blue',size=3)

duration_plot <- ggplot(data = filtered,aes(x=filtered$duration,y=filtered$distance)) +
  geom_point()+
  geom_point(data = filtered,col='blue',size=3)

duration_plot
air_speed_plot
Pitch_plot
Height_plot
Speed_ground_plot
Passenger_plot

#12
filtered$aircraft <- revalue(filtered$aircraft,c("boeing"=1))
filtered$aircraft <- revalue(filtered$aircraft,c ("airbus"=0))
filtered$aircraft <- as.integer(filtered$aircraft)
str(filtered$aircraft)

cor(filtered$aircraft,filtered$distance)

#13
model1 <- lm(distance~speed_ground, data = filtered)
summary(model1)

model2<- lm(distance~no_pasg, data = filtered)
summary(model2)

model3 <- lm(distance~speed_air, data = filtered)
summary(model3)

model4 <- lm(distance~height,data = filtered)
summary(model4)

model5<- lm(distance~aircraft, data = filtered)
summary(model5)

model6<- lm(distance~duration,data = filtered)
summary(model6)

model7 <- lm(distance~pitch,data=filtered)
summary(model7)

table2 <- matrix(c('Speed_Ground','<2.2e-16','Positive','Speed_Air','<2.2e-16','Positive','Aircraft','4.156e-12','Positive',
                   'Height','.004057','Positive','Pitch','.01196','Positive','Duration','0.1514','Positive',
                   'No-pasg','.6039','Positive'),ncol = 3,byrow = TRUE) 
colnames(table2) <- c('Name','P-value Size','Direction')
table2

#14
filtered$standardized_height <- {filtered$height-mean(filtered$height)}/sd(filtered$height)

filtered$standardized_aircraft <-{filtered$aircraft-mean(filtered$aircraft)}/sd(filtered$aircraft)

filtered$standardized_no_pasg <-{filtered$no_pasg-mean(filtered$no_pasg)}/sd(filtered$no_pasg)

filtered$standardized_no_pasg <-{filtered$no_pasg-mean(filtered$no_pasg)}/sd(filtered$no_pasg)

filtered$standardized_speed_ground<-{filtered$speed_ground-mean(filtered$speed_ground)}/sd(filtered$speed_ground)

filtered$standardized_speed_air<-{filtered$speed_air-mean(filtered$speed_air)}/sd(filtered$speed_air)

filtered$standardized_pitch <- {filtered$pitch-mean(filtered$pitch)}/sd(filtered$pitch)

lm(distance~standardized_pitch,data=filtered)
lm(distance~standardized_speed_ground,data=filtered)
lm(distance~standardized_no_pasg,data=filtered)
lm(distance~standardized_no_pasg,data=filtered)
lm(distance~standardized_aircraft,data=filtered)
lm(distance~standardized_height,data=filtered)

table3 <- matrix(c('Speed_Ground','776.1','Positive','Aircraft','212.6','Positive',
                   'Height','89.17','Positive','Pitch','78.03','Positive',
                   'No_pasg','-16.14','Negative'),ncol = 3,byrow = TRUE) 
colnames(table3) <- c('Name','Regression coefficient Size','Direction')
table3

#15
# The results are consistent as ground speed is the most important factor in all 3, which then follows by aircraft,
# height, and pitch having strong correlations. 
table1
table2
table3

# The variable rankings would be 1. ground speed 2. air speed and 3. aircraft
#16 

collinearity1<- lm(distance~speed_ground, data = filtered) 
collinearity2<- lm(distance~speed_air,data = filtered)
collinearity3<- lm(distance~speed_ground+speed_air,data=filtered)
collinearity1
collinearity2
collinearity3

#In model 3 air speed is added to ground speed and in this model ground speed decreases 
# as air speed continues to increase. As ground speed is a more important factor in terms of 
# correlation size, p-value, and regression coefficient size, I would keep this.

#17 
ranking_model1 <- lm(distance~speed_ground,data=filtered)
ranking_model2<- lm(distance~speed_ground+aircraft,data=filtered)
ranking_model3 <- lm(distance~speed_ground+aircraft+height,data = filtered)
ranking_model4 <- lm(distance~speed_ground+aircraft+height+pitch,data = filtered)
ranking_model5 <- lm(distance~speed_ground+aircraft+height+pitch+duration,data = filtered)
ranking_model6 <- lm(distance~speed_ground+aircraft+height+pitch+duration+no_pasg,data = filtered)

r.squared.1<- summary(ranking_model1)$r.squared; print(r.squared.1)
r.squared.2<- summary(ranking_model2)$r.squared; print(r.squared.2)
r.squared.3<- summary(ranking_model3)$r.squared; print(r.squared.3)
r.squared.4<- summary(ranking_model4)$r.squared; print(r.squared.4)
r.squared.5<- summary(ranking_model5)$r.squared; print(r.squared.5)
r.squared.6<- summary(ranking_model6)$r.squared; print(r.squared.6)
plot(c(1,2,3),c(r.squared.1,r.squared.2,r.squared.3),type="b",ylab="R squared",xlab="The number of predictors")

#18
adj.r.squared.1<-summary(ranking_model1)$adj.r.squared; print(adj.r.squared.1) 
adj.r.squared.2<-summary(ranking_model2)$adj.r.squared; print(adj.r.squared.2) 
adj.r.squared.3<-summary(ranking_model3)$adj.r.squared; print(adj.r.squared.3)
adj.r.squared.4<-summary(ranking_model4)$adj.r.squared; print(adj.r.squared.4) 
adj.r.squared.5<-summary(ranking_model5)$adj.r.squared; print(adj.r.squared.5) 
adj.r.squared.6<-summary(ranking_model6)$adj.r.squared; print(adj.r.squared.6)
plot(c(1,2,3),c(adj.r.squared.1,adj.r.squared.2,adj.r.squared.3),type="b",ylab="Adjusted R squared",xlab="The number of predictors")

#19
AIC(ranking_model1)
AIC(ranking_model2)
AIC(ranking_model3)
AIC(ranking_model4)
AIC(ranking_model5)
AIC(ranking_model6)

#We see that Model one has the lowest AIC and therefore has the most parsimonious fit. 

#20 

#In step 19, the AIC ranking goes from 1 to 3 to 2, while the adjusted r squared rankings 
# in step 18 go from 1 to 2 to 3
stepAIC(ranking_model1)
stepAIC(ranking_model2)
stepAIC(ranking_model3)
stepAIC(ranking_model4)
stepAIC(ranking_model5)

 
