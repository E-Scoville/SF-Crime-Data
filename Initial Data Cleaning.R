rm(list=ls())
setwd('C:/Users/Arathen/Desktop/Github Projects/SF Crime/')

library(DataExplorer)
library(tidyverse)
library(caret)

train <- read.csv("train.csv")
test <- read.csv("test.csv")
data <- bind_rows(train, test)

# Drop variables I won't be using
data <- drop_columns(data=data, c(3,4,6,7))
str(data)

# Remove duplicate rows
data <- data %>% distinct()
plot_missing(data)

# Turn fields with levels into factors and Dates into datetime
data <- data %>% mutate_at(vars(Category,PdDistrict), factor)
data$Dates <- as.POSIXct(data$Dates, format="%Y-%m-%d %H:%M:%S")

# Some of the X and Y values are incorrect. Replace them with the average
# X and Y values for their district
str(data)
sub1 <- data[which(data$Y != 90),]
sub2 <- data[which(data$Y == 90),]
for (i in c(1:length(sub2$X))){
  if (sub2$PdDistrict[i] == "NORTHERN") {
    sub2$X[i] <- mean(sub1[which(sub1$PdDistrict == "NORTHERN"), 4])
    sub2$Y[i] <- mean(sub1[which(sub1$PdDistrict == "NORTHERN"), 5])
  } else if (sub2$PdDistrict[i] == "PARK") {
    sub2$X[i] <- mean(sub1[which(sub1$PdDistrict == "PARK"), 4])
    sub2$Y[i] <- mean(sub1[which(sub1$PdDistrict == "PARK"), 5])
  } else if (sub2$PdDistrict[i] == "INGLESIDE") {
    sub2$X[i] <- mean(sub1[which(sub1$PdDistrict == "INGLESIDE"), 4])
    sub2$Y[i] <- mean(sub1[which(sub1$PdDistrict == "INGLESIDE"), 5])
  } else if (sub2$PdDistrict[i] == "BAYVIEW") {
    sub2$X[i] <- mean(sub1[which(sub1$PdDistrict == "BAYVIEW"), 4])
    sub2$Y[i] <- mean(sub1[which(sub1$PdDistrict == "BAYVIEW"), 5])
  } else if (sub2$PdDistrict[i] == "RICHMOND") {
    sub2$X[i] <- mean(sub1[which(sub1$PdDistrict == "RICHMOND"), 4])
    sub2$Y[i] <- mean(sub1[which(sub1$PdDistrict == "RICHMOND"), 5])
  } else if (sub2$PdDistrict[i] == "CENTRAL") {
    sub2$X[i] <- mean(sub1[which(sub1$PdDistrict == "CENTRAL"), 4])
    sub2$Y[i] <- mean(sub1[which(sub1$PdDistrict == "CENTRAL"), 5])
  } else if (sub2$PdDistrict[i] == "TARAVAL") {
    sub2$X[i] <- mean(sub1[which(sub1$PdDistrict == "TARAVAL"), 4])
    sub2$Y[i] <- mean(sub1[which(sub1$PdDistrict == "TARAVAL"), 5])
  } else if (sub2$PdDistrict[i] == "TENDERLOIN") {
    sub2$X[i] <- mean(sub1[which(sub1$PdDistrict == "TENDERLOIN"), 4])
    sub2$Y[i] <- mean(sub1[which(sub1$PdDistrict == "TENDERLOIN"), 5])
  } else if (sub2$PdDistrict[i] == "MISSION") {
    sub2$X[i] <- mean(sub1[which(sub1$PdDistrict == "MISSION"), 4])
    sub2$Y[i] <- mean(sub1[which(sub1$PdDistrict == "MISSION"), 5])
  } else if (sub2$PdDistrict[i] == "SOUTHERN") {
    sub2$X[i] <- mean(sub1[which(sub1$PdDistrict == "SOUTHERN"), 4])
    sub2$Y[i] <- mean(sub1[which(sub1$PdDistrict == "SOUTHERN"), 5])
  }
}

data <- bind_rows(sub1, sub2)
hist(data$X)
hist(data$Y)

# Feature Engineering
# Extract month, day, year, hour, minute
data$Month <- format(data$Dates, format="%m")
data$Day <- format(data$Dates, format="%d")
data$Hour <- format(data$Dates, format="%H")
data$Minute <- format(data$Dates, format="%M")
data <- data %>% mutate_at(vars(Month, Day, Hour, Minute),
                           as.numeric)
data <- data %>% select(-Dates) # Drop dates so it's not coerced to chr

str(data)
plot_missing(data)

# One-hot encoding
dummy <- dummyVars(Category~., data=data)
newdata <- predict(dummy, newdata=data)  %>% as.data.frame() %>%
  bind_cols(., (data %>% select(Category)))

# Split data again
newtrain <- newdata %>% filter(is.na(Id)) %>% select(-Id)
newtest <- newdata %>% filter(is.na(Category)) %>% select(-Category)

# I have no clue why the previous step is making date values go missing.
# Just drop them for now I guess...
newtrain <- newtrain %>% filter(!is.na(Dates))
newtest <- newtest %>% filter(!is.na(Dates))

# Create cleaned CSVs
write_csv(x=newtrain, path="./CleanedTrain.csv")
write_csv(x=newtest, path="./CleanedTest.csv")
