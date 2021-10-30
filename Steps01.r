#acquire & import data sets:
setwd("D:\\University\\third year\\6th semester(fall 2021)\\CMPS 276 (DS)\\Project\\datasets")
covid.cases <- read.csv("owid-covid-data.csv", stringsAsFactors = F)
flights <- read.csv("flights-merged.csv", stringsAsFactors = F)
vaccination <- read.csv("country_vaccinations.csv", stringsAsFactors = F)
policies.1 <- read.csv("coronanet_release.csv", stringsAsFactors = F)
policies.2 <- read.csv("all_inds_weighted.csv", stringsAsFactors = F)

#some exploration:
#countries:
length(unique(covid.cases$location))
length(unique(flights$country))
length(unique(vaccination$country))
length(unique(policies.1$country))
length(unique(policies.2$country))
#date:
#how many dates each data set has:
length(unique(covid.cases$date))
length(unique(flights$day))
length(unique(vaccination$date))
length(unique(policies.1$date_start))
length(unique(policies.2$date_policy))

#ranges of dates:
c(min(unique(covid.cases$date)),max(unique(covid.cases$date)))

c(min(unique(flights$day)), max(unique(flights$day)))

c(min(unique(vaccination$date)), max(unique(vaccination$date)))

c(min(unique(policies.1$date_start)), max(unique(policies.1$date_start)))

c(min(unique(policies.2$date_policy)), max(unique(policies.2$date_policy)))

#the date format of flights data set seems to be different, also the date column should be of class "Date" for all data sets; fix for merging:
flights$day <- as.Date(flights$day, format = "%m/%d/%y")
covid.cases$date <- as.Date(covid.cases$date)
vaccination$date <- as.Date(vaccination$date)
policies.1$date_start <- as.Date(policies.1$date_start)
policies.2$date_policy <- as.Date(policies.2$date_policy)
class(flights$day)
class(covid.cases$date)
class(vaccination$date)
class(policies.1$date_start)
class(policies.2$date_policy)

#make the data sets with same date and country names for later work, this doesn't affect split:
if(names(covid.cases)[3] != "country") { names(covid.cases)[3] <- "country" }
if(names(flights)[2] != "date") {names(flights)[2] <- "date"}
if(names(policies.1)[9] != "date") {names(policies.1)[9] <- "date"}

#data cleaning on each data set alone:
#for covid.cases dataset:
#1. remove zerovariance columns
install.packages("caret")
library(caret)
covid.cases.NZV = nearZeroVar(covid.cases , saveMetrics = TRUE )
to_drop_basedon_zerovar <- rownames(covid.cases.NZV[covid.cases.NZV[,"zeroVar"] + covid.cases.NZV[,"nzv"] > 0, ])

#2. remove columns that contain more than 85% of its values as NA
NA.percentage <- sapply(covid.cases , function(x) as.numeric(table(is.na(x))["TRUE"])/(length(x)))
to_drop_basedon_fewvalues <- na.omit(names(NA.percentage[NA.percentage>0.85]))
to.drop <- c(to_drop_basedon_fewvalues, to_drop_basedon_zerovar)
covid.cases <- covid.cases[ ,!( names(covid.cases) %in% to.drop )]
ncol(covid.cases)

#check if there is a logical type among the columns to be deleted:
sapply(to.drop, class)
table(is.na(covid.cases))


#for flights dataset:
#1. remove zerovariance columns
flights.NZV = nearZeroVar(flights , saveMetrics = TRUE )
to_drop_basedon_zerovar.flights <- rownames(flights.NZV[flights.NZV[,"zeroVar"] + flights.NZV[,"nzv"] > 0, ])
#no near-zero variance columns.

#2. remove columns that contain more than 85% of its values as NA
NA.percentage.flights <- sapply(flights , function(x) as.numeric(table(is.na(x))["TRUE"])/(length(x)))
to_drop_basedon_fewvalues.flights <- na.omit(names(NA.percentage.flights[NA.percentage.flights>0.85]))
to.drop <- to_drop_basedon_fewvalues.flights
#not applicable. because this dataset was already cleaned before importing. 


#for vaccination dataset:
#1. remove zero-variance columns
vaccination.NZV = nearZeroVar(vaccination , saveMetrics = TRUE )
to_drop_basedon_zerovar.vaccination <- rownames(vaccination.NZV[vaccination.NZV[,"zeroVar"] + vaccination.NZV[,"nzv"] > 0, ])
#not applicable.

#2. remove columns that contain more than 85% of its values as NA
NA.percentage.vacc <- sapply(vaccination , function(x) as.numeric(table(is.na(x))["TRUE"])/(length(x)))
to_drop_basedon_fewvalues.vacc <- na.omit(names(NA.percentage.vacc[NA.percentage.vacc>0.85]))
to.drop <- c(to_drop_basedon_fewvalues.vacc, to_drop_basedon_zerovar.vaccination)
#not applicable.

#for policies.1 dataset:
#1. remove zerovariance columns
pol1.NZV = nearZeroVar(policies.1 , saveMetrics = TRUE )
to_drop_basedon_zerovar.pol1 <- rownames(pol1.NZV[pol1.NZV[,"zeroVar"] + pol1.NZV[,"nzv"] > 0, ])

#2. remove columns that contain more than 85% of its values as NA
NA.percentage.pol1 <- sapply(policies.1 , function(x) as.numeric(table(is.na(x))["TRUE"])/(length(x)))
to_drop_basedon_fewvalues.pol1 <- na.omit(names(NA.percentage.pol1[NA.percentage.pol1>0.85]))
to.drop.pol1 <- c(to_drop_basedon_fewvalues.pol1, to_drop_basedon_zerovar.pol1)

#check if there is a logical type among the columns to be deleted:
sapply(to.drop.pol1, class)
table(is.na(policies.1))

ncol(policies.1)
policies.1 <- policies.1[ ,!( names(policies.1) %in% to.drop.pol1 )]
ncol(policies.1)


#for policies.2 dataset:
#2. remove columns that contain more than 85% of its values as NA
NA.percentage.pol2 <- sapply(policies.2 , function(x) as.numeric(table(is.na(x))["TRUE"])/(length(x)))
to_drop_basedon_fewvalues.pol2 <- na.omit(names(NA.percentage.pol2[NA.percentage.pol2>0.85]))
#not applicable.
ncol(policies.2)


#merge covid.cases & flights into one data set:
names(covid.cases)
names(flights)

class(flights$country)
class(covid.cases$country)
class(flights$date)
class(covid.cases$date)
merged.dataset <- merge(covid.cases, flights, by = c("date", "country"), all = T)
#rm(merged.dataset)
ncol(merged.dataset)
names(merged.dataset)
merged.dataset$date

length(flights$date)
length(covid.cases$date)
length(merged.dataset$date)

summary(covid.cases$date)
summary(flights$date)
summary(vaccination$date)
summary(policies.1$date_start)
summary(policies.2$date_policy)
summary(merged.dataset$date)

#keep track of the change in nb of rows:
nrow(covid.cases)
nrow(flights)
nrow(vaccination)
nrow(policies.1)
nrow(policies.2)
nrow(merged.dataset)

#merge merged.dataset & vaccination:
class(vaccination$date)
merged.dataset <- merge(merged.dataset, vaccination, by = c("date", "country"), all = T)
nrow(merged.dataset)

#merge merged.dataset & policies.1
class(policies.1$date)
names(policies.1)
merged.dataset <- merge(merged.dataset, policies.1, by = c("date", "country"), all = T)
nrow(merged.dataset)

#merge merged.dataset & policies.2
class(policies.2$date_policy)
names(policies.2)
if(names(policies.2)[2] != "date") { names(policies.2)[2] <- "date" }
merged.dataset <- merge(merged.dataset, policies.2, by = c("date", "country"), all = T)
nrow(merged.dataset)
merged.dataset

#dates ranges after merging
c(min(unique(covid.cases$date)),max(unique(covid.cases$date)))

c(min(unique(flights$date)), max(unique(flights$date)))

c(min(unique(vaccination$date)), max(unique(vaccination$date)))

c(min(unique(policies.1$date)), max(unique(policies.1$date)))

c(min(unique(policies.2$date)), max(unique(policies.2$date)))

c(min(unique(merged.dataset$date)), max(unique(merged.dataset$date)))

nrow(merged.dataset)
ncol(merged.dataset)
#Split into train-test before data cleaning:

#1. Split merged data set into Training and Testing:
sample_size = floor(0.9*nrow(merged.dataset))
set.seed(777)

picked = sample(seq_len(nrow(merged.dataset)),size = sample_size)
train_data = merged.dataset[picked,]
test_data = merged.dataset[-picked,]
#re-sort based on date and country:
train_data <- train_data[with(train_data, order(country, date)),]
test_data <- test_data[with(test_data, order(country, date)),]
nrow(train_data)
nrow(test_data)


#Data cleaning:
#get rid of rows where date > 2021:
ncol(merged.dataset)
names(merged.dataset)
table(merged.dataset$date)
threshold.date <- as.Date(c("10/27/21"), format = "%m/%d/%y")
merged.dataset <- subset(merged.dataset, date < threshold.date)

#impute vaccination columns for 0 in 2019 until 2020:
# table(is.na(merged.dataset))
# vacc.cols <- intersect(colnames(merged.dataset), colnames(vaccination))
# vacc.cols <- vacc.cols[c(-1,-2,-7,-8)]
# vacc.date <- as.Date(c("12/01/20"), format = "%m/%d/%y")
# merged.dataset.datels2020 <- subset(merged.dataset, date < vacc.date)
# max(vaccination$date)

#impute(merged.dataset.datels2020[vacc.cols], 0)
#merged.dataset[vacc.cols] [is.na( merged.dataset.[vacc.cols] )] <- 0
#table(is.na(merged.dataset))

#remove outliers:
#outliers_removed <- merged.dataset[merged.dataset > ( mean(merged.dataset, na.rm = T) - 3*sd(merged.dataset, na.rm = T)) & merged.dataset < ( mean (merged.dataset, na.rm = T) + 3*sd(merged.dataset, na.rm = T))]

# Scale:
#   #Scaling:
#   summary(covid.cases$positive_rate)
# summary(flights$total_flights)
# summary(vaccination$total_vaccinations)
# summary(policies.1$compliance)
# summary(policies.2$mask_everywhere)
# 
# head(merged.dataset)
