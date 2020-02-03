#rm(list = ls())
#libraries
library(plyr)
library(dplyr)
library(lubridate) ##for date fuctions
library(stargazer) ##for nice regression & export to Latex
library(zoo)
library(reshape2) 
library(ggplot2)
library(MASS)
library(alr3)
library(geosphere)
library(plm)
library(data.table)

#DATA:
#import Airbnb listing
nyc1 <- read.csv("~/Desktop/SeniorThesis/Data/AirbnbListing/NYC/nyc2015-01-01.csv")
#change discrepancies in first scrape
colnames(nyc1)[14] <- "calculated_host_listings_count"
nyc1$neighbourhood_group <- as.character(nyc1$neighbourhood_group)
nyc1$neighbourhood_group[nyc1$neighbourhood_group == "The Bronx"] <- "Bronx"
#get all csv from folder
path      <- "~/Desktop/SeniorThesis/Data/AirbnbListing/NYC"
filenames <- list.files(path=path,pattern="*.csv",full.names = TRUE)
all       <- lapply(filenames,function(i){ 
  read.csv(i, header=TRUE)}) ##all is list of csv elements
df <- do.call(rbind.data.frame, all[-1]) ##without first element
#bind fill with first scrape
df <- rbind.fill(nyc1,df)
#clean
df <- df[df$price != 0,] ##remove prices == 0 (570 cases); prices == 1 (4 cases);#listing listed 4 times for $1, 2 others listed twice for $5 and $8
df <- df[-54750,] ##remove obs 54750 for wacky distance from Empire State
df <- df[df$price != 97598,]; df <- df[df$price != 67646,] ##remove 2 big outliers
df <- df[is.na(df$availability_365)==FALSE,] ##remove null of availability_365
df <- df[df$neighbourhood_group!="",] ##drop "Unknown" Borough specification
#add Date column
dates <- list.files(path)
dates <- as.Date(dates, "nyc%Y-%m-%d") 
dates <- paste(dates) ##get Date List
rows <- c()
for (i in 1:length(all)) {
  temp <- nrow(all[[i]])
  rows <- c(rows,temp)
} ##get Nrow List
start <- 1
end   <- 0
for (i in 1:44) { ##get Date column from 2 lists above
  end <- end+rows[i]
  df[start:end,"date"] = dates[i]
  start = end+1
}
#add Month, Year, Month-year
df$date       = as.Date(df$date, "%Y-%m-%d")
df$month      = month(ymd(df$date))
df$year       = year(ymd(df$date))
df$month_year = as.yearmon(as.character(df$date, "%Y-%m"))
#convert all to factor for smart regressions
df[,"id"]         = as.factor(df[,"id"])
df[,"host_id"]    = as.factor(df[,"host_id"])
df[,"month"]      = as.factor(df[,"month"])
df[,"year"]       = as.factor(df[,"year"])
df[,"month_year"] = as.factor(df[,"month_year"])
#get dates since last review
df$date        <- as.character(df$date)
df$last_review <- as.character(df$last_review)
df$days_last_review = difftime(as.POSIXct(df$date,format="%Y-%m-%d"),
                               as.POSIXct(df$last_review,format="%Y-%m-%d"),units="days")
df$days_last_review <- as.integer(df$days_last_review) ##412423 cases no review
#break reviews into bins
df2 <- df[df$number_of_reviews==0,] ##400,000 cases of no reviews
output <- character(nrow(df)) ##initialize output vector for run-time optimization
for (i in 1:nrow(df)) {
  x = df[i,"number_of_reviews"]
  if (x==0) {output[i]          = "NoReviews"}
  if (x>=1 & x<5) {output[i]    = "OneToFiveReviews"}
  if (x>=5 & x<20) {output[i]   = "FiveToTwentyReview"}
  if (x>=20 & x<100) {output[i] = "TwentyToHundredReview"}
  if (x>=100) {output[i]        = "MoreEq100Review"}}
df$review_bin = output
#break availability 365 into bins
output2 <- character(nrow(df))
for (i in 1:nrow(df)) {
  x = df[i,"availability_365"]
  if (x<100) {output2[i] = "LessEq100"}
  else if (x>=100 & x<200) {output2[i] = "100To200"}
  else if (x>=200 & x<=365) {output2[i] = "200To365"}
}
df$availability_bin = output2
#add Distance from Empire State
df$DistanceCentral <- distHaversine(cbind(df$longitude,df$latitude),c(-73.985428,40.748817))/1000 #meters

#import hotel & airport
hotel <- read.csv("~/Desktop/SeniorThesis/Data/Hotel/NYCraw.csv",header = T)
pass  <- read.csv("~/Desktop/SeniorThesis/Data/AirportPassenger/AirportAggregated.csv",header = T)
#convert to yearmon then date
hotel$Date      <- as.Date(as.yearmon(hotel$Date,format="%b %y"))
hotel$Month     <- month(ymd(hotel$Date))
hotel$Year      <- year(ymd(hotel$Date))
hotel$MonthYear <- as.yearmon(hotel$Date)
#factorize
hotel$Month     <- as.factor(hotel$Month)
hotel$Year      <- as.factor(hotel$Year)
hotel$MonthYear <- as.factor(hotel$MonthYear)
pass$Year       <- as.factor(pass$Year)
pass$Month      <- as.factor(pass$Month)

#ANALYSIS:
#Intro stats
nyc2 <- read.csv("~/Desktop/SeniorThesis/Data/AirbnbListing/NYC/nyc2018-12-06.csv")
a <- nyc2$host_id[nyc2$calculated_host_listings_count>1] #number of listings where hosts have multiple properties
length(a)/nrow(nyc2) ##take up 34% of properties
length(unique(a)) ##unique multiple listing hosts
length(unique(nyc2$host_id)) ##all unique hosts
length(unique(a))/length(unique(nyc2$host_id))
#hedonic model
df$logPrice <- log(df$price)
model1 <- lm(logPrice~review_bin + room_type +
               availability_bin + neighbourhood_group + calculated_host_listings_count +
               month_year,data=df);summary(model1)
#hedonic model with Distance from Empire State -- use this!
model1b <- lm(logPrice~review_bin + room_type + DistanceCentral +
                availability_bin + neighbourhood_group + calculated_host_listings_count +
                month_year,data=df);summary(model1b)

#get price median
median(df$price,na.rm=TRUE)
#get descriptive stats for Airbnb 
df4 <- df[,c("price","minimum_nights","reviews_per_month","calculated_host_listings_count",
             "number_of_reviews","availability_365","DistanceCentral")]
#get descriptive stats for Room Type and Borough
u           <- unique(df[c("id", "room_type","price")]) ##df of unique id and room type 169396
u$room_type <- as.character(u$room_type)
roomtype <- as.data.frame(u %>% ##consider duplicate listings when it has diffrent prices
                            group_by(room_type,add=FALSE) %>%
                            summarise(N = n(), Percentage = n()/nrow(u),Median = median(price),Mean=mean(price)))

u2      <- unique(df[c("id","neighbourhood_group","price")])
borough <- as.data.frame(u2 %>% ##consider duplicate listings when it has diffrent prices
                           group_by(neighbourhood_group) %>%
                           summarise(N = n(), Percentage = n()/nrow(u),Median = median(price),Mean=mean(price)))

u3    <- unique(df[c("id","availability_bin","price")])
avbin <- as.data.frame(u3 %>% ##consider duplicate listings when it has diffrent prices
                         group_by(availability_bin) %>%
                         summarise(N = n(), Percentage = n()/nrow(u),Median = median(price),Mean=mean(price)))

count  = length(unique(df$id));count ##number of unique listings
count2 = length(unique(df$host_id));count2 ##number of unique hosts
#get hedonic Airbnb price index
out <- model1b$coefficients[16:58] ##availability as categories
out <- exp(out)*100
out <- c(100,out)
#prep Airbnb listings to merge with others
airbnb <- as.data.frame(table(df$date))
airbnb$Month       = month(ymd(airbnb$Var1))
airbnb$Year        = year(ymd(airbnb$Var1))
airbnb$IndexAirbnb = out
airbnb             = airbnb[,-1] ##remove unesscesary date column
colnames(airbnb)[1] <- "QuantityAirbnb"
#merge airbnb, pass, and hotel, But first merge hotel and CPI ##Base year:
cpi <- read.csv("~/Desktop/SeniorThesis/Data/CPI/CPI.csv")
hotel <- merge(hotel,cpi, by=c("Year","Month"))
hotel$Month  <- as.factor(hotel$Month)
hotel$ADRAdj <- hotel$ADR/(hotel$CPI/hotel$CPI[1])

df_m <- merge(hotel,pass,by = c("Year","Month"))
df_m <- merge(df_m,airbnb,by = c("Year","Month"))
df_m$logQuantityAirbnb <- log(df_m$QuantityAirbnb)
df_m$TotalNYC          <- df_m$TotalJFK + df_m$TotalLG ##get total for both aiports in NY
df_m$logTotalNYC       <- log(df_m$TotalNYC)
df_m$Month             <- as.factor(df_m$Month)
#get Hotel price index
model5 <-lm(log(ADRAdj)~MonthYear,data=df_m);summary(model5)
out2 <- model5$coefficients[2:44] ##actually 2 months less
out2 <- exp(out2)*100
out2 <- c(100,out2)
#get descriptive stats for hotel
df6 <- hotel[181:226,c("Occupancy","ADR","ADRAdj","RevPAR","Supply","Demand")] ##only 2015-2018
#regression model for effect of Airbnb on hotel profits with Month fixed effect
model2 <- lm(log(Occupancy)~logQuantityAirbnb + logTotalNYC + log(IndexAirbnb) + Month, data=df_m);summary(model2)
model3 <- lm(log(ADR)~logQuantityAirbnb + logTotalNYC + log(IndexAirbnb) + Month, data=df_m);summary(model3)
model4 <- lm(log(RevPAR)~logQuantityAirbnb + logTotalNYC +log(IndexAirbnb)+ Month, data=df_m);summary(model4)

#Welfare calculations:
#model to see if the Months are truly driving Airbnb prices (instead of occupancy)
model6 <- lm(Occupancy~ADR+Month,data=hotel);summary(model6) ##Coefficient on ADR not negative?!
#model for hotel price change
edateairbnb <- as.Date("01-01-2009",format = "%d-%m-%Y"); edateairbnb ##Airbnb entrance date
hotel$Date  <- as.character(hotel$Date)
for (i in 1:nrow(hotel)){ ##assign Before/After Airbnb
  x = as.Date(hotel$Date[i],format="%Y-%m-%d")
  if (x > edateairbnb) {
    hotel$BeforeAfter[i] <- "After"
  } else {
    hotel$BeforeAfter[i] <- "Before"
  }
}
hotel$BeforeAfter <- as.factor(hotel$BeforeAfter)
model7 <- lm(ADRAdj~BeforeAfter + Month, data=hotel);summary(model7) ##Before is #10.910 more expensive
#price elasticity
df_m$IndexHotel <- out2[1:42]
model8  <- lm(log(Demand) ~ log(IndexHotel) + log(IndexAirbnb) + Month,data=df_m);summary(model8) ##-0.15290
model8b <- lm(log(Demand) ~ log(IndexHotel) + log(IndexAirbnb) + log(QuantityAirbnb) + Month,data=df_m);summary(model8b)
#df_m$IndexRent <- out3[1:42] ##AFTER RUNNING EVERYTHING
#model8c <- lm(log(Demand) ~ log(IndexHotel) + log(IndexAirbnb) + log(IndexRent) + log(QuantityAirbnb) + Month,data=df_m);summary(model8c)

#welfare
out4 <- 0.5*((df_m$Demand/30)/df_m$ADRAdj)*(10.910)^2*(0.15290)
welfare     <- data.frame(MonthYear=df_m$MonthYear, AvgRoomsSold=df_m$Demand/30,
                          ADRAdj=df_m$ADRAdj,DWLAdj = out4)
welfare$DWL <- welfare$DWLAdj*(df_m$CPI/hotel$CPI[1])

#import Rental data (by Borough)
price  <- read.csv("~/Desktop/SeniorThesis/Data/Rental/NYC/rentalIndex_All.csv") ##price
price  <- price[-4] ##remove NYC aggregate column
invent <- read.csv("~/Desktop/SeniorThesis/Data/Rental/NYC/rentalInventory_All.csv") ##rent
invent <- invent[-5] ##remove NYC aggregate column
colnames(invent)[6] <- "Staten Island" ##consistent with Airbnb data
med    <- read.csv("~/Desktop/SeniorThesis/Data/Rental/NYC/medianAskingRent_All.csv")
colnames(med)[6] <- "Staten Island" ##consistent with Airbnb data
#melt
price  <- melt(price)
invent <- melt(invent)
med    <- melt(med)
#Price has: Brooklyn, Manhattan, Queens
#Invent has: Brooklyn, Manhattan, Queens, Bronx, Staten Island
#Med has: Brooklyn, Manhattan, Queens, Bronx, Staten Island
#change column names 
colnames(price)[1] <- "Date"
colnames(price)[2] <- "Borough"
colnames(price)[3] <- "RentalPrice"
colnames(invent)[1] <- "Date"
colnames(invent)[2] <- "Borough"
colnames(invent)[3] <- "RentalInvent"
colnames(med)[2] <-"Borough"
colnames(med)[3] <-"MedPrice"
#get month, year to merge
invent$Date  <- paste(invent$Date,"-01")
invent$Date <- as.Date(invent$Date,format="%Y-%m -%d")
invent$Month <- month(ymd(invent$Date))
invent$Year  <- year(ymd(invent$Date))
invent       <- invent[-1]

price$Date  <- as.Date(price$Date,format="%Y-%m-%d")
price$Month <- month(ymd(price$Date))
price$Year  <- year(ymd(price$Date))
price       <- price[-1]

med$Date  <- paste(med$Date,"-01")
med$Date  <- as.Date(med$Date,format="%Y-%m -%d")
med$Month <- month(ymd(med$Date))
med$Year  <- year(ymd(med$Date))
med       <- med[-1]
#create Airbnb borough quantity df
temp <- as.data.frame(table(df$neighbourhood_group,df$date))
colnames(temp)[1] <-"Borough"
colnames(temp)[2] <-"Date"
colnames(temp)[3] <-"AirbnbQuantity"
temp$Month <- month(ymd(temp$Date))
temp$Year  <- year(ymd(temp$Date))
temp       <- temp[-2]
#characterize Boroughs
invent$Borough <- as.character(invent$Borough)
temp$Borough   <- as.character(temp$Borough)
med$Borough    <- as.character(med$Borough)
price$Borough  <- as.character(price$Borough)
#merge AirbnbQuantity & Inventory & Med; AirbnbQuantity & Price
df_invent <- merge(temp,invent,by = c("Year","Month","Borough"))
df_rent   <- merge(df_invent,med,by = c("Year","Month","Borough"))
df_price  <- merge(temp,price,by = c("Year","Month","Borough")) ##df_price does not include Bronx/Staten Island
#get descriptive stats for rental by Borough
borough2 <- as.data.frame(df_rent %>%
                            group_by(Borough) %>%
                            summarise(N = sum(RentalInvent),Percentage = sum(RentalInvent)/sum(df_rent$RentalInvent),
                                      MeanInventory = mean(RentalInvent),
                                      MeanPrice = mean(MedPrice),
                                      SDPrice = sd(MedPrice)));borough2
#regression model for Airbnb Quantity on Inventory, Price Index, and Med Asking Price
df_rent$Month <- as.factor(df_rent$Month)
df_price$Month  <- as.factor(df_price$Month)
model9  <- lm(log(RentalInvent)~log(AirbnbQuantity) + Borough + Month,data=df_rent);summary(model9) ##(+) coeff because location variable not granular enough?
model10 <- lm(log(MedPrice)~log(AirbnbQuantity) + Borough + Month,data=df_rent);summary(model10)
model11 <- lm(log(RentalPrice)~log(AirbnbQuantity) + Borough + Month,data=df_price);summary(model11) ##not used

#import Rental data (by Neighborhood)
#StreetEasy aggreagate has more neighborhoods
medall          <- read.csv("~/Desktop/SeniorThesis/Data/Rental/NYC/medianAskingRentNeighbor.csv")
colnames(medall) <- as.character(unlist(medall[1,]))##unlist first, then assign col names as first row
medall        <- medall[-1,]
medall          <- melt(medall,id = "Date")
colnames(medall)[2] <- "Neighborhood"
colnames(medall)[3] <- "MedPrice"

invent2              <- read.csv("~/Desktop/SeniorThesis/Data/Rental/NYC/rentalInventoryNeighbor.csv")
colnames(invent2)    <- as.character(unlist(invent2[1,]))##unlist first, then assign col names as first row
invent2              <- invent2[-1,]
invent2              <- melt(invent2,id = "Date")
colnames(invent2)[2] <- "Neighborhood"
colnames(invent2)[3] <- "RentalInvent"

med1bed              <- read.csv("~/Desktop/SeniorThesis/Data/Rental/NYC/ZillowNeighbor1Bedroom.csv")
colnames(med1bed)    <- as.character(unlist(med1bed[1,]))##unlist first, then assign col names as first row
med1bed              <- med1bed[-1,]
colnames(med1bed)[1] <- "Date"
med1bed              <- melt(med1bed,id = "Date")
colnames(med1bed)[2] <- "Neighborhood"
colnames(med1bed)[3] <- "MedPrice1Bed"

med2bed              <- read.csv("~/Desktop/SeniorThesis/Data/Rental/NYC/ZillowNeighbor2Bedroom.csv")
colnames(med2bed)    <- as.character(unlist(med2bed[1,]))##unlist first, then assign col names as first row
med2bed              <- med2bed[-1,]
colnames(med2bed)[1] <- "Date"
med2bed              <- melt(med2bed,id = "Date")
colnames(med2bed)[2] <- "Neighborhood"
colnames(med2bed)[3] <- "MedPrice2Bed"

med3bed              <- read.csv("~/Desktop/SeniorThesis/Data/Rental/NYC/ZillowNeighbor3Bedroom.csv")
colnames(med3bed)    <- as.character(unlist(med3bed[1,]))##unlist first, then assign col names as first row
med3bed              <- med3bed[-1,]
colnames(med3bed)[1] <- "Date"
med3bed              <- melt(med3bed,id = "Date")
colnames(med3bed)[2] <- "Neighborhood"
colnames(med3bed)[3] <- "MedPrice3Bed"

medstd              <- read.csv("~/Desktop/SeniorThesis/Data/Rental/NYC/ZillowNeighborStudio.csv")
colnames(medstd)    <- as.character(unlist(medstd[1,]))##unlist first, then assign col names as first row
medstd              <- medstd[-1,]
colnames(medstd)[1] <- "Date"
medstd              <- melt(medstd,id = "Date")
colnames(medstd)[2] <- "Neighborhood"
colnames(medstd)[3] <- "MedPriceStudio"

#convert Date & get Month,Year to merge
invent2$Date  <- paste(invent2$Date,"-01")
invent2$Date  <- as.Date(invent2$Date,format="%Y-%m -%d")
invent2$Month <- month(ymd(invent2$Date))
invent2$Year  <- year(ymd(invent2$Date))
invent2       <- invent2[-1]

medall$Date  <- paste(medall$Date,"-01")
medall$Date  <- as.Date(medall$Date,format="%Y-%m -%d")
medall$Month <- month(ymd(medall$Date))
medall$Year  <- year(ymd(medall$Date))
medall       <- medall[-1]

med1bed$Date  <- paste(med1bed$Date,"-01")
med1bed$Date  <- as.Date(med1bed$Date,format="%Y-%m -%d")
med1bed$Month <- month(ymd(med1bed$Date))
med1bed$Year  <- year(ymd(med1bed$Date))
med1bed       <- med1bed[-1]

med2bed$Date  <- paste(med2bed$Date,"-01")
med2bed$Date  <- as.Date(med2bed$Date,format="%Y-%m -%d")
med2bed$Month <- month(ymd(med2bed$Date))
med2bed$Year  <- year(ymd(med2bed$Date))
med2bed       <- med2bed[-1]

med3bed$Date  <- paste(med3bed$Date,"-01")
med3bed$Date  <- as.Date(med3bed$Date,format="%Y-%m -%d")
med3bed$Month <- month(ymd(med3bed$Date))
med3bed$Year  <- year(ymd(med3bed$Date))
med3bed       <- med3bed[-1]

medstd$Date  <- paste(medstd$Date,"-01")
medstd$Date  <- as.Date(medstd$Date,format="%Y-%m -%d")
medstd$Month <- month(ymd(medstd$Date))
medstd$Year  <- year(ymd(medstd$Date))
medstd       <- medstd[-1]

#create Airbnb neighborhood quantity df
temp2 <- as.data.frame(table(df$neighbourhood,df$date)) ##Airbnb data set has 273 neighborhooods
colnames(temp2)[1] <-"Neighborhood"
colnames(temp2)[2] <-"Date"
colnames(temp2)[3] <-"AirbnbQuantity"
temp2$Month <- month(ymd(temp2$Date))
temp2$Year  <- year(ymd(temp2$Date))
temp2       <- temp2[-2]
#merge rental dfs
dftemp   <- merge(temp2,invent2,by = c("Year","Month","Neighborhood"))
df_rent2 <- merge(dftemp,medall,by = c("Year","Month","Neighborhood")) ##df_rent2 is df for streeteasy price and invent
df_rentz <- merge(df_rent2,med1bed,by = c("Year","Month","Neighborhood")) ##z is for Zillow
df_rentz <- merge(df_rentz,med2bed,by = c("Year","Month","Neighborhood"))
df_rentz <- merge(df_rentz,medstd,by = c("Year","Month","Neighborhood"))
df_rentz <- merge(df_rentz,med3bed,by = c("Year","Month","Neighborhood"))
#regression model for Airbnb Quantity on Inventory and Med Asking Price by Neighborhood
df_rent2$Neighborhood <- as.character(df_rent2$Neighborhood)
df_rent2$Month        <- as.factor(df_rent2$Month)
df_rentz$Month        <- as.factor(df_rentz$Month)

df_rent2$RentalInvent <- as.integer(df_rent2$RentalInvent)
df_rent2$MedPrice     <- as.integer(df_rent2$MedPrice)
df_rentz$MedPrice1Bed <- as.integer(df_rentz$MedPrice1Bed)
df_rentz$MedPrice2Bed <- as.integer(df_rentz$MedPrice2Bed)
df_rentz$MedPrice3Bed <- as.integer(df_rentz$MedPrice3Bed)
df_rentz$MedPriceStudio <- as.integer(df_rentz$MedPriceStudio)

df_rent2 <- df_rent2[df_rent2$AirbnbQuantity != 0,] ##remove AirbnbQuantity == 0: 535 cases
df_rent2 <- df_rent2[df_rent2$RentalInvent != 0,] ##remove RentalInvent == 0: 295 cases
df_rentz <- df_rentz[df_rentz$AirbnbQuantity != 0,] ##remove AirbnbQuantity == 0: 535 cases

model12 <- lm(log(RentalInvent)~log(AirbnbQuantity) + Neighborhood + Month,data=df_rent2);summary(model12) 
model13 <- lm(log(MedPrice)~log(AirbnbQuantity) + Neighborhood + Month,data=df_rent2);summary(model13)
model14 <- lm(log(MedPrice1Bed)~log(AirbnbQuantity) + Neighborhood + Month,data=df_rentz);summary(model14)
model15 <- lm(log(MedPrice2Bed)~log(AirbnbQuantity) + Neighborhood + Month,data=df_rentz);summary(model15)
model16 <- lm(log(MedPrice3Bed)~log(AirbnbQuantity) + Neighborhood + Month,data=df_rentz);summary(model16)
model17 <- lm(log(MedPriceStudio)~log(AirbnbQuantity) + Neighborhood + Month,data=df_rentz);summary(model17)

#ROBUSTNESS
#random effects equation
rd <- pdata.frame(df[,c("id","month_year","logPrice","review_bin","room_type","DistanceCentral","availability_bin",
                        "neighbourhood_group","calculated_host_listings_count")]) 
rd <- rd[!duplicated(rd[,c("id","month_year")]),] ##unique id-month_year pair
any(table(rd$id,rd$month_year)>1) ##should be FALSE
#modelrd <- plm(logPrice~review_bin + room_type + DistanceCentral + factor(neighbourhood_group) +
#                factor(availability_bin) + calculated_host_listings_count,data=rd,model="random",
#               index=c("id","month_year"))
summary(modelrd)

#modelrd2 <- plm(logPrice~review_bin + room_type + DistanceCentral + factor(neighbourhood_group) +
#                 factor(availability_bin) + calculated_host_listings_count + factor(month_year),data=rd,model="random",
#              index="id") ##this might be the right one
summary(modelrd2)
#get price indexes
out5 <- modelrd2$coefficients[16:58]
out5 <- exp(out5)*100
out5 <- c(100,out5)

#new constructions data
library("RSocrata")

dfcon <- read.socrata(
  "https://data.cityofnewyork.us/resource/ipu4-2q9a.csv?borough=BROOKLYN",
  app_token = "fruB0NPMd4YXrvb2fUBRMr4Qa",
  email     = "tdnguyen@haverford.edu",
  password  = "@Nddt050797"
)


#TABLES:
#Airbnb Descriptive Stats
stargazer(df4,median=T)
#Airbnb Room Type
stargazer(roomtype[1:3,],summary=F)
#Airbnb By Borough
stargazer(borough[1:5,],summary=F)
#Airbnb By Availability bin
stargazer(avbin[2:4,],summary=F)
#Hotel Descriptive Stats (2015-2018)
stargazer(df6,median=T,type="text")
#Hedonic Model
stargazer(model1b,single.row = T,type="text")
#Regression Model for effect of Airbnb on hotel outcomes
stargazer(model2,model3,model4,single.row = T)
#Regression of hotel price on occupance (the seasons are driving occupancy)
stargazer(model6,single.row = T)
#Regression Model for price change of hotel pre and post Airbnb
stargazer(model7,single.row = T)
#Price Elasticity Model
stargazer(model8,model8b,model8c,single.row=T)
#Welfare Table
stargazer(welfare,summary=F,digits=0,rownames=F,single.row=T)
#Rental Descriptive Stats (Monthly-Borough)
stargazer(df_rent,type="text")
#Rental Descriptive Stats (Monthly-Neighborhood)
stargazer(df_rentz,type="text")
#Rental by Borough
stargazer(borough2,summary=F)
#Regression Model for effect of Airbnb Quantity on rental Invent and Med Asking (Borough)
stargazer(model9,model10,single.row=T,type="text")
#Regression Model for effect of Airbnb Quantity on rental Invent and Med Asking (Neighborhood)
stargazer(model12,model13,model14,model15,model16,model17,single.row=T)
#Random Effects
stargazer(modelrd,single.row=T)
stargazer(modelrd2,single.row=T)

#FIGURES:
#Plot of Airbnb Price Index over time
b <- airbnb
b$Month <- month.abb[b$Month]
b$Date  <- paste(01,b$Month,b$Year)
b$Date  <- as.Date(b$Date,format = "%d %b %Y")
plot(b$Date,out,type="l",
     xlab = "Date", ylab = "Airbnb Price Index")
#Plot of Airbnb & Hotel Price Index over time
plot(b$Date,out,type="l", ylim=c(90, 180), lty=1,
     xlab = "Date", ylab = "Price Index")
lines(b$Date,out2,lty=2)
legend("topright",legend=c("Airbnb","Hotel"),
       lty=c(1, 2), cex=0.8)
#Plot Airbnb Growth over time
df_m$AirbnbGrowth = df_m$QuantityAirbnb/(df_m$QuantityAirbnb + (df_m$Supply/30)) ##divided by 30
df_m$HotelGrowth  = df_m$Supply/(df_m$QuantityAirbnb*30 + df_m$Supply)
plot(b[1:42,]$Date,df_m$AirbnbGrowth, lty=1, type="l", ##b[1:42] since only 42 months
     xlab = "Date", ylab = "Estimated Shares of Rooms", ylim=c(0, 1))
lines(b[1:42,]$Date,df_m$HotelGrowth, lty=2)
legend("topright",legend=c("Airbnb","Hotel"),
       lty=c(1, 2), cex=0.8)
#Plot Airbnb Spread map
#See the other file!!
#Plot of Airbnb & Rental Price Index over time
df_rent$Month <- as.integer(df_rent$Month)
df_rent$Month2    <- month.abb[df_rent$Month]
df_rent$MonthYear <- paste(df_rent$Month2,"-01-",df_rent$Year)
df_rent$Date      <- as.Date(as.yearmon(df_rent$MonthYear,format="%b -%d- %Y"))
df_rent$MonthYear <- as.factor(as.yearmon(df_rent$Date))
model10 <- lm(log(MedPrice)~MonthYear, data=df_rent);summary(model10)
out3 <- model10$coefficients[2:44] ##full 2015-2018
out3 <- exp(out3)*100
out3 <- c(100,out3)
plot(b$Date,out,type="l", ylim=c(90, 120), lty=1,
     xlab = "Date", ylab = "Price Index")
lines(b$Date,out3,lty=2)
legend("topleft",legend=c("Airbnb","Rental"),
       lty=c(1, 2), cex=0.8)
#Plot Rental Median Price vs. Inventory by Neighborhood
df_rent$Borough <- as.factor(df_rent$Borough)
out3 <- lm(log(RentalInvent)~log(MedPrice),data=df_rent);summary(out3)
plot(log(df_rent$MedPrice),log(df_rent$RentalInvent),
     pch=c(0,1,2,4,5)[df_rent$Borough],xlab="log(Rent)",
     ylab="log(Rental Inventory)")
legend("bottomright",legend=c("Bronx","Brooklyn","Manhattan","Queens","Staten Island"),
       pch=c(0,1,2,4,5))
abline(out3)
#Plot of Airbnb Price Index between 2 models
plot(b$Date,out,type="l", ylim=c(90, 110),lty=1,
     xlab = "Date", ylab = "Price Index")
lines(b$Date,out5,lty=2)
legend("topleft",legend=c("FE Model","RE Model"),
       lty=c(1, 2), cex=0.8)