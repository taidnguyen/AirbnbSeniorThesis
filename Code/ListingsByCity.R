#set csv folder as working directory (CHANGE:NYC/SF/Boston)
#import all csv files in folder
temp = list.files(pattern="*.csv")
myfiles = lapply(temp,read.delim)
#create df
x <- c("Date","nListings")
df <- data.frame(matrix(ncol=2,nrow=0))
colnames(df) <-x

#get number of listings
for (i in 1:length(myfiles)){
  df[nrow(df)+1,] = c(i,nrow(myfiles[[i]]))
  i+1
}
#get dates
ndates <- list.files()
ndates <- as.Date(ndates, "bos%Y-%m-%d");ndates #CHANGE (nyc/sf/bos)
df["Date"] = ndates
#set location & df
df["Location"] = "Boston"#CHANGE (New York City/San Francisco/Boston)
df_bos = df #CHANGE (df_nyc/df_sf/df_bos)

#combine dfs
df_total <- rbind(df_nyc,df_sf,df_bos)
#write to csv (set right wd first)
write.csv(df_total,file="ListingsByCity.csv")
#plot
attach(df_total)
require(ggplot2)
ggplot(df_total,aes(x=Date,y=nListings,col=Location))+
  geom_line(alpha=0.5)+
  ggtitle("Airbnb Market Penetration")+
  ylab("Number of Listings")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

#going back
listing <- read.csv("~/Desktop/SeniorThesis/Exhibits/ListingsByCity.csv")
df_total<-data.frame(listing) #make sure type is double-integer-character

#get date of week
attach(df_total)
Date <- as.Date(Date, "%Y-%m-%d")
df_total["DayOfWeek"] = weekdays(Date)
#bar plot
attach(df_total)
DayOfWeek<-ordered(DayOfWeek,levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                      "Friday", "Saturday", "Sunday"))
barplot(table(DayOfWeek),
        main="Day of Week Distribution for Web-scraped Data",
        ylab="Frequency",
        density=10,
        las=2)


