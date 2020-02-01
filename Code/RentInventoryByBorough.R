#read file & df
inventory <- read.csv("~/Desktop/SeniorThesis/Exhibits/RentInventoryByBorough.csv")
df <- data.frame(inventory)
#remove first 2 rows
df<-df[-c(1,2),]
rownames(df) <-NULL
df["areaName"] <- paste(as.character(df[["areaName"]]),"-01")
df["Date"] <- as.Date(df[["areaName"]], format="%Y-%m -%d")


#plot
attach(df)
require(ggplot2)

ggplot(df,aes(x=Date,y=NYC,group=1))+
  geom_line(aes(color="All NYC"))+
  geom_line(data=df,aes(x=Date,y=as.integer(All.Brooklyn),color="All.Brooklyn"))+
  geom_line(data=df,aes(x=Date,y=as.integer(All.Downtown),color="red"))+
  geom_line(data=df,aes(x=Date,y=as.integer(All.Midtown),color="pink"))+
  geom_line(data=df,aes(x=Date,y=as.integer(All.Queens),color="tan"))+
  geom_line(data=df,aes(x=Date,y=as.integer(All.Upper.East.Side),color="gold"))+
  geom_line(data=df,aes(x=Date,y=as.integer(All.Upper.Manhattan),color="orange"))+
  geom_line(data=df,aes(x=Date,y=as.integer(All.Upper.West.Side),color="brown"))+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Rental Inventory in NYC by Borough")+
  ylab("Number of Inventory")
