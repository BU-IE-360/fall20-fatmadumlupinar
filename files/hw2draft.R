library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(lubridate)
library(zoo)
library(corrplot)
library(hrbrthemes)
library(viridis)
library(ggridges)
setwd("C:/Users/fatma/Desktop/IE360/HW/hw2")


data<-read_excel("data/data.xlsx")  

data<-data%>%rename(Date=Tarih,
                      ExchangeRate_USD=`TP DK USD S YTL`, 
                      Vehicle.InterestRate=`TP KTF11`,
                      Vehicle.PriceIndex=`TP FG J071`)%>%
  mutate(Date=as.Date(as.yearmon(Date)))

data<-data[,c(1,2,5,8)]

min1<-min(data$ExchangeRate_USD)
max1<-max(data$ExchangeRate_USD)
min2<-min(data$Vehicle.InterestRate)
max2<-max(data$Vehicle.InterestRate)
min3<-min(data$Vehicle.PriceIndex)
max3<-max(data$Vehicle.PriceIndex)

data<-data%>%mutate(N_ExchangeRate_USD=(ExchangeRate_USD-min1)/(max1-min1),
              N_Vehicle.InterestRate=(Vehicle.InterestRate-min2)/(max2-min2),
              N_Vehicle.PriceIndex=(Vehicle.PriceIndex-min3)/(max3-min3))

ggplot(data,aes(x=Date))+
  geom_line(size=1,color="brown",aes(y=ExchangeRate_USD))+
  theme_ipsum()+
  ggtitle("Time Series of Exchange Rate USD/TRY",
          subtitle="Between the Years 2012-2020")

ggplot(data,aes(x=Date))+
  geom_line(size=1,color="purple",aes(y=Vehicle.PriceIndex))+
  theme_ipsum()+
  ggtitle("Time Series of Price Index of Vehicle",
          subtitle="Between the Years 2012-2020")
ggplot(data,aes(x=Date))+
  geom_line(size=1,color="darkgreen",aes(y=Vehicle.InterestRate))+
  theme_ipsum()+
  ggtitle("Time Series of Vehicle Interest Rate",
          subtitle="Between the Years 2012-2020")
  


dist1<-data%>%
  ggplot(.,aes(x=ExchangeRate_USD))+
  geom_density(color="darkgrey",fill="brown",adjust=1.5, alpha=.4) +
  theme_ipsum()+
  ggtitle("Distribution of Exchange Rate Values")
dist1
print(paste("Variance of Exchange Rate Values:", var(data$ExchangeRate_USD)))
print(paste("Mean of Exchange Rate Values:",mean(data$ExchangeRate_USD)))

dist1+
  facet_wrap(~year(Date))

dist2<-data%>%
  ggplot(.,aes(x=Vehicle.PriceIndex))+
  geom_density(color="darkgrey",fill="purple",adjust=1.5, alpha=.4) +
  theme_ipsum()+
  ggtitle("Distribution of Price Index Values")
dist2
print(paste("Variance of Price Index Values:",var(data$Vehicle.PriceIndex)))
print(paste("Mean of Price Index Values:",mean(data$Vehicle.PriceIndex)))

dist2+
  facet_wrap(~year(Date))

dist3<-data%>%
  ggplot(.,aes(x=Vehicle.InterestRate))+
  geom_density(color="darkgrey",fill="darkgreen",adjust=1.5, alpha=.4) +
  theme_ipsum()+
  ggtitle("Distribution of Interest Rate Values")
dist3
print(paste("Variance of Interest Rate Values:",var(data$Vehicle.InterestRate)))
print(paste("Mean of Interest Rate Values:",mean(data$Vehicle.InterestRate)))

dist3+
  facet_wrap(~year(Date))

cor_numVar <- cor(data[,c(2,3,4)], use="pairwise.complete.obs") 

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")



data%>%pivot_longer(.,cols=c(N_ExchangeRate_USD,N_Vehicle.InterestRate,N_Vehicle.PriceIndex),
                    names_to="name",
                    values_to="values")%>%
  ggplot(.,aes(x=Date,y=values,color=name,shape=name))+
  geom_line(size=1)+
  labs(color="Normalized Economic Measures",
       y="Normalized Values")+
  scale_colour_discrete()+
  theme_ipsum()+
  ggtitle("Comparison of Time Series",
          subtitle="With the Normalized Values")


norm1<-data%>%pivot_longer(.,cols=c(N_ExchangeRate_USD,N_Vehicle.InterestRate,N_Vehicle.PriceIndex),
                    names_to="name",
                    values_to="values")%>%
  ggplot(.,aes(x=values,y=factor(name),fill=factor(name)))+
  geom_density_ridges() +
  labs(color="Normalized Economic Measures",
       y="Normalized Values")+
  theme_ipsum()+
  theme(legend.position = "none")+
  ggtitle("Distribution of the Normalized Values")
norm1

var(data$N_ExchangeRate_USD)
var(data$N_Vehicle.PriceIndex)
var(data$N_Vehicle.InterestRate)

norm1+
  facet_wrap(~year(Date))
d<-data%>%group_by(year(Date))%>%
  mutate(ExchangeRateUSD_VehicleInterestRate=cor(N_ExchangeRate_USD,N_Vehicle.InterestRate),
         ExchangeRateUSD_VehiclePriceIndex=cor(N_ExchangeRate_USD,N_Vehicle.PriceIndex),
         VehiclePriceIndex_VehicleInterestRate=cor(N_Vehicle.InterestRate,N_Vehicle.PriceIndex))

res1 <- cor.test(data$N_ExchangeRate_USD, data$N_Vehicle.InterestRate,  method="kendall")
res1
res2 <- cor.test(data$N_ExchangeRate_USD, data$N_Vehicle.PriceIndex,  method="kendall")
res2
res3 <- cor.test(data$N_Vehicle.PriceIndex, data$N_Vehicle.InterestRate,  method="kendall")
res3

d%>%
  pivot_longer(.,cols=c(ExchangeRateUSD_VehicleInterestRate,ExchangeRateUSD_VehiclePriceIndex,VehiclePriceIndex_VehicleInterestRate))%>%
  ggplot(., aes(x=year(Date), y=value)) +
  geom_line( aes(color=name)) +
  geom_point(aes( fill=name),shape=21, color="black", size=6) +
  theme_ipsum()+
  labs(fill="Between:",color="Between:",
       y="Correlation",
       x="Year")+
  facet_wrap(~name)+
  ggtitle("Pairwise Yearly Correlations")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.x = element_blank())+
  scale_fill_discrete(labels=c("ExchangeRate-InterestRate","ExchangeRate-PriceIndex","PriceIndex-InterestRate"))+
  scale_color_discrete(labels=c("ExchangeRate-InterestRate","ExchangeRate-PriceIndex","PriceIndex-InterestRate"))
  


d%>%mutate(avg_corr=sum(ExchangeRateUSD_VehicleInterestRate,
                         ExchangeRateUSD_VehiclePriceIndex,
                         VehiclePriceIndex_VehicleInterestRate)/(3*length(year(Date))))%>%
  ggplot(., aes(x=year(Date), y=avg_corr)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  theme_ipsum()+
  labs(y="Correlation",
       x="Year")+
  ggtitle("Average of the Pairwise Correlations",
          subtitle="Yearly")
