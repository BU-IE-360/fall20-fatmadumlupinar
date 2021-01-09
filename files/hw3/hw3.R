library(readxl)
library(zoo)
library(dplyr)
library(ggplot2)
library(lubridate)
library(forecast)
library(corrplot)
library(useful)
library(hrbrthemes)

data<-read_excel("data/data.xlsx")  
data<-data%>%
  mutate(Tarih=as.yearmon(Tarih))%>%
  rename(Date=Tarih,
         CPI=`TP FG J01`,
         ExRate_dollar=`TP DK USD S YTL`,
         interestRate=`TP KTF10`,
         cci=`TP GY1 N2`)%>%
  mutate(lag6=lag(CPI,6),
         lag1=lag(CPI,1),
         lag12=lag(CPI,12))
cci1<-read_excel("data/cci1.xlsx")%>%mutate(Tarih=as.yearmon(Tarih))%>%
  rename(Date=Tarih,
         cci1=`TP TG2 Y01`)
data<-left_join(data,cci1)
data<-data[,-5]%>%rename(cci=cci1)%>%relocate(cci,.before = lag6)


data<-data%>%mutate(trend=row_number(),
                    month=month(Date),
                    year=year(Date))
ggplot(data,aes(x=Date,y=CPI))+
  geom_line(size=1.2, color="darkred")+
  scale_x_yearmon()+
  labs(x="Date",
       y="CPI for Food and Non-Alcoholic Beverages",
       title="Monthly CPI for Food and Non-Alcoholic Beverages",
       subtitle="Over the years between 2003-2020")+
  theme_ipsum()

TimeSeries<-ts(data[,c(2,5,8,3,6,7,4)],frequency=12,start=c(2003,1))
plot(TimeSeries)
cor_numVar <- cor(data[,c(4,3,2,5,6,7,8)], use="pairwise.complete.obs") 

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

fit1<-lm(log(CPI)~trend+log(ExRate_dollar),data=data%>%filter(Date<"2020-12"))
summary(fit1)
checkresiduals(fit1)
fit2<-lm(log(CPI)~trend+log(ExRate_dollar)+
           as.factor(month),data=data%>%filter(Date<"2020-12"))
summary(fit2)
checkresiduals(fit2)
fit<-lm(log(CPI)~trend+log(ExRate_dollar)+log(lag1)+
          as.factor(month),data=data%>%filter(Date<"2020-12"))
summary(fit)
checkresiduals(fit)
predicted=predict(fit,newdata=data%>%filter(Date=="2020-12"))
binded1<-cbind(data[-c(1,216),],predicted=fitted(fit),residuals=residuals(fit))
binded2<-cbind(data[216,],predicted,residuals=NA)
data<-rbind(binded1,binded2)

ggplot(data = data%>%filter(Date<"2020-12"))+
  geom_point(aes(x=exp(predicted),y=residuals))+
  geom_abline(slope=1,color="red")+
  labs(x="CPI predicted",
       y="Residuals",
       title="Residuals Vs. Predicted Values")+
  theme_ipsum()

ggplot(data = data%>%filter(Date<"2020-12"))+
  geom_point(aes(x=lag1,y=residuals))+
  geom_abline(slope=1,color="red")+
  labs(x="lag1",
       y="Residuals",
       title="Residuals Vs. lag1")+
  theme_ipsum()

ggplot(data = data%>%filter(Date<"2020-12"))+
  geom_point(aes(x=ExRate_dollar,y=residuals))+
  geom_abline(slope=1,color="red")+
  labs(x="USD Exchange Rate",
       y="Residuals",
       title="Residuals Vs. USD Exchange Rate")+
  theme_ipsum()

ggplot(data = data%>%filter(Date<"2020-12"))+
  geom_point(aes(x=exp(predicted),y=CPI))+
  geom_abline(slope=1,color="red")+
  labs(x="CPI predicted",
       y="CPI actual",
       title="Actual Vs. Predicted Values")+
  theme_ipsum()

exp(predicted)
colors<-c("Predicted" = "red", "Actual" = "limegreen")
ggplot()+
  geom_line(data=data,aes(x=Date,y=exp(predicted),color="Predicted"),size=1.2)+
  geom_line(data=data,aes(x=Date,y=CPI,color="Actual"),size=1.2)+
  labs(x="Date",
       y="CPI",
       title="Actual and Predicted Values between 2003-2020",
       subtitle = "Prediction of CPI in 2020-12",
       color="CPI")+
  theme_ipsum()+
  theme(axis.text.x = element_text(angle = 45))+
  scale_color_manual(values = colors)

ggplot()+
  geom_line(data=data%>%filter(Date>"2019-12"),aes(x=Date,y=exp(predicted),color="Predicted"),size=1.2)+
  geom_line(data=data%>%filter(Date>"2019-12"),aes(x=Date,y=CPI,color="Actual"),size=1.2)+
  labs(x="Date",
       y="CPI",
       title="Actual and Predicted Values",
       subtitle = "Closer Look at the Prediction of CPI in 2020-12",
       color="CPI")+
  theme_ipsum()+
  theme(axis.text.x = element_text(angle = 45))+
  scale_color_manual(values = colors)

