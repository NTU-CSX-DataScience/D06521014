library(dplyr)
library(ggplot2)
library(gridExtra)
crashdata<- read.csv("Airplane_Crashes_and_Fatalities_Since_1908.csv", header=T, sep=",")

##   get year, month, day,hour data
crashdata$year<-substr(crashdata$Date, start =7 , stop =10 )
crashdata$month<-as.factor(substr(crashdata$Date, start =1 , stop =2 ))
crashdata$day<-substr(crashdata$Date, start =4 , stop =5)

#death events in each month
death_month<-
  crashdata%>%
  group_by(month)%>%
  summarise(cumulative_number=n())
ggplot(death_month, aes(x=month,y=cumulative_number)) + 
  geom_bar(stat="identity")+
  geom_text(aes(label=cumulative_number),vjust=1.5,colour="white")

#death events in each year
death_year<-
  crashdata%>%
  select(year,Aboard,Fatalities)%>%
  group_by(year)%>%
  na.omit()%>%
  summarise(events_number=n(),aboard=sum(Aboard),fatalities=sum(Fatalities),survival=sum(Aboard)-sum(Fatalities))

death_year$year<-as.integer(death_year$year)
death_year$survival_rate<-death_year$survival/death_year$aboard
g1<-ggplot(death_year, aes(year,events_number)) + 
  geom_line()+
  ggtitle("number of events")+
  theme(plot.title=element_text(vjust=-1.5))+
  theme(axis.title.x=element_blank())+
g2<-ggplot(death_year, aes(year,aboard)) + 
  geom_line()+
  ggtitle("aboard ")+
            theme(plot.title=element_text(vjust=-1.5))+
  theme(axis.title.x=element_blank())
g3<-ggplot(death_year, aes(year,fatalities)) + 
  geom_line()+
  ggtitle("fatalities" )+
            theme(plot.title=element_text(vjust=-1.5))+
  theme(axis.title.x=element_blank())
g4<-ggplot(death_year, aes(year,survival_rate)) + 
  geom_line()+
  ggtitle("survival_rate")+
  theme(plot.title=element_text(vjust=-1.5))+
  theme(axis.title.x=element_blank())
grid.arrange(g1, g2,g3,g4,nrow = 4)

ggplot(death_year, aes(year,survival_rate)) + 
  geom_line()

#death location

death_operator<-
  crashdata%>%
  filter(Fatalities>1& !is.na(crashdata$Operator) )%>%
  select(Operator,Aboard,Fatalities)%>%
  group_by(Operator)%>%
  na.omit()%>%
  summarise(cumulative_number=n(),fatalities=sum(Fatalities))%>%
  arrange(desc(fatalities))
death_operator_top<-head(death_operator,5)

ggplot(death_operator_top, aes(x=Operator,y=fatalities)) + 
  geom_bar(stat="identity")+
  geom_text(aes(label=fatalities),vjust=1.5,colour="white")




#death location

#for (i in 1:length(crashdata)){
#  x[i]<-strsplit(as.character(crashdata$Location[i]), split = ",")
#}

            death_location_tmp<-
              crashdata%>%
              select(Location,Aboard,Fatalities)%>%
              group_by(Location)%>%
              na.omit()
            x<-strsplit(as.character(death_location_tmp$Location), split = ",")
            x[[1]]
            tail(x[[1]],1)
            
            for (i in 1:length(death_location_tmp)){
              death_location_tmp$location[i]<-tail(x[[i]],1)
            }
            death_location_tmp$location[10]
            
            death_location<-death_location_tmp%>%
            group_by(location)%>%
              summarise(cumulative_number=n(),fatalities=sum(Fatalities))%>%
              arrange(desc(fatalities))

            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
death_location_tmp<-
  crashdata%>%
  select(Location,Aboard,Fatalities)%>%
  group_by(Location)%>%
  na.omit()

death_location_tmp$location<-death_location_tmp$Location

x<-strsplit(as.character(death_location_tmp$Location), split = ",")
for (i in 1:length(x)){
   death_location_tmp$location[i]<-x[[i]]%>%
    as.vector()%>%
    tail(1)
}
y<-tail(x,1)
for (i in 1:length(x)){
  death_location_tmp$location[i]<-tail(x[[i]],1)
}

#y<-unlist(x)
#  data.frame(matrix(unlist(x), nrow=length(x), byrow=T))


for (i in 1:length(death_location_tmp)){
  death_location_tmp$location[i]<-x[[i]][-1]
}

death_location_tmp$countury<-x[,-1]



  summarise(events_number=n(),aboard=sum(Aboard),fatalities=sum(Fatalities),survival=sum(Aboard)-sum(Fatalities))


