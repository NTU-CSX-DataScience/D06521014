#0928 R class
library(dplyr)
#library(hflights)
data <- read.csv("customer.csv", header=T, sep=",")
##############################
#             iss 1          #     
mutate(data,cardspent+card2spent)
data <-data%>%
  mutate(spendtotal = cardspent+card2spent)
###############################
#             iss 2           #
data <-data%>%
  mutate(pet=pets_cats+pets_dogs+pets_birds+pets_small+pets_saltfish+pets_freshfish)
#marriage
marri<-data%>%
  select(pet,marital) %>%
  group_by(marital)

boxplot(marri$pet,marri$marital)
# income cardspent 

incard<-filter(data,income>=mean(income),cardspent>mean(cardspent),card2spent>mean(card2spent))
###############################
#             iss 3           #
# income cardspent
incard2<-select(data,income,cardspent,card2spent)
plot(incard2$income,incard2$cardspent)
# income cardspent
incard3<-select(data,income,edcat,spendtotal)%>%
  filter(income>=mean(income),edcat<=2)
incard4<-select(data,income,edcat,spendtotal)%>%
  filter(income>=mean(income),edcat>2)
boxplot(incard3$spendtotal,incard4$spendtotal)
