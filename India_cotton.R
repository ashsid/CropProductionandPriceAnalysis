#https://economictimes.indiatimes.com/news/economy/agriculture/india-set-to-be-worlds-largest-cotton-producer-by-2022/articleshow/20464710.cms
#https://economictimes.indiatimes.com/markets/commodities/organic-cotton/articleshow/31803212.cms

food <- read.csv(file="crop_production.csv", header=TRUE, sep=",")
food$Yeild = food$Production/food$Area
cotton_1 = food[food$Crop=="Cotton(lint)" ,]
cotton_1 = cotton_1[cotton_1$Season=="Kharif" ,]
cotton_2 = aggregate(cotton_1$Production, by=list(State_name=cotton_1$State_Name,Crop_year=cotton_1$Crop_Year),na.rm=TRUE, FUN=sum)

cotton_3 = aggregate(cotton_2$x, by=list(State_name=cotton_2$State_name),FUN=sum)
#top prouducers of cotton
#Gujarat, Maharashtra, Telangana, Punjab, Haryana, Andhra Pradesh, Karnataka, Madhya Pradesh, Rajasthan, Tamil Nadu

top_cotton_state = cotton_2[cotton_2$State_name %in% c("Gujarat", "Maharashtra", "Telangana", "Punjab", "Haryana", "Andhra Pradesh", "Karnataka", "Madhya Pradesh", "Rajasthan", "Tamil Nadu"),]

library(ggplot2)
p = ggplot(data=top_cotton_state, aes(x=Crop_year, y=x, group = State_name, colour = State_name)) +
  geom_line() +
  geom_point( size=1, fill="white")
print(p)

cotton_4 = aggregate(top_cotton_state$x, by=list(Crop_year=top_cotton_state$Crop_year),na.rm=TRUE, FUN=sum)

p1 = ggplot(data=cotton_4, aes(x=Crop_year, y=x)) +
  geom_line() +
  geom_point( size=1, fill="white")
print(p1)

