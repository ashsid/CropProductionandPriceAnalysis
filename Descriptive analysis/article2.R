#Descriptive Analysis of the Article -:
#https://economictimes.indiatimes.com/news/economy/agriculture/gujarat-records-highest-decadal-agricultural-growth-rate-of-10-97/articleshow/9243480.cms

library(ggplot2)

data <- read.csv('../Dataset/crop_production.csv')
guj <- subset(data, data$State_Name == "Gujarat")
guj <- data.frame(guj$Crop_Year,guj$Area,guj$Production)
colnames(guj) <- c("Year","Area","Production")
guj$Yield <- guj$Production/guj$Area
guj_data <- data.frame(aggregate(list(guj$Area,guj$Production,guj$Yield), by = list(Year = guj$Year), na.rm= TRUE , FUN =sum))
colnames(guj_data)<- c("Year", "Area","Production","Yield")
#the Total yield ,area and Production of Crops in Gujarat over the years.


p1 <- ggplot(data = guj_data, aes(x = Year, y = Area)) +
  geom_line() +
  geom_point(size = 1 , fill ="white")
print(p1 + ggtitle("Gujarat Area Plot"))
#The Gujarat Area plot

p2 <- ggplot(data = guj_data, aes(x = Year, y = Production)) +
  geom_line() +
  geom_point(size = 1 , fill ="white")
print(p2 + ggtitle("Gujarat Production Plot"))
#The Gujarat Production Plot

p3 <- ggplot(data = guj_data, aes(x = Year, y = Yield)) +
  geom_line() +
  geom_point(size = 1 , fill ="white")
print(p1 + ggtitle("Gujarat Yield Plot"))
#The Gujarat Yield Plot


com_p <- ggplot(guj_data, aes(Year)) + 
  geom_line(aes(y = scale(Area), colour = "Area")) + 
  geom_line(aes(y = scale(Production), colour = "Production"))+
  geom_line(aes(y = scale(Yield), colour = "Yield"))
print(com_p + ggtitle("Gujarat Combined Plot") + ylab("Value"))

#The Gujarat Combined plot




