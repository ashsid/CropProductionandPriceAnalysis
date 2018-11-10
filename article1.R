#Descriptive Analysis of the article:
#https://www.livemint.com/Politics/c0mp0BDBW974jmXjqlTbTM/Where-are-the-horticulture-hotspots-of-India.html

data <- read.csv('crop_production.csv')
crops <- unique(data[["Crop"]])
fruits <- data.frame(c('Banana','Citrus Fruits','Grapes','Mango','Orange','Other Fresh Fruits','Papaya','Pome Fruit','Pome Granet','Sapota','Pineapple','Jack Fruit','Other Citrus Fruits','Water Melon','Apple','Peach','Pear','Litchi','Plums','Lemon','Tomato'))
colnames(fruits)[1] <- "Fruits"
fruits_data <- subset(data, data$Crop %in% fruits$Fruits)
fruits_data <- data.frame(fruits_data$State_Name,fruits_data$Crop_Year,fruits_data$Crop,fruits_data$Production)
states <- unique(data[["State_Name"]])
fruits_data[, 2] <- as.numeric(as.character( fruits_data[, 2] ))
fruits_data[, 4] <- as.numeric(as.character( fruits_data[, 4] ))
colnames(fruits_data)[1] <- "State"
colnames(fruits_data)[2] <- "Year"
colnames(fruits_data)[3] <- "Crop"
colnames(fruits_data)[4] <- "Production"
fruits_data %>%
  group_by(State,Crop,Year) %>%
  summarise_all(sum) %>%
  data.frame() -> res1
colnames(res1)[1] <- "State"
colnames(res1)[2] <- "Crop"
colnames(res1)[3] <- "Year"
colnames(res1)[4] <- "Total_Fruit_Production"
Years <- data.frame(c("2010","2011","2012","2013","2014"))
colnames(Years)[1] <- "LATEST_YEARS"
res1_latest_years <- data.frame(subset(res1, res1$Year %in% Years$LATEST_YEARS))
res1_latest_years <- data.frame(res1_latest_years$State,res1_latest_years$Year,res1_latest_years$Total_Fruit_Production)

colnames(res1_latest_years)[1] <- "State"
colnames(res1_latest_years)[2] <- "Year"
colnames(res1_latest_years)[3] <- "Total_Production"

res1_latest_years %>%
  group_by(State,Year) %>%
  summarise_all(sum) %>%
  data.frame() -> res1_every_year


major_states <- data.frame(aggregate(res1_every_year$Total_Production, by = list(State = res1_every_year$State) , na.rm =TRUE , FUN = mean))
major_states <- major_states[order(-major_states$x),]
colnames(major_states)[2] <- "Average Production"
top_5_states <- data.frame(major_states[1:5,"State"])
colnames(top_5_states)[1] <- "State"

print(top_5_states)
#The Top 5 states in horticluture(fruit cultivation)
#According to article , the top 5 states were Maharashtra , Andhra Pradesh, Uttar Pradesh ,Gujarat and Tamil Nadu.
#We have got the top 5 states to be - Tamil Nadu , Andhra Pradesh, Gujarat , Karnataka and Assam.(very less data for Maharashtra)

#Computation 2 , growth of various crops in each of the top 5 states as obtained above.
res2 <- data.frame(subset(data, data$State_Name %in% top_5_states$State))
best_states <- data.frame(res2$State_Name, res2$Crop, res2$Crop_Year,res2$Production )
best_states_fruit <- data.frame(subset(best_states, best_states$res2.Crop %in% fruits$Fruits))

colnames(best_states_fruit)[1] <- "State"
colnames(best_states_fruit)[2] <- "Crop"
colnames(best_states_fruit)[3] <- "Year"
colnames(best_states_fruit)[4] <- "Production"

ap_data <- data.frame(subset(best_states_fruit, best_states_fruit$State =='Andhra Pradesh'))
ap_data <- data.frame(ap_data$Crop,ap_data$Year,ap_data$Production)

colnames(ap_data)[1] <- "Crop"
colnames(ap_data)[2] <- "Year"
colnames(ap_data)[3] <- "Production"

ap_data <- data.frame(aggregate(ap_data$Production, by = list(ap_data$Crop,ap_data$Year) , na.rm =TRUE , FUN = sum))

colnames(ap_data)[1] <- "Crop"
colnames(ap_data)[2] <- "Year"
colnames(ap_data)[3] <- "Production"

ap_p <- ggplot(data = ap_data, aes(x = Year, y = Production, group = Crop, colour = Crop)) +
  geom_line() +
  geom_point(size = 1 , fill ="white")
print(ap_p + ggtitle("Andhra Pradesh Fruit Cultivation"))
#The plot for Andhra Pradesh Fruit Cultivation.


tn_data <- data.frame(subset(best_states_fruit, best_states_fruit$State =='Tamil Nadu'))
tn_data <- data.frame(tn_data$Crop,tn_data$Year,tn_data$Production)

colnames(tn_data)[1] <- "Crop"
colnames(tn_data)[2] <- "Year"
colnames(tn_data)[3] <- "Production"

tn_data <- data.frame(aggregate(tn_data$Production, by = list(tn_data$Crop,tn_data$Year) , na.rm =TRUE , FUN = sum))

colnames(tn_data)[1] <- "Crop"
colnames(tn_data)[2] <- "Year"
colnames(tn_data)[3] <- "Production"

tn_p <- ggplot(data = tn_data, aes(x = Year, y = Production, group = Crop, colour = Crop)) +
  geom_line() +
  geom_point(size = 1 , fill ="white")
print(tn_p + ggtitle("Tamil Nadu Fruit Cultivation"))
#The plot for Tamil Nadu Fruit Cultivation.

gj_data <- data.frame(subset(best_states_fruit, best_states_fruit$State =='Gujarat'))
gj_data <- data.frame(gj_data$Crop,gj_data$Year,gj_data$Production)

colnames(gj_data)[1] <- "Crop"
colnames(gj_data)[2] <- "Year"
colnames(gj_data)[3] <- "Production"

gj_data <- data.frame(aggregate(gj_data$Production, by = list(gj_data$Crop,gj_data$Year) , na.rm =TRUE , FUN = sum))

colnames(gj_data)[1] <- "Crop"
colnames(gj_data)[2] <- "Year"
colnames(gj_data)[3] <- "Production"

gj_p <- ggplot(data = gj_data, aes(x = Year, y = Production, group = Crop, colour = Crop)) +
  geom_line() +
  geom_point(size = 1 , fill ="white")
print(gj_p + ggtitle("Gujarat Fruit Cultivation"))
#The plot for Gujarat Fruit Cultivation.

kt_data <- data.frame(subset(best_states_fruit, best_states_fruit$State =='Karnataka'))
kt_data <- data.frame(kt_data$Crop,kt_data$Year,kt_data$Production)

colnames(kt_data)[1] <- "Crop"
colnames(kt_data)[2] <- "Year"
colnames(kt_data)[3] <- "Production"

kt_data <- data.frame(aggregate(kt_data$Production, by = list(kt_data$Crop,kt_data$Year) , na.rm =TRUE , FUN = sum))

colnames(kt_data)[1] <- "Crop"
colnames(kt_data)[2] <- "Year"
colnames(kt_data)[3] <- "Production"

kt_p <- ggplot(data = kt_data, aes(x = Year, y = Production, group = Crop, colour = Crop)) +
  geom_line() +
  geom_point(size = 1 , fill ="white")
print(kt_p + ggtitle("Karnataka Fruit Cultivation"))
#The plot for Karnataka Fruit Cultivation.



as_data <- data.frame(subset(best_states_fruit, best_states_fruit$State =='Assam'))
as_data <- data.frame(as_data$Crop,as_data$Year,as_data$Production)

colnames(as_data)[1] <- "Crop"
colnames(as_data)[2] <- "Year"
colnames(as_data)[3] <- "Production"

as_data <- data.frame(aggregate(as_data$Production, by = list(as_data$Crop,as_data$Year) , na.rm =TRUE , FUN = sum))

colnames(as_data)[1] <- "Crop"
colnames(as_data)[2] <- "Year"
colnames(as_data)[3] <- "Production"

as_p <- ggplot(data = as_data, aes(x = Year, y = Production, group = Crop, colour = Crop)) +
  geom_line() +
  geom_point(size = 1 , fill ="white")
print(as_p + ggtitle("Assam Fruit Cultivation"))
#The plot for Assam Fruit Cultivation.



