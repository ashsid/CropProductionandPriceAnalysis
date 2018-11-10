#Descriptive Analysis of Spices cultivation in Kerala
#Article:
#http://www.fnbnews.com/Top-News/Spice-production-in-Kerala-increasing-despite-drop-in-cultivation-area


data <- read.csv('crop_production.csv')
crops <- data.frame(unique(data$Crop))
spices <- data.frame(c("Black pepper","Turmeric","Dry chillies","Garlic","Cardamom","Ginger","Cond-spcs other","Dry ginger"))
colnames(spices)[1]<-"Crop"
spices_data <- subset(data, data$Crop %in% spices$Crop)
spices_data <- data.frame(spices_data$State_Name, spices_data$Crop_Year,spices_data$Production)

colnames(spices_data) <- c("State","Year","Production")
spices_data_re <- data.frame(aggregate(spices_data$Production, by = list(spices_data$State,spices_data$Year),na.rm = TRUE ,FUN = sum))

colnames(spices_data_re) <- c("State","Year","Production")

spices_plot <- ggplot(data = spices_data_re, aes(x = Year, y = scale(Production), group = State, colour = State)) +
  geom_line() +
  geom_point(size = 1 , fill ="white")
print(spices_plot + ggtitle("Indian States Spices Cultivation") + ylab("Production"))
#The Plot of Spice Cultivation in the states of India

#Analysis for Kerala, as the article speaks about Kerala:
data$Yield <- data$Production/data$Area
spices_data_1 <- subset(data, data$Crop %in% spices$Crop)
spices_data_1 <- data.frame(spices_data_1$State_Name, spices_data_1$Crop_Year, spices_data_1$Crop, spices_data_1$Area,spices_data_1$Production,spices_data_1$Yield)
colnames(spices_data_1) <- c("State","Year","Crop","Area","Production","Yield")
kerala_spice_data <- subset(spices_data_1, spices_data_1$State == 'Kerala')
kerala_spice_data <- kerala_spice_data[, -1]

kerala_spice_data <- data.frame(aggregate(list(kerala_spice_data$Area,kerala_spice_data$Production,kerala_spice_data$Yield), by = list(kerala_spice_data$Year,kerala_spice_data$Crop), na.rm = TRUE , FUN = mean))
colnames(kerala_spice_data) <- c("Year","Crop","Area","Production","Yield")

#Kerala Spices Plot of Production over Years:

kr_1 <- ggplot(data = kerala_spice_data, aes(x = Year, y = scale(Production), group = Crop, colour = Crop)) +
  geom_line() +
  geom_point(size = 1 , fill ="white")
print(kr_1 + ggtitle("Kerala Spices:") + ylab("Production"))


#Kerala Spices Plot of Area over Years:

kr_2 <- ggplot(data = kerala_spice_data, aes(x = Year, y = scale(Area), group = Crop, colour = Crop)) +
  geom_line() +
  geom_point(size = 1 , fill ="white")
print(kr_2 + ggtitle("Kerala Spices:") + ylab("Area"))

#Kerala Spices Plot of Yield over Years:

kr_3 <- ggplot(data = kerala_spice_data, aes(x = Year, y = scale(Yield), group = Crop, colour = Crop)) +
  geom_line() +
  geom_point(size = 1 , fill ="white")
print(kr_3+ ggtitle("Kerala Spices:") + ylab("Yield"))

#Analysation of Spices growth in Kerala, as ascertained by the above plots.