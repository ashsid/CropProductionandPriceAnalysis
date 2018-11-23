#link to article : https://www.hindustantimes.com/business/india-s-foodgrain-production-to-grow-by-0-6-in-2012-13-cmie/story-wf18uyV180Pi8Q23JP9rGL.html

library(plotly)
library(ggplot2)

production_data = read.csv("../Dataset/crop_production.csv")
rainfall_data = read.csv("../Dataset/rainfall in india 1901-2015.csv")

production_data$yield = production_data$Production/production_data$Area
production_data = subset(production_data, is.na(yield)==FALSE)
production_from_2011_2013 = subset(production_data, Crop_Year >= 2011 & Crop_Year <=2013)
production_from_2011_2013 = production_from_2011_2013[order(production_from_2011_2013$Crop_Year),]


#visualizing rainfall from year 2000 to 2013
rainfall_2000_2013 = subset(rainfall_data, YEAR >=2000 & YEAR<=2013)
rainfall_2000_2013 = rainfall_2000_2013[order(rainfall_2000_2013$YEAR),]
rainfall_average = aggregate(rainfall_2000_2013$ANNUAL, by = list(rainfall_2000_2013$YEAR),na.rm=TRUE, FUN = mean)
colnames(rainfall_average) = c("year","avg")
plot_ly(rainfall_average, x=~year)%>%
  add_trace(y=~avg,mode="lines+markers")



#sugarcane
production_sugarcane = subset(production_from_2011_2013, Crop == "Sugarcane" & is.na(Production)==FALSE)
production_sugarcane_avg = aggregate(production_sugarcane$yield,by=list(production_sugarcane$Crop_Year,production_sugarcane$Season),na.rm=TRUE, FUN=mean)
colnames(production_sugarcane_avg) = c("YEAR","SEASON","AVG")
#production_sugarcane_avg = production_sugarcane_avg[order(production_sugarcane_avg$YEAR),]

plot(production_sugarcane_avg$AVG,type='l')

sugarcane = ggplot(data=production_sugarcane_avg, aes(x=YEAR, y = AVG, group = SEASON, colour = SEASON))+
  geom_line()+
  geom_point(size=1, fill="white")
print(sugarcane)

#cotton
production_cotton = subset(production_from_2011_2013, Crop == "Cotton(lint)" & is.na(Production)==FALSE)
production_cotton_avg = aggregate(production_cotton$yield,by=list(production_cotton$Crop_Year,production_cotton$Season),na.rm=TRUE, FUN=mean)
colnames(production_cotton_avg) = c("YEAR","SEASON","AVG")
production_cotton_avg = subset(production_cotton_avg, SEASON != "Whole Year")
cotton = ggplot(data=production_cotton_avg, aes(x=YEAR, y = AVG, group = SEASON, colour = SEASON))+
  geom_line()+
  geom_point(size=1, fill="white")
print(cotton)

#oilseeds
production_oilseeds = subset(production_data, Crop %in% c("Oilseeds total","other oilseeds")  & Crop_Year>=2010 & Crop_Year<=2013)
production_oilseeds = production_oilseeds[order(production_oilseeds$Crop_Year),]
oilseeds_avg = aggregate(production_oilseeds$yield, by=list(production_oilseeds$Crop_Year,production_oilseeds$Season),na.rm = TRUE, FUN = mean)
colnames(oilseeds_avg) = c("YEAR","SEASON","AVG")
oilseeds_avg = subset(oilseeds_avg, SEASON == "Whole Year")
plot_ly(oilseeds_avg,x=~YEAR,y=~AVG,mode="lines+markers")


#total ptoduction
total_average_production = aggregate(production_data$yield, by=list(production_data$Crop_Year,production_data$Season),na.rm=TRUE,FUN=mean)
colnames(total_average_production) = c("YEAR","SEASON","AVERAGE")
total_average_production = subset(total_average_production, YEAR %in% c("2011","2012","2013"))
total = ggplot(data=total_average_production, aes(x=YEAR, y = AVERAGE, group = SEASON, colour = SEASON))+
  geom_line()+
  geom_point(size=1, fill="white")
print(total)
