#https://timesofindia.indiatimes.com/business/india-business/Foodgrain-production-during-2014-15-crop-year-declines-by-13-92-MT/articleshow/47268453.cms

library(dplyr)
library(ggplot2)

crop <- read.csv(file="crop_production.csv", header=TRUE, sep=",")
crop_state = aggregate(crop$Production, by=list(StateName = crop$State_Name , Year=crop$Crop_Year , Season=crop$Season, Crop=crop$Crop),na.rm=TRUE, FUN=sum)
grain_state = crop_state[crop_state$Crop=="Rice" | crop_state$Crop=="Wheat" | crop_state$Crop=="Maize" | crop_state$Crop=="Arhar/Tur",]
grain_state = crop_state[crop_state$Crop %in% c("Rice","Wheat","Maize","Arhar/Tur","Urad","Bajra","Jowar"),]

#rice_state = grain_state[grain_state$Crop=="Rice",]
grain_year = aggregate(grain_state$x, by=list(Year=grain_state$Year, Crop=grain_state$Crop),na.rm=TRUE, FUN=sum)
grain_year = grain_year[grain_year$Year<"2015",]
grain_year = rename(grain_year,Production = x)

p = ggplot(data=grain_year, aes(x=Year, y=Production, group = Crop, colour = Crop)) +
  geom_line() +
  geom_point( size=1, fill="white")
print(p)

grain_year_scaled <- grain_year %>% group_by(Crop) %>% mutate(Scaled_production = scale(Production))

p1 = ggplot(data=grain_year_scaled, aes(x=Year, y=Scaled_production, group = Crop, colour = Crop)) +
  geom_line() +
  geom_point( size=2, fill="white")
print(p1)

agg_grain_scaled = aggregate(grain_year_scaled$Scaled_production, by=list(Year = grain_year_scaled$Year),na.rm=TRUE, FUN=sum)
agg_grain_scaled = rename(agg_grain_scaled,Scaled_production = x)
p2 = ggplot(data=agg_grain_scaled, aes(x=Year, y=Scaled_production)) +
  geom_line() +
  geom_point( size=2, fill="white")
print(p2)
#
