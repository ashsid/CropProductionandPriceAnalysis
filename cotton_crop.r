#article yb Business standard
#topic : Cotton crop likely to rise 15% in Karnataka
#date of publish : 30th Jan 2013

#article summary:
#the article describes the loss in cotton production due to floods in the year 20090
#Karnataka is the 6th largest producer of cotton in india.
#Due to floods and heavy rain in north karnataka districts, crop production has been damaged.
#Some of the north Karnataka districts effected are : Davangere, Dharwad, Raichur, Gulbarga. 
#While the recent heavy rains have damaged crop in some areas, in other areas the crop is likely to be more.


#crop production data : extracting cotton data
data = read.csv("crop_production.csv")
kar_data = subset(data, State_Name == "Karnataka" & Crop == "Cotton(lint)" &   Crop_Year <= 2011 & Crop_Year >= 2007)
kar_data = kar_data[order(kar_data$Crop_Year),]
kar_data$yield = kar_data$Production/kar_data$Area

#rainfall data
rainfall_data = read.csv("rainfall in india 1901-2015.csv")
rainfall_north_interior_kar = subset(rainfall_data,SUBDIVISION == "NORTH INTERIOR KARNATAKA" & YEAR >=2007 & YEAR <=2011 )

library(plotly)
#visualizing annual rainfall for the year 2007 - 2011
plot_ly(rainfall_north_interior_kar,x = ~YEAR) %>%
   add_trace(y = ~ANNUAL, name = "Annual rainfall", mode = "lines + markers")
#visualizing kharif season rainfall(june,july,aug,sept) for the years 2007-2010

plot_ly(rainfall_north_interior_kar, x= ~YEAR) %>%
  add_trace(y = ~JUN, name = "June",mode = "lines+makers")%>%
  add_trace(y = ~JUL, name = "July",mode = "lines+makers")%>%
  add_trace(y = ~AUG, name = "August",mode = "lines+makers")%>%
  add_trace(y = ~SEP, name = "September",mode = "lines+makers")

#BELGAUM
belgaum_production = subset(kar_data, District_Name == "BELGAUM" & Season != "Rabi")
plot_ly(belgaum_production, x=~Crop_Year) %>%
  add_trace(y = ~yield, name="DAVANGERE", mode = "lines+markers")
#from our observation we can makeout that, cotton production in 2009 is more compared to 2008 
#but the growth form 2009 to 2010 is not considerable.

#DAVANGERE
#visualization of cotton yield in davangere district
davangere_production = subset(kar_data, District_Name == "DAVANGERE" & Season != "Rabi")
plot_ly(davangere_production, x=~Crop_Year) %>%
  add_trace(y = ~yield, name="DAVANGERE", mode = "lines+markers")
#from the rainfall and above graph we can make out that cotton production and considerably decreased
# in the year 2009 due to heavy rainfall.
#the Articles turns out to be correct based on our data

#RAICHUR
raichur_production = subset(kar_data, District_Name == "RAICHUR" & Season != "Rabi")
plot_ly(raichur_production, x=~Crop_Year) %>%
  add_trace(y = ~yield, name="DAVANGERE", mode = "lines+markers")
#same with raichur, the production is seen decreased in the year 2009-2010

#GULBARGA
gulbarga_production = subset(kar_data, District_Name == "GULBARGA" & Season != "Rabi")
plot_ly(gulbarga_production, x=~Crop_Year) %>%
  add_trace(y = ~yield, name="DAVANGERE", mode = "lines+markers")

