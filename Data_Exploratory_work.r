
#Dataset - 1
#Dataset consists of 246091 rows with 7 columns
#the 7 attributes are {"State_Name","District_Name","Crop_Year","Season","Crop","Area","Production"}

crop_production = read.csv("crop_production.csv")
crop_production$yield = crop_production$Production/crop_production$Area
#extract state-list
state_list = as.data.frame(table(crop_production$State_Name))
state_list = state_list[order(-state_list$Freq),]

#district-list
district_list = as.data.frame(table(crop_production$District_Name))
district_list = district_list[order(-district_list$Freq),]

#different crops grown in india


crop_list = as.data.frame(unique(crop_production$Crop))

#the indian agriculture crops  are divided amoung major - 3 seasons
#Kharif : June - September
#Rabi :  October - December
#Summer : January - May
#Whole-year : Jan - Dec

#lets divided crops amoung season : 

Kharif_crops = subset(crop_production, Season == "Kharif")
Kharif_crops = Kharif_crops[order(Kharif_crops$Crop_Year),]
rabi_crops = subset(crop_production, Season == "Rabi")
rabi_crops = rabi_crops[order(rabi_crops$Crop_Year),]
summer_crops = subset(crop_production, Season == "Summer")
summer_crops = summer_crops[order(summer_crops$Crop_Year),]
whole_year = subset(crop_production, Season == "Whole Year")
whole_year = whole_year[order(whole_year$Crop_Year),]


#summary of each crop - Total production, average production, average land area

summary_crop_year = data.frame(year = integer(),
                               crop = character(),
                               average_production = double(),
                               total_production = double(),
                               average_area = double())
year_list = unique(sort(crop_production$Crop_Year))

for (year in year_list)
{
  crop_list = subset(crop_production, Crop_Year == year)
  crop_list = as.data.frame(unique(crop_list$Crop))
  for(crop in crop_list$`unique(crop_list$Crop)`)
  {
    crop_data = subset(crop_production, Crop_Year == year & Crop == crop & is.na(Production) == FALSE)
    avg_production =  mean(crop_data$Production)
    avg_area = mean(crop_data$Area)
    total_production = avg_production * nrow(crop_data)
    summary_crop_year <- as.data.frame(rbind(as.matrix(summary_crop_year), c(year,crop,avg_production,total_production,avg_area)))
    
  }
}
