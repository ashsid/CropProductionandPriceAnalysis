
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
install.packages("wordcloud")
install.packages("RColorBrewer")
library(wordcloud)
library(RColorBrewer)
crop_list = as.data.frame(table(crop_production$Crop))
crop_list = as.data.frame(table(crop_list$`unique(crop_production$Crop)`))
set.seed(1234)
wordcloud(words = crop_list$Var1, freq = crop_list$Freq, min.freq = 0,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#the indian agriculture crops  are divided amoung major - 3 seasons
#Kharif : June - September
#Rabi :  October - December
#Summer : January - May
#Whole-year : Jan - Dec

#lets divided crops amoung season : 

Kharif_crops = subset(crop_production, Season == "Kharif")
Kharif_crops = as.data.frame(table(Kharif_crops$Crop))
wordcloud(words = Kharif_crops$Var1, freq = Kharif_crops$Freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

rabi_crops = subset(crop_production, Season == "Rabi")
rabi_crops = as.data.frame(table(rabi_crops$Crop))
wordcloud(words = rabi_crops$Var1, freq = rabi_crops$Freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

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

summary_crop_year[,5] = as.numeric(as.character(summary_crop_year[,5]))

area_distribution = aggregate(summary_crop_year$average_area, by = list(summary_crop_year$year),na.rm=TRUE,FUN = sum)
library(plotly)
colnames(area_distribution) = c("Year","Total_Area_Under_Agriculture")
plot_ly(data = area_distribution, x=~Year,y=~Total_Area_Under_Agriculture,type="scatter",mode="lines")


summary_crop_year[,3] = as.numeric(as.character(summary_crop_year[,3]))
production_distribution = aggregate(summary_crop_year$average_production, by = list(summary_crop_year$year),na.rm=TRUE,FUN = sum)
library(plotly)
colnames(production_distribution) = c("Year","Total_Production_Under_Agriculture")
plot_ly(data = production_distribution, x=~Year,y=~Total_Production_Under_Agriculture,type="scatter",mode="lines")
