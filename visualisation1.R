args <- commandArgs(TRUE)
statename<-as.character(args[1])  
districtname<-as.character(args[2])
data <- read.csv('crop_production.csv')
data$Yield <- data$Production / data$Area
state <- as.data.frame(subset(data, data$State_Name == 'Karnataka'))
district <- subset(state, state$District_Name == "BAGALKOT")
#hardcoded the Region to BAGALKOT district of Karnataka.
#Custom input can be taken using "readline" but with this line by line execution needs to be carried out.


top5 <- as.data.frame(table(district$Crop))
top5 <- top5[order(-top5$Freq),]
top5_crops <- top5[1:5, ]
print(top5_crops[["Var1"]])
#The top 5 crops of the region


crop1 <- as.character(top5_crops[["Var1"]][1])
crop2 <- as.character(top5_crops[["Var1"]][2])
crop3 <- as.character(top5_crops[["Var1"]][3])
crop4 <- as.character(top5_crops[["Var1"]][4])
crop5 <- as.character(top5_crops[["Var1"]][5])
#Extracting the Top5 Crops for the region.

library(plotly)
#year <- as.Date(unique(district$Crop_Year), origin = "1995-1-1")
year <- as.data.frame(district$Crop_Year)
crop1_data <- subset(district, district$Crop == crop1)
crop1data <- subset(crop1_data, crop1_data$Season == 'Kharif')
crop1data <- as.data.frame(crop1data$Yield)

crop2_data <- subset(district, district$Crop == crop2)
crop2data <- subset(crop2_data, crop2_data$Season == 'Kharif')
crop2data <- as.data.frame(crop2data$Yield)

crop3_data <- subset(district, district$Crop == crop3)
crop3data <- subset(crop3_data, crop3_data$Season == 'Kharif')
crop3data <- as.data.frame(crop3data$Yield)

crop4_data <- subset(district, district$Crop == crop4)
crop4data <- subset(crop4_data, crop4_data$Season == 'Kharif')
crop4data <- as.data.frame(crop4data$Yield)

crop5_data <- subset(district, district$Crop == crop5)
crop5data <- subset(crop5_data, crop5_data$Season == 'Kharif')
crop5data <- as.data.frame(crop5data$Yield)


#The Plot that represents the Yield of the top 5 crops in the region over years:

p <- plot_ly(x = ~year) %>%
  add_lines(y = ~crop1data$`crop1data$Yield`, name = crop1) %>%
  add_lines(y = ~crop2data$`crop2data$Yield`, name = crop2, visible = F) %>%
  add_lines(y = ~crop3data$`crop3data$Yield`, name = crop3, visible = F) %>%
  add_lines(y = ~crop4data$`crop4data$Yield`, name = crop4, visible = F) %>%
  add_lines(y = ~crop5data$`crop5data$Yield`, name = crop5, visible = F) %>%
  layout(
    title = "Top 5 Best Crops in the Area",
    xaxis = list(rangeslider = list(type = "date"), title= "Period"),
    yaxis = list(title="Yield"),
    
    updatemenus = list(
      list(
        buttons = list(
          list(method = "restyle",
               args = list("visible", list(TRUE, FALSE,FALSE,FALSE,FALSE)),
               label = crop1),
          list(method = "restyle",
               args = list("visible", list(FALSE,TRUE,FALSE,FALSE,FALSE)),
               label = crop2),
          list(method = "restyle",
               args = list("visible", list(FALSE, FALSE,TRUE,FALSE,FALSE)),
               label = crop3),
          list(method = "restyle",
               args = list("visible", list(FALSE, FALSE,FALSE,TRUE,FALSE)),
               label = crop4),
          list(method = "restyle",
               args = list("visible", list(FALSE, FALSE,FALSE,FALSE,TRUE)),
               label = crop5)
          
          
        ))))
print(p)




