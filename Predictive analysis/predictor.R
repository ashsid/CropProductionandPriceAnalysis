#Test cases
state_input = "Karnataka"
district_input = "TUMKUR"
crop_input = "Maize"
season = "Kharif"
rainfall_division = "SOUTH INTERIOR KARNATAKA"

#extracting crop data
crop_production_data = read.csv("../Dataset/crop_production.csv")
rainfall = read.csv("../Dataset/rainfall in india 1901-2015.csv")
#calculating yield
crop_production_data$YIELD = crop_production_data$Production/crop_production_data$Area

#Kharif = april - september
#rabi = oct - dec
#summer = jan - march

#extracting rainfall data

rainfall_data = read.csv("rainfall in india 1901-2015.csv")
rainfall_data = rainfall_data[order(-rainfall_data$YEAR),]
rainfall_places = as.data.frame(unique(rainfall_data$SUBDIVISION))

rainfall_state = subset(rainfall_data, SUBDIVISION == rainfall_division)

state_list  = as.data.frame(unique(crop_production_data$State_Name))
colnames(state_list) = c("State")
district_list_main  = as.data.frame(unique(crop_production_data$District_Name))
colnames(district_list_main) = c("District")


#extracting data required for a particular user

#extracting data for particular state
state_data = subset(crop_production_data,State_Name == state_input)
district_list = as.data.frame(unique(state_data$District_Name))


#extacting district data
colnames(district_list) = c("District")
#district_input = readline(prompt="Enter District: ")
district_data = subset(state_data,District_Name == district_input)

#extracting crop-data
#crop_list = as.data.frame(unique(district_data$Crop))
crop_count = as.data.frame(table(district_data$Crop))
crop_count =  subset(crop_count,Freq>0)
crop_count = crop_count[order(-crop_count$Freq),]
#crop_input = readline(prompt = "Enter crop: ")
crop_data = subset(district_data,Crop == crop_input)


#order to data is ascending order

crop_data = crop_data[order(-crop_data$Crop_Year),]
#season = readline(prompt = "Enter season: ")
crop_data_season = subset(crop_data,Season==season)
library(plotly)
#plot the production and area data
plot_ly(crop_data_season,x=~Crop_Year,y=~Area, type = 'scatter', mode = 'lines')
plot_ly(crop_data_season,x=~Crop_Year,y=~YIELD, type = 'scatter', mode = 'lines')

colnames(crop_data_season)[3] = c("YEAR")
crop_data_season = merge(x=crop_data_season,y=rainfall_state, by = "YEAR")
jan = crop_data_season$JAN
feb = crop_data_season$FEB
march = crop_data_season$MAR
april = crop_data_season$APR
may = crop_data_season$MAR
june = crop_data_season$JUN
july = crop_data_season$JUL
august = crop_data_season$AUG
sep = crop_data_season$SEP
oct = crop_data_season$OCT
nov = crop_data_season$NOV
dec = crop_data_season$DEC
year = crop_data_season$YEAR
yeild = crop_data_season$YIELD
crop_data_season_new = cbind(year,jan,feb,march,april,may,june,july,august,sep,oct,nov,yeild)
crop_data_season_new = as.data.frame(crop_data_season_new)
crop_data_season_new = crop_data_season_new[order(-crop_data_season_new$year),]
#splitting the data into test and train

#smp_size <- floor(0.75 * nrow(crop_data_season_new))
#set.seed(123)
#train_ind <- sample(seq_len(nrow(crop_data_season_new)), size = smp_size)
#train = crop_data_season_new[train_ind,]
#test = crop_data_season_new[-train_ind,]

#the below code splits the dataset as , train contains data till 2013 test has data of 2014.
train = as.data.frame(crop_data_season_new[-1,])
test = crop_data_season_new[1,]


#xgboost prediction model

library(xgboost)
model_xg = xgboost(data = as.matrix(train[,1:13]),label = train$yeild,nrounds = 80)

#model with random forest
#train_rf = train[,c('Crop_Year','YEILD')]
#test_rf = test[,c('Crop_Year','YEILD')]
#library(randonForest)
#model_rf = randomForest(train_rf$YEILD ~ . , data=train_rf,ntree=3000)

#predict
#we are training the model till 2013 and predicting it for 2014.
y_pred_xg = predict(model_xg,newdata = as.matrix(test[,1:13]))
print("The expected yield for the given input crop is =")
print(y_pred_xg)
