data <- read.csv('crop_production.csv')
#read the file 

data$Yield <- data$Production / data$Area
#create new column for yield

state <- as.data.frame(subset(data, data$State_Name == "Gujarat"))
district <- subset(state, state$District_Name == "AHMADABAD")
train <- data.frame(district$Crop,district$Season,district$Yield)

#above three lines consider computations for Ahmadabad district of Gujarat

colnames(train) <- c("Crop", "Season", "Yield")
#rename the columns

rating <- dcast(train, Season~Crop , value.var ="Yield", fun.aggregate=mean)
#form the matrix like the user-item matrix, here it forms the season crop matrix to be used by the recommender
rating2 <- as.matrix(rating[,-1])
ratingmat <- as(rating2, "realRatingMatrix")
ratingmat <- normalize(ratingmat)

#Final Matrix
