#State Recommender:

fruit_crop <- data.frame(data$State_Name,data$Crop,data$Production)
colnames(fruit_crop) <- c("State","Crop","Production") 
train<- data.frame(aggregate(fruit_crop$Production, by = list(fruit_crop$State,fruit_crop$Crop), na.rm =TRUE ,FUN =mean))
colnames(train) <- c("State", "Crop", "Production")
rating <- dcast( train,Crop~State , value.var ="Production", fun.aggregate=mean)
rating2 <- as.matrix(rating[,-1])
ratingmat <- as(rating2, "realRatingMatrix")
ratingmat <- normalize(ratingmat)
rec_mod <- Recommender(ratingmat, method = "UBCF", param=list(method="Cosine",nn=10))
Top_5_pred <- predict(rec_mod, ratingmat[5], n=5)
Top_5_States_List <- as(Top_5_pred, "list")
top5states <- data.frame(Top_5_States_List[[1]])
colnames(top5states)[1] <- "State"
print(top5states)