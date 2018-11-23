#https://economictimes.indiatimes.com/news/economy/agriculture/india-set-to-be-worlds-largest-cotton-producer-by-2022/articleshow/20464710.cms
#https://economictimes.indiatimes.com/markets/commodities/organic-cotton/articleshow/31803212.cms

library(ggplot2)
library(dplyr)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

food <- read.csv(file="../Dataset/crop_production.csv", header=TRUE, sep=",")

cotton_1 = food[food$Crop=="Cotton(lint)" ,]
cotton_1 = cotton_1[cotton_1$Season=="Kharif" ,]
cotton_2 = aggregate(cotton_1$Production, by=list(State_name=cotton_1$State_Name,Crop_year=cotton_1$Crop_Year),na.rm=TRUE, FUN=sum)

cotton_3_mean = aggregate(cotton_2$x, by=list(State_name=cotton_2$State_name),FUN=mean)
#top prouducers of cotton
#Gujarat, Maharashtra, Telangana, Punjab, Haryana, Andhra Pradesh, Karnataka, Madhya Pradesh, Rajasthan, Tamil Nadu

top_cotton_state = cotton_2[cotton_2$State_name %in% c("Gujarat", "Maharashtra", "Telangana", "Punjab", "Haryana", "Andhra Pradesh", "Karnataka", "Madhya Pradesh", "Rajasthan", "Tamil Nadu"),]
top_cotton_state = rename(top_cotton_state,Production = x)

p = ggplot(data=top_cotton_state, aes(x=Crop_year, y=Production, group = State_name, colour = State_name)) +
  geom_line() +
  geom_point( size=2, fill="white") + 
  scale_x_continuous(breaks=seq(1995,2015,1))

cotton_4 = aggregate(top_cotton_state$Production, by=list(Crop_year=top_cotton_state$Crop_year),na.rm=TRUE, FUN=sum)
cotton_4 = rename(cotton_4,Production = x)

p1 = ggplot(data=cotton_4, aes(x=Crop_year, y=Production)) +
  geom_line() +
  geom_point( size=1, fill="white") + 
  scale_x_continuous(breaks=seq(1995,2015,1))

multiplot(p,p1)

