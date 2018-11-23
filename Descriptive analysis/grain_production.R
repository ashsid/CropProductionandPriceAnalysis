#http://business.rediff.com/report/2009/sep/15/food-grain-output-may-dip-10-15-percent-this-year.htm

library(dplyr)
library(ggplot2)

crop <- read.csv(file="../Dataset/crop_production.csv", header=TRUE, sep=",")
crop_state = aggregate(crop$Production, by=list(StateName = crop$State_Name , Year=crop$Crop_Year , Season=crop$Season, Crop=crop$Crop),na.rm=TRUE, FUN=sum)
grain_state = crop_state[crop_state$Crop %in% c("Rice","Wheat","Maize","Arhar/Tur","Urad","Bajra","Jowar"),]

grain_year = aggregate(grain_state$x, by=list(Year=grain_state$Year, Crop=grain_state$Crop),na.rm=TRUE, FUN=sum)
grain_year = grain_year[grain_year$Year<"2011",]
grain_year = rename(grain_year,Production = x)

data = read.csv(file="../Dataset/rainfall in india 1901-2015.csv", header=TRUE, sep=",")
data$monsoon = data$JUN+data$JUL+data$AUG
annual_data = data.frame(data$SUBDIVISION,data$YEAR,data$monsoon)
india_annual_data = aggregate(annual_data$data.monsoon, by=list(Year=annual_data$data.YEAR),na.rm=TRUE, FUN=sum)
india_annual_data = rename(india_annual_data,`Monsoon Rainfall` = x)

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

p = ggplot(data=grain_year, aes(x=Year, y=Production, group = Crop, colour = Crop)) +
  geom_line() +
  geom_point( size=1, fill="white")+
  scale_x_continuous(breaks=seq(1996,2011,1))
print(p)


p0 = ggplot(data=india_annual_data, aes(x=Year, y=`Monsoon Rainfall`)) +
  geom_line() +
  geom_point( size=1, fill="white")+
  scale_x_continuous(breaks=seq(1996,2011,1),limits=c(1997,2010))

grain_year_scaled <- grain_year %>% group_by(Crop) %>% mutate(`Scaled Production` = scale(Production))

p1 = ggplot(data=grain_year_scaled, aes(x=Year, y=`Scaled Production`, group = Crop, colour = Crop)) +
  geom_line() +
  geom_point( size=2, fill="white")+
  scale_x_continuous(breaks=seq(1996,2011,1))

multiplot(p1,p0)

agg_grain_scaled = aggregate(grain_year_scaled$`Scaled Production`, by=list(Year = grain_year_scaled$Year),na.rm=TRUE, FUN=sum)
agg_grain_scaled = rename(agg_grain_scaled,`Scaled Production` = x)
p2 = ggplot(data=agg_grain_scaled, aes(x=Year, y=`Scaled Production`)) +
  geom_line() +
  geom_point( size=2, fill="white")+
  scale_x_continuous(breaks=seq(1996,2011,1))
print(p2)

