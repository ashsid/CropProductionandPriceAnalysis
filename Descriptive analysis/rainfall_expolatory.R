#https://www.livemint.com/Politics/4zZswny6uKtTCDKEhsxVRK/Exceptionally-low-rainfall-in-last-decade-nobody-really-kno.html

library(ggplot2)

data = read.csv(file="../Dataset/rainfall in india 1901-2015.csv", header=TRUE, sep=",")
data$monsoon = data$JUN+data$JUL+data$AUG
annual_data = data.frame(data$SUBDIVISION,data$YEAR,data$monsoon)

#rainfall in india over the years
india_annual_data = aggregate(annual_data$data.monsoon, by=list(Year=annual_data$data.YEAR),na.rm=TRUE, FUN=sum)
colnames(india_annual_data)[2] <- "Monsoon Rain"
plot(india_annual_data$Year,india_annual_data$`Monsoon Rain` ,type='l',xlab = "YEAR",ylab = "Monsoon Rainfall(mm)")

#avg rainfall in india over all years
avg = mean(india_annual_data$`Monsoon Rain` )

india_decade = india_annual_data
india_decade$Year = india_decade$Year - india_decade$Year %% 10

#average rainfall in India decade wise
india_decade_data = aggregate(india_decade$`Monsoon Rain`, by=list(Year=india_decade$Year), FUN=mean)
colnames(india_decade_data)[2] <- "Monsoon Rain"
india_last = india_decade_data$`Monsoon Rain`[0:-11]

#percentage change in rainfall in the last decade over the average rainfall
print((india_last-avg)*100/avg)
#Summer monsoon months between June 2000 and 21 August 2009 registered only 8,124mm of rainfall, 4.12% short of the decadal normal.
#(5.5% according to the survey)

#plot of rainfall over the decades in India
barplot(india_decade_data$`Monsoon Rain`,names.arg=india_decade_data$Year, ylim = c(25000, 35000),xpd=FALSE,ylab = "Monsoon Rainfall(mm)",xlab="Year")
#Rainfall in the decade between 1999 and 2009 was the lowest in the last eight decades.

#library(tseries)
#adf.test(india_annual_data$x , alternative = "stationary")
#plot(stl( ts(india_annual_data$x, frequency = 10), s.window = "periodic"))

#ggplot(india_annual_data$Category,india_annual_data$x, geom='smooth', span =0.4) + ggplot(india_annual_data$Category,india_annual_data$x)
p <- ggplot() +
  geom_line(data=india_annual_data, aes(x=Year, y=`Monsoon Rain`)) + 
  geom_smooth(data=india_annual_data, aes(x=Year, y=`Monsoon Rain`), fill="lightblue",colour="blue",span=0.3, size=1,level=0.95)+
  geom_vline(xintercept=1900,colour="red", size=1)+
  geom_vline(xintercept=1930,colour="red", size=1)+
  geom_vline(xintercept=1960,colour="red", size=1)+
  geom_vline(xintercept=1990,colour="red", size=1)+
  geom_vline(xintercept=2020,colour="red", size=1)+
  scale_x_continuous(breaks=seq(1900,2020,10))
print(p)

#Meteorologists add that monsoon rainfall this century broadly followed a 30-year cycle. The 1900s to 1930 saw severe droughts and rainfall years were usually below normal. The 1930s to the 1960's saw good rainfall and few drought years; the 1960's to the 1990s, again saw more droughts and depressed rainfall.
#"Therefore, the 1990s to 2020 should have been characterized by good rainfall. The 90s were promising-there were no droughts," said Rajeevan. "But this decade has completely overturned that trend. The last time we had excess rainfall was 1994."
#Why these 10 years saw exceptionally low rainfall is still a matter of research and debate, unlikely to be resolved anytime soon.
