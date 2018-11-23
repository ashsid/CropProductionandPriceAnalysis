crop_production = read.csv("../Dataset/crop_production.csv")
rainfall_data = read.csv("../Dataset/rainfall in india 1901-2015.csv")
rainfall_data = rainfall_data[order(rainfall_data$YEAR),]

#ARTICLE_TITLE : Skymet forecasts well-distributed, adequate monsoon

#The article talks about mansoon in 2013
#The summer monsoon is critical to India as 55% of farmland does not have access to irrigation.
#Agriculture accounts for about one-fifth of the economy, and bigger harvests may cool the highest food inflation among major economies and sustain exports of rice and wheat.
#Good rains will be a positive sign for agricultural growth and are likely to boost production this year
#mansoon is a rainy season lasting from July to september
#as per the article, India’s June-September monsoon rainfall is likely to be “well distributed” and “adequate”
#The monsoon will be on time and therefore allow the normal sowing of the summer crop across the country, Skymet said.
#India is expected to get more rain than it usually does in the four months and there is only a 12% chance of deficient rainfall, it said.
#The irregular nature of the monsoon rain last year reduced India’s summer crop and contributed to rising inflation as well as depressed consumer demand.
#The weakest monsoon in three years in 2012 parched parts of Maharashtra, Karnataka and Gujarat, cutting the harvests of sugar cane, cotton and rice. 
#The agriculture sector is set to expand 1.8% this year, the least in three years, according to the government’s annual Economic Survey.
#The economy will expand 5% in 2012-2013, the least in a decade, according to the government.

#POINTS-TO-BE-WORKED
# 1)Rainfall variations in the month of june-september from 2010 to 2013
# 2)See the variations of summer crops like rice, wheat, cotton

#POINTS-TO-BE-CONCLUDED
# 1)rainfall and crop variation due to lack of regular summer monsoon in india.

rainfall_2013 = subset(rainfall_data, YEAR == 2013)
rainfall_2010_2013 = subset(rainfall_data, YEAR > 2009 & YEAR < 2014)

#extracting required months

rainfall_2010_2013_monsoon = rainfall_2010_2013[,c("SUBDIVISION","YEAR","JUL","AUG","SEP","Jun.Sep")]
rainfall_2010 = subset(rainfall_2010_2013_monsoon, YEAR == 2010)
rainfall_2011 = subset(rainfall_2010_2013_monsoon, YEAR == 2011)
rainfall_2012 = subset(rainfall_2010_2013_monsoon, YEAR == 2012)
rainfall_2013 = subset(rainfall_2010_2013_monsoon, YEAR == 2013)


#plot of variation of monsoon rainfall
plot(rainfall_2012$JUL,type='o',col='red',xlab='SUBDIVISION',ylab='2013',main='Rainfall Variation in the month of july between 2012 n 2013')
lines(rainfall_2013$JUL,type='o',col='black',ylab='2013')

plot(rainfall_2012$AUG,type='o',col='red',xlab='SUBDIVISION',ylab='2013',main='Rainfall Variation in the month of August between 2012 n 2013')
lines(rainfall_2013$AUG,type='o',col='black',ylab='2013')


plot(rainfall_2012$SEP,type='o',col='red',xlab='SUBDIVISION',ylab='2013',main='Rainfall Variation in the month of September between 2012 n 2013')
lines(rainfall_2013$SEP,type='o',col='black',ylab='2013')


plot(rainfall_2012$Jun.Sep,type='o',col='red',xlab='SUBDIVISION',ylab='2013',main='Rainfall Variation in the Average monsoon between 2012 n 2013')
lines(rainfall_2013$Jun.Sep,type='o',col='black',ylab='2013')


plot(rainfall_2010$Jun.Sep,type='o',col='red',xlab='SUBDIVISION',ylab='2010',main='Rainfall Variation in the month of August between 2012 n 2013')
lines(rainfall_2011$Jun.Sep,type='o',col='blue',ylab='2013')
lines(rainfall_2012$Jun.Sep,type='o',col='black')


