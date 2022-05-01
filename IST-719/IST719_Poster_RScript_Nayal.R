#Author: Mohamad Nayal
#Week 3 Homework

###Part 1 Visualize This Chapter 4

#reading in data
full <- read.csv('/Users/mohamadnayal/Documents/Syracuse/winter_2022/IST_719/Homwork/week3_homework/crime.csv', header = T, sep=',',stringsAsFactors = F)
dim(full)

#removing NAs
philly <- (na.omit(full))
(ncol(philly)*4)*(nrow(philly)/100)

#Looking at Structure
str(philly)
philly$Dispatch_Date <- as.Date(philly$Dispatch_Date)
philly$Police_Districts[philly$Police_Districts==1] <- '01'
philly$Police_Districts[philly$Police_Districts==2] <- '02'
philly$Police_Districts[philly$Police_Districts==3] <- '03'
philly$Police_Districts[philly$Police_Districts==4] <- '04'
philly$Police_Districts[philly$Police_Districts==5] <- '05'
philly$Police_Districts[philly$Police_Districts==6] <- '06'
philly$Police_Districts[philly$Police_Districts==7] <- '07'
philly$Police_Districts[philly$Police_Districts==8] <- '08'
philly$Police_Districts[philly$Police_Districts==9] <- '09'
philly$Service_Area <- paste(philly$Police_Districts, '-', philly$Psa)


#Plotting Crime By hour - In poster
colz=c(rep('#162D63',5),rep('#FFA6A1',15), rep('#162D63',4)) 
borderz=c(rep('white',5),rep('black',15), rep('white',4)) 
hist(philly$Hour,
     main='Philadelphia Crimes by Hour of the Day'
     ,ylab='Frequency of Crimes'
     ,xlab='Hour of the Day',
     ,xlim = c(0,26)
     ,col=colz
     ,border=borderz
     ,xaxt="n"
     ,breaks = 25)
axis(side=1,at=seq(0,23,1),labels=c('12AM', "1AM", '2AM', '3AM','4AM', '5AM', '6AM', '7AM', '8AM', '9AM', '10AM', '11AM', '12PM', "1PM", '2PM', '3PM','4PM', '5PM', '6PM', '7PM', '8PM', '9PM', '10PM', '11PM'))


#Plotting Crime by Police Service Area
#Plotting Crime by Police Service Area

#not in poster
distAg <- aggregate(philly$Dc_Key, list(philly$Service_Area), length)
colnames(distAg) <- c('Service_Area', "Count")
polColz=c("#0a3490", "#697d9f", "#bfc3d1", "#1f2c37")
distAg2 <- distAg[distAg$Count>5000,]
par(mar=c(6,6,4,2)+0.1, xpd=TRUE)
barplot(distAg2$Count
        ,main='Philadelphia Crimes by Police Service Areas'
        ,ylab='Frequency of Crimes'
        ,xlab='Service Area'
        ,col=polColz
        ,names.arg = distAg2$Service_Area
        ,las=3)



#Plotting Crime Timeline not in poster
dateAg <- aggregate(philly$Dc_Key, list(philly$Dispatch_Date), length)
colnames(dateAg) <- c('Date', "Count")
dateAg$Date <- as.Date(dateAg$Date)
plot(x=dateAg$Date,y=dateAg$Count
     , type='l'
     , main='Timeline of Crimes in Philadelphia'
     , col='red'
     ,xlab = 'Date'
     ,ylab='Frequency of Crimes'
     ,lwd=0.25)



#Making a plot of type of crime by Police District
#getting top 6 crime types
library(ggplot2)
ctypeAg <- aggregate(philly$Dc_Key, by=list(philly$Text_General_Code), length)
colnames(ctypeAg) <- c('Crime_Type', 'Count')
ctypeAg <- ctypeAg[order(ctypeAg$Count, decreasing = TRUE),]
topCrimes <- ctypeAg$Crime_Type[1:6]

crimeOrder <- rev(order(ctypeAg$Count))
ctypeAg2 <- ctypeAg[crimeOrder[1:10],]
ctypeAg2$Crime_Type <- as.factor(ctypeAg2$Crime_Type)
options(scipen = 10)
ctypeAg2$color <- heat.colors(10)

#in poster
ggplot(data = ctypeAg2, aes(x = reorder(Crime_Type, -Count), y=Count))+
  geom_bar(stat = 'identity', fill=ctypeAg2$color)+
  ggtitle('Most Common Crime Types')+
  theme(plot.title = element_text(hjust = 0.5, colour = "red", face='bold'))+
  theme(axis.text.x = element_text(angle = -90),
        axis.title.x = element_blank())+
  theme(panel.background = element_blank())+
  theme(axis.ticks.x=element_blank())
  

par(mar=c(3,3,3,3))  



#plotting top 6 crimes for each district
p2 <- philly[philly$Text_General_Code %in% topCrimes,]
distTypeAg <-tapply(p2$Dc_Key, list(p2$Text_General_Code, p2$Police_Districts), length) 
colnames(distTypeAg) <- c('Crime_Type', 'District','Count')

#not in poster
barplot(distTypeAg 
        ,col = c('blue', '#BB2020','#06111C','#BEC7C7','#FFEB4D', 'purple')
        ,main="Top Crimes by Police District"
        ,xlab='Police District'
        ,ylab='Crimes'
        ,ylim=c(0,150000))
legend("topleft", inset=.02,rownames(distTypeAg), fill= c('blue', '#BB2020','#06111C','#BEC7C7','#FFEB4D', 'purple'), cex=0.8)



#making a map of murders
library(ggmap)

height <- max(philly$Lat) - min(philly$Lat)
width <- max(philly$Lon) - min(philly$Lon)
philly_borders <- c(bottom  = min(philly$Lat), 
                 top     = max(philly$Lat),
                 left    = min(philly$Lon),
                 right   = max(philly$Lon))
map <- get_stamenmap(philly_borders, zoom = 10, maptype = "toner-lite")
codeText <- unique(philly$Text_General_Code)
philly$violent <- ifelse(philly$Text_General_Code %in% codeText[c(16,4,9,10,12,20,22,31)], 'Violent', 'Non-Violent')
philly$vColor <- ifelse(philly$Text_General_Code %in% codeText[c(16,4,9,10,12,20,22,31)], 'darkred', 'lightblue')

ggmap(map)+ggtitle('Murders in Philadelphia')+
  theme(plot.title = element_text(hjust = 0.5, colour = 'red', face='bold'))+
  geom_point(data = philly[philly$Text_General_Code=='Homicide - Criminal',]
             , mapping = aes(x = Lon, y = Lat)
             , col='red'
             , alpha=0.2
             , size=0.8)+
  labs(x='Longitude', y="Latitude")

#violent vs nonviolent crime
par(mar=c(0,0,0,0))
philly$year <- format(philly$Dispatch_Date,"%Y")
p16 <- philly[philly$year==2016,]

ggmap(map)+
  theme(plot.title = element_text(hjust = 0.5, colour = 'red', face='bold'))+
  geom_point(data = p16
             , mapping = aes(x = Lon, y = Lat)
             , col=p16$vColor
             , alpha=0.8
             , size=0.1)+
  ggtitle('Violent and Non Violent Crime 2016')+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
    


####################################################################
##################################################################
#map of crime by police districts
library(rgdal)
fiveOO <- readOGR('/Users/mohamadnayal/Documents/Syracuse/winter_2022/IST_719/poster_preping_stuff/Boundaries_District')
library(plotrix)
crimeAgg <- aggregate(philly$Dc_Key, by=list(philly$Dc_Dist), length)
crimeAgg$index <- round(rescale(x=crimeAgg$x, c(1, 10)),0)
num.cols <- 10
my.color.vec <- rev(heat.colors(num.cols))
crimeAgg$color <- my.color.vec[crimeAgg$index]
plot(fiveZero, color=fiveZero$color)

library (ggmap)
library (raster)
library(broom)
library(maptools)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()
map <- fortify(fiveOO, region='DISTRICT_')
spdf_fortified <- tidy(fiveOO, region = "DIST_NUM")
spdf_fortified$id <- as.numeric(spdf_fortified$id)
colorVector <- crimeAgg$color[spdf_fortified$id]

###############
#crimes by police district
#########################
#in poster
labelLocationsLat <- aggregate(spdf_fortified$lat, by=list(spdf_fortified$id), mean)
labelLocationsLong <- aggregate(spdf_fortified$long, by=list(spdf_fortified$id), mean)
colnames(labelLocationsLong) <- c('District', 'long')
colnames(labelLocationsLat) <- c('District', 'lat')
districtLables <- cbind(labelLocationsLong, lat=labelLocationsLat$lat)

fun_color_range <- colorRampPalette(c("#FF0000", "#FFFF00", "white"))
my_colors <- fun_color_range(25)
polDistAgg <- aggregate(philly$Dc_Key, list(philly$Dc_Dist), length)
districtLables$Count <- polDistAgg$x

orderNum <- order(-polDistAgg$x)
polDistAgg <- polDistAgg[orderNum,]
polDistAgg$Color <- my_colors
polDistAgg <- polDistAgg[order(polDistAgg$Group.1),]
colnames(polDistAgg) <- c('id', 'count', 'Color')
spdf_fortified$polygonColor <- polDistAgg$Color[spdf_fortified$id]

scary <- merge(spdf_fortified, polDistAgg, by='id')
ggplot() +
  geom_polygon(data = scary, aes( x = long, y = lat, group = group, fill=count),color="black") +
  theme_void()+
  ggtitle('Crimes by Police District')+
  theme(plot.title = element_text(hjust = 0.5, colour = "red", face='bold'))+
  scale_color_manual(values = "red")+
  geom_text(aes(label=districtLables$District, x=districtLables$long, y=districtLables$lat))+
  scale_fill_gradient(low = "#FFFFE0",
                      high = "#FF0000",
                      space = "Lab",
                      na.value = "grey50",
                      guide = "colourbar",
                      aesthetics = "fill")

















#####################
#murders by police district
########################
#in poster
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill='white',color="black") +
  theme_void()+
  ggtitle('Murders in Philadelphia')+
  theme(plot.title = element_text(hjust = 0.5, colour = "red", face='bold'))+
  geom_text(aes(label=districtLables$District, x=districtLables$long, y=districtLables$lat), size=5)+
  geom_point(data = philly[philly$Text_General_Code=='Homicide - Criminal',]
             , mapping = aes(x = Lon, y = Lat)
             , col='red'
             , alpha=0.2
             , size=0.8)




##################################################
#####testing seasonality plot
####################################################
dateAg$month <- format(dateAg$Date,"%m")
dateAg$Year <- format(dateAg$Date,"%Y")
dateAg$YearMonth <- format(dateAg$Date,"%Y-%m")
dateAgMonth <- aggregate(dateAg$Count, by=list(dateAg$YearMonth), sum)



dateAgMonth$Group.1 <- as.Date(paste(dateAgMonth$Group.1,"-01",sep=""))
library(xts)
crimeLinez <- xts(dateAgMonth[,-1], order.by=(dateAgMonth[,1]))
library(tsbox)
crimeLinez <-ts_ts(crimeLinez)
library(forecast)
ggseasonplot(crimeLinez, year.labels=TRUE, year.labels.left=TRUE, col = heat.colors(13))+
  ylab("Crimes Committed") +
  ggtitle("Seasonal plot: Crimes")+
  theme(plot.title = element_text(hjust = 0.5, colour = "red", face='bold'))+
  theme(panel.background = element_blank())

