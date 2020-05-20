#######################################################################################
# NAME: ADVAIT RAMESH IYER
# HOMEWORK 4
#######################################################################################
# Nathan's Hot Dog Eating Contest Results, 1980-2010
par(mar = c(3,3,7.5,1.25))
hot_dogs <- read.csv("http://datasets.flowingdata.com/hot-dog-contest-winners.csv"
                     , sep=",", header=TRUE)

#View(hot_dogs)

fill_colors <- c()
for (i in 1:length(hot_dogs$New.record)) {
  if(hot_dogs$New.record[i] == 1) {
    fill_colors <- c(fill_colors,"green4")
  } else{
    fill_colors <- c(fill_colors,"#cccccc")
  }
}

# Fig. 4-5:
plot1 <- barplot(hot_dogs$Dogs.eaten, col = fill_colors
                 , border = NA, space = 0.3, xpd = NA, bty = "n", ylim=c(0,70))

###############################################################################
# Hot Dogs Eating Contest Results
hot_dog_places <- read.csv("http://datasets.flowingdata.com/hot-dog-places.csv", sep=",", header=TRUE)
names(hot_dog_places) <- c("2000", "2001", "2002", "2003", "2004",
                           "2005", "2006", "2007", "2008", "2009", "2010")
View(hot_dog_places)

# Stacked Barchart - Fig. 4-21:
par(mar = c(4,3.5,8.25,2.25))
hot_dog_matrix <- as.matrix(hot_dog_places)
plot2 <- barplot(hot_dog_matrix, border=NA, space=0.3, xaxt ='n', ylim=c(0, 200)
                 , col = c("forestgreen","springgreen3","mediumspringgreen"))

###############################################################################
# Number of Subscribers
subscribers <- read.csv("http://datasets.flowingdata.com/flowingdata_subscribers.csv"
                        , sep=",", header=TRUE)

subscribers[1:5,]

par(mar = c(4,2,8,0.05))
plot3 <- plot(subscribers$Subscribers, border=NA, bty = 'n', ylab = NA, xlab = NA
              , ylim = c(0,30000), xlim = c(0,35))

# World Population
population <- read.csv("http://datasets.flowingdata.com/world-population.csv", sep=",", header=TRUE)

# Time Series Chart - Fig. 4-34:
par(mar = c(3,1.5,6.25,2))

plot4 <- plot(population$Year, xlab=NA, ylab=NA, population$Population, type="l"
              , ylim=c(0, 7000000000), bty = 'n', lwd = 4, xpd = FALSE)

###############################################################################
# Step Chart
par(mar = c(4.5,4,3,2))
postage <- read.csv("http://datasets.flowingdata.com/us-postage.csv", sep=",", header=TRUE)

plot(postage$Year, postage$Price, type="s")
par(mar=c(6,1,3,1))
plot5 <- plot(postage$Year, postage$Price, type="s",
              xlab=NA, ylab=NA, bty = 'n', yaxt = 'n'
              , lwd=3)

# Load data 
unemployment <- read.csv( "http://datasets.flowingdata.com/unemployment-rate-1948-2010.csv", sep=",")
unemployment[1:10,]
# Plain scatter plot 

plot(1:length(unemployment$Value), unemployment$Value)
scatter.smooth(x=1:length(unemployment$Value), y=unemployment$Value)

par(mar=c(4,6,2.5,4))
scatter.smooth(x=1:length(unemployment$Value),
               y=unemployment$Value, ylim=c(0,11), degree=2, col="#CCCCCC", span=0.5
               , bty='n',xaxt='n')

