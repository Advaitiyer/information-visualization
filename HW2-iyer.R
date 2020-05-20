#######################################################################################
# PART 1
#######################################################################################
# Nathan's Hot Dog Eating Contest Results, 1980-2010
par(mar = c(4.5,4,4,4))
hot_dogs <- read.csv("http://datasets.flowingdata.com/hot-dog-contest-winners.csv"
                     , sep=",", header=TRUE)

View(hot_dogs)

barplot(hot_dogs$Dogs.eaten)

barplot(hot_dogs$Dogs.eaten, names.arg = hot_dogs$Year)

barplot(hot_dogs$Dogs.eaten, names.arg=hot_dogs$Year, col="red"
        , border=NA, xlab="Year", ylab="Hot dogs and buns (HDB) eaten")

fill_colors1 <- c()
for (i in 1:length(hot_dogs$Country)) {
  if(hot_dogs$Country[i] == "United States") {
    fill_colors1 <- c(fill_colors1,"#821122")
  } else{
    fill_colors1 <- c(fill_colors1,"#cccccc")
  }
}

barplot(hot_dogs$Dogs.eaten, xlab = "Year"
        , names.arg = hot_dogs$Year
        , ylab = "Hot dogs and buns (HDB) eaten"
        , col = fill_colors1)

fill_colors2 <- c()
for (i in 1:length(hot_dogs$New.record)) {
  if(hot_dogs$New.record[i] == 1) {
    fill_colors2 <- c(fill_colors2,"#821122")
  } else{
  fill_colors2 <- c(fill_colors2,"#cccccc")
  }
}

barplot(hot_dogs$Dogs.eaten, xlab = "Year"
        , names.arg = hot_dogs$Year
        , ylab = "Hot dogs and buns (HDB) eaten"
        , col = fill_colors2)

# Barchart - Fig. 4-11:
plot1 <- barplot(hot_dogs$Dogs.eaten, ylab = "Hot dogs and buns (HDB) eaten"
        , xlab = "Year", names.arg = hot_dogs$Year
        , col = fill_colors2, border = NA, space = 0.3
        , main = "Nathan's Hot Dog Eating Contest Results, 1980-2010"
        , xpd = FALSE)

###############################################################################
# Hot Dogs Eating Contest Results
hot_dog_places <- read.csv("http://datasets.flowingdata.com/hot-dog-places.csv", sep=",", header=TRUE)
names(hot_dog_places) <- c("2000", "2001", "2002", "2003", "2004",
                            "2005", "2006", "2007", "2008", "2009", "2010")
View(hot_dog_places)

# Stacked Barchart - Fig. 4-22:
hot_dog_matrix <- as.matrix(hot_dog_places)
plot2 <- barplot(hot_dog_matrix, border=NA, space=0.25, ylim=c(0, 200)
        ,xlab="Year", ylab="Hot dogs and buns (HDBs) eaten"
        , main="Hot Dog Eating Contest Results, 1980-2010")

###############################################################################
# Number of Subscribers
subscribers <- read.csv("http://datasets.flowingdata.com/flowingdata_subscribers.csv"
                        , sep=",", header=TRUE)

subscribers[1:5,]

plot(subscribers$Subscribers)

plot(subscribers$Subscribers, type="p", ylim=c(0, 30000))

# Scatter Plot - Fig. 4-28:
par(mar = c(6,5,2,1))
plot3 <- plot(subscribers$Subscribers, type="h", ylim=c(0, 30000), xlab="Day", ylab="Subscribers")

points(subscribers$Subscribers, pch=19, col="black")

###############################################################################
# World Population
population <- read.csv("http://datasets.flowingdata.com/world-population.csv", sep=",", header=TRUE)

# Time Series Chart - Fig. 4-34:
par(mar = c(6,5,2,2))

plot4 <- plot(population$Year, population$Population, type="l"
     , ylim=c(0, 7000000000), xlab="Year", ylab="Population", bty = 'n', lwd = 3, xpd = FALSE)

###############################################################################
# Step Chart
par(mar = c(4.5,4,3,2))
postage <- read.csv("http://datasets.flowingdata.com/us-postage.csv", sep=",", header=TRUE)

plot(postage$Year, postage$Price, type="s")

plot5 <- plot(postage$Year, postage$Price, type="s",
     main="US Postage Rates for Letters, First Ounce, 1991-2010", cex.main = 1
     , xlab="Year", ylab="Postage Rate (Dollars)")


#######################################################################################
# PART 2
#######################################################################################
file1 <- file.choose()
my_dir <- "/Users/advaitiyer/Desktop/Syracuse University/Academics/2nd Semester/IST 719/HW 2/"
Art <- read.csv(file1, stringsAsFactors = FALSE, sep=",", header=TRUE)

str(Art)
summary(Art)
# Distribution of Total Sale
par(mfrow = c(2,2), mar = c(4.5,4.5,4.5,4.5))
boxplot(Art$total.sale, ylab = "Total Sales", main = "Boxplot of Total Sales", col = "light green", outlier.color = "black")

hist(Art$total.sale, ylab = "Number of Customers", xlab = "Total Sales", main = "Histogram of Total Sales", col = "turquoise")

drawing_paper <- subset(Art, Art$paper == "drawing")
watercolor_paper <- subset(Art, Art$paper == "watercolor")

d1 <- density(drawing_paper$total.sale)
plot(d1, main = "Total Sales - Drawing Paper", xlab = "Total Sales")
polygon(d1, col = "powderblue")

d2 <- density(watercolor_paper$total.sale)
plot(d2, main = "Total Sales - Watercolor Paper", xlab = "Total Sales")
polygon(d2, col = "lightsalmon2")

