##########################
# Name: Advait Ramesh Iyer
# Homework 6
##########################
crime <- read.csv('http://datasets.flowingdata.com/crimeRatesByState2005.csv', sep = ",", header = TRUE)
dim(crime)
colnames(crime)
colnames(crime) <- c("State Name", "Murder", "Forcible Rape"
                     , "Robbery", paste("Aggrevated", "Assault"
                     , sep = "\n"), "Burglary", "Larceny/Theft"
                     , paste("Motor Vehicle","Theft",sep = "\n")
                     , "Population")

crime2 <- crime[crime$`State Name`!= "District of Columbia",]
crime2 <- crime2[crime2$`State Name` != "United States",]
dim(crime2)
scatter.smooth(crime[,2:9])
par(mar = c(2,2,3,2))
dev.off()
# Fig 6-9:
pairs(crime2[,2:9], pch = 16, col = "grey", las = 1
      , panel = panel.smooth, cex = 0.7
      , col.smooth = rgb(4,50,190, max = 255))

mtext("Rates per 100,000 population", side = 3, las  = 1
      , line = 2.5, at = 0.12, cex = 0.9)

# Fig 6-15:
crime <- read.csv('http://datasets.flowingdata.com/crimeRatesByState2005.tsv', sep = "\t", header = TRUE)
dim(crime)
dev.off()
par(mar = c(7,5,6,1), bty= "n", xpd = NA)
radius <- sqrt(crime$population/pi)
symbols(crime$murder, crime$burglary, circles = radius
        , inches = 0.35, fg = "white", bg = "red", xlab = NA
        , ylab = NA, las = 1, xaxt = "n", yaxt = "n")

points(8.5,335, cex = 12)
points(8.5,280, cex = 8)
points(8.5,230, cex = 4)

axis(side=1, lwd=1.5, xpd=TRUE, pos = 50, at = seq(0,10,2)
     , labels = FALSE, col = "black", col.ticks = "black")

axis(side=2, lwd=1.5, xpd=TRUE, at = seq(200, 1200,200)
     , labels = FALSE, col = "black", col.ticks = "black"
     , las = 1)

mtext(seq(0,10,2), side = 1, line = 2.5, cex = 0.8
      , at = seq(0,10,2))

mtext(seq(200,1200,200), side = 2, line = 1, cex = 0.8
      , at = seq(200,1200,200), las = 1)

mtext("Burglaries", side = 2, line = -0.6, at = 1400, las = 1
      , cex = 0.8)

mtext("per 100,000 population", side = 2, line = -4.4, at = 1350
      , las = 1, cex = 0.8)

mtext("Murders", side = 1, line = 3.5, at = 0.25, cex = 0.8)

mtext("per 100,000 population", side = 1, line = 4.4
      , at = 0.9, cex = 0.8)

text(crime$murder, crime$burglary, crime$state, cex = 0.5)

mtext("Source: U.S. Census Bureau | Nathan Yau", side = 1
      , line = 5, at = 9, cex = 0.7)

mtext("MURDER VERSUS BURGALARIES IN THE UNITED STATES", side = 2
      , las = 1, line = -32.5, cex = 1.5, at = 1600)

mtext("Population", side = 1, las = 1, line = -6.5, cex = 0.8
      , at = 8.5)

mtext("40m", side = 1, las = 1, line = -5.5, cex = 0.8, at = 8.5)

mtext("20", side = 1, las = 1, line = -4, cex = 0.8, at = 8.5)

mtext("3", side = 1, las = 1, line = -2.5, cex = 0.8, at = 8.5)


# Fig 6-24:
birth <- read.csv('http://datasets.flowingdata.com/birth-rate.csv', sep = ",", header = TRUE)
dim(birth)
dev.off()
par(mar = c(4.5,4.5,8,2),bty = "o", xpd = FALSE)

hist(birth$X2008, xlim = c(0,60), las = 1, col = "darkorchid4"
     , border = "white", main = NA, xlab = NA, ylab = NA
     , yaxt = "n", xaxt = "n")

abline(h = 10, col = "gray48", lty = 3)
abline(h = 20, col = "gray48", lty = 3)
abline(h = 30, col = "gray48", lty = 3)
abline(h = 40, col = "gray48", lty = 3)
abline(h = 50, col = "gray48", lty = 3)
abline(h = 60, col = "gray48", lty = 3)

hist(birth$X2008, add = TRUE, xlim = c(0,60), las = 1
     , col = "darkorchid4", border = "white", main = NA
     , xlab = NA, ylab = NA, yaxt = "n", xaxt = "n")

axis(side=1, lwd=1.5, xpd=TRUE, pos = c(0,60)
     ,at = c(0,10,20,30,40,50,60), labels = FALSE)

mtext(c("0","10","20","30","40","50","60"), side = 2
      , line = 0.5, at = c(0,10,20,30,40,50,60), las = 1
      , cex = 0.8)

points(19,37, pch = 16, col = "black", cex = 1)

mtext(c("Country","Count"), side = 2, line = 0.02, at = c(67,64)
      , las = 1, cex = 0.8)

mtext(c("Live births per 1,000 population"), side = 1, line = 1
      , at = c(8.3), las = 1, cex = 0.8)

mtext(c("Source: The World Bank"), side = 1, line = 1.75
      , at = c(55), las = 1, cex = 0.7)

line1 <- "In 2008, most countries had birth rates less than 25 live births per 1,000 population."

line2 <- "There are, however, many developing countries where women tend to bear more"

line3 <- "children."

mtext(line1, side = 2, line = -28.2, at = c(87), las = 1
      , cex = 1)

mtext(line2, side = 2, line = -27.2, at = c(82), las = 1
      , cex = 1)

mtext(line3, side = 2, line = -0.5, at = c(77), las = 1
      , cex = 1)

mtext(seq(0,60,10), side = 1, line = 0.25, at = seq(0,60,10)
      , las = 1, cex = 0.8)

mtext(c("GLOBAL DISTRIBUTION OF BIRTH RATES"), side = 2,
      line = -23.8, at = c(95), las = 1, cex = 1.6)

segments(19,37,19,53)

segments(19,53,25,53)

segments(-5,0,0,0, lty = 1, lwd = 1.5)

line4 <- "The median birth rate was"

line5 <- "19 live births per 1,000 population"

mtext(line4, side = 1, line = -14.5, at = c(33.5)
      , las = 1, cex = 0.8)

mtext(line5, side = 1, line = -13.5, at = c(36)
      , las = 1, cex = 0.8)

# Fig 6-32

dev.off()
birth2008 <- birth$X2008[!is.na(birth$X2008)]
d2008 <- density(birth2008)


par(mar = c(4,4,4,2), bty = "n")
plot(d2008, type = "n", xaxt = "n", yaxt = "n", xlab = NA
     , ylab = NA, main = NA)

install.packages("lattice")
library(lattice)

#histogram(birth$X2008, breaks = 10)
#lines(d2008)

rect(par("usr")[1], par("usr")[3] + 0.0017, par("usr")[2], par("usr")[4], col = "gray90", border = "white")

abline(h = seq(0.00,0.04,0.01), v = seq(0,60,10), col = "white")

polygon(d2008, col = "#821122", border = "#cccccc")

axis(side=1, lwd=1.5, xpd=TRUE, pos = c(0,60)
     , at = c(0,10,20,30,40,50,60), labels = FALSE
     , col = "white", col.ticks = "black")

axis(side=2, lwd=1.5, xpd=TRUE, at = seq(0.00, 0.04, 0.01)
     , labels = FALSE, col = "white", col.ticks = "black")

# Labels for X axis
mtext(seq(0,60,10), side = 1, line = 0, at = seq(0,60,10)
      , cex = 0.8)

# Labels for Y axis
mtext(seq(0.01, 0.04, 0.01), side = 2, line = 1
      , at = seq(0.01,0.04,0.01), cex = 0.8, las = 1)

# For 0.00
mtext("0.00", side = 2, line = 1, at = 0, cex = 0.8, las = 1)

mtext("Density", side = 2, line = 1, at = 0.044, cex = 0.8, las = 1)

mtext("Live births per 1,000 population", side = 1, line = 1.5
      , at = 8, cex = 0.8, las = 1)

mtext("Source: The World Bank", side = 1, line = 1.5
      , at = 57, cex = 0.8, las = 1)

points(19,0, pch = 16, col = "black")

segments(19,0,19,0.032, lty = 1, col  = "black")

segments(19,0.032,30,0.032, lty = 1, col  = "black")

mtext(line4, side = 1, line = -16, at = c(38)
      , las = 1, cex = 0.8)

mtext(line5, side = 1, line = -15, at = c(40), las = 1
      , cex = 0.8)

mtext(c("GLOBAL DISTRIBUTION OF BIRTH RATES IN 2008")
      , side = 2, line = -28.2, at = c(0.05), las = 1, cex = 1.6)


fname <- file.choose() # Using art dataset
art <- read.csv(fname, stringsAsFactors = FALSE, header = TRUE)
summary(art)
art$year <- as.character(art$year)
histogram(~total.sale | year, data=art1, layout=c(2,2)
          , main = "Total sale across years")



