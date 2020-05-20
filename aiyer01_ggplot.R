##########################
# Name: Advait Ramesh Iyer
# Week 7: Independent Work
##########################

# Figure 2-2:
mtcars
library(ggplot2)
plot1 <- ggplot(mtcars, aes(x=mtcars$wt, y=mtcars$mpg)) + geom_point() + theme(plot.margin = unit(c(1,2,0.5,2),"cm"))
plot1

# Figure 2-7: 
install.packages("gridExtra")
library(gridExtra)
plot2 <- ggplot(mtcars, aes(mtcars$cyl)) + geom_histogram() + theme(plot.margin = unit(c(1,1,0.75,1),"cm"))
plot3 <- ggplot(mtcars, aes(factor(mtcars$cyl))) + geom_bar() + theme(plot.margin = unit(c(1,1,0.75,1),"cm"))
grid.arrange(plot2, plot3, ncol=2)

# Figure 2-11:
ToothGrowth
plot4 <- ggplot(ToothGrowth, aes(x=ToothGrowth$supp, y=ToothGrowth$len)) + geom_boxplot() + scale_y_continuous(breaks = c(5,10,15,20,25,30,35), limits = c(min(ToothGrowth$len),max(ToothGrowth$len)))+theme(plot.margin = unit(c(1,1,1,1),"cm"))
plot5 <- ggplot(ToothGrowth, aes(x=interaction(ToothGrowth$supp, ToothGrowth$dose), y=ToothGrowth$len)) + geom_boxplot()+ scale_y_continuous(breaks = c(5,10,15,20,25,30,35), limits = c(min(ToothGrowth$len),max(ToothGrowth$len)))+theme(plot.margin = unit(c(1,1,1,1),"cm"))
library(grid)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 3)))
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
}
print(plot4, vp = define_region(1,1))
print(plot5, vp = define_region(1,2:3))

# Figure 3-4:
install.packages("gcookbook")
library(gcookbook)
cabbage_exp
plot6 <- ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_col(position="dodge") + scale_y_continuous(breaks = c(0.0,0.5,1.0,1.5,2.0,2.5,3.0))+theme(plot.margin = unit(c(1,2,1,2),"cm"))
plot6

# Figure 3-11:
csub <- subset(climate, Source=="Berkeley" & Year >= 1900) 
csub$pos <- csub$Anomaly10y >= 0
csub
install.packages("ggExtra")
library(ggExtra)
plot7 <-  ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) + geom_bar(stat="identity", position="identity") + scale_y_continuous(breaks = c(-0.2,0.0,0.2,0.4,0.6,0.8))+theme(plot.margin = unit(c(1,1,1,1),"cm"))+removeGrid(y=FALSE,x=FALSE)
plot7

# Figure 4-19:
sunspotyear <- data.frame(Year=as.numeric(time(sunspot.year))
                          , Sunspots=as.numeric(sunspot.year))
plot8 <- ggplot(sunspotyear, aes(x=Year, y=Sunspots)) + geom_area(fill="blue", alpha=.2)+geom_line() + theme(plot.margin = unit(c(1,1,1,1),"cm"))+removeGrid(y=FALSE,x=FALSE)
plot8

# Figure 4-24:
library(gcookbook) 
library(plyr)
uspopage_prop <- ddply(uspopage, "Year", transform
                       , Percent = Thousands / sum(Thousands) * 100)

plot9 <- ggplot(uspopage_prop, aes(x=Year, y=Percent, fill=AgeGroup))+geom_area(colour="black", size=.2, alpha=.4)+scale_fill_brewer(palette="Blues", breaks=rev(levels(uspopage$AgeGroup)))+theme(plot.margin = unit(c(1,1,1,1),"cm"))+removeGrid(y=FALSE,x=FALSE)+ylab("Percent")
plot9
