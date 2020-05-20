fname <- file.choose()
sales <- read.csv(fname, stringsAsFactors = F, header = T)

recipts <- tapply(sales$recipt, list(sales$sales.rep), sum)
units <- tapply(sales$units.sold, list(sales$sales.rep), sum)
expenses <- tapply(sales$expenses, list(sales$sales.rep), sum)       
feedback <- tapply(sales$rep.feedback, list(sales$sales.rep), mean)

options(scipen = 99)
M1 <- rbind(recipts, units, expenses, feedback)
M2 <- cbind(recipts, units, expenses, feedback)
M1
M2

# Heatmap
install.packages("RColorBrewer")
library(RColorBrewer)
par(mar=c(4,4,4,4))
matrix_M1 <- as.matrix(M1)
heatmap_M1 <- heatmap(matrix_M1, Rowv = NA, Colv = NA, col= brewer.pal(9, "Oranges")
                      , scale = "row", margins = c(8,8), xlab = "Sales Rep", ylab="Attributes"
                      , main = "Heatmap of Sales Reps Vs. Attributes", cexRow = 1)

# Chernoff Faces
install.packages("aplpack")
library(aplpack)
par(mar=c(4,4,4,4))
faces(M2, cex = 0.8, face.type =2, main = "Performance of Sales Representatives")

# Star Charts
par(mar=c(4,4,4,4))
coul = brewer.pal(12, "Paired")
coul = colorRampPalette(coul)(25) 
stars(M2, flip.labels = F, draw.segments = F, scale = TRUE, cex = 0.75
      , col.stars = coul, main = "Star Plot for Sales Representatives")

# Parallel Coordinates Plot
install.packages("lattice")
library(lattice)

sales_colors <- c()
for (i in 1:length(sales$recipt)) {
  
  if (sales$recipt[i] > mean(sales$recipt)) {
    col <- "red"
  } else {
    col <- "blue"
  }
  sales_colors <- c(sales_colors, col)
}

parallelplot(M2, horizontal.axis=F, col=sales_colors
         , main = "Parallel Coordinates Plot for Sales Reps Vs. Total recipts"
         , legend=unique(sales_colors), lty=1)
