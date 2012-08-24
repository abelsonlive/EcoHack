# grand arcs
# apapted from flowingdata.com / paul butler by brian abelson @ EcoHack 2012
setwd("~/Dropbox/GitRepository/EcoHack/")
#install libraries

require("maps")
require("geosphere")
require("RColorBrewer")
 
# the code is written for each coordinate to be stored in a separate column (EG from_x, from_y, to_x, to_y)
# However, it can also be adpated to deal with columns that contain coordinate pairs (EG from_x.y, to_x.y)
# If the latter is preferred, just get rid of the c() functions inside gcIntermedaite []EG gcIntermediate(from_x.y, to_x.y)]
data <- read.csv("unqiue_connections_export.csv")
data <- data[,-c(9,10,11)]
# remove NAs which don't plot correctly
data <-na.omit(data)

#order data by quantity so smaller transaction counts plot first
data <- data[order(data$quantity),]

#set bins for transaction count categories. I just created them manually from looking at the quantity quartiles
#one could also use pretty() to make the breaks.
data$bin[data$quantity ==1] <- 1
data$bin[data$quantity > 1 & data$quantity <= 10] <- 2
data$bin[data$quantity > 10 & data$quantity <= 62] <- 3
data$bin[data$quantity > 62] <- 4

#set colors from color brewer
colors <- brewer.pal(4, "YlGnBu")

#add transparency by pasting a number onto the end of the hex code
colors <- adjustcolor(colors, 0.05)

#plot base map
png("/Users/brian/Dropbox/GitRepository/EcoHack/images/map.png",width=1600, height=900, units="px")
map("world", col="grey10", fill=TRUE, bg="black", lwd=0.05, lty=1)

#plot connecting lines
n <- nrow(data)
for (j in 1:n) {
	#break data into an individual coord pair
	coord.pair <- data[j,]	
	
	#create great circle from coord pair
	#breakAtDateLine=TRUE breaks lines into two pieces and stores them in a list of length two
	inter <- gcIntermediate(c(coord.pair$from_x, coord.pair$from_y), 
										  c(coord.pair$to_x, coord.pair$to_y), 
												addStartEnd=FALSE, 
												breakAtDateLine=TRUE)

# find coord pairs which cross the dateline and split into two lines
	if(length(inter)==2){
		
	# assign color from the bin number
	colindex <- coord.pair$bin
	
	#plot lines
	lines(jitter(inter[[1]], 1), col= colors[colindex], lwd=1, lty=1) # add jitter to increase visibility of clusters
    lines(jitter(inter[[2]], 1), col= colors[colindex], lwd=1,  lty=1) # add jitter to increase visibility of clusters
    }

# plot coord pairs that don't cross the dateline
   else{
   	
   	# assign color from the bin number
   colindex <- coord.pair$bin
   
   #plot line
	lines(jitter(inter, 1), col=colors[colindex], lwd=1,   lty=1)
   }
}
dev.off()
