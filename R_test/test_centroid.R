


# simulate the data
x <- rnorm(100, mean = 50, sd = 15) + 50
y <- rnorm(100, mean = 50, sd = 15)
xy <- data.frame(x, y, 'id' = rep('a', 100))
centroid <- apply(xy[, 1:2], 2, mean)

# percentages
d <- sqrt(((xy[, 1] - centroid[1])^2) + ((xy[, 2] - centroid[2])^2))
indx <- 1:length(d)
pct <- indx[d <= quantile(d, .8)]
mcp.pts <- xy[pct, ]

brdr <- chull(mcp.pts[, 1], mcp.pts[, 2])
xy.brdr <- mcp.pts[brdr, ]
xy.brdr <- rbind(xy.brdr[nrow(xy.brdr), ], xy.brdr)

# add mcp to plot
plot(x, y, asp = 1)
points(centroid[1], centroid[2], pch = 19, col = 'red')
lines(xy.brdr[, 1], xy.brdr[, 2], col = 'green', lwd = 3)
points(xy.brdr[, 1], xy.brdr[, 2], col = 'red', pch = 19)


# deduce centroid from polygon
xy.brdr
centroid2 <- apply(xy.brdr[, 1:2], 2, mean)
points(centroid2[1], centroid2[2], pch = 19, col = 'blue')

test <- xy.brdr[, 1:2]
test$PID <- 1
test$POS <- 1:nrow(test)
colnames(test)[1:2] <- c("X", "Y")

centroid3 <- PBSmapping::calcCentroid(test)
points(centroid3["X"], centroid3["Y"], pch = 19, col = 'purple')
