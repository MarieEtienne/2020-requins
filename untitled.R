# Scaling factor needed for correct plotting
# From mrds:::scalevalue
scale.value <- function (key.scale, z)
{
	exp(as.matrix(z) %*% key.scale)
}

# This function calculates the detection probability according to a hazard rate model
# from mrds:::keyfct.hz
key.fct.hz <- function (distance, key.scale, key.shape)
{
	return(1 - exp(-(distance / key.scale) ^ (-key.shape)))
}

# Vector of distances used to re-create the detection function (from 0 out to truncation distance of 250 m)
distances <- seq(0, 0.30, length.out = 100)

# Using a hazard-rate model with one (dichotomous, 1/0) factor variable that only affects the scale
# (not the shape) of the detection function:

# Shape parameter
key.shape <-
	scale.value(detfc.sea.hr$ddf$ds$aux$ddfobj$shape$parameters,
							matrix(1, nrow = 100, 1))

# Scale parameter
# Factor = 0
mat0 <- matrix(1, nrow = 100, ncol = 4)
mat0[, 2:4] <- 0
key.scale0 <- scale.value(detfc.sea.hr$ddf$ds$aux$ddfobj$scale$parameters, mat0)

# Factor = 1
mat1 <- matrix(1, nrow = 100, ncol = 4)
mat0[, 3:4] <- 0
key.scale1 <- scale.value(detfc.sea.hr$ddf$ds$aux$ddfobj$scale$parameters, mat1)

# Factor = 2
mat2 <- matrix(1, nrow = 100, ncol = 2)
mat0[, c(2,4)] <- 0
key.scale2 <- scale.value(detfc.sea.hr$ddf$ds$aux$ddfobj$scale$parameters, mat2)

# Factor = 3
mat3 <- matrix(1, nrow = 100, ncol = 2)
mat0[, c(2,3)] <- 0
key.scale3 <- scale.value(detfc.sea.hr$ddf$ds$aux$ddfobj$scale$parameters, mat3)


# Calculate detection probability values
y.val0 <- key.fct.hz(distances, key.scale0, key.shape)
y.val1 <- key.fct.hz(distances, key.scale1, key.shape)
y.val2 <- key.fct.hz(distances, key.scale2, key.shape)
y.val3 <- key.fct.hz(distances, key.scale3, key.shape)

# Now let's re-create the histogram
# from mrds:::detfct
bindata <- function(x, r, breaks) {
	return(hist(r[r >= x[1] & r <= x[2]], breaks = breaks,
							plot = FALSE)$counts)
}

sumit <- function(x, n, wt) {
	return(sum(x / (wt * n)))
}

selected <- rep(TRUE, nrow(detfc.sea.hr$ddf$ds$aux$ddfobj$xmat))

# Detection probability for each fitted value & Nhat estimate
if (length(detfc.sea.hr$ddf$fitted) == 1) {
	pdot <- rep(detfc.sea.hr$ddf$fitted, sum(as.numeric(selected)))
} else {
	pdot <- detfc.sea.hr$ddf$fitted[selected]
	Nhat <- sum(1 / pdot)
}

# Create a dummy histogram (h1)
# dist.data is the data.frame object of the distance data
# Right-truncating here at 250 m
# If you inspect the internal functions of the mrds package, you will find how the number of histogram breaks is calculated. In my case, the value is 11
h1 <-
	hist(distdata[distdata$distance <= 0.30, ]$distance, breaks = seq(0, 0.3, 0.3 /
																																			11))

# Calculate expected counts for each distance value
expected.counts <-
	apply(
		t(as.matrix(c(0, 250))),
		1,
		bindata,
		r = (0:1000) * 250 / 1001,
		breaks = seq(0, 250, 250 / 11)
	)
expected.counts <-
	apply(expected.counts, 1, sumit, n = 1001, wt = pdot)

# Re-scale the counts
h1$counts <- h1$counts / expected.counts

# Use the ggplot2 package to plot the histogram (as rectangles) and overlay the detection function
# plotting options
gg.opts <- theme(
	panel.grid.major = element_blank(),
	panel.grid.minor = element_blank(),
	panel.background = element_blank()
)

ggplot() +
	geom_line(data = data.frame(x = distances, y = y.val0), aes(x = x, y = y)) + # seaState = 0
	geom_line(data = data.frame(x = distances, y = y.val1), aes(x = x, y = y), col = "red") + # seaState =  1
	geom_line(data = data.frame(x = distances, y = y.val2), aes(x = x, y = y), col = "orange") + # seaState = 2
	geom_line(data = data.frame(x = distances, y = y.val3), aes(x = x, y = y), col = "yellow") + # seaState =  3
	
	gg.opts +
	xlab("Distance (m)") +
	ylab("Detection probability")
