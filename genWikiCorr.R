require(ggplot2)
require(ggthemes)
# dcor
require(energy)
# Hilbert Schmidt
require(dHSIC)
# multivariate normal
require(MASS)

genLine <- function(n=1000, scale=1.0, min=0, max=1)
{
	x <- runif(n, min=min, max=max)
	x_center <- mean(x)
	y <- (x - x_center)*scale + x_center
	return(data.frame(x=x, y=y))
}

genBivarGauss <- function(n=1000, scale=1.0, var=0.1, cov=0, mu=0)
{
	cov_matrix <- matrix(c(var,cov,cov,var), nrow=2)
	data <- mvrnorm(n=n, mu=c(mu,mu), Sigma=cov_matrix)
	data[,2] <- (data[,2]- mean(data[,2]))*scale
	return(data.frame(x=data[,1], y=data[,2]))
}

genSquare <- function(n=1000, alpha=90, min=0, max=1)
{
	x <- runif(n, min=min, max=max)
	y <- runif(n, min=min, max=max)
	rotaMat <- matrix(c(cos(alpha), -sin(alpha), sin(alpha), cos(alpha)), nrow=2)
	data <- cbind(x,y) %*% rotaMat
	return(data.frame(x=data[,1],y=data[,2]))
}

circle <- function(x, r, x_0, y_0)
{
	return(sqrt(r^2 - (x - x_0)^2) + y_0)
}
genCircle <- function(n=1000, r=0.5, center=c(0.5,0), sigma=0.01)
{
	x_pos <- runif(n/2, min=center[1]-r, max=center[1]+r)
	x_neg <- runif(n/2, min=center[1]-r, max=center[1]+r)
	y_pos <- sapply(x_pos, function(x) {circle(x,r,center[1],center[2])}) + rnorm(n/2, mean=0, sd=sigma)
	y_neg <- -sapply(x_neg, function(x) {circle(x,r,center[1],center[2])}) + rnorm(n/2, mean=0, sd=sigma)
	return(data.frame(x=c(x_pos,x_neg),y=c(y_pos,y_neg)))
}

gen4Points <- function(n=1000)
{
	l1 <- genBivarGauss(n/4, var=0.005)
	l1$x <- l1$x - 0.25
	l2 <- genBivarGauss(n/4, var=0.005)
	l2$x <- l2$x + 0.25
	l3 <- genBivarGauss(n/4, var=0.005)
	l3$x <- l3$x - 0.25
	l3$y <- l3$y + 0.5
	l4 <- genBivarGauss(n/4, var=0.005)
	l4$x <- l4$x + 0.25
	l4$y <- l4$y + 0.5
	return(data.frame(x=c(l1$x,l2$x,l3$x,l4$x), y=c(l1$y,l2$y,l3$y,l4$y)))
}

genMoon <- function(n=1000, r=0.5, center=c(0.5,0), sigma=0.02)
{
	x_pos <- runif(n, min=center[1]-r, max=center[1]+r)
	y_pos <- sapply(x_pos, function(x) {circle(x,r,center[1],center[2])}) + rnorm(n, mean=0, sd=sigma)
	return(data.frame(x=x_pos,y=y_pos))
}

genDoubleMoon <- function(n=1000, r=0.5, center=c(0.5,0), sigma=0.02)
{
	x_pos <- runif(n/2, min=center[1]-r, max=center[1]+r)
	x_neg <- runif(n/2, min=center[1]-r, max=center[1]+r)
	y_pos <- sapply(x_pos, function(x) {circle(x,r,center[1],center[2])})-r + rnorm(n/2, mean=0, sd=sigma)
	y_neg <- -sapply(x_neg, function(x) {circle(x,r,center[1],center[2])})+r + rnorm(n/2, mean=0, sd=sigma)
	return(data.frame(x=c(x_pos,x_neg),y=c(y_pos,y_neg)))
}

genBat <- function()
{
}


genPlot <- function(data)
{
	data$x <- scale(data$x, center=min(data$x), scale=(max(data$x)-min(data$x)))
	if (var(data$y) != 0)
	{
		data$y <- scale(data$y, center=min(data$y), scale=(max(data$y)-min(data$y)))
	} else {
		data$y <- data$y - min(data$y)
	}
	corP <- round(cor(data$x, data$y, method="pea"), digits=3)
	corS <- round(cor(data$x, data$y, method="spe"), digits=3)
	dc <- round(dcor(data$x, data$y), digits=3)

	pHS <- round(dhsic.test(data$x, data$y, alpha=0.01)$p.value, digits=5)
	gg <- ggplot(data, aes(x=x,y=y))
	gg <- gg + geom_point(alpha=0.8)
	gg <- gg + theme_tufte()
	gg <- gg + theme(axis.title=element_blank(),
        			axis.text=element_blank(),
        			axis.ticks=element_blank()
				)
	gg <- gg + annotate("rect", xmin=0.68, xmax=0.92, ymin=0.02, ymax=0.23, alpha=.4)
	gg <- gg + annotate("text", color="white", size=4, x=0.8, y=0.2, label=paste("Pearson: ", corP, sep=""))
	gg <- gg + annotate("text", color="white", size=4, x=0.8, y=0.15, label=paste("Spearman: ", corS, sep=""))
	gg <- gg + annotate("text", color="white", size=4, x=0.8, y=0.1, label=paste("dcor: ", dc, sep=""))
	gg <- gg + annotate("text", color="white", size=4, x=0.8, y=0.05, label=paste("pHS ", pHS, sep=""))
	return(gg)
}


d1 <- genLine()
d2 <- genLine(scale=0.6)
d3 <- genLine(scale=0.2)
d4 <- genLine(scale=0)
d5 <- genLine(scale=-0.2)
d6 <- genLine(scale=-0.6)
d7 <- genLine(scale=-1)
plots <- lapply(list(d1,d2,d3,d4,d5,d6,d7), genPlot)
pdf("lineCorr.pdf")
for (p in plots)
{
	print(p)
}
dev.off()

d8 <- genBivarGauss()
d9 <- genBivarGauss(cov=0.05)
d10 <- genBivarGauss(cov=0.08)
d11 <- genBivarGauss(cov=-0.08)
d12 <- genBivarGauss(cov=-0.05)

plots <- lapply(list(d8,d9,d10,d11,d12), genPlot)
pdf("gaussCorr.pdf")
for (p in plots)
{
	print(p)
}
dev.off()

d13 <- genCircle()

d14 <- gen4Points()

d15 <- genMoon()
d16 <- genDoubleMoon()

d17 <- genSquare(alpha=10)
d18 <- genSquare(alpha=45)

plots <- lapply(list(d13,d14,d15,d16,d17,d18), genPlot)

pdf("complexCorr.pdf")
for (p in plots)
{
	print(p)
}
dev.off()
