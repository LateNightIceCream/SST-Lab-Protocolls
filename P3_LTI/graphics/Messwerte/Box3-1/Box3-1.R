library(ggplot2)
library(drc)

width <- 10
height <- width * 0.618

outputName <- "Box3-1.pdf"

box3_1.dat <- read.csv("Box3-1.csv")

attach(box3_1.dat)

box3_1.model <- drm( u ~ f)

fmax <- 10^6
deltaf <- 1000

fpoints <- seq(0,fmax,deltaf)

new <- data.frame(f = fpoints)
prediction <- predict(box3_1.model, newdata=new)

new <- cbind(new, prediction)

p <- ggplot(data=box3_1.dat, aes(x=f, y=u)) +
    theme_minimal() +
    geom_point() +
    scale_x_continuous( trans="log10" ) +
    geom_point(data=new, aes(x=f, y=prediction), color="green")


pdf(outputName, width, height)
p
