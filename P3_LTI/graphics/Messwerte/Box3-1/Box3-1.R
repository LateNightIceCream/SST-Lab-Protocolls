library(ggplot2)
library(mosaic)

width <- 10
height <- width * 0.618

outputName <- "Box3-1.pdf"

box3_1.dat <- read.csv("Box3-1.csv")

attach(box3_1.dat)

box3_1.dat <- cbind(box3_1.dat, 20*log10(u/1000))

names(box3_1.dat)[3] <- "udB"

#box3_1.model <- nls( u ~ k2*f^-2, start=list(k2=2))
modelFun <- udB ~ A+B*exp(K*f)
box3_1.model <- fitModel(modelFun, data=box3_1.dat, start=c(A=20*log10(100/1000), B=20, K=-0.00002))
#box3_1.model <- lm( log(u) ~ log(f))

#summary(box3_1.model)

max(f)
min(f)

max(u)
min(u)

fmax <- 10^6
deltaf <- 1000

fpoints <- seq(0,fmax,deltaf)

box3_1.dat

#new <- data.frame(f = fpoints)
#prediction <- predict(box3_1.model, newdata=new)

#new <- cbind(new, prediction)

snacky <- function(f) {

    20*log10(100/1000) + 20*exp( -(0.00002)*f )

}


p <- ggplot(data=box3_1.dat, aes(x=f, y=udB)) +
    theme_minimal() +
    geom_point() +
#geom_smooth(se=FALSE, method="auto", n=2000, color="red2") +
    scale_x_continuous( trans="log10" ) +
#    geom_path(color = "blue4") +
   stat_function(fun=box3_1.model, n=4000, color="red")
 #   stat_function(fun=snacky, n=4000)
#    geom_point(data=new, aes(x=f, y=prediction), color="green") +
#    geom_path(data=new, aes(x=f, y=prediction), color="blue4")


pdf(outputName, width, height)
p
