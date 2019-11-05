library(ggplot2)

outputName   <- "2_2_Amplivergleich.pdf"
outputWidth  <- 10
outputHeight <- 0.618 * outputWidth

# function parametersa
A   <- 2
m   <- 10 # number of amplitude values

ylabel <- bquote( "|a"[n]~"| / V" )
xlabel <- bquote( "f / f"[0] )


amplifun <- function (n) {

    abs( 4 * 2 / (n * pi) * sin(n * pi/2) )

}

deltaX <- 1
deltaCounter <- 0

lineCoordinatesX <- c()
lineCoordinatesY <- c()

lineCoordinatesY_Measurement <- c()

print(amplifun(1))

for (i in 1:m) {

    deltaCounter <- deltaCounter + deltaX

    lineCoordinatesX <- c( lineCoordinatesX, deltaCounter)
    lineCoordinatesY <- c( lineCoordinatesY, amplifun(i) )

    lineCoordinatesY_Measurement <- c( lineCoordinatesY_Measurement, amplifun(i) * 0.5 )

}

maxY <- ceiling(max( lineCoordinatesY ))
minY <- floor(max( lineCoordinatesY ))

ylim   <- c(0, (maxY+minY)/2)
xlim   <- c(0, m)

ybreaks <- seq(0, (maxY+minY)/2, 0.2)
xbreaks <- seq(0,m, 1)


hsBlue <- "#00b1db"
theoreticalColor <- "#afafafFF"


plot <- ggplot(data.frame(x=c(0,2), y=c(0,2)), aes(x=x)) +
    theme_minimal() +
    theme(legend.position="none", plot.title = element_text(color = "gray21", size=1.618^5), plot.subtitle = element_text(color = "grey80", size=1.618^5)) +

    #ggtitle(bquote("Amplitudenspektrum von x(t), Rot: theoretische Werte")) +
    #labs(subtitle="D=0.25, T=1us")

    xlab(xlabel) +
    ylab(ylabel) +

    scale_y_continuous(limits= ylim, breaks=ybreaks) +
    scale_x_continuous(limits= xlim, breaks=xbreaks)


linewidth <- (1-0.6180339887498948)*10

for (i in 1:m) {

   if(lineCoordinatesY[i] < lineCoordinatesY_Measurement[i] ) {

       # plot measurement first, then plot theoreticals on top

   plot <- plot + geom_segment(
            x = lineCoordinatesX[i],
            y = 0,
            xend = lineCoordinatesX[i],
            yend = lineCoordinatesY_Measurement[i],
            color=hsBlue,
            linetype="solid", size = linewidth
        )

   plot <- plot + geom_segment(
            x = lineCoordinatesX[i],
            y = 0,
            xend = lineCoordinatesX[i],
            yend = lineCoordinatesY[i],
            color=theoreticalColor,
            linetype="solid", size = linewidth
        )
   } else {

       # plot theoretical first, then plot measurement

   plot <- plot + geom_segment(
            x = lineCoordinatesX[i],
            y = 0,
            xend = lineCoordinatesX[i],
            yend = lineCoordinatesY[i],
            color=theoreticalColor,
            linetype="solid", size = linewidth
        )

   plot <- plot + geom_segment(
            x = lineCoordinatesX[i],
            y = 0,
            xend = lineCoordinatesX[i],
            yend = lineCoordinatesY_Measurement[i],
            color=hsBlue,
            linetype="solid", size = linewidth
        )


   }

}



pdf(outputName, width = outputWidth, height = outputHeight)
plot
