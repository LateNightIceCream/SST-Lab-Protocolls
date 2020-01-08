library(ggplot2)

outputName   <- "2_1_704kbit_Amplivergleich.pdf"
outputWidth  <- 10
outputHeight <- 0.618 * outputWidth

data <- read.csv("data.csv")


xValues  <- data$f
yValues1 <- data$Eingang
yValues2 <- data$Ausgang

hsBlue <- "#99ffb5"
theoreticalColor <- "#fbde96"

dataSchnib <- data.frame(x = xValues, y1=yValues1, y2=yValues2, dif = yValues2-yValues1)

print(dataSchnib$dif)

xlab <- "f / MHz"
ylab <- "dBm"

ylimits <- c(-70, 10)
ybreaks <- c(0, -20, -40, -60)

xlimits <- c(min(xValues), 10)

plot <- ggplot(data = dataSchnib, aes(x = x, y=y2) ) +
    theme_minimal() +
    xlab(xlab) +
    ylab(ylab) +
    scale_y_continuous(limits = ylimits, breaks = ybreaks) +
    scale_x_continuous(limits = xlimits, trans="log10") +
    geom_point(color="#00000000") +
    geom_point(aes(x=x, y=dif), color="#FF00FF")

################################

linewidth <- (1-0.6180339887498948)*10

## for( i in 1:nrow(data)) {

##    if(yValues2[i] < yValues1[i] ) {

##        # plot measurement first, then plot theoreticals on top

##    plot <- plot + geom_segment(
##             x = xValues[i],
##             y = 0,
##             xend = xValues[i],
##             yend = yValues2[i],
##             color=hsBlue,
##             linetype="solid", size = linewidth
##         )

##    plot <- plot + geom_segment(
##             x = xValues[i],
##             y = 0,
##             xend = xValues[i],
##             yend = yValues1[i],
##             color=theoreticalColor,
##             linetype="solid", size = linewidth
##         )
##    } else {

##        # plot theoretical first, then plot measurement

##    plot <- plot + geom_segment(
##             x = xValues[i],
##             y = 0,
##             xend = xValues[i],
##             yend = yValues1[i],
##             color=theoreticalColor,
##             linetype="solid", size = linewidth
##         )

##    plot <- plot + geom_segment(
##             x = xValues[i],
##             y = 0,
##             xend = xValues[i],
##             yend = yValues2[i],
##             color=hsBlue,
##             linetype="solid", size = linewidth
##         )

##    }


## }


pdf(outputName, width = outputWidth, height = outputHeight)
plot

