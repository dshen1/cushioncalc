# TODO: Add comment
#
# Author: solomon
###############################################################################

setwd("~/Dropbox/diamonds-data/")
load("BigDiamonds.Rda")

library(ggplot2)
library(GGally)
library(scales)

## Create big matrix
diasamp = diamonds[sample(1:length(diamonds$price), 10000),]
png("plots/ggpairs.png", height=1000, width=1000)
ggpairs(na.omit(diasamp), params = c(shape = I("."), outlier.shape = I(".")))
dev.off()

## Function to calc cube root
cubroot_trans = function() trans_new("cubroot", transform= function(x) x^(1/3), inverse = function(x) x^3 )

## Look at price distribution
qplot(carat, price, data=diamondsbig, geom="point", alpha=I(.3)) +
		scale_x_continuous(trans=cubroot_trans(), limits = c(0.2,4),
				breaks = c(0.2, 0.5, 1, 2, 3, 4)) +
		scale_y_continuous(trans=log10_trans(), limits = c(350,70000),
				breaks = c(350, 1000, 5000, 10000, 15000, 20000, 30000, 40000)) +
		theme_bw() +
		ggtitle("Exhibit 1: Price (log10) of Cushion Diamonds by Cubed-Root of Carat")

## Look at distribution by certification
qplot(carat, price, data=diamondsbig, geom="point", alpha=I(.3),
      colour = cert) +
  scale_colour_brewer(type = "div",
                      guide = guide_legend(title = NULL, reverse=T,
                                           override.aes = list(alpha = 1))) +
  scale_x_continuous(trans=cubroot_trans(), limits = c(0.2,4),
                     breaks = c(0.2, 0.5, 1, 2, 3, 4)) +
  scale_y_continuous(trans=log10_trans(), limits = c(350,70000),
                     breaks = c(350, 1000, 5000, 10000, 15000, 20000, 30000, 40000)) +
  theme_bw() +
  ggtitle("Exhibit 2: Price (log10) of Cushion Diamonds by Cubed-Root of Carat & Cert")

## Look at distribution by clarity
qplot(carat, price, data=diamondsbig, geom="point", alpha=I(.3),
      colour = clarity) +
  scale_colour_brewer(type = "div",
                      guide = guide_legend(title = NULL, reverse=T,
                                           override.aes = list(alpha = 1))) +
  scale_x_continuous(trans=cubroot_trans(), limits = c(0.2,4),
                     breaks = c(0.2, 0.5, 1, 2, 3, 4)) +
  scale_y_continuous(trans=log10_trans(), limits = c(350,70000),
                     breaks = c(350, 1000, 5000, 10000, 15000, 20000, 30000, 40000)) +
  theme_bw() +
  ggtitle("Exhibit 3: Price (log10) of Cushion Diamonds by Cubed-Root of Carat & Clarity")

## Look at distribution by cut
qplot(carat, price, data=diamondsbig, geom="point", alpha=I(.3),
      colour = cut) +
  scale_colour_brewer(type = "div",
                      guide = guide_legend(title = NULL, reverse=T,
                                           override.aes = list(alpha = 1))) +
  scale_x_continuous(trans=cubroot_trans(), limits = c(0.2,4),
                     breaks = c(0.2, 0.5, 1, 2, 3, 4)) +
  scale_y_continuous(trans=log10_trans(), limits = c(350,70000),
                     breaks = c(350, 1000, 5000, 10000, 15000, 20000, 30000, 40000)) +
  theme_bw() +
  ggtitle("Exhibit 4: Price (log10) of Cushion Diamonds by Cubed-Root of Carat & Cut")

## Look at distribution by color
qplot(carat, price, data=diamondsbig, geom="point", alpha=I(.3),
      colour = color) +
  scale_colour_brewer(type = "div",
                      guide = guide_legend(title = NULL, reverse=T,
                                           override.aes = list(alpha = 1))) +
  scale_x_continuous(trans=cubroot_trans(), limits = c(0.2,4),
                     breaks = c(0.2, 0.5, 1, 2, 3, 4)) +
  scale_y_continuous(trans=log10_trans(), limits = c(350,70000),
                     breaks = c(350, 1000, 5000, 10000, 15000, 20000, 30000, 40000)) +
  theme_bw() +
  ggtitle("Exhibit 5: Price (log10) of Cushion Diamonds by Cubed-Root of Carat & Color")

## New column for the log price
diamondsbig$logprice  <-  log(diamondsbig$price) 

## Create a model to estimate value of diamond based on attributes
library('memisc')
m1 = lm(logprice~  I(carat^(1/3)), 
    data=diamondsbig[diamondsbig$price < 50000 & diamondsbig$cert == "GIA",])
m2 = update(m1, ~ . + carat)
m3 = update(m2, ~ . + cut )
m4 = update(m3, ~ . + color + clarity)
mtable(m1, m2, m3, m4)

## Model fit
thisDiamond = data.frame(carat = 2, cut = "Good", color = "G", clarity="VS1")
modEst = predict(m4, newdata = thisDiamond, interval="prediction", level = .95)
exp(modEst)

