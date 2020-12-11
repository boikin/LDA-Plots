## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(LDAPlots)

## ----fig.height = 5, fig.width = 6, fig.align = "center"----------------------
library(MASS)
iris.lda <- lda(Species ~., data = iris)
p<-lda_gplot(lda=iris.lda, groups = iris$Species)
p

## ----fig.height = 5, fig.width = 6, fig.align = "center"----------------------
library(ggplot2)
p<-lda_gplot(lda=iris.lda, groups = iris$Species,
             normal.CI = FALSE, circles = TRUE, biplot = FALSE)
p + ggtitle("95% asymtopic CI's")

## ----fig.height = 5, fig.width = 6, fig.align = "center"----------------------
p<-lda_gplot(lda=iris.lda, groups = iris$Species,
             normal.CI = FALSE,boot.CI = TRUE, fill = FALSE, biplot = FALSE)
p + ggtitle("95% bootstrap CI's")

## ----fig.height = 5, fig.width = 6, fig.align = "center"----------------------
p<-lda_gplot(lda=iris.lda, groups = iris$Species,
             normal.CI = FALSE,perm.CI = TRUE, fill = FALSE, biplot = FALSE)
p + ggtitle("95% permutation CI's")

## ----fig.height = 5, fig.width = 6, fig.align = "center"----------------------
lda_posterior_plot(lda=iris.lda, groups = iris$Species)

## -----------------------------------------------------------------------------
q=100
data <- as.data.frame(list(x1 = runif(q), x2 = rnorm(q), x3 = rlnorm(q), 
                           group = sample(c('s','d','w'),q,replace=TRUE)))

## -----------------------------------------------------------------------------
lda <- lda(group ~., data = data)
# format data, groups need to be numeric, can not be factors of characters
V1<-as.numeric(as.factor(data$group))
lda.vec<-as.data.frame(lda$scaling)
lda.p <- predict(lda)
v <- as.data.frame(cbind(V1, lda.p$x))

## -----------------------------------------------------------------------------
str(v)

## -----------------------------------------------------------------------------
b <- boot_sample(values = v)
str(b)

## -----------------------------------------------------------------------------
b <- boot_sample(values = v)
str(b)

