##' lda_gplot()  creates a ggplot object of LDA projections, and the corresponding biplot and various CI's
##' @title Function to make LDA plots with confidence bounds
##' @param lda the output from lda() in the package MASS it is an lda class object.
##' @param groups A vector of the original groups from the data used in lda(), the Y variable in the lda model.
##' @param alpha Alpha value for CI's default in .05 for 95% CI's
##' @param N.boot The number of bootstrap sample means to be estimated.
##' @param N.perm The number of permutation sample means to be estimated.
##' @param fill TRUE/FALSE value if CI's plots should be filled/shaded in. Fill is not available for circles.
##' @param means TRUE/FALSE value to indicate if group LDA means should be include.
##' @param biplot TRUE/FALSE value to indicate if group LDA means should be include.
##' @param normal.CI TRUE/FALSE value to indicate if group normal 95% CI's for LDA projected data should be included.
##' @param circles TRUE/FALSE value to indicate if asymptotic 95% CI"s should be included for group LDA means.
##' @param boot.CI TRUE/FALSE value to indicate if bootstrap estimated 95% CI"s should be included for group LDA means.
##' @param perm.CI TRUE/FALSE value to indicate if permutation estimated 95% CI"s should be included for group LDA means.
##' @param scale TRUE/FALSE value to indicated if axis should be scaled to 0,1 before plotting.
##' @param groups.names user supplied name for groups variable, should be character string.
##' @param labels user supplied group names vector of character strings with length same as nlevels(groups)
##' @param x.axis user supplied x axis labels
##' @param y.axis user supplied y axis labels
##' @param size user supplied with of CI's
##' @return returns a ggolot2 object of LDA projections, biplot, and various CI's
##' @author James Colee
##' @export
##'
##' @examples
##' # basic plot using Fisher's Iris data
##' library(MASS)
##' library(ggplot2)
##' iris.lda <- lda(Species ~., data = iris)
##' p<-lda_gplot(lda=iris.lda, groups = iris$Species)
##' p
##'
##' #resulting output can be modified using ggplot2
##' # adding a title
##' p + ggtitle("Fisher's Iris data")
##'
##' #no legend
##' p + theme(legend.position='none')
##'
##' # no legend title
##' p + theme(legend.title = element_blank())
##'
##' # add a theme
##' p+ theme_bw()
##'
##'
##' # simulated data set to give random groups
##' q=100
##' data <- as.data.frame(list(x1 = runif(q), x2 = rnorm(q), x3 = rlnorm(q), group = sample(c('s','d','w','t'),q,replace=TRUE)))
##' lda <- lda(group ~., data = data)
##'
##' # The function in anosim in the vegan package can be used to test multivariate differences in groups
##' library(vegan)
##' anosim(data[,-ncol(data)],data$group)
##'
##' # asymtopic CI's
##' p <- lda_gplot(lda=lda, groups = data$group, normal.CI = FALSE, circles = TRUE, fill = FALSE)
##' p + ggtitle("95% asymtopic CI's")
##'
##' # bootstrap CI's
##' p <- lda_gplot(lda=lda, groups = data$group, normal.CI = FALSE, boot.CI = TRUE, fill = FALSE)
##' p + ggtitle("95% bootstrap CI's")
##'
##' # permutation CI's
##' p <- lda_gplot(lda=lda, groups = data$group, normal.CI = FALSE, perm.CI = TRUE, fill = FALSE)
##' p + ggtitle("95% permutation CI's")



lda_gplot <- function(lda, groups, alpha = 0.05, N.boot = 101, N.perm = 101, fill = TRUE, means = TRUE, biplot = TRUE, normal.CI = TRUE, circles = FALSE,
    boot.CI = FALSE, perm.CI = FALSE, scale = FALSE, groups.names = NULL, labels = NULL, x.axis = NULL, y.axis = NULL, size = 0.1) {
    requireNamespace("ggplot2")
    requireNamespace("ggforce")
    # check null
    if (is.null(groups.names)) {
        groups.names <- c("groups")
    }
    if (is.null(labels)) {
        labels <- as.factor(lda$lev)
    }
    if (is.null(x.axis)) {
        x.axis <- paste("LDA1 ", round(100 * lda$svd[1]^2/sum(lda$svd^2), 1), "%")
    }
    if (is.null(y.axis)) {
        y.axis <- paste("LDA2 ", round(100 * lda$svd[2]^2/sum(lda$svd^2), 1), "%")
    }

    V1 <- as.numeric(as.factor(groups))
    lda.vec <- as.data.frame(lda$scaling)
    lda.p <- predict(lda)
    lda.values <- as.data.frame(cbind(V1, lda.p$x))
    if (scale) {
        lda.values <- as.data.frame(cbind(V1, scale(lda.p$x)))
    }
    g <- length(lda$lev)
    mean <- aggregate(. ~ V1, lda.values, FUN = "mean")
    n <- lda$counts
    N <- lda$N


    # base plot
    p <- ggplot2::ggplot(lda.values) + ggplot2::geom_point(ggplot2::aes(x = LD1, y = LD2, colour = as.factor(V1)), size = 1) + ggplot2::labs(color = groups.names, x = x.axis, y = y.axis) +
      ggplot2::scale_color_hue(labels = labels)

    # add 95% ci for points
    if (normal.CI) {
        p <- p + ggplot2::stat_ellipse(ggplot2::aes(x = LD1, y = LD2, colour = as.factor(V1)), type = "norm", linetype = 1, level = 1 - alpha, size = size, show.legend = FALSE)
        if (fill) {
            p <- p + ggplot2::stat_ellipse(geom = "polygon", alpha = 0.25, ggplot2::aes(x = LD1, y = LD2, colour = as.factor(V1), fill = as.factor(V1)), type = "norm",
                linetype = 1, level = 1 - alpha, show.legend = FALSE)
        }
    }

    # add mean points
    if (means) {
        p <- p + ggplot2::geom_point(data = mean, ggplot2::aes(x = LD1, y = LD2, colour = as.factor(V1)), shape = 3, size = 3, show.legend = FALSE)
    }

    # add circles
    if (circles) {
        p <- p + ggforce::geom_circle(ggplot2::aes(x0 = LD1, y0 = LD2, colour = as.factor(V1), r = sqrt(qchisq(1 - alpha/2, 2)/n)), data = mean, size = size, show.legend = FALSE)
    }

    # add biplot
    if (biplot) {
        p <- p + ggplot2::geom_segment(data = lda.vec, ggplot2::aes(x = 0, y = 0, xend = LD1, yend = LD2), arrow = ggplot2::arrow(length = ggplot2::unit(1/2, "picas")), size = size) +
            ggplot2::geom_text(data = lda.vec, ggplot2::aes(x = LD1, y = LD2, label = rownames(lda.vec)), hjust = 0, vjust = 0, size = 5)
    }
    # boot strap CI
    if (boot.CI) {
        boot <- boot_sample(values = lda.values, N.boot)
        p <- p + ggplot2::stat_ellipse(data = boot, ggplot2::aes(x = V2, y = V3, colour = as.factor(V1)), type = "norm", linetype = 1, level = 1 - alpha, show.legend = FALSE)
        if (fill) {
            p <- p + ggplot2::stat_ellipse(data = boot, geom = "polygon", alpha = 0.25, ggplot2::aes(x = V2, y = V3, colour = as.factor(V1), fill = as.factor(V1)),
                type = "norm", linetype = 1, level = 1 - alpha, show.legend = FALSE)
        }
    }
    # permutation CI
    if (perm.CI) {
        perm <- perm_sample(values = lda.values, N.perm)
        p <- p + ggplot2::stat_ellipse(data = perm, ggplot2::aes(x = V2, y = V3, colour = as.factor(V1)), type = "norm", linetype = 1, level = 1 - alpha, show.legend = FALSE)
        if (fill) {
            p <- p + ggplot2::stat_ellipse(data = perm, geom = "polygon", alpha = 0.25, ggplot2::aes(x = V2, y = V3, colour = as.factor(V1), fill = as.factor(V1)),
                type = "norm", linetype = 1, level = 1 - alpha, show.legend = FALSE)
        }
    }
    return(p)
}

