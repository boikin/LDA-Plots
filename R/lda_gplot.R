##' discribtion
##' @title Function to make LDA plots wich confidence bounds
##' @param LDA
##' @param groups
##' @param alpha
##' @param N.boot
##' @param N.perm
##' @param fill
##' @param means
##' @param biplot
##' @param normal.CI
##' @param circles
##' @param boot.CI
##' @param perm.CI
##' @param scale
##' @param group.names
##' @param labels
##' @param x.axis
##' @param y.axis
##' @param size
##' @return returns a ggolot2 object of the plot LDA projects with CI's
##' @author James Colee
##' @export
##' 
##' @examples
##' library(MASS)
##' q=100
##' data <- as.data.frame(list(x1 = runif(q), x2 = rnorm(q), x3 = rlnorm(q), group = sample(c('s','d','w'),q,replace=TRUE)))
##' lda <- lda(group ~., data = data) 
##' p<-lda_gplot(lda=lda, groups = data$group,perm.CI = TRUE,boot.CI=TRUE, circles = TRUE)
##' p
##' #no legend
##' p + theme(legend.position='none')
##' no legend title
##' p + theme(legend.title = element_blank()) +  scale_color_hue(labels = labels)
##' # add a theme
##' p+ theme_bw()



lda_gplot <- function(lda, groups, alpha = 0.05, N.boot = 101, N.perm = 101, fill = TRUE, means = TRUE, biplot = TRUE, normal.CI = TRUE, circles = FALSE, 
    boot.CI = FALSE, perm.CI = FALSE, scale = FALSE, groups.names = NULL, labels = NULL, x.axis = NULL, y.axis = NULL, size = 0.1) {
    require("ggplot2")
    require("ggforce")
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
    p <- ggplot(lda.values) + geom_point(aes(x = LD1, y = LD2, colour = as.factor(V1)), size = 1) + labs(color = groups.names, x = x.axis, y = y.axis) + 
        scale_color_hue(labels = labels)
    
    # add 95% ci for points
    if (normal.CI) {
        p <- p + stat_ellipse(aes(x = LD1, y = LD2, colour = as.factor(V1)), type = "norm", linetype = 1, level = 1 - alpha, size = size, show.legend = FALSE)
        if (fill) {
            p <- p + stat_ellipse(geom = "polygon", alpha = 0.25, aes(x = LD1, y = LD2, colour = as.factor(V1), fill = as.factor(V1)), type = "norm", 
                linetype = 1, level = 1 - alpha, show.legend = FALSE)
        }
    }
    
    # add mean points
    if (means) {
        p <- p + geom_point(data = mean, aes(x = LD1, y = LD2, colour = as.factor(V1)), shape = 3, size = 3, show.legend = FALSE)
    }
    
    # add circles
    if (circles) {
        p <- p + geom_circle(aes(x0 = LD1, y0 = LD2, colour = as.factor(V1), r = sqrt(qchisq(1 - alpha/2, 2)/n)), data = mean, size = size, show.legend = FALSE)
    }
    
    # add biplot
    if (biplot) {
        p <- p + geom_segment(data = lda.vec, aes(x = 0, y = 0, xend = LD1, yend = LD2), arrow = arrow(length = unit(1/2, "picas")), size = size) + 
            geom_text(data = lda.vec, aes(x = LD1, y = LD2, label = rownames(lda.vec)), hjust = 0, vjust = 0, size = 5)
    }
    # boot strap CI
    if (boot.CI) {
        boot <- boot_sample(values = lda.values, N.boot)
        p <- p + stat_ellipse(data = boot, aes(x = V2, y = V3, colour = as.factor(V1)), type = "norm", linetype = 1, level = 1 - alpha, show.legend = FALSE)
        if (fill) {
            p <- p + stat_ellipse(data = boot, geom = "polygon", alpha = 0.25, aes(x = V2, y = V3, colour = as.factor(V1), fill = as.factor(V1)), 
                type = "norm", linetype = 1, level = 1 - alpha, show.legend = FALSE)
        }
    }
    # permutation CI
    if (perm.CI) {
        perm <- perm_sample(values = lda.values, N.perm)
        p <- p + stat_ellipse(data = perm, aes(x = V2, y = V3, colour = as.factor(V1)), type = "norm", linetype = 1, level = 1 - alpha, show.legend = FALSE) + 
            theme(legend.position = "none")
        if (fill) {
            p <- p + stat_ellipse(data = perm, geom = "polygon", alpha = 0.25, aes(x = V2, y = V3, colour = as.factor(V1), fill = as.factor(V1)), 
                type = "norm", linetype = 1, level = 1 - alpha, show.legend = FALSE)
        }
    }
    return(p)
}

