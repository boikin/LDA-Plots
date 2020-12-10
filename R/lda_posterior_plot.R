##' lda_posterior_plot() creates a plot of the posterior probabilities of each predicted group membership based a lda model.
##' @title Function to make LDA posterior probabilities by group
##' @param lda the output from lda() in the package MASS it is an lda class object.
##' @param groups A vector of the original groups from the data used in lda(), the Y variable in the lda model.
##' @param scale TRUE/FALSE value to indicated if axis should be scaled to 0,1 before plotting.
##' @param groups.names user supplied name for groups variable, should be character string.
##' @param labels user supplied group names vector of character strings with length same as nlevels(groups).
##' @param title user supplied text vector to change the title segment before group ID.
##' @param x.axis user supplied x axis labels.
##' @param y.axis user supplied y axis labels.
##' @return returns a plots of LDA posterior probabilities for each group.
##' @author James Colee
##' @export
##'
##' @examples
##' # posterior probablities plot using Fisher's Iris data
##' library(MASS)
##' iris.lda <- lda(Species ~., data = iris)
##' # three plots are produced for each species
##' # white indicates higher posterior probabilities
##' # black indicates lower posterior probabilities
##' lda_posterior_plot(lda=iris.lda, groups = iris$Species)

lda_posterior_plot <- function(lda, groups, scale = FALSE, groups.names = NULL,title=NULL, labels = NULL, x.axis = NULL, y.axis = NULL) {
    requireNamespace("ggplot2")
    requireNamespace("reshape2")
    requireNamespace("stringr")
    requireNamespace("boot")
    if (is.null(groups.names)) {
        groups.names <- c("groups")
    }

    if (is.null(title)) {
        title <- paste("Posterior Probabilities for")
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

    lda.p <- predict(lda)
    V1 <- as.numeric(as.factor(groups))
    lda.values <- as.data.frame(cbind(V1, lda.p$x[,1:2]))
    if (scale) {
        lda.values <- as.data.frame(cbind(V1, scale(lda.p$x[,1:2])))
    }

    for (i in seq_along(lda$lev)) {
        lda.post <- predict(lda)$posterior
        lda.post <- data.frame(lda.values, lda.post[, i])
        colnames(lda.post)[4] <- "post"
        xgrid <- seq(min(lda.post$LD1), max(lda.post$LD1), (max(lda.post$LD1) - min(lda.post$LD1))/50)
        ygrid <- seq(min(lda.post$LD2), max(lda.post$LD2), (max(lda.post$LD2) - min(lda.post$LD2))/50)
        grid <- expand.grid(LD1 = xgrid, LD2 = ygrid)
        data.loess <- loess(boot::logit((post + 0.001)/1.002) ~ LD1 * LD2 + LD1:LD1 + LD2:LD2, data = lda.post)
        lda.3d <- predict(data.loess, newdata = grid)
        lda.3d.melt <- reshape2::melt(lda.3d, id.vars = c("LD1", "LD2"), measure.vars = "post")
        lda.3d.melt$LD1 <- as.numeric(stringr::str_sub(lda.3d.melt$LD1, stringr::str_locate(lda.3d.melt$LD1, "=")[1, 1] + 1))
        lda.3d.melt$LD2 <- as.numeric(stringr::str_sub(lda.3d.melt$LD2, stringr::str_locate(lda.3d.melt$LD2, "=")[1, 1] + 1))
        names(lda.3d.melt) <- c("LD1", "LD2", "post")
        lda.3d.melt$post <- boot::inv.logit(lda.3d.melt$post)

        c <- ggplot2::ggplot(lda.3d.melt, ggplot2::aes(x = LD1, y = LD2, z = post)) +
          ggplot2::stat_contour(geom = "polygon") +
          ggplot2::geom_tile(ggplot2::aes(fill = post)) +
          ggplot2::scale_fill_gradient(low = "black", high = "white", name = "posterior") +
          ggplot2::geom_point(data = lda.post, ggplot2::aes(x = LD1, y = LD2, colour = as.factor(V1)), size = 2.5) +
          ggplot2::labs(title = paste(title, lda$lev[i]), color = groups.names, x = x.axis, y = y.axis) +
          ggplot2::scale_color_hue(labels = labels)

        print(c)
    }
}



