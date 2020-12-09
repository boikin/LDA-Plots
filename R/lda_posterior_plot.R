##' discribtion
##' @title Function to make LDA posterior probabilities by group
##' @param LDA
##' @param groups
##' @param group.names
##' @param labels
##' @param x.axis
##' @param y.axis
##' @return returns multiple plots of LDA posterior probabilities by groups
##' @author James Colee
##' @export
##' 
##' @examples
##' #posterior probability contour plot by goup
##' #examples
##' library(MASS)
##' q=100
##' data <- as.data.frame(list(x1 = runif(q), x2 = rnorm(q), x3 = rlnorm(q), group = sample(c('s','d','w'),q,replace=TRUE)))
##' lda <- lda(group ~., data = data) 
##' lda_posterior_plot(lda=lda, groups = data$group, scale = TRUE)

lda_posterior_plot <- function(lda, groups, scale = FALSE, groups.names = NULL, labels = NULL, x.axis = NULL, y.axis = NULL) {
    require("ggplot2")
    require("reshape2")
    require("stringr")
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
    lda.p <- predict(lda)
    V1 <- as.numeric(as.factor(groups))
    lda.values <- as.data.frame(cbind(V1, lda.p$x))
    if (scale) {
        lda.values <- as.data.frame(cbind(V1, scale(lda.p$x)))
    }
    
    for (i in seq_along(lda$lev)) {
        lda.post <- predict(lda)$posterior
        lda.post <- data.frame(lda.values, lda.post[, i])
        colnames(lda.post)[4] <- "post"
        xgrid <- seq(min(lda.post$LD1), max(lda.post$LD1), (max(lda.post$LD1) - min(lda.post$LD1))/50)
        ygrid <- seq(min(lda.post$LD2), max(lda.post$LD2), (max(lda.post$LD2) - min(lda.post$LD2))/50)
        grid <- expand.grid(LD1 = xgrid, LD2 = ygrid)
        data.loess <- loess(logit((post + 0.001)/1.002) ~ LD1 * LD2 + LD1:LD1 + LD2:LD2, data = lda.post)
        lda.3d <- predict(data.loess, newdata = grid)
        lda.3d.melt <- melt(lda.3d, id.vars = c("LD1", "LD2"), measure.vars = "post")
        lda.3d.melt$LD1 <- as.numeric(str_sub(lda.3d.melt$LD1, str_locate(lda.3d.melt$LD1, "=")[1, 1] + 1))
        lda.3d.melt$LD2 <- as.numeric(str_sub(lda.3d.melt$LD2, str_locate(lda.3d.melt$LD2, "=")[1, 1] + 1))
        names(lda.3d.melt) <- c("LD1", "LD2", "post")
        lda.3d.melt$post <- inv.logit(lda.3d.melt$post)
        
        c <- ggplot(lda.3d.melt, aes(x = LD1, y = LD2, z = post)) + stat_contour(geom = "polygon") + geom_tile(aes(fill = post)) + scale_fill_gradient(low = "black", 
            high = "white", name = "posterior") + geom_point(data = lda.post, aes(x = LD1, y = LD2, colour = as.factor(V1)), size = 2.5) + labs(title = paste("posterior for group", 
            lda$lev[i]), color = groups.names, x = x.axis, y = y.axis) + scale_color_hue(labels = labels)
        
        print(c)
    }
}



