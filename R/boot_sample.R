##' discribtion
##' @title Function to create bootstrap samples of LDA projected means
##' @param values
##' @param N.b
##' @return returns matrix of lda projected means dim n.b*number of groups by 3, columns group, LDA1, and LDA2
##' @author James Colee
##' @export
##' 
##' @examples
##' #example
##' library(MASS)
##' q <-100
##' data <- as.data.frame(list(x1 = runif(q), x2 = rnorm(q), x3 = rlnorm(q), group = sample(c('s','d','w'),q,replace=TRUE)))
##' lda <- lda(group ~., data = data) 
##' as.numeric(as.factor(data$group))
##' lda.p <- predict(lda)
##' v <- as.data.frame(cbind(V1, lda.p$x))
##' b <- boot_sample(values = v)
##' str(b)

boot_sample <- function(values, N.b = 100) {
    G <- max(values$V1)
    boot.s <- as.data.frame(array(NA, c(N.b * G, 3)))
    statfun = function(d, i) {
        colMeans(d[i, ])
    }
    for (i in 1:G) {
        boot.s[N.b * (i - 1) + (1:N.b), 2:3] <- boot(values[values$V1 == i, 2:3], statfun, R = N.b)$t
        boot.s[N.b * (i - 1) + (1:N.b), 1] <- i
    }
    return(boot.s)
}

