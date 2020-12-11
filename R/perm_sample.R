##' Permutation resampling used for mean CI estimation
##' @title Function to create permutation estimated samples of LDA projected means
##' @param values A matrix of group ID's and the first 2 lda projections of the data
##' @param N.p The number of permutation sample means to be estimated
##' @return returns matrix of lda projected means dim n.b*number of groups by 3, columns group, LDA1, and LDA2
##' @author James Colee
##' @export
##'
##' @examples
##' library(MASS)
##' # simulated data set to give random groups
##' q=100
##' data <- as.data.frame(list(x1 = runif(q), x2 = rnorm(q), x3 = rlnorm(q), group = sample(c('s','d','w'),q,replace=TRUE)))
##'
##' # create lda projections, though not needed function will work with any bivariate data and corresponding grouping column
##' lda <- lda(group ~., data = data)
##'
##' # format data, groups need to be numeric, can not be factors of characters
##' V1<-as.numeric(as.factor(data$group))
##' lda.vec<-as.data.frame(lda$scaling)
##' lda.p <- predict(lda)
##' v <- as.data.frame(cbind(V1, lda.p$x))
##' str(v)
##'
##' # create permutation sample means
##' b<-perm_sample(values = v)
##' str(b)

perm_sample <- function(values, N.p = 100) {
    G <- max(values$V1)
    mean <- aggregate(. ~ V1, values, FUN = "mean")
    perm.s <- as.data.frame(array(NA, c(N.p * G, 3)))
    j = 1
    for (i in 1:G) {
        for (b in 1:N.p) {
            bdata <- values
            bdata$V1 <- sample(values$V1)
            perm.s[j, 2:3] <- mean[i, 2:3] + aggregate(. ~ V1, bdata, FUN = "mean")[i, 2:3]
            perm.s[j, 1] <- i
            j = j + 1
        }
    }
    return(perm.s)
}


