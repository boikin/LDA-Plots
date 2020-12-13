##' Bootstrap resampling used for mean CI estimation
##' @title Function to create bootstrap samples of LDA projected means
##' @param values A matrix of group ID's and the first 2 lda projections of the data.
##' @param N.b The number of bootstrap sample means to be estimated.
##' @return returns matrix of lda projected means dim n.b*number of groups by 3, columns group, LDA1, and LDA2.
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
##' # create bootstrap sample means
##' b <- boot_sample(values = v)
##' str(b)

boot_sample <- function(values,N.b = 100)
{
  G <- max(values$V1)
  boot.s <- as.data.frame(array(NA,c(N.b*G,3)))
  boot2 <- function(x,n.bb)
  {
    p <- ncol(x)
    n <- nrow(x)
    b_sample <- matrix(NA, n.bb, p)
    for (i in 1:n.bb) {
      ind <- sample(1:n, replace=TRUE)
      b_sample[i,]<-colMeans(x[ind,])

    }
    return(b_sample)
  }
  for(i in 1:G)
  {
    boot.s[N.b*(i-1)+(1:N.b),2:3] <- boot2(values[values$V1==i,2:3],N.b)
    boot.s[N.b*(i-1)+(1:N.b),1] <- i
  }
  return(boot.s)
}

