# #functions for inverse gamma distribution
#
# rinvgamma <- function(n, shape, scale = 1, rate = 1/scale)
# {
#   if (!missing(rate) && !missing(scale)) {
#     if (abs(rate * scale - 1) < 1e-15)
#       warning("specify 'rate' or 'scale' but not both")
#     else stop("specify 'rate' or 'scale' but not both")
#   }
# 	1/rgamma(n, shape = shape, scale = 1/scale)
# }
# dinvgamma <- function(x, shape, scale = 1, rate = 1/scale)
# {
#   if (!missing(rate) && !missing(scale)) {
#     if (abs(rate * scale - 1) < 1e-15)
#       warning("specify 'rate' or 'scale' but not both")
#     else stop("specify 'rate' or 'scale' but not both")
#   }
# 	dgamma(1/x, shape = shape, scale = 1/scale)/x^2
# }
# pinvgamma <- function(q, shape, scale = 1, rate = 1/scale, lower.tail = TRUE, log.p = FALSE)
# {
#   if (!missing(rate) && !missing(scale)) {
#     if (abs(rate * scale - 1) < 1e-15)
#       warning("specify 'rate' or 'scale' but not both")
#     else stop("specify 'rate' or 'scale' but not both")
#   }
# 	1 - pgamma(1/q, shape = shape, scale = 1/scale, lower.tail = lower.tail, log.p = log.p)
# }
# qinvgamma <- function(p, shape, scale = 1, rate = 1/scale, lower.tail = TRUE, log.p = FALSE)
# {
#   if (!missing(rate) && !missing(scale)) {
#     if (abs(rate * scale - 1) < 1e-15)
#       warning("specify 'rate' or 'scale' but not both")
#     else stop("specify 'rate' or 'scale' but not both")
#   }
# 	1/qgamma(1 - p, shape = shape, scale = 1/scale, lower.tail = lower.tail, log.p = log.p)
# }
#
# #functions for inverse chisquare distribution
#
# rinvchisq <- function(n, df, scale)
# {
# 	rinvgamma(n, shape = df/2, scale = df/2*scale^2)
# }
# dinvchisq <- function(x, df, scale)
# {
# 	dinvgamma(x, shape = df/2, scale = df/2*scale^2)
# }
# pinvchisq <- function(q, df, scale)
# {
# 	pinvgamma(q, shape = df/2, scale = df/2*scale^2)
# }
# qinvchisq <- function(p, df, scale)
# {
# 	qinvgamma(p, shape = df/2, scale = df/2*scale^2)
# }
#
# #functions for scaled and shifted t distribution
#
# dst <- function(x, df, mean = 1, sd = 1)
# {
# 	dt((x - mean)/sd, df = df)/sd
# }
#
# rst <- function(n, df, mean = 1, sd = 1)
# {
# 	mean + sd * rnorm(n) * sqrt(df/rchisq(n, df = df))
# }
#
# qst <- function(p, df, mean = 0, sd = 1)
# {
#   mean + sd * qt(p, df = df)
# }
#
# pst <- function(p, df, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
# {
#   pt((p - mean)/sd, df = df, lower.tail = lower.tail, log.p = log.p)
# }
#
# #functions for scaled and shifted t distribution
#
# dlst <- function(x, df, mean = 1, sd = 1)
# {
#   dt((x - mean)/sd, df = df)/sd
# }
#
# rlst <- function(n, df, mean = 1, sd = 1)
# {
#   mean + sd * rnorm(n) * sqrt(df/rchisq(n, df = df))
# }
#
# qlst <- function(p, df, mean = 0, sd = 1)
# {
#   mean + sd * qt(p, df = df)
# }
#
# plst <- function(p, df, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
# {
#   pt((p - mean)/sd, df = df, lower.tail = lower.tail, log.p = log.p)
# }
#
#
#
# rinvwish <- function(n, df, Sigma)
# {
#   x = rWishart(n, df, solve(Sigma))
#   xl = lapply(seq(dim(x)[3]), function(y) x[ , , y])
#   g = lapply(xl, solve)
#   g2 = array(unlist(g), dim = c(dim(g[[1]]), length(g)))
#   return(g2)
# }
#
# decomp_cov <- function(V, method = "eigen")
# {
#   #sanity check
#   if(!(is.matrix(V)  || inherits(V, "Matrix")))
#   {
#     stop("V should be a matrix or Matrix")
#   }
#   if(nrow(V)!=ncol(V))
#   { stop("V must be a square numeric matrix")}
#   if(!is.element(method, c("chol", "eigen", "svd")))
#   { stop("method must be 'chol', 'eigen', or 'svd'")}
#
#   if(method == "eigen")
#   {
#     eigenV <- eigen(V)
#     return(eigenV$vectors %*% diag(sqrt(pmax(eigenV$values,0))))
#   }else if(method == "chol")
#   {
#     return(t(chol(V)))
#   }else if(method == "svd")
#   {
#     svdV <- svd(V)
#     return(tcrossprod(svdV$u %*% diag(sqrt(svdV$d)), svdV$v))
#   }
# }
#
# # mu is the mean of the multivariate normal
# # should be a vector of length d
# # V is the covariance matrix of the multivariate normal, of size dxd
# rmvnorm = function(nsim = 1, mu, V, method = "eigen")
# {
#   mu = as.vector(mu)
#   n = length(mu)
#
#   # check argument validity
#   if(nsim < 0 || !is.finite(nsim))
#   { stop("nsim should be a non-negative integer")}
#   if(!is.numeric(mu))
#   { stop("mu must be a numeric vector")}
#   if(!is.matrix(V) || nrow(V)!=ncol(V) || !is.numeric(V))
#   { stop("V must be a square numeric matrix")}
#   if(n != nrow(V))
#   { stop("length(mu)!=nrow(V)")}
#   if(!is.element(method, c("chol", "eigen", "svd")))
#   { stop("method must be 'chol', 'eigen', or 'svd'")}
#
#   # return simulated values
#   mu + decomp_cov(V, method)%*%matrix(rnorm(n*nsim), nrow = n, ncol = nsim)
# }
#
# #create a function to draw path from two-dimensional gibbs sample
# #takes x0, a vector of length 2 containing the starting point,
# #x, an nx2 matrix containing the posterior simulations,
# #add, whether this path should be added to a current plot
# #..., additional parameter to pass to the plotting functions
#
# plot.mcmc.path <- function(x, add = FALSE, x0 = NULL, ...)
# {
#   if(!is.null(x0))
#   {
#     x <- rbind(x0, x)
#   }
#   if(!add)
#   {
#     plot(x, type = "n", ...)
#   }
#   points(x[1, 1], x[1, 2], ...)
#   for(i in 1:(nrow(x) - 1))
#   {
#     lines(c(x[i, 1], x[i+1, 1]), c(x[i, 2], x[i, 2]),  ...)
#     lines(c(x[i+1, 1], x[i+1, 1]), c(x[i, 2], x[i + 1, 2]),  ...)
#   }
# }
#
# # convert fitted stan models to coda list
# stan2coda <- function(fit) {
#   mcmc.list(lapply(1:ncol(fit), function(x) mcmc(as.array(fit)[,x,])))
# }
#
# # takes stan model or stan samples
# # assumes that we have a parameter log_lik
# # contained in the output that is the
# # log likelihood of samples of theta from the posterior
# # distribution
# # Little function to calculate posterior variances from simulation
# colVars <- function (a){
#   diff <- a - matrix (colMeans(a), nrow(a), ncol(a), byrow=TRUE)
#   vars <- colMeans (diff^2)*nrow(a)/(nrow(a)-1)
#   return (vars)
# }
#
# # The calculation of Waic!  Returns lppd, p_waic_1, p_waic_2, and waic, which we define
# # as 2*(lppd - p_waic_2), as recommmended in BDA
# waic <- function (stanfit)
# {
#   log_lik <- extract (stanfit, "log_lik")$log_lik
#   lppd <- sum (log (colMeans(exp(log_lik))))
#   p_waic_1 <- 2*sum (log(colMeans(exp(log_lik))) - colMeans(log_lik))
#   p_waic_2 <- sum (colVars(log_lik))
#   waic_2 <- -2*lppd + 2*p_waic_2
#   return (list (waic=waic_2, p_waic=p_waic_2, lppd=lppd, p_waic_1=p_waic_1))
# }
#
#
#
#
#
#
#