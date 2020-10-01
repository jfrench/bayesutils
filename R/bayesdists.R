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
