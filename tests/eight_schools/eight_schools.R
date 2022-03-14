#! /usr/bin/env Rscript

##############
## Preamble ##
##############

options(warn=1)
library(rstan)
myseed <- 20220304
set.seed(myseed)


##################
## Prepare data ##
##################

## Taken from Table 1 of Rubin 1981
ds <- data.frame(list(school=c("A","B","C","D","E","F","G","H"),
                      id=1:8,
                      nt=c(28, 39, 22, 48, 25, 37, 24, 16),
                      nc=c(22, 40, 17, 43, 74, 35, 70, 19),
                      mean=c(28.39, 7.94, -2.75, 6.82, -0.64, 0.63, 18.01, 12.16),
                      se=c(14.9, 10.2, 16.3, 11.0, 9.4, 11.4, 10.4, 17.6),
                      resid_var=c(2415, 1880, 2168, 2612, 1623, 2046, 1841, 2314)),
                 stringsAsFactors=FALSE)

## Adding some variables that will be useful later
ds <- within(ds, {
    ucl <- mean + 1.96*se
    lcl <- mean - 1.96*se
    ref <- paste0("mu_i[", id, "]")
})


######################################################
## Pooled estimate of treatment effect: Frequentist ##
######################################################

## sources: https://en.wikipedia.org/wiki/Inverse-variance_weighting
##          and Rubin 1981

## Mean: Sum, weighted by inverse of variance
mu <- ds[["mean"]]
vinv <- 1/(ds[["se"]])^2
w <- vinv/sum(vinv)
mean_pooled <- sum(mu*w)

## Standard error of this estimate:
se_mean_pooled <- sqrt(1/sum(vinv))

## 95% CI
mean_pooled
c(mean_pooled - 1.96*se_mean_pooled, mean_pooled + 1.96*se_mean_pooled)

## If we believe the pooled mean is the best estimate for all eight schools,
## then the school specific standard error is
se_pooled <- sqrt(mean(ds[["se"]]^2))


###################################################
## Pooled estimate of treatment effect: Bayesian ##
###################################################

input <- list(I=nrow(ds), y=ds[["mean"]], se=ds[["se"]])
par.lst <- c("mu","yhat_mean","yhat_sd","yhat_min","yhat_max")
model_pool <- stan_model(file="model_pooled.stan")
fit_pool <- sampling(object=model_pool, data=input, seed=myseed, pars=par.lst,
                     chains=5, iter=2000, warmup=1000, thin=1)
print(fit_pool)
#model.mcmc <- as.data.frame(extract(fit_pool, pars=par.lst))
#rm(par.lst, model.mcmc)
rm(par.lst)


####################################################
## Partially pooled estimates of treatment effect ##
####################################################

par.lst <- c("mu_i","mu","sigma","yhat_mean","yhat_sd","yhat_min","yhat_max")
model_part_pool <- stan_model(file="model_partial_pool.stan")
fit_part_pool <- sampling(object=model_part_pool, data=input, seed=myseed, pars=par.lst,
                          chains=5, iter=2000, warmup=1000, thin=1,
                          control=list(adapt_delta=0.85, max_treedepth=10))
print(fit_part_pool)
summary.mcmc <- as.data.frame(summary(fit_part_pool, pars=c("mu_i"))[["summary"]])
summary.mcmc <- summary.mcmc[c("mean", "2.5%", "97.5%")]
names(summary.mcmc) <- paste("post", names(summary.mcmc), sep="_")
summary.mcmc[["ref"]] <- row.names(summary.mcmc)
#model.mcmc <- as.data.frame(extract(fit_part_pool, pars=par.lst))


###############################################
## Summarise all the analyses into one graph ##
###############################################

## Construct data table
new_ds <- ds[c("ref","mean","se","lcl","ucl")]
new_row <- data.frame(list(ref="Pooled", mean=mean_pooled, se=se_mean_pooled,
                           lcl=mean_pooled - 1.96*se_mean_pooled, ucl=mean_pooled + 1.96*se_mean_pooled),
                      stringsAsFactors=FALSE)
new_ds <- rbind(new_ds, new_row)


## merge data for plotting etc.
new_ds <- merge(x=new_ds, y=summary.mcmc, by="ref", all.x=TRUE) ## Left join


## Plot is all

setEPS()
postscript("eight_schools_forest_plot.eps", family="serif", width=9, height=4.5)

par(mfrow=c(1,2))

## Plot original estimates with 95% CI
## along with the pooled point and interval estimate
plot(NA, NA, xlim=c(-40, 60), ylim=rev(c(1, nrow(new_ds))), yaxt="n",
     ylab="", xlab="Treatment effect", main="Frequentist estimators")
points(x=new_ds[["mean"]], y=1:nrow(new_ds), pch=19)
segments(x0=new_ds[["lcl"]], x1=new_ds[["ucl"]], y0=1:nrow(new_ds), y1=1:nrow(new_ds), lwd=2)
abline(v=new_row[["mean"]], lty=2, col=1)
axis(2, at=1:nrow(new_ds), labels=new_ds[["ref"]], las=2)

## Plot point and interval estimates from partial pooling
## along with grey triangles to represent the pre-pooling means
plot(NA, NA, xlim=c(-40, 60), ylim=rev(c(1, nrow(new_ds))), yaxt="n",
     ylab="", xlab="Treatment effect", main="Bayesian shrinkage estimators\n(partial pooling)")
points(x=new_ds[["mean"]], y=1:nrow(new_ds), pch=17, col="darkgrey")
points(x=new_ds[["post_mean"]], y=1:nrow(new_ds), pch=19, col="blue")
segments(x0=new_ds[["post_2.5%"]], x1=new_ds[["post_97.5%"]], y0=1:nrow(new_ds), y1=1:nrow(new_ds), lwd=2, col="blue")
abline(v=new_row[["mean"]], lty=2, col=1)

dev.off()

