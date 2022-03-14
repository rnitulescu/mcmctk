data {
    int<lower=1> I;
    vector[I] y;
    vector<lower=0>[I] se;
}
parameters {
    real mu;
}
model {
    target += normal_lpdf(y | mu, se);
}
generated quantities {
    vector[I] yhat;
    real yhat_mean;
    real<lower=0> yhat_sd;
    real yhat_min;
    real yhat_max;
    for (i in 1:I) {
        yhat[i] = normal_rng(mu, se[i]);
    }
    yhat_mean = mean(yhat);
    yhat_sd = sd(yhat);
    yhat_min = min(yhat);
    yhat_max = max(yhat);
}

