data {
    int<lower=1> I;
    vector[I] y;
    vector<lower=0>[I] se;
}
parameters {
    real mu;
    vector[I] mu_i_raw;
    real<lower=0> sigma;
}
transformed parameters {
    vector[I] mu_i;
    // i.e., mu_i ~ normal(mu, sigma)
    mu_i = mu + (sigma * mu_i_raw);
}
model {
    target += normal_lpdf(mu | 0, 100);
    target += cauchy_lpdf(sigma | 0, 25);
    target += normal_lpdf(mu_i_raw | 0, 1);
    target += normal_lpdf(y | mu_i, se);
}
generated quantities {
    vector[I] yhat;
    real yhat_mean;
    real<lower=0> yhat_sd;
    real yhat_min;
    real yhat_max;
    for (i in 1:I) {
        yhat[i] = normal_rng(mu_i[i], se[i]);
    }
    yhat_mean = mean(yhat);
    yhat_sd = sd(yhat);
    yhat_min = min(yhat);
    yhat_max = max(yhat);
}

