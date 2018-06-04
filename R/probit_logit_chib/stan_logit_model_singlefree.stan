data{
	int<lower=0> N;
	int<lower=0> k;
	
	int<lower=0, upper=1> y[N];
	matrix[N, k] X;
	
	real mu_beta;
	real<lower=0> sd_beta;
}
parameters{
	real beta;
}
transformed parameters{
	vector[N] z;
	
	z = to_vector(X * beta);
}
model{
	beta ~ normal(mu_beta, sd_beta);
	
	y ~ bernoulli(inv_logit(z));
}
