data{
	int<lower=0> N;
	int<lower=0> k;
	
	vector[N] y;
	matrix[N, k] X;
	
	vector[k] mu_beta;
	vector<lower=0>[k] sd_beta;
	
	real<lower=0> tau_shape;
	real<lower=0> tau_rate;
}
parameters{
	vector[k] beta;
	real<lower=0> sigma_sq;
}
model{
	beta ~ normal(mu_beta, sd_beta);
	sigma_sq ~ inv_gamma(tau_shape, tau_rate);
	
	y ~ normal(X * beta, sqrt(sigma_sq));
}
