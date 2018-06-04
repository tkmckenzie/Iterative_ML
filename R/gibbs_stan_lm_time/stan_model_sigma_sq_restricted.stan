data{
	int<lower=0> N;
	int<lower=0> k;
	
	vector[N] y;
	matrix[N, k] X;
	
	vector[k] mu_beta;
	vector<lower=0>[k] sd_beta;
	
	real<lower=0> sigma_sq_restricted;
}
parameters{
	vector[k] beta;
}
model{
	beta ~ normal(mu_beta, sd_beta);
	
	y ~ normal(X * beta, sqrt(sigma_sq_restricted));
}
