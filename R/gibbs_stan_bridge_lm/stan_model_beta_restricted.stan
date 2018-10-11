data{
	int<lower=0> N;
	int<lower=0> k;
	
	int<lower=0> num_free_params;
	int<lower=0> num_restricted_params;
	
	vector[N] y;
	matrix[N, num_free_params] X;
	matrix[N, num_restricted_params] X_restricted;
	
	vector[num_restricted_params] beta_restricted;
	
	vector[num_free_params] mu_beta;
	vector<lower=0>[num_free_params] sd_beta;
	
	real<lower=0> sigma_sq_restricted;
}
parameters{
	vector[num_free_params] beta;
}
model{
	beta ~ normal(mu_beta, sd_beta);
	
	y ~ normal(X * beta + X_restricted * beta_restricted, sqrt(sigma_sq_restricted));
}
