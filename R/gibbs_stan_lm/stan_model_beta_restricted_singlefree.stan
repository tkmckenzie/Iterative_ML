data{
	int<lower=0> N;
	int<lower=0> k;
	
	int<lower=0> num_free_params;
	int<lower=0> num_restricted_params;
	
	vector[N] y;
	matrix[N, 1] X;
	matrix[N, num_restricted_params] X_restricted;
	
	vector[num_restricted_params] beta_restricted;
	
	vector[1] mu_beta;
	vector<lower=0>[1] sd_beta;
	
	real<lower=0> sigma_sq_restricted;
}
parameters{
	real beta;
}
model{
	beta ~ normal(mu_beta, sd_beta);
	
	y ~ normal(to_vector(X * beta) + X_restricted * beta_restricted, sqrt(sigma_sq_restricted));
}
