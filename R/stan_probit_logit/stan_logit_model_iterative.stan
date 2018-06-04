data{
	int<lower=0> N;
	int<lower=0> num_free_params;
	int<lower=0> num_restricted_params;
	
	int<lower=0, upper=1> y[N];
	matrix[N, num_free_params] X;
	matrix[N, num_restricted_params] X_restricted;
	
	vector[num_restricted_params] beta_restricted;
	
	vector[num_free_params] mu_beta;
	vector<lower=0>[num_free_params] sd_beta;
}
parameters{
	vector[num_free_params] beta;
}
transformed parameters{
	vector[N] z;
	
	z = X * beta + X_restricted * beta_restricted;
}
model{
	beta ~ normal(mu_beta, sd_beta);
	
	y ~ bernoulli(inv_logit(z));
}
