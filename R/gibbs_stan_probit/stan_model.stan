data{
	int<lower=0> N;
	int<lower=0> k;
	
	int<lower=0, upper=1> y[N];
	matrix[N, k] X;
	
	vector[k] mu_beta;
	vector<lower=0>[k] sd_beta;
}
parameters{
	vector[k] beta;
}
transformed parameters{
	vector[N] z;
	
	z = X * beta;
}
model{
	beta ~ normal(mu_beta, sd_beta);
	
	y ~ bernoulli(Phi(z));
}
