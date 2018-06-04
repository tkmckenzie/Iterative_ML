data{
	int<lower=1> N;
	vector[N] y;
	real x[N];
}
transformed data{
	vector[N] zeros;
	
	zeros = rep_vector(0, N);
}
parameters{
	real<lower=0> length_scale;
	real<lower=0> alpha;
	real<lower=0> sigma;
}
model{
	matrix[N, N] L_cov;
	{
		matrix[N, N] cov;
		cov = cov_exp_quad(x, alpha, length_scale);
		for (n in 1:N)
			cov[n, n] = cov[n, n] + square(sigma);
		L_cov = cholesky_decompose(cov);
	}
	
	length_scale ~ gamma(2, 0.1);
	alpha ~ normal(0, 1);
	sigma ~ normal(0, 1);
	
	y ~ multi_normal_cholesky(zeros, L_cov);
}
