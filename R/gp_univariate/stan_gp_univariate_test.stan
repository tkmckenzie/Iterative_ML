functions{
	vector gp_pred_rng(real[] x_pred,
		vector y_is,
		real[] x_is,
		real alpha,
		real length_scale,
		real sigma
	)
	{
		vector[size(x_pred)] f_pred;
		int N_pred;
		int N;
		N_pred = size(x_pred);
		N = rows(y_is);
		
		{
			matrix[N, N] L_Sigma;
			vector[N] K_div_y_is;
			matrix[N, N_pred] k_x_is_x_pred;
			matrix[N, N_pred] v_pred;
			vector[N_pred] f_pred_mu;
			matrix[N_pred, N_pred] cov_f_pred;
			matrix[N_pred, N_pred] nug_pred;
			matrix[N, N] Sigma;
			
			Sigma = cov_exp_quad(x_is, alpha, length_scale);
			for (n in 1:N)
				Sigma[n, n] = Sigma[n, n] + square(sigma);
			L_Sigma = cholesky_decompose(Sigma);
			K_div_y_is = mdivide_left_tri_low(L_Sigma, y_is);
			K_div_y_is = mdivide_right_tri_low(K_div_y_is', L_Sigma)';
			k_x_is_x_pred = cov_exp_quad(x_is, x_pred, alpha, length_scale);
			f_pred_mu = (k_x_is_x_pred' * K_div_y_is);
			v_pred = mdivide_left_tri_low(L_Sigma, k_x_is_x_pred);
			cov_f_pred = cov_exp_quad(x_pred, alpha, length_scale) - v_pred' * v_pred;
			nug_pred = diag_matrix(rep_vector(1e-12, N_pred));
			
			f_pred = multi_normal_rng(f_pred_mu, cov_f_pred + nug_pred);
		}
		
		return f_pred;
	}
}
data{
	int N;
	
	vector[N] y;
	real x[N];
}
parameters{
	real<lower=0> length_scale;
	real<lower=0> alpha;
	real<lower=0> sigma;
	vector[N] eta;
}
transformed parameters{
	vector[N] f;
	{
		matrix[N, N] L;
		matrix[N, N] K;
		
		K = cov_exp_quad(x, alpha, length_scale);
		for (n in 1:N)
			K[n, n] = K[n, n] + 1e-12;
		L = cholesky_decompose(K);
		f = L * eta;
	}
}
model{
	length_scale ~ gamma(1, 0.1);
	alpha ~ normal(0, 1);
	sigma ~ normal(0, 1);
	eta ~ normal(0, 1);
	y ~ normal(f, sigma);
}
generated quantities{
	vector[N] f_pred;
	vector[N] y_pred;
	
	f_pred = gp_pred_rng(x, y, x, alpha, length_scale, sigma);
	for (n in 1:N)
		y_pred[n] = normal_rng(f_pred[n], sigma);
}
