data{
	int N;
	int N_pred;

	real x_pred[N_pred];
	vector[N] y_is;
	real x_is[N];
	real alpha;
	real length_scale;
	real sigma;
}
model{
}
generated quantities{
	matrix[2, N_pred] f_pred;
	
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
	f_pred[1,] = f_pred_mu';
	for (n in 1:N_pred)
		f_pred[2,n] = sqrt(cov_f_pred[n, n] + square(sigma));
}
