functions{
	real multi_normal_kernel(row_vector x,
		matrix H_inv,
		real H_inv_sqrt_det
	)
	{
		return H_inv_sqrt_det * exp(-0.5 * x * H_inv * x');
	}
}
data{
	int N;
	
	row_vector[N] x;
}
model{
}
generated quantities{
	real test_val;
	
	test_val = multi_normal_kernel(x, diag_matrix(rep_vector(1, N)));
}
