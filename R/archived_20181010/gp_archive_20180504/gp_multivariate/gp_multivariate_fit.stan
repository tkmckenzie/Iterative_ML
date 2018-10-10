functions{
	real multi_normal_kernel(row_vector x,
		matrix H_inv,
		real H_inv_sqrt_det
	)
	{
		return H_inv_sqrt_det * exp(-0.5 * x * H_inv * x');
	}
	matrix multi_normal_cov(matrix x,
		matrix H_inv
	)
	{
		//This function generates covariance between any two rows of x
		int N;
		real H_inv_sqrt_det;
		real temp_result;
		matrix[rows(x), rows(x)] result;
		
		N = rows(x);
		
		H_inv_sqrt_det = sqrt(determinant(H_inv));
		
		for (i in 1:(N - 1)){
			result[i, i] = H_inv_sqrt_det;
			for (j in (i+1):N){
				temp_result = multi_normal_kernel(row(x, i) - row(x, j), H_inv, H_inv_sqrt_det);
				result[i, j] = temp_result;
				result[j, i] = temp_result;
			}
		}
		result[N, N] = H_inv_sqrt_det;
		
		return result;
	}
}
data{
	int<lower=1> N;
	int<lower=1> k;
	
	vector[N] y;
	matrix[N, k] x;
}
transformed data{
	vector[N] zeros;
	cov_matrix[k] I_k;
	
	zeros = rep_vector(0, N);
	I_k = diag_matrix(rep_vector(1, k));
}
parameters{
	cov_matrix[k] H_inv;
	real<lower=0> sigma;
}
model{
	matrix[N, N] L_cov;
	{
		matrix[N, N] cov;
		cov = multi_normal_cov(x, H_inv);
		for (n in 1:N)
			cov[n, n] = cov[n, n] + square(sigma);
		L_cov = cholesky_decompose(cov);
	}
	
	H_inv ~ wishart(k, I_k);
	sigma ~ normal(0, 1);
	
	y ~ multi_normal_cholesky(zeros, L_cov);
}
