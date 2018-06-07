functions{
	real normal_kernel(row_vector x,
		real alpha,
		matrix H_inv
	)
	{
		return alpha^2 * exp(-0.5 * x * H_inv * x');
	}
	
	matrix normal_cov(matrix X,
		real alpha,
		matrix H_inv
	)
	{
		int N;
		real temp_result;
		matrix[rows(X), rows(X)] result;
		
		N = rows(X);
		
		for (i in 1:(N-1)){
			result[i, i] = alpha^2;
			
			for (j in (i+1):N){
				temp_result = normal_kernel(row(X, i) - row(X, j), alpha, H_inv);
				result[i, j] = temp_result;
				result[j, i] = temp_result;
			}
		}
		result[N, N] = alpha^2;
		
		return result;
	}
	
	real normal_plus_halfnormal_lpdf(vector epsilon,
		real sigma,
		real lambda
	)
	{
		return num_elements(epsilon) * (log(2) - log(sigma)) + normal_lpdf(epsilon ./ sigma | 0, 1) + normal_lcdf(-epsilon * (lambda / sigma) | 0, 1);
	}
}
data{
	int N;
	int k;
	
	vector[N] y;
	matrix[N, k] X;
	
	real<lower=0> sigma_u;
	real<lower=0> sigma_v;
	
	vector<lower=0>[k] H_inv_diag;
	vector[N] eta;
}
transformed data{
	vector[N] zeros;
	matrix[k, k] H_inv;
	
	zeros = rep_vector(0, N);
	H_inv = diag_matrix(H_inv_diag);
}
parameters{
	real<lower=0> alpha;
}
transformed parameters{
	vector[N] f;
	
	{
		matrix[N, N] cov;
		matrix[N, N] L;
		
		cov = normal_cov(X, alpha, H_inv);
		for (i in 1:N){
			cov[i, i] = cov[i, i] + 1e-12;
		}
		L = cholesky_decompose(cov);
		
		f = L * eta;
	}
}
model{
	alpha ~ normal(0, 1);
	
	target += normal_plus_halfnormal_lpdf(y - f | sqrt(sigma_u^2 + sigma_v^2), sigma_u / sigma_v);
}
