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
		return log(2) - log(sigma) + normal_lpdf(epsilon ./ sigma | 0, 1) + normal_lccdf(epsilon * (lambda / sigma) | 0, 1);
	}
}
data{
	int N;
	int k;
	
	vector[N] y;
	matrix[N, k] X;
	
	real<lower=0> sigma_u;
	real<lower=0> sigma_v;
	
	vector[k] beta;
}
parameters{
	real beta_const;
}
model{
	beta_const ~ normal(0, 10);
	
	target += normal_plus_halfnormal_lpdf(y - (beta_const + X * beta) | sqrt(sigma_u^2 + sigma_v^2), sigma_u / sigma_v);
}
