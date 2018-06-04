functions{
	real normal_kernel(real x,
		real alpha,
		real h
	)
	{
		return alpha^2 * exp(-0.5 * x^2 / (h^2));
	}
	
	matrix normal_cov(vector x,
		real alpha,
		real h
	)
	{
		int N;
		real temp_result;
		matrix[rows(x), rows(x)] result;
		
		N = rows(x);
		
		for (i in 1:(N-1)){
			result[i, i] = alpha^2;
			
			for (j in (i+1):N){
				temp_result = normal_kernel(x[i] - x[j], alpha, h);
				result[i, j] = temp_result;
				result[j, i] = temp_result;
			}
		}
		result[N, N] = alpha^2;
		
		return result;
	}
}
data{
	int N;
	
	vector[N] y;
	vector[N] x;
	//real x[N];
}
transformed data{
	vector[N] zeros;
	
	zeros = rep_vector(0, N);
}
parameters{
	real<lower=0> alpha;
	real<lower=0> h;
	
	real<lower=0> sigma;
	
	vector[N] eta;
}
transformed parameters{
	vector[N] f;
	
	{
		matrix[N, N] cov;
		matrix[N, N] L;
		
		cov = normal_cov(x, alpha, h);
		for (i in 1:N){
			cov[i, i] = cov[i, i] + 1e-12;
		}
		L = cholesky_decompose(cov);
		
		f = L * eta;
	}
}
model{
	alpha ~ normal(0, 1);
	h ~ gamma(1, 0.1);
	sigma ~ normal(0, 1);
	
	eta ~ normal(0, 1);
	
	
	y ~ normal(f, sigma);
}
