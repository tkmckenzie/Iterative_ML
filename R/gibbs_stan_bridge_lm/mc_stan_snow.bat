for /l %%x in (1, 1, 1000) do (
	Rscript mc_stan_snow.R
)
pause