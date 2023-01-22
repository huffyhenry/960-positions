data {
  array[960] int wins;
  array[960] int draws;
  array[960] int losses;
}

parameters {
  array[3] real<lower=0.0> alpha;
  array[960] simplex[3] rates;
}

model {
  alpha ~ cauchy(0, 50);
  rates ~ dirichlet(to_vector(alpha));
  
  for (i in 1:960)
    {wins[i], draws[i], losses[i]} ~ multinomial(rates[i]);
}

generated quantities{
  array[960] real xp;
  
  for (i in 1:960){
    xp[i] = rates[i][1] + 0.5*rates[i][2];
  }
}
