# Temps d'attentes A2
A2 <- c(413, 14, 58, 37, 100, 65, 9, 169, 447, 184, 36, 201, 118, 34, 31, 18, 18, 67, 57, 62, 7, 22, 34) 
# Temps d'arrivés A2
T2 <- cumsum(A2)
# Taille du vecteur des Ti
n = length(T2)
# Vecteur des -ln
V <- -log(T2[1:n-1]/T2[n])
# Test d'adéquation à une loi expo (test si Weibull)
Test <- LcKS(V, cdf = "pexp")
# Extraction des simulation de Monte Carlo du test
D_sim <- sort(Test$D.sim)
# Extraction du quantile d'ordre 1-0.05 (d'où le 95/100)
Test_quantile <- D_sim[round(95/100*length(D_sim))]
# On affiche la fonction de répartition empirique
plot(ecdf(V))
# Beta chapeau du test
beta_hat <- 1/mean(V)
# Fonction de répartition expo du test
F_hat <- 1-exp(-beta_hat*seq(0, 2, 0.01))
# Bornes
lines(x = seq(0, 2, 0.01), F_hat+Test_quantile)
lines(x = seq(0, 2, 0.01), F_hat-Test_quantile)
