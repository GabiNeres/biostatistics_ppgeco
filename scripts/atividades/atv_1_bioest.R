
est_func <- function(vetor){
  SOMA <- sum(vetor)
  N <- length(vetor)
  MEDIA <- SOMA/N
  DIF <- c()
  for (i in 1:N){
    DIF <- c(DIF,(vetor[i] - MEDIA))
  }
  DIF2 <- c()
  for (i in 1:N){
    DIF2 <- c(DIF2, (vetor[i] - MEDIA)^2)
  }
  SQ <- sum (DIF2)
  MQ <- SQ/(N-1)
  VAR <- MQ
  DP <- sqrt(VAR)
  EP <- DP/sqrt(N)
  t_value <- MEDIA/EP
  
  return (t_value)
}

vetor <- c(1, 3, 5, 7, 9)
est_func(vetor)
t.test(vetor)