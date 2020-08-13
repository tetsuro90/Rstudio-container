# R12-4.R ハプロタイプ頻度推定
Make3x3 <- function(f) {
  tmp <- f %*% t(f)   # tmpは4*4のmatrix
  matrix(c(tmp[1, 1], 2 * tmp[1, 2], tmp[2, 2], 
           2 * tmp[1, 3], 2 * (tmp[1, 4] + tmp[2, 3]), 2 * tmp[2, 4],
           tmp[3, 3], 2 * tmp[3, 4], tmp[4, 4]),
         nrow = 3, byrow = TRUE)
}

CalcLike3x3 <- function(g = matrix(1, 3, 3),  # 3x3の1行列
                        f = c(0.25, 0.25, 0.25, 0.25)) {
  F <- Make3x3(f)
  sum(g * log(F))
}

CalcR <- function(h) {
  p_A <- h[1] + h[3]
  p_B <- h[1] + h[2]
  (h[1] - p_A * p_B)/sqrt(p_A * (1 - p_A) * p_B * (1 - p_B))
}

ns <- matrix(c(2, 5, 2, 10, 18, 14, 4, 25, 20),
             nrow = 3, byrow = TRUE)

N <- 10000

p <- c(1, 1, 1, 1)

sampled <- gtools::rdirichlet(N, p)

loglikes <- apply(sampled, 1, CalcLike3x3, g=ns)

maxset <- sampled[which(loglikes == max(loglikes)),]  # which: 論理演算子でTRUEとなるindexを取得する。 # 最大尤度を返した4はプロタイプ

maxp_A <- maxset[1] + maxset[3]

maxp_B <- maxset[1] + maxset[2]

maxr <- CalcR(maxset)
maxset



# R12-5.R ハプロタイプ頻度推定②
p_A <- 0.4; p_B <- 0.3

# HWEの時の各ハプロタイプの頻度（独立を仮定しているということ）
f1 <- p_A * p_B
f2 <- (1 - p_A) * p_B
f3 <- p_A * (1 - p_B)
f4 <- (1 - p_A) * (1 - p_B)

rs <- seq(from = -1, to = 1, by = 0.001)

# ハプロタイプの頻度
ds <- rs * sqrt(p_A * (1 - p_A) * p_B * (1 - p_B))
f1 <- ds + f1
f2 <- -ds + f2
f3 <- -ds + f3
f4 <- ds + f4

F <- matrix(c(f1, f2, f3, f4), nrow = length(f1))

minF <- apply(F, 1, min)

rs <- rs[minF > 0]

F <- F[minF > 0, ]

loglikes2 <- apply(F, 1, CalcLike3x3, g=ns)

maxset2 <- F[which(loglikes == max(loglikes2)), ]

maxp_A2 <- maxset2[1] + maxset2[3]
maxp_B2 <- maxset2[1] + maxset2[2]

maxr2 <- CalcR(maxset2)

plot(rs, loglikes2, type = "l")


# R12-6.R : EMアルゴリズム

EM2loci <- function(n, p = NULL, Niter = 1000) {
  if ( is.null(p)) p <- rep(0.25, 4)
  
  f <- p
  rs <- rep(0, Niter)
  logLikes <- rep(0, Niter)
  fs <- matrix(0, Niter, 4)   # 1000 * 4 の行列
  
  fixed <- c(n[1, 1] * 2 + n[1, 2] + n[2, 1],
             n[1, 3] * 2 + n[1, 2] + n[2, 3],
             n[3, 1] * 2 + n[2, 1] + n[3, 2],
             n[3, 3] * 2 + n[2, 3] + n[3, 2])
  
  for (i in 1:Niter) {
    tmpratio <- f[1]*f[4]/(f[1]*f[4] + f[2] * f[3])
    tmp <- rep(0, 4)
    tmp[1] <- fixed[1] + n[2, 2] * tmpratio
    tmp[2] <- fixed[2] + n[2, 2] * (1 - tmpratio)
    tmp[3] <- fixed[3] + n[2, 2] * tmpratio
    tmp[4] <- fixed[4] + n[2, 2] * (1 - tmpratio) 
    
    fs[i,] <- tmp/sum(tmp)
    logLikes[i] <- CalcLike3x3(n, tmp)
    rs[i] <- CalcR(tmp)
    f <- tmp
  }
  
  list(f = f, r = rs[Niter], logLike = logLikes[Niter],
       fHistory = fs, rHistory = rs,
       logLikeHistory = logLikes)
}

emout <- EM2loci(ns)

maxp_A <- emout$f[1] + emout$f[3]
maxp_B <- emout$f[1] + emout$f[2]
maxr <- emout$r

plot(emout$logLikeHistory[1:20], type = "b")






























