# ライブラリの読み込み
library(irtoys)

# データの読み込み
# 1:実データ
# 2:人工データ
data1 <- read.csv("./csv/20171222/result.csv", header=F)
# data2 <- read.csv("./dataLogic.csv", header=F)

# IRTでパラメータの推定
item_param <- est(resp=data1, model="2PL", engine="ltm", a.prior=FALSE, b.prior=FALSE, c.prior=FALSE)

value <- iif(item_param$est)

# 2母数ロジスティックモデルの項目情報量
iif2PL <- function(a, b, theta) {
  p <- 1 / (1+exp(-1.7*a*(theta-b)))
  q <- 1 - p
  p_prime <- 1.7 * a * p * q
  iif <- p_prime^2 / (p*q)
  iif
}

# 余白の設定
par(mai=c(1.2,1.0,0.3,0.3))

curve(iif2PL(item_param$est[1,1], item_param$est[1,2], x), xlim=c(-6.0,6.0), ylim=c(0,2), xlab = "θ", ylab = expression(paste(I[j](θ))), lty = 1, lwd = 3, cex.lab = 1.5, cex.axis = 2, col = 1)

for (i in 2:(length(item_param$est)/3)) {
    curve(iif2PL(item_param$est[i,1], item_param$est[i,2], x), add = TRUE, lty = i, lwd = 3, col = i%/%7+1)
}

#for (i in 2:12) {
#    curve(iif2PL(item_param$est[i,1], item_param$est[i,2], x), add = TRUE, lty = i, lwd = 3, col = i%/%7+1)
#}
#curve(iif2PL(item_param$est[13,1], item_param$est[13,2], x), add = TRUE, lty = 1, lwd = 3, col = 3)


legend("topright", legend = c("No.1","No.2","No.3","No.4","No.5","No.6","No.7","No.8","No.9","No.10","No.11","No.12","No.13"), lty=c(1,2,3,4,5,6,7,8,9,10,11,12,13), col=c(1,1,1,1,1,1,2,2,2,2,2,2,3), lwd=3, cex=1.5)
