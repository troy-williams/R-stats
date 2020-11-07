# 6.1 二項分布(Binomial distribution)　---> dbinom, pbinom
# x: vector of quantiles, size: number of trials, prob: vector of probabilies
help("dbinom")
dbinom(x=6, size=10, prob=0.6)    # 25%

# P(X=1), P(X=4), P(X=10)を一気に計算する
dbinom(x=c(1,4,10), size=10, prob=0.6)

# P(X<=4)
help("pbinom")
pbinom(q=4, size=10, prob=0.6)   # p ---> CDF. ４以下の確率の累積
# or
sum(dbinom(x=0:4, size=10, prob=0.6))

# 補足：小数点の丸めはpythonと同じくround(X, 4)で小数点４位まで松寿できる。

# plot binomial distribution
barplot(dbinom(x=0:10, size=10, prob=0.5),
        names=0:10,
        xlab="x",
        col=x)
      


# 6.3 ポアソン分布
# 確率pの事象がN回の試行でx回発生する確率分布は二項分布で表せるが、Nが大きくなると
# 計算が複雑になってしまう。そこでpが小さくでNが大きい事象を近似できるのが
# ポアソン分布。pが大きくなると正規分布に近づいていくだったかな。。。
# パラメータはlambdaのみ
barplot(dpois(x=0:10, lambda=2.3),
        names=0:10,
        xlab="x",
        ylab="Density")

# 飛行機事故の件数の例で見てみる
years <- c(1,6,6,8,5,7,0,1,0)
m <- sum((0:8)*years/sum(years))
m # 3.05
# 1年当たりの事故の回数が約3.05をlambdaとするポアソン分布を書いてみる
data <- data.frame(years, sum(years)*dpois(0:8, lambda = m))
data
barplot(t(data),
        col = c("black", "gray"),
        beside = TRUE,
        names.arg = 0:8,
        xlab="number of accidents",
        legend=c("real", "theory"))
# 理論値とのずれが誤差の範囲内かの検定：適合度検定。やらない


# 6.4 幾何分布(geometric distribution)
# １回の試行で確率pの事象が起きるまでの試行回数Xがkとなる確率
# P(X = k) = p(1 - p)^(k - 1)
# 期待値：1/p, 分散：(1-p)/p^2
# p=1/6, N=10^5 のgeometric distribution
first <- rgeom(100000, prob=1/6)
hist(first, prob=TRUE)       # ---> 最初の５回でpの事象が発生するものが多い。
# 確率は非常に小さくなるがX回目に発生するというXは無限大をとりうる。


# 6.5 負の二項分布(negative binomial distribution or Pascal distribution)
# r回目の事象が起きるまでの失敗回数Xがkである確率
k <- 0:40
barplot(dnbinom(k, size=3, prob=0.2),
        xlab="number of failuer",
        names.arg = k)
# ポアソン分布に近いがr,pを変えると形状が柔軟に変わる。
# 期待値：r(1-p)/p, 分散：q(1-p)/p^2


# ７章：連続確率分布
# 7.1 正規分布(normal distribution): 
# Johann Carl Friedrich Gauss ---> Gauss distribution
help("dnorm")
help("curve")
x<- 10
curve(dnorm(x,mean=8, sd=4), -10, 20)
# X ? N(8,4)で P(X>9)を求めるには、1 - P(X<=9)を求める
1 - pnorm(9, mean=8, sd=2)

# 二項分布Binom(n, p)はNが十分大きい時に正規分布N(np, npq)でよく近似できる。
help("dbinom")
x<- 0:20
plot(dbinom(x=0:20,size=20, prob=1/2), xlim=c(0,20), 
     xlab="",
     ylab="")
par(new=TRUE)    # グラフの重ね書き（多分。。。
curve(dnorm(x,mean = 10, sd=sqrt(20*0.5*0.5)),
      xlim=c(0,20),
      xlab="x",
      ylab="Density",
      axes=FALSE)


# 7.2 対数正規分布(lognormal distribution):
# Xの対数を取ったものが正規分布をすること。
# 正の値をとる独立な確率変数の積を取ると現れる。
curve(dlnorm(x, meanlog = 1, sdlog = 1), xlim = c(0,10))


# 7.3 指数分布(exponential distribution):
# 独立に生じる事象が一定期間の間に起きる回数の分布はポアソン分布に従う
# 指数分布は事象が発生する間隔の時間
# Rではlambdaはrateと表示される
curve(dexp(x,rate=2.3))
# 期待値：1/lambda, 分散：1/lambda^2
# 単位時間内に到着した指数分布に従う事象の数を数えると以下の通り

# xの行列にtimeが１以上になるまでの回数が入っていく（１以上の乱数が確認されるまでの数）
# xnumに１以上になった回数が入る。　xとxnumの比
n <-10^3
r <- rexp(n, rate=2.3)
x <-0
xnum<-0
count<-0
time<-0
for (i in 1:n){
  time <- time + r[i]
  if(time < 1) count <- count + 1
  else {
    x[xnum] <- count
    xnum <- xnum + 1
    time <- 0
    count <- 0
  }
}　　　　　
barplot(table(x)/xnum)

# また指数分布を用いてポアソン分布に従う事象が起きたタイミングを記録すると
plot(c(0, 150),
     c(0, 1),
     type = "n",
     axes = FALSE,
     xlab = "",
     ylab = "")
axis(1)
n <- 50
r <- rexp(n, rate=0.5)
pos <- numeric(n)                         # 0で入れ物を作る
for (i in 1:n) pos[i] <- sum(r[1:i])      # 指数分布に従う乱数rの最初からi番目までの和を格納
segments(pos, 0.2, pos, 0.8)              # pos[i]はi番目の事象が発生するまでにかかった時間。
# segments(x1, x2, y1, y2)は点(x1, y1)と点(x2, y2)を繋ぐ線分を描くもの。


# 7.4 コーシー分布(Cauchy distribution):
# 位置母数(location parameter)：x0, 尺度母数(scale parameter)
x <- seq(-3, 5, 0.1)
curve(dcauchy(x,location = 1, scale = 2), xlim=c(-3,5))
# 期待値と分散が存在しない ---> 期待値として積分して極限を取った時に１つの値に収束しない。

# 7.5 ワイブル分布(Weibull distribution):
# スウェーデンの機械工学者Qallodi Weibullがベアリングの寿命予測のために考案
# 形状母数(shape param):shape, 尺度母数(scale param): scvale
x <- seq(0,5,by=0.01)
curve(dweibull(x,shape=1, scale=3), 0, 5)
# shapeを変更すると形状が大きく変わる
# shape>1 ---> 摩耗故障型、shape=1 ---> 偶発故障型、shape<1 ---> 初期故障型
# scaleは全体のスケールが変更される。

# 生命表 ---> ワイブル分布がよく当てはまる。1横軸年齢で10万人あたりの死亡数を縦軸
age <- 1:104
deathrate <- read.table(pipe("pbpaste"))/10000
deathrate
# macのscanがだるすぎて飛ばす。


# 第８章：多変量正規分布
# 8.1 独立な離散的確率変数の和の分布
# 二項分布する２変数の和は二項分布に従う。---> 再生性を持つという。
# 積率母関数を使うと証明は楽


# 8.2 独立な連続的確率変数の和の分布
# 8.3.2 正規分布の再生性
 # ---> 独立な2変数 X~N(µ1,sigma1^2) + Y~N(µ2,sigma^2): 
  # X + Y ~ N(µ1+µ2, sigma1^2+sigma2^2)


# 8.4 ガンマ分布(Gamma distribution):
# mean=k*scale, variance=k*scale^2
x <- seq(0,8,0.1)
curve(dgamma(x, shape=3, scale=1),0,8)  # shape param, scale param
# アーラン分布、カイ二乗分布、指数分布を含む。


# 8.5 アーラン分布(Erlang distribution):
# mean=1/lambdaの指数分布に従う独立なk個の確率変数の和の分布
# mean=k*scale, variance=k*scale^2
# lambda=1の指数分布に従う独立な確率変数X1, X2, X3に対しその和の分布をみる
x <- rexp(3*100000, rate=1)
xm <- matrix(x, nrow=100000, ncol=3)
help("apply")
s <- apply(xm,1,sum) # 列ごとに適用する場合は２に変更
hist(s, prob=TRUE)


# 8.6 カイ二乗分布
help("dchisq")
hist(dchisq(x, 5, ncp = 0))
# 定理1：確率変数X,Yが各自由度k1,k2のカイ二乗分布する時 
#  ---> X+Y ~ 自由度k1+k2のカイ二乗分布に従う
# 定理2：N(0,1)に従う独立なk個の確率変数X1,..,Xnについて
# ---> Z = ∑Xj^2 は自由度kのカイ二乗分布に従う。 






