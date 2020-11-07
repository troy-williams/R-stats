# 第９章
# 9.1 サイコロを1000回振って結果の収束の様子を見てみよう
n <- numeric(1000)
set.seed(1)
# as.integerで整数を返す。サイコロは正数値のみを取るため。
random <- as.integer(runif(1000,min = 1, max=7))
for (i in 1:1000) n[i] <- mean(random[1:i])
plot(1:1000,n, ylim = c(3,5))
abline(h=3.5)
# サンプルサイズを大きくすると標本平均は真の平均に近い値になる。

# 9.2 モンテカルロ法
x <- runif(10000,-1,1)
y <- runif(10000,-1,1)
plot(x,y,col = ifelse(x^2+y^2<1, "black", "white"),
     asp = 1,
     pch = 20)
# 正方形の面積４、円の面積Pi ---> 点が円に入る確率はPi/4
#  ---> 円の点を4倍して10000で割るとPiになるはず
4 * sum(x^2+y^2<1)/10000
# ランダムに落とした点の比率が真の値に近いことが体感できたかな？

# 補足　pch: 点の種類
x1 <- 1:25
y1 <- numeric(25)
plot(x1, y1, pch=x1)


# 第１０章：中心極限定理
# 期待値と分散さえ存在すればその分布が何であっても標本平均の分布が正規分布に近く
# X1,...,Xn ---> 和も正規分布(再生性)　お互いに同分布である必要すらない。
#lambda=5の指数分布で見てみよう
plot(function(x)dexp(x, rate = 5))
curve(dexp(x, rate=5))

x <- rexp(1000*50, rate = 5)
xm <- matrix(x, nrow = 1000, ncol = 50)
sum_xm <- apply(xm, 1, mean)
hist(sum_xm, xlim = c(0.1,0.3),
     ylim = c(0,15),
     prob=TRUE,
     ylab="")
par(new=TRUE)

curve(dnorm(x, mean = mean(sum_xm),
            sd = sd(sum_xm)),
            xlim = c(0.1,0.3),
            ylim = c(0, 15),
            xlab = "",
            ylab = "",
            lwd =2)


# 10.3 期待値が存在しない場合。
# 期待値も分散も存在しないコーシー分布(Cauchy distribution)の場合
# SIMで見てみよう

ave <- numeric(10000)     # 10000個の0を並べたベクトルを用意
x <- rcauchy(10000)       # コーシー分布に従う乱数を１万個用意
for (n in 1:10000) ave[n] <- mean(x[1:n])    # 最初のn個の平均値を格納
plot(1:10000, ave)
abline(h=0)
# 極端な値が全体に影響を及ぼすため一定値に収束することが期待できない。
# これは数学的に困るだけでなく、大数の法則が成り立たない厄介なもの