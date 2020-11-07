# 第３章：
# 3.1 相関関係は因果関係ではない
# 実験を伴わない因果関係の推論はどうしても曖昧さが残る。
# ある要因が結果に対して影響を与えているのか、その程度を統計データに基づいて
# 推論する方法は統計的因果推論の分野。


# 3.2 切断効果
# ある集団内の２変数に正の大きな相関が見られる時、その集団を切断して小さな
# 階級に分けた時、元の集団の相関に比べて小さくなる現象。 ---> 切断効果or選抜効果
# ex. ボクシングは体重制で体重が重い方が強いが、体重で分けられた級内での相関は小さくなる
cor(math, phys)
# 数学の点数を60で境界として２つに分けて相関を見てみる
cor(math[math<60], phys[math<60])      # 0.23
cor(math[math>=60], phys[math>=60])    # 0.45
# 数学と物理の合計点が120以上の人の数学と物理の相関を見てみる
cor(math[math+phys>=120], phys[math+phys>=120])   # 0.18 ほぼ相関がない。。。
# 切断効果は自覚していないと間違いに気づかないことが多いため注意。


# 3.3 外れ値の影響
# 相関係数は特にサンプル数が小さい時に外れ値の影響を極端に受ける
x <- rnorm(20, 30, 10)
y <- rnorm(20, 30, 10)
cor(x, y)               # 0.13

xa <- c(x, 100)         # xに100を追加
ya <- c(y, 100)
cor(xa, ya)             # 0.72

# このような場合外れ値を除去して考えることが多い。この問題は根深い
# 外れ値であるかを判断するには元の事象がどんな確率分布であるかを分かっている必要がある
# しかしそれには大量のサンプルが必要で、そんな時に外れ値らしきものが入っていても
# それが外れ値であるかは間違いかもしれない。元の確率分布がそのようなものかもしれないからだ。
# このあたりは実際は「習慣で」判断される。

# 機械的に外れ値を弾くパッケージ ---> robustbase, robust


# 3.4 三変量以上のデータの記述
# 3変量以上は散布図で視覚的に変量の関係をみることが困難 ---> 散布図行列を導入
head(airquality)
plot(airquality[,1:4])   # seabornでいうpair plotのようなもの


# 3.5 分散共分散行列と相関行列
# テキストには難しいこと書いてあるが要は多変数の分散と相関の一覧
iris_mat <- iris[,1:4]
# 分散共分散行列 ---> 異なる変数の組み合わせは共分散、同じ変数の組み合わせは分散
var(iris_mat)

# 相関行列 ---> 同じ変数の組み合わせは当然１になる
cor(iris_mat)


# 3.6 章末問題
# 3-1 
x <- rnorm(100, 0, 1)
y <- rnorm(100, 0, 2)
a <- c(x[1:50], y[1:50])
b <- c(x[51:100], y[51:100])

cor(x, y)
cor(a, b)

# 3-2
x <- c(1,2,2,3)
barplot(x, names.arg = c("poor", "not bad", "good", "very good"),
        xlab = "feeling",
        ylab = "population",
        width = 0.8,
        legend.text = c("poor", "not bad", "good", "very good"),
        beside = FALSE,
        col = cm.colors(4),
        main = "title only in English",
        sub = "sub title"
        
        )
help("barplot")

# 4.3.3: PDF, probability density function:
 # ---> uniform distribution's PDF : dunif
curve(dunif, from = -1,to= 2)
 # ---> normal distribution's PDF : dnorm
curve(dnorm, -2, 2)

# 4.3.4: CDF, cumulative probability density function
 # ---> uniform distribution's CDF : punif
curve(punif, from = -1, to = 2)
 # ---> normal distribution's CDF : pnorm
curve(pnorm, -1,2)

# 4.3.4: quantile function: 
# 一様分布の0.95分位点は明らかに0.95
qunif(0.95,min=0, max=1)    # ---> 0.95
# 標準正規分布でも
qnorm(0.95, mean=0, sd=1)   # ---> 95パーセンタイル点。-∞から0.95%までの面積の総和
# 累積分布関数と分位点関数は互いに逆関数。同じ値が返ってくる
pnorm(qnorm(0.95))
qnorm(pnorm(0.95))

# 4.3.6: 一様乱数の発生
# min < X < maxの範囲の連続一様分布に従う乱数を発生させることができる
set.seed(1)
runif(6, min=-1, max=1)    # 6個発生させた

# 整数の乱数を発生させたい場合 ---> 小数点以下を切り捨てる
x <- runif(5,1,9)
as.integer(x)






