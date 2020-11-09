# 多変量解析
# 1.1 統計で用いられるデータの種類
# 1.1.1 質的データ(qualitative data): カテゴリカルデータ、カテゴリデータとも
# 数値として観測することに意味がなく、属性や状態がわかるデータのこと。
  # 名義尺度：データの値が同一かどうかの区別のみが意味を持つデータ。
　# 順序データ：データ順の並びや大小が意味を持つデータ。


# 1.2 適合度検定
# 月別の南海トラフの発生回数
nankai <- c(0,2,0,0,0,0,0,2,1,1,1,5)
month <- 1:12
barplot(nankai,
        names.arg=month,
        xlab="month",
        ylab="frequency")
# データに偏りがあるように見えるがこれは偶然なのかそうでないのかを判断したい
# H0: 大地震の起きやすい月はない。つまりどの月でも同じ確率で地震がおきる
# 期待度数は1月に１回になっている
# これとこれまでの発生月のデータ（観測度数）を比較する
# ---> 適合度検定
# 問題は図のように偏る確率がどれくらい「小さいか」


# 1.3 適合度検定をやってみる
# ズレの大きさを適切に表現してその値が一定以上に大きくなる確率を計算する
# ---> ピアソンのカイ二乗統計量(chi-squared statistic)


# 一様分布の時
# H0: 月と独立に地震が起きる＝各々の月に地震が起きる確率が12分の1
chisq.test(nankai)
# X-squared:24, p-value:0.01273
  # ---> X-squared statisticの値が得られる確率は1.23% つまり月に関係なく地震が起きると仮定するのは無理がある

# 一様分布出ない時
# 大型航空機事故の例
years <- c(1,6,6,8,5,7,0,1,0)
mean_years <- sum((0:8)*years)/sum(years)
# ポアソン分布の確率
theory <- dpois(0:8, lambda=mean_years)

# 実際は８回しか事故が起こらなかったのでどこかで打ち切る必要がある
# ８回を８回以上と解釈した場合（自由度８）
poisprob <- dpois(0:7, lambda=m)
theory <- c(poisprob, 1-sum(poisprob))

chisq.test(years, p=theory)

# 8回までは理論値とし９回以上の実現値が０であると解釈（自由度９）
poisprob2 <- dpois(0:8,lambda=m)
theory2 <- c(poisprob2, 1-sum(poisprob2))
years2 <- c(years, 0)
chisq.test(years2, p=theory2)
# いずれの場合もH0を棄却できない。問題になるのはどうやってズレを謀っているのか


# 1.4 カイ二乗統計量
# ∑(観測度数ー期待度数)^2/観測度数
# 期待度外れの度合い^2 / 期待度数
?rchisq
x <- rchisq(1000,3)
hist(x)


# 2.1　分割表の独立性の検定
# 行ラベルが喫煙の程度、列ラベルが運動習慣
SmokeEx <- table(survey$Smoke, survey$Exer)
SmokeEx
# 喫煙習慣がある人はあまり運動しないように思えるがこそ感覚は正当だろうか。
# ---> 独立性の検定を行ってみよう。
# H0: 属性A,Bが独立である
chisq.test(SmokeEx)
# P値が大きいため独立であるという帰無仮説は棄却されない


# 2.2 2＊2分割表
# 2＊2の分割表における独立性の検定もchisq.test()を使うが少し扱いに注意が必要
# カイ二乗検定では多項分布に従う離散的な確率変数を連続的なカイ二乗分布で近似して検定を
# 行うので誤差が大きい時は若干の工夫がいる
# 2*2の分割表においてサンプル数が小さい時P-valueが小さくなることが知られている
# そこでカイ二乗統計量を小さく補正することを考える。
# イエーツの補正(観測度数ー期待度数から0.5を引く)
# Rでは2*2の分割表に対して自動でイエーツの補正をかけている
sb <- matrix(c(1085,703,55623,441239),2,2)
sb
chisq.test(sb,correct=FALSE) # correct: イエーツの補正
chisq.test(sb)

# 2.3 母比率の差の検定から飛ばして単回帰分析に移った


# 第３章：単回帰分析
# cars(車の時速と停車までに走行する距離)を見てみよう
plot(cars)
# lm()---> linear model関数で回帰直線が引ける 
res <- lm(formula = dist~speed, data = cars)
plot(cars)
abline(res)
summary(res)
# 回帰直線の当てはまりの良さは決定係数をみる ---> 分散説明率


# 3.1.2 最小二乗法と最尤推定との関係
# 残差が正規分布するという仮定の元では最小二乗法によって得られた係数は最尤推定値に一致する。
# ただし残差の分布が正規分布意外だと最尤推定との間に無視できない誤差が出る。９章で解説。


# 定数項を０とした場合
# speedが０であればdistも０であるから定数項（切片）も０にすべき？---> 誤り
# 切片０とした場合決定係数の意味が全く異なってしまう。
result <- lm(formula = dist ~ speed -1, data=cars)    # formulaに−１を書くと切片０になる
summary(result)
plot(cars, xlim=c(0,25))
abline(result)    # 原点を通っている
# 切片０の場合、最小二乗法では決定係数の分母∑(yi-ßxj)^2でß=0として残差平方和を最小化するåを選ぶのに対し
# 分子はå=0として残差平方和を最小化するßを選んでいる。このように分母と分子が関係なくなってしまうため
# 分散説明率としての決定係数の意味が全く異なってしまう。


# 3.4 外れ値の影響をシミュレーション
x <- 1:20
y <- x + rnorm(length(x),0,1)
plot(x, y)
res <- lm(formula = y ~ x)
abline(res)  # R^2=0.89,当てはまりが良い

# y1とy5を極端に大きくしてみる
y[c(1,5)] <- c(20,18)
plot(x, y)
res_outlier <- lm(y ~ x)
abline(res_outlier)
summary(res_outlier)  # outlierの影響を強く受ける


# 第４章：赤池情報量基準によるモデル選択
# 4.1 cars再考
# carsの関係は放物線の方がよくねー？
# ２次関数にするにはpoly()の中の数字の次数を２。raw=TRUEは必須。FALSEだと直行多項式による回帰になる
res2 <- lm(formula = dist ~ poly(speed,2,raw=TRUE), data=cars)
summary(res2)
# dist = 2.47014 + 0.91329*speed + 0.09996*speed^2
plot(cars)
lines(cars$speed, fitted(res2))   # 関数fitted()でspeedに対する予測値を求め、linesで折線を描画

# 3次４次も書いてみよう
res3 <- lm(formula = dist ~ poly(speed,3,raw=TRUE), data=cars)
res4 <- lm(formula = dist ~ poly(speed,4,raw=TRUE), data=cars)
lines(cars$speed, fitted(res3), type = "c")
lines(cars$speed, fitted(res4), type = "b")


# 4.2 AIC(赤池情報量基準)
# 多項式フィッティングは次数を上げていくにつれ誤差が小さくなり決定係数が大きくなっていく
summary(res)$r.squared
summary(res2)$r.squared
summary(res3)$r.squared
summary(res4)$r.squared
# しかし次数を上げて推定パラメータを増やしていくと本来はノイズと考えられる変化も取り込んでしまう
# ---> 過学習；オーバーフィッティング
# モデル選択問題の１つの解として赤池情報量基準AICがある
# 定義：AIC = -2MLL + 2k (MLL:最大対数尤度maximum log likelihood, k:パラメータ数)
# MLL--->当てはまりが良いと減少（符号マイナス）する方向に働く。
# k  --->少なければ少ないほど良いので大きくなると増加する方向に働く。
# AIC--->MLLとkの綱引き
AIC(res, res2, res3,res4)
# AIC が最小のものはk=2...であるが係数が有意出ないため早計
# speedが０のときdistも０になるモデルが適切であるから
res02 <- lm(formula = dist ~ poly(speed,2,raw = TRUE) -1,
            data = cars)
summary(res02)
# いずれの係数も有意。ただし決定係数は使えない。AICで比較しよう
AIC(res02) # --->最小なので適切。


# 第６章：曲線の当てはめ
# 6.1 lm()を用いた曲線の当てはめがうまくいく場合
# lm()がうまくいくのは最小化条件において係数の一次式が得られる場合である
# よって三角関数なども当てはめれる
pi <- 3.141592
x <- seq(0, 2*pi, length=100)
y <- 1 + 2*sin(x) + 3*cos(2*x) + rnorm(length(x),0,0.2)  # 最後の項でノイズを加える
plot(x, y)
res_sin <- lm(y ~ I(sin(x)) + I(cos(2*x)))
summary(res_sin)
lines(x, fitted(res_sin))


# 6.2 lmによる当てはめが使えない場合：非線形最小二乗法
# 例えばロジスティック曲線を当てはめたい時(a/(1+b*exp(-cx))) ---> nls() :nonlinear least square
curve(1/(1+exp(-x)), -10, 10)

diffusion_rate <- scan(pipe("pbpaste"))
head(diffusion_rate)
year <- 1987:2014
plot(year, diffusion_rate, ylim = c(5,83))     # ロジスティック曲線：細菌の増殖や噂の伝播率
# nls(): 反復法---> 適当な初期値から残差平方和が小さくなるようパラメータを更新しある値以下で打ち切る
year0 <- year - 1987  # yearが大きすぎるので結果bが極端に大きくなるため適当に引いている
res <- nls(diffusion_rate ~ a/(1+b*exp(-c*year0)),
           start=c(a=80,b=1,c=1),    # a=80くらいなのがわかってるため80。20くらいからスタートでも良い
           trace=TRUE,
           algorithm="default")      # Available for plinear and port 
summary(res)
lines(year,fitted(res))
# 若干当てはまりが悪いところもあるが概ね良い。注目すべきは最終的なPC普及率が81%ということ。
# うまくいかない原因は初期値にあるため、エラーが出たら初期値を疑った方がいい。
# Default: Gause=Newton アルゴリズム
# plinear: Golub=Pereyraによる局所線形最小二乗法のアルゴリズム
# nlsでは誤差０の人工的なデータに使うと収束しないため注意。


# 6.4 変数変換と直線回帰の組み合わせ
# 6.4.1 両対数グラフが直線的な場合
city <- scan(pipe("pbpaste"))
# cityの中の数字の大きさの順位が降順で格納される
ord <- order(city, decreasing=TRUE)
# 順位に従ってデータを並び替える
city_popl <- city[ord]
plot(1:21, city_popl)
# ぱっと見明らかに直線関係ではないことがわかる
# 一般にランキングデータはべき型の関係（y=Cx^ß）であることが多い
# 両対数をとると、logy=logC + ßlogx となり直線関係になる
plot(log(1:21), log(city_popl))  # ---> 直線関係になった
rank <- 1:21
result <- lm(log(city_popl)~log(rank))
summary(result)
abline(result)
# あくまで両対数の残差が最小化されているのであってR^2も両対数グラフを直線近似した場合のもの
