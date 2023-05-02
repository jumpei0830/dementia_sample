#####Step0: Rの基本的な操作####

#現在地の取得
getwd()

#現在地の変更
setwd(" ") #""の中にディレクトリ(フォルダ）の場所（パス）を指定 C:¥Program Files¥.....
#Tips:位置をわかりやすくするためにDesktop上にRフォルダを作成し、その中にスクリプトや解析データをまとめておくと良い
#R projectを作成する理由は、Rprojectがある位置を現在値としてくれるから

#ベクトル（数列）の作成
c(1,2,3)
c(1:4)
rep(1:3,5)
v <- 2 + (seq(1:10) - 1) * 3 #等差数列
v <- 2 * 3 ^(seq(1:10) - 1) #等比数列
v
v[2] #n項目 
rm(v) #変数の除去

#マトリクス（行列）の作成
m <- matrix(1:12,3,4)
m
m1 <- matrix(rep(1,9),3,3)
m1
m1 <- cbind(m1, c(1:3))
m1
m <- m + m1
m
m[2,4]
m[,2]

#データフレームの作成
v1 <- c(1:4)
v2 <- rep(1,4)
data.frame(v1,v2)
#Tips: 解析データはdata.frame型で読み込むのが基本
#どの構造かを調べる
class(matrix(1:4,2,2))
#構造を変える
as.matrix(data.frame(v1,v2))

#character型は文字
char <- "晴れ"
char
paste("今日は", char, "ですね")　#paste()で繋げる

#numeric型は数値なので四則計算できる
v1<- c(1:100)
v2<- c(101:200)
sum(v1)
mean(v1)
sd(v1)
length(v1) #長さ
sum(v1)/sum(v2)
#Tips: 量的変数、一部の質的変数（評価表など順序尺度だが足してスコア化できるもの）はnumeri型にしておく

#factor型は因子なので四則計算できないが、要素数を数えられる
gender <- c(rep("male",10), rep("female", 10))
gender
class(gender)
gender <- as.factor(gender) 
class(gender)
levels(gender)
table(gender)
#Tips: 質的変数でクロス集計、カイ二乗検定する時はfactor型にしておく、table()で一発で集計表が作れる

#forを使いこなす etc.

       
####step1: データの読み込み####

#パッケージのインストール(一回しておけばok)
install.packages("tidyverse")
install.packages("psych")
install.packages("car")
install.packages("DT")

#パッケージの展開(Rstudioを開くたびに実行する)
library(tidyverse)
library(psych)
library(car)
library(DT)

#データの読み込み(data.frame)
data <- read.csv("/Users/jumpeikudo/Desktop/R/dementia_sample/dementia_data.csv")

#データの要約
summary(data)

#欠損値NAがあると何かと不都合が起きることがあるので除去(今回はcomplete dataのみ使用) 欠損値の扱い方は注意！
data <- na.omit(data)
summary(data) #NAの行を削除したので300→296と減少しているが仕方ない(分析時に除去する方法でもありna.rm = Tなど)

####Step2: データの整理####

#education, cdrは順序尺度(厳密にはfimもだが一旦パス)なのでfactor型に変更する
data$education <- as.factor(data$education)
data$cdr <- as.factor(data$cdr)
levels(data$cdr)
table(data$education) #各水準の度数


####Step3: データを色々いじくり回す####

#任意の行、列を取り出す: data$列名 or data[,列番号] or data[,"列名"] 空白は全体を指す
data$gender
data[,3]
data[,"gender"]
data[,c(2,3)]　#複数行欲しい時は列番号にvector型を入れる
data[,2:4]
data[,c(1,3,5)]
data[,c("gender","education")]
data[1,] 
data[10:15,]
data[seq(1,300,50),] #seq(最小値,最大値,間隔)

#新たに変数resを作り、種々の計算をしてみる
res <- data[,1:4] # <- でres変数に代入
View(res)
res <- data[,c("fim_selfcare", "fim_communication")]
res
res <- apply(res, 1, sum) #項目の足し算を行う、applyは(引数：データ,1は行2は列, 処理 meanやsd sum)
res
res_sum <- apply(data[,7:9],1,sum)
res_sum
res_mean <- apply(data[,7:9], 1, mean)
res_sd <- apply(data[,7:9], 1, sd)
res <- cbind(res_sum, res_mean) #cbindでres_sum行列に右からres_mean列を追加する（行追加はrbind）
res <- cbind(res, res_sd)
res
#Task1: fim_motor_itemとfim_cognitive_item, fim_total (motor + cognitive)を作りdataに追加してみよう


#条件を満たす特定の行（又は列）だけ取り出す
data[data$gender == "male",] #genderがmaleである行の、列全体を出す
data[data$mmse < 20,] 
data[20 < data$mmse & data$mmse < 25, ] # &は　かつ
data[data$mmse < 20 | 25 < data$mmse,]　# |は または
#Task2: cdr 0 と cdr 0.5 のmmseスコアを取り出し、各群の度数と平均値の差を検定してみよう


#Task1 Answer: 
fim_motor_item <- data[,c("fim_selfcare","fim_sphincter_control","fim_mobility","fim_locomotion")]
fim_motor_item <- apply(fim_motor_item, 1, sum )
data <- cbind(data, fim_motor_item)
fim_cognitive_item <- apply(data[,11:12],1,sum)
data <- cbind(data, fim_cognitive_item)
fim_total <- fim_motor_item + fim_cognitive_item
data <- cbind(data, fim_total)
View(data)

#Task2 Answer: 
table(data$cdr)
t.test(data[data$cdr == 2, "mmse"], data[data$cdr == 0.5, "mmse"])
#CDR1と2の男女で差があるのか？MMSE20未満と以上でfim_cognitive_itemに有意差あるのか？などの仮説も1行で結果が出せる！



####Step4: 統計解析してみる####

#####分散分析（帰無仮説：CDRの違いでfimやmmseの値に違いがあるのか）#####
#1: 群分けを行う
var <- "mmse" #varの中に目的変数を入れる,mmseでもfim_totalでもなんでもok
cdr0 <- data[data$cdr == 0, var]
cdr0.5 <- data[data$cdr == 0.5, var]
cdr1 <- data[data$cdr == 1, var]
cdr2 <- data[data$cdr == 2, var]

#2: 各群のvar値の比較をしてみる（２群比較）,group1,2に比較したい群を入れる
group1 <- cdr0 
group2 <- cdr0.5
t.test(group1,group2)
length(group1) #n数
length(group2)

#3: 分散分析の実行 aovはformulaとして値(cdr)~要因(fac)をそれぞれvector型にして代入
cdr <- c(cdr0,cdr0.5,cdr1,cdr2)
fac <- c(rep("cdr0",length(cdr0)), rep("cdr0.5",length(cdr0.5)),rep("cdr1",length(cdr1)),rep("cdr2",length(cdr2)))
aov <- aov(cdr~fac) #aov(data$mmse ~ data$cdr)の方がシンプル
summary(aov) #事前検定anova 
TukeyHSD(aov) #多重比較Tukey method
#Tips: 最初の行でvarを定義したので後はこれを好きな変数に変えるだけで直ぐにanovaができる!!

#4: 結果を棒グラフで示す
mean_cdr <- c(mean(cdr0), mean(cdr0.5), mean(cdr1), mean(cdr2))
barplot <- barplot(mean_cdr, names.arg = c("CDR0", "CDR0.5", "CDR1", "CDR2"), ylim = c(0,30)) #names.argでx軸ラベル、ylimでy軸の幅を指定する
barplot
sd_cdr <- c(sd(cdr0), sd(cdr0.5), sd(cdr1), sd(cdr2))
arrows(barplot, mean_cdr - sd_cdr, barplot, mean_cdr + sd_cdr, code = 3, lwd = 1, angle = 90, length = 0.1) #arrowsを使ってsd線の追加
barplot(lty = 1)  #上に突き抜けている場合は天井効果あり

#補足：ggplotパッケージを使うとスマートになる
install.packages("ggplot2")
library(ggplot2)
# データフレームの作成
df <- data.frame(
  CDR = c("CDR0", "CDR0.5", "CDR1", "CDR2"),
  Mean = c(mean(cdr0), mean(cdr0.5), mean(cdr1), mean(cdr2)),
  SD = c(sd(cdr0), sd(cdr0.5), sd(cdr1), sd(cdr2))
)
# ggplotを使用して棒グラフを作成
ggplot(df, aes(x = CDR, y = Mean)) + 
  geom_col() +
  labs(x = "CDR", y = "Mean") +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2)
#dplyr(tidyverse)パッケージを使うとまとめるのが楽になる、詳しくはパイプ演算子などを参照、ただいま勉強中
data %>%
  group_by(cdr) %>%
  summarize(mean = mean(.data[["fim_selfcare"]]))




######回帰分析 独立変数をmmseに、目的変数をfim各項目で予測してみる#####
#1: 目的変数fim_totalで単回帰
plot(data$fim_total, data$mmse)
cor.test(data$fim_total, data$mmse)
lm <- lm(formula = data$mmse ~ data$fim_total) #formula=独立変数　~ 目的変数でモデルを立てる
abline(lm) #回帰直線
#2: 目的変数を色々なfimで重回帰分析
table <- data[,c("age", "mmse", "fim_selfcare", "fim_sphincter_control", "fim_mobility", "fim_locomotion", "fim_communication","fim_social_cognition")]
table #モデルに必要な変数をまとめて一つのdata.frameを作る（整理のため）
cor() #各変数の相関分析（高いと怪しい）
lm <- lm(mmse~., table) #重回帰分析（formula, data） ~.は独立変数は全ての列を使う
summary(lm)
lm <- step(lm, direction = "backward") #独立変数を減らすためにstepwise変数減少法
summary(lm) #R-squaredが決定係数
vif(lm) #variance inflation factor(VIF)で多重共線性をチェック criterion<10 
#Tips: ロジスティック回帰分析ならglm


####forループやif条件文の活用####

#cdr別でのとfim各項目のt検定を一個ずつ実施するのはめんどくさい→for文で回す
#基本構造
result <- c()
for (i in 1:10){  #(i in 1:10)はiを1~10まで回す
  result[i] <- i + 2 #resultのi番目にi+2の数値を代入する
}
result

#matrix, data.frameに対しても同様にできる
x <- matrix(1:100, 10,10)
result <- NULL
for (i in 1:ncol(x)){ #ncolはxの列数を取得
  result[i] <- mean(x[, i]) #xのi列目の平均をresultのi番目に代入
}
rbind(x, result) #末行に平均値を追加
#行や列に対してはapply(x,2,mean)のようにapplyを使うのが簡略



####応用：tidyverseパッケージで色々便利に処理する####
"
tidyverseはggplot2やdplyrといったデータ処理や可視化に便利なツールをひとまとめにしたもの
今までは1列目、３行目といった数字で扱っていたが、変数の並び順や種類が変化した時に対応できるようにする（余談:pythonでは１番目は0から始まる）
全部使いこなすのは大変だがシンプルで汎化できるメリットがある
"

####1: dplyrパッケージの活用####
'
例えば列平均を新たに末行に追加するとき
x <- matrix(1:100, 10,10)
mean <- apply(x, 2, mean)
x <- rbind(x, mean)　としていたがmeanというvalueが無駄に生成されてしまう
x <- rbind(matrix(1:100, 10, 10), apply(matrix(1:100,10,10), 2, mean))としても良いが複雑で分かりづらい
そこで、パイプ演算子%>%を使うことで各処理に対して処理がシンプルに記述される
'
#パイプ演算子%>%は、左の結果を右の第１引数に代入する
x <- matrix(1:100, 10, 10) %>%
  as.data.frame() %>% #データフレームに変換して、
  bind_rows(colMeans(.)) #列平均を行追加する(.は%>%の左の結果)
x

data %>% #dataに対して、
  group_by(gender) %>% #group_byでグループに分けて、
  summarise(mean(mmse)) #mmseのmeanでsummarizeする


result <- data %>%
  filter(mmse >= 20 & cdr != 0) %>%　#mmseが20点以上でかつcdrが0点でない人を抽出して
  select(cdr, starts_with("fim")) %>% #cdrとfimで始まる列を選択して
  group_by(cdr) %>% #cdrでグループに分けて
  summarize(across(everything(), list(mean = mean, sd = sd))) #everythingの列に対して各列(across)のmeanとsdでsummarizeする
result


####2: ggplot2の活用####
"
ggplot2はグラフの便利な描画ツール、通常のplotとは異なり色や軸など細かな設定ができる
ggplotはlayer構造になっており、背景＋軸＋プロット＋直線＋テキストなどを積み重ねていく
先述dplyrデータ加工と合わせる（任意の群の平均値やカウントをしてggplot2でグラフ化)
"
#カラーパレットのインストール
install.packages("RColorBrewer")
library(RColorBrewer)

result <- ggplot(data, mapping = aes(x = fim_motor_item, y = fim_cognitive_item, color = cdr)) + #データの選択、colorは色分け群
  geom_point() + #散布図をプロット
  geom_smooth(method = "lm", se = FALSE) + #回帰直線の作図
  scale_color_brewer(palette = "Blues") + #RClolorBrewerパッケージのパレットを選択
  theme_bw() +  #背景
  labs(title = "scatter plot") #タイトル
result

result + facet_wrap(~cdr) #群分けて表示できる

#dplyrを使い相関係数とp値を算出しておけばggplot上に表示できる(result内でまとめて記述するのが難しい、、色分け以外なら可能)
cor <- data %>%
  group_by(cdr) %>%
  summarize(cor = cor.test(fim_motor_item, fim_cognitive_item)$estimate, p = cor.test(fim_motor_item, fim_cognitive_item)$p.value) %>% #cor.test()$estimate, p.valueで相関係数とp値を取り出す
  mutate(p = ifelse(p < 0.05, "< 0.05", p)) #ifelseは(条件,TRUEの時の出力、FALSEの時の出力)でmutateで上書き
result <- ggplot(data, mapping = aes(x = fim_motor_item, y = fim_cognitive_item, color = cdr)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text(data = cor, aes(label = paste("cor: ", round(cor, 2), "\n", "p-value ",p )), #pasteで文字列と数字を繋げる　\nは改行
            x = max(data$fim_motor_item), y = max(data$fim_cognitive_item), hjust = 1, vjust = 1) + #textの位置を調整
  facet_wrap(~cdr) #分けて表示
result

#棒グラフgeom_barも可能 education以外にもcdrやgenderで分けるのも可能
data %>%
  group_by(cdr, education) %>% #cdrとgenderでグループ分け
  summarize(mean = mean(mmse)) %>% #mmseの平均値を算出(cdr, gender毎)
  ggplot(mapping = aes(x = cdr, y = mean, fill = education)) +
  geom_bar(stat = "identity", position = "dodge") #position = "dodge"で並列表示




#補足： tidyverse(dplyrとggplot2)とforを活用して結果をlist構造（数値やグラフなどまとめて入る箱）にまとめるとシンプル（詳細はchatGPTで）
#今回は目的変数fimとしてvarで群分けして、各群の中で分散分析、tukey多重比較、テーブル、プロットをまとめて実行している
var <- "cdr"  # 変更したい説明変数の名前(genderやeducation, cdrといった質的変数)を自由に入れる
result <- list() #カラのlistを作成
for (i in colnames(select(data, starts_with("fim")))){  #fimで始まる列のみ取り出すselect(data, starts_with(" "))
  result[[i]] <- list(
    aov = aov(data[,i] ~ data[, var]), #分散分析の結果:aov
    p_value = summary(aov(data[,i] ~ data[, var]))[[1]]$Pr[[1]], #分散分析のp値のみ(pre-test):p_value
    tukey = TukeyHSD(aov(data[,i] ~ data[, var])), #Tukey-krammer method多重比較補正後の結果(post-hoc):tukey 
    table = data %>%     #各群の平均値と標準偏差の表:table
      group_by(!!sym(var)) %>% #!!sym()はシンボリックにする"cdr"をcdrに変える
      summarise(mean_value = mean(.data[[i]]), sd_value = sd(.data[[i]])), 
    plot = data %>% #棒グラフ:plot
      group_by(!!sym(var)) %>%
      summarise(mean_value = mean(.data[[i]]), sd_value = sd(.data[[i]])) %>%
      ggplot(aes(x = !!sym(var), y = mean_value)) +
      geom_col() +
      geom_errorbar(mapping = aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value), width = 0.2) 
  )
}
#結果を参照するにはこちら(見たい要素に書き換えるだけでグラフも分散分析も直ぐに見れる！R便利！！)
lapply(result, '[[',"tukey") #全体の参照：lapply(result, '[[', "要素tukeyやp_valueなど")
result[["fim_selfcare"]][["table"]] #各項目の参照：result[[目的変数]][[要素tableやp_valueなど]]
 




#まとめて
{
data <- read.csv("/Users/jumpeikudo/Desktop/R/dementia_sample/dementia_data.csv")
data <- na.omit(data)
data$education <- as.factor(data$education)
data$cdr <- as.factor(data$cdr)
fim_motor_item <- data[,c("fim_selfcare","fim_sphincter_control","fim_mobility","fim_locomotion")]
fim_motor_item <- apply(fim_motor_item, 1, sum )
data <- cbind(data, fim_motor_item)
fim_cognitive_item <- apply(data[,11:12],1,sum)
data <- cbind(data, fim_cognitive_item)
fim_total <- fim_motor_item + fim_cognitive_item
data <- cbind(data, fim_total)
}

#memo
"
package tidyverse = dplyr(filter, select, mutate, group_by, summarize, ), ggplot2(ggplot, geom_col///), ///
apply familiy (apply, mapply, sapply, lapply, tapply)

"

