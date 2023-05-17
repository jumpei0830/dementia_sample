#Project: dementia_advanced


####応用１：tidyverseパッケージの活用####

#データは基本で使用したdataを使用

"
tidyverseはggplot2やdplyrといったデータ処理や可視化に便利なツールをひとまとめにしたもの
全部使いこなすのは大変だがシンプルで汎化できるメリットがある
R解説ブログなどでも多用しているので知っておく必要あり
"
library(tidyverse)

####1: dplyrパッケージの活用####
'
例えば列平均を新たに末行に追加するとき
x <- matrix(1:100, 10,10)
mean <- apply(x, 2, mean)
x <- rbind(x, mean)　としていたがmeanというvalueが無駄に生成されてしまう
x <- rbind(matrix(1:100, 10, 10), apply(matrix(1:100,10,10), 2, mean))としても良いが複雑で分かりづらい
そこで、パイプ演算子%>%を使うことで各処理に対して処理がシンプルに記述される
'
#パイプ演算子%>%は、左の結果を右の第１引数に代入する、右では第一引数を省略できる
x <- matrix(1:100, 10, 10) %>%
  as.data.frame() %>% #データフレームに変換して、
  bind_rows(colMeans(.)) #列平均を行追加する(.は%>%の左の結果), colMeansで列平均を算出
x

data %>% #dataに対して、
  group_by(gender) %>% #group_byでgenderグループに分けて、
  summarise(mean(mmse)) #mmseのmeanでsummarizeする


result <- data %>%
  filter(mmse >= 20 & cdr != 0) %>%　#mmseが20点以上でかつcdrが0点でない人(!は余事象)を抽出して
  select(cdr, starts_with("fim")) %>% #cdrとfimで始まる列(starts_with)を選択して
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

#dplyrを使い相関係数corとp値を算出しておけばggplot上に表示できる(result内でまとめて記述するのが難しい、、色分け以外なら可能)
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




#活用： tidyverse(dplyrとggplot2)とforを活用して結果をlist構造（数値やグラフなどまとめて入る箱）にまとめるとシンプル（詳細はchatGPTで）
#今回は目的変数fimとしてvarで群分けして、各群の中で分散分析、tukey多重比較、テーブル、プロットをまとめて実行して一つに保存
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
result[["fim_total"]][["plot"]] #各項目の参照：result[[目的変数]][[要素tableやp_valueなど]]



#データ読み込み これをtidyverse使うとどれくらいシンプルになるでしょう？
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
#少しシンプルになった!
data <- read.csv("/Users/jumpeikudo/Desktop/R/dementia_sample/dementia_data.csv") %>% 
  na.omit() %>%
  mutate(education = as.factor(education),
         cdr = as.factor(cdr),
         fim_motor_item = fim_selfcare + fim_sphincter_control + fim_mobility + fim_locomotion,
         fim_cognitive_item = fim_communication + fim_social_cognition,
         fim_total = fim_motor_item + fim_cognitive_item)


####応用２：　テキストデータの解析####
"
日本語のテキストデータの解析では、形態素解析（品詞分解）から始めていく
形態素解析ではMeCabパッケージを使用する。使用方法はRMeCabホームページを参照する（MeCab本体をインストールしてからRMeCabを使う）
詳しくは http://rmecab.jp/wiki/index.php?RMeCabFunctions
"

#必要なパッケージの準備
install.packages("RMeCab", repos = "https://rmecab.jp/R") 
Sys.setenv(MECABRC = "/opt/homebrew/etc/mecabrc")　#mecabrc引数に設定ファイルを指定（homebrewでinstallした人用：基本なくてもok、原因不明）
#install.packages("extrafont") #文字化け対策
library(RMeCab)
library(tidyverse)
#library(extrafont) #文字化けする場合に使用、なければ無視
#font_import()  
#loadfonts()  


#動作確認
RMeCabC("こんにちは")
RMeCabC("すもももももももものうち")
#RMeCabC("すもももももももものうち", mecabrc = "/opt/homebrew/etc/mecabrc") 

#データ読み込み(data.frame) ※ RMeCabはデータを直接読み込むことが多い
talk <- read.table("dementia_talk.txt")   #テキストデータは架空でchatGPTで生成してもらいました
head(talk) 

####1: wordcloud可視化####
install.packages("wordcloud")
library(wordcloud)
result <- RMeCabFreq("dementia_talk.txt")  #頻度分析、ファイルを直接指定
result
result[order(result$Freq, decreasing = TRUE), ] %>% head(30) #頻度多い順(decreasing)に30個表示(head())
result <- subset(result, Info1 %in% c("名詞", "動詞")) #名詞と動詞を抽出,subsetは行指定、selectは列指定
par(family = "HiraKakuProN-W3") 
wordcloud(result$Term, result$Freq)
#Mac等日本語文字化けする場合は以下でフォント指定（任意のフォントに設定するには？？）
wordcloud(result$Term, result$Freq, family = "HiraKakuProN-W3")

####2: 共起ネットワーク可視化####
install.packages("igraph")
library(igraph)
result <- NgramDF("dementia_talk.txt", type = 1, N = 2)   #N-gram(連続N個の単語で分割)、typeは0が最小単位,1が単語,2が品詞
result
result <- result %>% filter(!str_detect(Ngram1, "こと") & !str_detect(Ngram2, "こと")) #「こと」が多いので削除(!は余事象&は∩なので、「こと」以外をfilter)
result
result %>% subset(Freq > 2) %>% arrange(-Freq) #Freq nより上を降順(-)で表示(arrange)
result %>% subset(Freq > 2) %>% #共起ネットワーク作図(文字化け可能性あり)
  graph.data.frame(directed = FALSE) %>%
  plot(edge.label = E(.)$Freq, edge.width = E(.)$Freq/max(E(.)$Freq) * 10)　#edge.パラメータでFreq数値表示や太さ調整

#参考： TF-IDFによる文書ごとの特徴を算出(複数文書ある場合に最適) 
# TF(term frequency): term frequency in document / total words in document 
# IDF(inverse document frequency): 1 + log2(total documents / documents with term)
arrange(RMeCabFreq("dementia_talk.txt"), -Freq) %>% head(100)
result <- docDF(talk, column = 1, type = 1, N = 1, pos = c("名詞"), weight = "tf*idf") %>%  #tf-idf表示（今回は行ごとで）
  filter(POS2 == "一般") #一般名詞のみ
result
     
####3: 感情分析####
install.packages("magrittr") #%<>%使用(a <- a %>%と同値)
library(magrittr)
# 単語感情極性対応表の取得(東京工業大学高村研究室による配布)
dic <- read.table("http://www.lr.pi.titech.ac.jp/~takamura/pubs/pn_ja.dic", 
                  sep = ":", stringsAsFactors = FALSE, fileEncoding = "CP932", encoding = "UTF-8") #windowならshift-JIS?
head(dic) #V4が感情の程度を示している
dic %<>% select(V1, V4) %>% rename(TERM = V1, Params = V4) %>% distinct(TERM, .keep_all = TRUE)　#V1, V4使用、distinct重複削除
head(dic)
#テキストデータ用意
result <- RMeCabFreq("dementia_talk.txt") %>% rename(TERM = Term) %>%  
  filter(Info1 == c("名詞", "動詞", "形容詞")) #必要な品詞選択
head(result)
#シンプルな感情分析実行
result %<>% left_join(dic)  #resultにdicを対応付け結合(TERM一致した時にdicのV4を追加)
result %>% arrange(desc(Params)) %>% head(20) #positive上位
result %>% arrange(Params) %>% head(20) #negative上位

#横軸を時間、縦軸を感情にして、感情の推移をグラフ化させることもできる
install.packages("zoo")
library(zoo)
result <- RMeCabText("dementia_talk.txt") %>% as.data.frame() 
result <- result[1,] %>% t() %>% as.data.frame() #文章の流れで単語だけ抽出（並び替えない）
colnames(result) <- "TERM"
#感情分析(naはグラフ化の邪魔なのでomit)
result %<>% left_join(dic) 
result %<>% na.omit()
result <- rollmean(result$Params,2) #zooパッケージで移動平均
plot(result, ylim = c(-1,1), type = "l", xlab = "time flow", ylab = "emotion")
abline(h=0)
