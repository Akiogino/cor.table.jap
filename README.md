# cor.table.jap

研究用の相関表を手早く作るための軽量Rパッケージです。日本語の論文・報告書向けに、**小数点位置の揃え**と**有意記号**を前提にした表を出力できます。

## 特長
- 下三角の相関表を作成
- **正の相関は先頭に半角スペース**を入れて小数点位置を揃える
- 有意確率を `* / ** / ***` で付与
- CSV出力にそのまま使える表形式

## インストール（GitHub公開後）
```r
install.packages("devtools")
devtools::install_github("Akiogino/cor.table.jap")
```

## 使い方

### 1) 下三角の相関表
```r
library(cor.table.jap)

vars <- c("PAS_特定項目", "SSS_総合得点", "PSS_総合得点", "DASS_抑うつ")

tab <- lower_triangle_corr_table(df, vars)
print(tab)

# CSV保存
out <- cbind(変数 = rownames(tab), tab)
write.csv(out, "corr_lowertri.csv", row.names = FALSE, fileEncoding = "UTF-8")
```

### 1-b) 下三角の相関表（r / p / stars の3ファイル＋Excel3シート）
```r
vars <- c("PAS_特定項目", "SSS_総合得点", "PSS_総合得点", "DASS_抑うつ")

# CSV（_r/_p/_stars/_r_sig が付く。_r_sigの*列見出しは空白）
write_lower_triangle_corr_tables(df, vars, "corr_lowertri")

# Excel（3シート: r_sig / r / p。r_sigの*列見出しは空白）
write_lower_triangle_corr_xlsx(df, vars, "corr_lowertri.xlsx")
```

### 2) 行×列の相関表（特定の項目×尺度など）
```r
rows <- c("PAS_特定項目", "PAS_2", "PAS_5")
cols <- c("STAI_総合得点", "DASS_総合得点")

mat <- nice_correlation(df, rows, cols)
print(mat)
```

## 出力形式のルール
- 相関係数は小数点以下2桁に固定
- **正の値は先頭を非改行スペースに置換**（小数点を揃えるため）
- 有意確率に `* / ** / ***` を付与
- Excel出力は `openxlsx` を利用（既定フォント: Times New Roman）

## よくある質問

### Q. 欠損値がある場合は？
各ペアごとに `complete.cases()` を使って欠損を除外します。

### Q. 別の有意記号に変えたい
`R/corr_tables.R` の `sig` 部分を編集してください。

---

必要に応じて関数や出力形式は拡張可能です。研究室のニーズに合わせてカスタマイズして使ってください。
