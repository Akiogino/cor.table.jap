# cor.table.jap

研究用の相関表を手早く作るための軽量Rパッケージです。日本語の論文・報告書向けに、<strong>有意記号や小数点位置の揃え（任意）</strong>に対応した表を出力できます。

## 特長
- 下三角の相関表を作成
- 有意確率を `* / ** / ***` で付与
- CSV/Excel出力にそのまま使える表形式

## インストール（GitHub）
```r
install.packages("remotes")
remotes::install_github("Akiogino/cor.table.jap")
```

## チュートリアルの実行
環境を揃えるために `renv` を使います。

```r
renv::restore()
```

```sh
quarto render notebooks/tutorial_cor_table_jap.qmd
```

補足:
- ノートブック内の `pkg_ref` を変更すると、GitHub 参照先（タグ/コミット）を切り替えられます。
- 開発中にローカルの R/ を読み込む場合は `COR_TABLE_JAP_DEV=1` を設定してください。

## 使い方

### 1) 下三角の相関表
```r
library(cor.table.jap)

# 尺度名の例
vars <- c("FFMQ", "STAI", "PSS", "DASS")

tab <- lower_triangle_corr_table(df, vars)
print(tab)

# CSV保存
out <- cbind(変数 = rownames(tab), tab)
write.csv(out, "corr_lowertri.csv", row.names = FALSE, fileEncoding = "UTF-8")
```

### 1-b) 下三角の相関表（r / p / stars の3ファイル＋Excel3シート）
```r
vars <- c("FFMQ", "STAI", "PSS", "DASS")

# CSV（_r/_p/_stars/_r_sig が付く。_r_sigの*列見出しは空白）
write_lower_triangle_corr_tables(df, vars, "corr_lowertri")

# Excel（3シート: r_sig / r / p。r_sigの*列見出しは空白）
# 日本語の変数名は font_name で日本語フォントを指定推奨
write_lower_triangle_corr_xlsx(df, vars, "corr_lowertri.xlsx", font_name = "Hiragino Sans")
```

### 2) 行×列の相関表（特定の項目×尺度など）
```r
rows <- c("FFMQ", "PSS")
cols <- c("STAI", "DASS")

mat <- nice_correlation(df, rows, cols)
print(mat)
```

## 出力形式のルール
- 相関係数は小数点以下2桁に固定
- 有意確率は `* / ** / ***`（r/p/stars を分ける出力では stars 列に出力）
- 旧来の「rと*を1セルにまとめる表」では小数点合わせのために先頭スペースを入れる場合あり（`quote_text` で非改行スペース化可）
- Excel出力は `openxlsx` を利用（既定フォント: Times New Roman）
- 日本語変数名を崩さないために、`write_lower_triangle_corr_xlsx(..., font_name = "日本語フォント名")` の指定を推奨

## よくある質問

### Q. 欠損値がある場合は？
各ペアごとに `complete.cases()` を使って欠損を除外します。

### Q. 別の有意記号に変えたい
`R/corr_tables.R` の `sig` 部分を編集してください。

---

必要に応じて関数や出力形式は拡張可能です。研究室のニーズに合わせてカスタマイズして使ってください。
