library(tidyverse)


# combine_wage_excels.R
# 説明:
# data/以下に年次ごとのExcelファイル(.xlsx/.xls)が置かれているという前提で、
# 各シートを読み取り、
# 列: 年次, 都道府県, 産業, 性別, 年齢階級, 企業規模, 属性, 値
# のロング形式に統合します。
#
# 依存パッケージ: tidyverse, readxl, janitor, stringr, fs, glue, zoo
# (必要に応じて install.packages("xxx") してください)


library(tidyverse)
library(readxl)
library(janitor)
library(stringr)
library(fs)
library(glue)
library(zoo)

# ---- 県名のパターン（かっこ有無・県/府/都の揺れ対応） ----
.pref_full <- c(
  "北海道","青森県","岩手県","宮城県","秋田県","山形県","福島県",
  "茨城県","栃木県","群馬県","埼玉県","千葉県","東京都","神奈川県",
  "新潟県","富山県","石川県","福井県","山梨県","長野県","岐阜県",
  "静岡県","愛知県","三重県","滋賀県","京都府","大阪府","兵庫県",
  "奈良県","和歌山県","鳥取県","島根県","岡山県","広島県","山口県",
  "徳島県","香川県","愛媛県","高知県","福岡県","佐賀県","長崎県",
  "熊本県","大分県","宮崎県","鹿児島県","沖縄県"
)
.pref_core <- c(
  "北海道","青森","岩手","宮城","秋田","山形","福島",
  "茨城","栃木","群馬","埼玉","千葉","東京","神奈川",
  "新潟","富山","石川","福井","山梨","長野","岐阜",
  "静岡","愛知","三重","滋賀","京都","大阪","兵庫",
  "奈良","和歌山","鳥取","島根","岡山","広島","山口",
  "徳島","香川","愛媛","高知","福岡","佐賀","長崎",
  "熊本","大分","宮崎","鹿児島","沖縄"
)
.pref_variants <- unique(c(
  .pref_full,
  # 東京都/京都府/大阪府の「都/府」表記とコア名
  "東京都","京都府","大阪府",
  .pref_core
))

# ---- 会社規模と属性の候補辞書 ----
company_sizes <- c("企業規模計（10人以上）","1,000人以上","100～999人","10～99人")
attributes <- c(
  "年齢","勤続年数","所定内実労働時間数","超過実労働時間数",
  "きまって支給する現金給与額","年間賞与その他特別給与額","労働者数","所定内給与額"
)
gender_values <- c("男女計","男","女")

# ---- ユーティリティ ----
strip_leading_code <- function(x) {
  # 先頭の「01-01」「01_01」「1-1」等のコードと空白類を除去（全角/半角の混在も許容）
  x %>%
    str_replace("^\\s*[0-9０-９]+[\\-‐–—−_]?[0-9０-９]+\\s*", "") %>%
    str_replace("^\\s*[0-9０-９]+\\s*", "")
}

parse_pref_industry <- function(sheet_name) {
  # 余分なコードや空白を除去
  s <- sheet_name %>%
    strip_leading_code() %>%
    stringr::str_replace_all("\\s", "")   # 全角/半角スペース除去
  
  # 候補（都道府県の表記ゆれ＋括弧付きも追加）
  pref_full <- c(
    "北海道","青森県","岩手県","宮城県","秋田県","山形県","福島県",
    "茨城県","栃木県","群馬県","埼玉県","千葉県","東京都","神奈川県",
    "新潟県","富山県","石川県","福井県","山梨県","長野県","岐阜県",
    "静岡県","愛知県","三重県","滋賀県","京都府","大阪府","兵庫県",
    "奈良県","和歌山県","鳥取県","島根県","岡山県","広島県","山口県",
    "徳島県","香川県","愛媛県","高知県","福岡県","佐賀県","長崎県",
    "熊本県","大分県","宮崎県","鹿児島県","沖縄県"
  )
  pref_core <- c(
    "北海道","青森","岩手","宮城","秋田","山形","福島",
    "茨城","栃木","群馬","埼玉","千葉","東京","神奈川",
    "新潟","富山","石川","福井","山梨","長野","岐阜",
    "静岡","愛知","三重","滋賀","京都","大阪","兵庫",
    "奈良","和歌山","鳥取","島根","岡山","広島","山口",
    "徳島","香川","愛媛","高知","福岡","佐賀","長崎",
    "熊本","大分","宮崎","鹿児島","沖縄"
  )
  variants <- unique(c(pref_full, "東京都","京都府","大阪府", pref_core))
  # かっこ付き候補も足す
  variants_with_paren <- paste0("(", variants, ")")
  candidates <- unique(c(variants_with_paren, variants))
  
  # 長い候補を優先（例: "東京都" を "東京" より先に）
  candidates <- candidates[order(nchar(candidates), decreasing = TRUE)]
  
  pref_raw <- NA_character_
  hit <- NA_character_
  for (v in candidates) {
    if (stringr::str_detect(s, stringr::fixed(v))) {
      pref_raw <- v
      hit <- v
      break
    }
  }
  
  # 見つからない場合は NA のまま返す（上流で処理）
  if (is.na(pref_raw)) {
    return(list(pref = NA_character_, industry = s))
  }
  
  # 県名（括弧は剥がす）
  pref <- pref_raw %>%
    stringr::str_replace_all("^\\(|\\)$", "")
  
  # 産業名 = ヒット部分を除去した残り（先頭の記号・読点を整理）
  industry <- s %>%
    stringr::str_replace(stringr::fixed(hit), "") %>%
    stringr::str_replace("^[:：\\-ー・，,、]+", "")
  
  list(pref = pref, industry = industry)
}

is_numeric_like <- function(x) {
  # 数値/小数/欠損ハイフンを除いて判定
  y <- as.character(x)
  ok <- str_detect(y, "^[-−—]?$|^-?\\\\d+(?:[.,]\\\\d+)?$")
  ok[is.na(ok)] <- FALSE
  !is.na(as.numeric(str_replace(y, ",", "."))) | str_detect(y, "^[-−—]$")
}

# ---- シート -> tidy 変換 ----
read_sheet_tidy <- function(path, sheet, year_chr) {
  raw <- readxl::read_excel(path, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
  if (nrow(raw) == 0 || ncol(raw) == 0) return(tibble())
  
  # まず空列・空行を削除
  raw <- janitor::remove_empty(raw, which = c("rows","cols"))
  if (nrow(raw) == 0 || ncol(raw) == 0) return(tibble())
  
  # ★ 仮の列名を必ず付与（NA/"" を排除）
  names(raw) <- paste0("X", seq_len(ncol(raw)))
  
  # 「男女計/男/女」が初めて現れる行をデータ開始行とする
  gender_values <- c("男女計","男","女")
  has_gender <- apply(raw, 1, function(r) any(stringr::str_detect(as.character(r), paste0("^(", paste(gender_values, collapse="|"), ")$")), na.rm = TRUE))
  gender_first_row <- which(has_gender)[1]
  
  # フォールバック: 見つからなければ、非NAの多い行の直前を境界に
  if (is.na(gender_first_row)) {
    nn <- apply(raw, 1, function(r) sum(!is.na(r)))
    gender_first_row <- max(2, which.max(nn))
  }
  
  # ヘッダ行の範囲（最低1行は確保）
  header_rows <- seq_len(max(1, gender_first_row - 1))
  
  # ヘッダ部分を取り出し、再度 仮列名を付与してから文字化
  hdr <- raw[header_rows, , drop = FALSE]
  names(hdr) <- paste0("H", seq_len(ncol(hdr)))
  hdr <- hdr %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  hdr <- hdr %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~stringr::str_replace_all(., "\\s+", "")))
  
  # 列名ベクトルを作る（複数行ヘッダを連結, 空なら "col{j}"）
  colnames_vec <- purrr::map_chr(seq_len(ncol(raw)), function(j) {
    vals <- hdr[[j]]
    vals <- vals[!is.na(vals) & vals != ""]
    if (length(vals) == 0) glue::glue("col{j}") else paste(vals, collapse = "|")
  })
  names(raw) <- colnames_vec
  # ★ ここを追加：重複列名に __dup を付けてユニーク化（'|' は使わない）
  if (anyDuplicated(colnames_vec)) {
    colnames_vec <- make.unique(colnames_vec, sep = "__dup")
  }
  names(raw) <- colnames_vec
  
  # データ本体
  dat <- raw[seq(gender_first_row, nrow(raw)), , drop = FALSE]
  dat <- janitor::remove_empty(dat, which = c("rows"))
  if (nrow(dat) == 0) return(tibble())
  
  # ラベル列（文字が多い列）推定
  is_numeric_like <- function(x) {
    y <- as.character(x)
    y[y %in% c("-", "−", "—")] <- NA_character_
    suppressWarnings(!is.na(as.numeric(stringr::str_replace_all(y, ",", "."))))
  }
  is_text_col <- function(v) {
    vv <- as.character(v)
    ratio_text <- mean(!is.na(vv) & vv != "" & !is_numeric_like(vv))
    isTRUE(ratio_text > 0.5)
  }
  label_cols <- which(purrr::map_lgl(dat, is_text_col))
  if (length(label_cols) == 0) label_cols <- 1:min(2, ncol(dat))
  label_cols <- unique(c(label_cols, 1:min(2, ncol(dat)))) # 念のため
  
  # 性別列の特定
  gen_col <- NA_integer_
  for (idx in label_cols) {
    vals <- unique(na.omit(as.character(dat[[idx]])))
    if (any(stringr::str_detect(vals, paste0("^(", paste(gender_values, collapse="|"), ")$")))) { gen_col <- idx; break }
  }
  if (is.na(gen_col)) gen_col <- label_cols[1]
  
  # 年齢階級列の特定（「歳」を含む）
  age_col <- NA_integer_
  for (idx in label_cols) {
    vals <- unique(na.omit(as.character(dat[[idx]])))
    if (any(stringr::str_detect(vals, "歳"))) { age_col <- idx; break }
  }
  if (is.na(age_col)) {
    cand <- label_cols[label_cols != gen_col]
    age_col <- if (length(cand)) cand[1] else min(gen_col + 1, ncol(dat))
  }
  
  keep_cols <- unique(c(gen_col, age_col, setdiff(seq_len(ncol(dat)), label_cols)))
  dd <- dat[, keep_cols, drop = FALSE]
  names(dd)[1:2] <- c("性別","年齢階級")
  
  # 縦結合セルの下方向埋め
  dd <- dd %>% dplyr::mutate(性別 = zoo::na.locf(性別, na.rm = FALSE))
  
  # 値カラムを数値化（"-" は NA）
  if (ncol(dd) > 2) {
    dd <- dd %>% dplyr::mutate(dplyr::across(-(1:2), ~{
      x <- as.character(.)
      x[x %in% c("-", "−", "—")] <- NA_character_
      x <- stringr::str_replace_all(x, ",", "")
      suppressWarnings(as.numeric(x))
    }))
  }
  
  # dd を作った直後あたりに追加
  # 例: names(dd)[1:2] <- c("性別","年齢階級") のすぐ後
  
  # --- 値列の存在チェック（なければスキップ） ---
  value_cols <- setdiff(names(dd), c("性別","年齢階級"))
  # 値列そのものが無い / 1列も数値化できていない場合は捨てる
  if (length(value_cols) == 0 || all(colSums(!is.na(dd[value_cols])) == 0)) {
    return(tibble())
  }
  
  # 念のため: 列数が2以下（= 性別・年齢階級しか無い）もスキップ
  if (ncol(dd) <= 2) {
    return(tibble())
  }
  
  # ロング化→列ヘッダを「企業規模|属性」に分解
  long <- dd %>%
    tidyr::pivot_longer(cols = -(1:2), names_to = "colheader", values_to = "値") %>%
    tidyr::separate(colheader, into = c("企業規模","属性"), sep = "\\|", fill = "right", extra = "merge")
  
  # 正規化（辞書該当のみ）
  company_sizes <- c("企業規模計（10人以上）","1,000人以上","100～999人","10～99人")
  attributes <- c("年齢","勤続年数","所定内実労働時間数","超過実労働時間数",
                  "きまって支給する現金給与額","年間賞与その他特別給与額","労働者数","所定内給与額")
  
  size_pat <- paste(stringr::str_replace_all(company_sizes, "\\(", "\\\\("), collapse="|")
  attr_pat <- paste(attributes, collapse="|")
  
  long <- long %>%
    dplyr::mutate(
      企業規模 = stringr::str_extract(企業規模, size_pat),
      属性     = stringr::str_extract(paste(企業規模, 属性, sep="|"), attr_pat)
    ) %>%
    dplyr::filter(!is.na(企業規模), !is.na(属性))
  
  # シート名から 都道府県 / 産業 を取得
  strip_leading_code <- function(x) {
    x %>%
      stringr::str_replace("^\\s*[0-9０-９]+[\\-‐–—−_]?[0-9０-９]+\\s*", "") %>%
      stringr::str_replace("^\\s*[0-9０-９]+\\s*", "")
  }

  meta <- parse_pref_industry(sheet)
  
  tibble::tibble(
    年次 = as.integer(year_chr),
    都道府県 = meta$pref,
    産業 = meta$industry
  ) %>%
    dplyr::bind_cols(long %>% dplyr::select(性別, 年齢階級, 企業規模, 属性, 値)) %>%
    dplyr::filter(!is.na(値))
  }

# ---- ファイル全体の読み込み ----
list_excel_files <- function(data_dir = "data") {
  # 1) パスの展開（~ などを安全に展開）
  data_dir <- fs::path_expand(data_dir)
  
  # 2) まずは存在確認＆中身の簡易ツリーを出す（デバッグ用）
  if (!fs::dir_exists(data_dir)) {
    message("指定フォルダが見つかりません: ", data_dir,
            "\ngetwd() = ", getwd())
    return(character(0))
  }
  
  # 3) 大文字小文字を問わず .xls / .xlsx を拾う（glob はケース非依存）
  #   - ネストした年次フォルダ配下も再帰
  files <- fs::dir_ls(data_dir, recurse = TRUE, glob = "**/*.{xls,xlsx,XLS,XLSX}")
  
  # 4) 万一 3) で拾えないときのフォールバック（base::list.files）
  if (length(files) == 0) {
    files <- list.files(data_dir, pattern = "\\.xls[xX]?$", recursive = TRUE,
                        full.names = TRUE, ignore.case = TRUE)
  }
  
  # 5) 見つかった数を表示（デバッグ用）
  message("Found Excel files: ", length(files))
  if (length(files) <= 10) message(paste(files, collapse = "\n"))
  files
}


read_all_years_safe <- function(data_dir = "data", workers = max(1, parallel::detectCores()-2)) {
  files <- list_excel_files(data_dir)
  sheet_index <- tibble(file = files) |>
    mutate(year  = stringr::str_extract(file, "(19|20)\\d{2}") %>% as.integer(),
           sheet = map(file, readxl::excel_sheets)) |>
    unnest(sheet) |>
    mutate(idx = row_number())
  
  plan(multisession, workers = workers)
  
  log_path <- "parse_errors.csv"
  if (file.exists(log_path)) file.remove(log_path)
  
  safe_read <- function(file, sheet, year, idx) {
    out <- try(read_sheet_tidy(file, sheet, year), silent = TRUE)
    if (inherits(out, "try-error")) {
      tibble(idx=idx, file=file, sheet=sheet, year=year,
             error=as.character(attr(out, "condition")$message %||% "unknown")) |>
        write.table(log_path, sep=",", row.names=FALSE, col.names=!file.exists(log_path), append=TRUE)
      return(tibble()) # スキップ
    }
    out
  }
  
  future_pmap_dfr(
    list(sheet_index$file, sheet_index$sheet, sheet_index$year, sheet_index$idx),
    safe_read,
    .progress = TRUE
  )
}

result <- read_all_years_safe("data")
# 完成物を保存
readr::write_csv(result, "wage_panel_tidy.csv", na = "")
# 失敗ログ（あれば）
if (file.exists("parse_errors.csv")) readr::read_csv("parse_errors.csv", show_col_types = FALSE) %>% print(n=20)





# 
# 
# library(tidyverse)
# library(readxl)
# library(future)
# library(furrr)
# 
# # 1) ファイル×シートのインデックス表を作る（変数名は sheet_index に）
# files <- list_excel_files("data")
# 
# sheet_index <- tibble(file = files) |>
#   mutate(
#     year  = stringr::str_extract(file, "(19|20)\\d{2}") %>% as.integer(),
#     sheet = map(file, readxl::excel_sheets)
#   ) |>
#   unnest(sheet) |>
#   mutate(idx = row_number()) |>
#   relocate(idx, file, sheet, year)
# 
# # 2) 並列で“読むだけテスト”
# plan(multisession, workers = max(1, parallel::detectCores() - 2))
# 
# check_one <- function(file, sheet, year) {
#   tryCatch({
#     invisible(read_sheet_tidy(file, sheet, year))
#     tibble(status = "ok", msg = NA_character_)
#   }, error = function(e) {
#     tibble(status = "error", msg = conditionMessage(e))
#   })
# }
# 
# res <- future_pmap_dfr(
#   list(sheet_index$file, sheet_index$sheet, sheet_index$year),
#   check_one,
#   .progress = TRUE
# )
# 
# diag <- bind_cols(sheet_index, res)
# bad  <- filter(diag, status == "error")
# 
# # 問題箇所の上位を確認
# bad %>% arrange(idx) %>% slice_head(n = 20)
