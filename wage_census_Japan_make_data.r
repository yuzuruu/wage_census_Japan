# =========================================================
# Prefecture-level wage census of Japan
# Part 1 Read and make a tidy data
# 22nd. September 2025
# Yuzuru Utsunomiya, Ph. D.
# 
# 
# =========================================================

# ----- read.library -----
library(tidyverse)
library(readxl)
library(fs)
library(stringi)
library(rlang)
library(arrow)
library(sf)
library(furrr)
# set strategy for furrr::future_()
plan(multisession)
# 
# ----- definition -----
items <- c(
  "年齢","勤続年数","所定内実労働時間数","超過実労働時間数",
  "きまって支給する現金給与額","所定内給与額","年間賞与その他特別給与額","労働者数"
)
sizes <- c("合計","1000人以上","100-999人","10-99人")
COLS  <- as.character(outer(items, sizes, paste, sep = "_"))  # 8×4=28
GENDER_AGE <- c(
  "男女計",
  "男女計_～１９歳","男女計_２０～２４歳","男女計_２５～２９歳","男女計_３０～３４歳","男女計_３５～３９歳",
  "男女計_４０～４４歳","男女計_４５～４９歳","男女計_５０～５４歳","男女計_５５～５９歳","男女計_６０～６４歳",
  "男女計_６５～６９歳","男女計_７０歳～",
  "男",
  "男_～１９歳","男_２０～２４歳","男_２５～２９歳","男_３０～３４歳","男_３５～３９歳",
  "男_４０～４４歳","男_４５～４９歳","男_５０～５４歳","男_５５～５９歳","男_６０～６４歳",
  "男_６５～６９歳","男_７０歳～",
  "女",
  "女_～１９歳","女_２０～２４歳","女_２５～２９歳","女_３０～３４歳","女_３５～３９歳",
  "女_４０～４４歳","女_４５～４９歳","女_５０～５４歳","女_５５～５９歳","女_６０～６４歳",
  "女_６５～６９歳","女_７０歳～"
  ) 
# Exact prefecture name
.pref_full <- c(
  "北海道","青森県","岩手県","宮城県","秋田県","山形県","福島県",
  "茨城県","栃木県","群馬県","埼玉県","千葉県","東京都","神奈川県",
  "新潟県","富山県","石川県","福井県","山梨県","長野県","岐阜県",
  "静岡県","愛知県","三重県","滋賀県","京都府","大阪府","兵庫県",
  "奈良県","和歌山県","鳥取県","島根県","岡山県","広島県","山口県",
  "徳島県","香川県","愛媛県","高知県","福岡県","佐賀県","長崎県",
  "熊本県","大分県","宮崎県","鹿児島県","沖縄県"
)
# Essential prefecture name
.pref_core <- c(
  "北海道","青森","岩手","宮城","秋田","山形","福島",
  "茨城","栃木","群馬","埼玉","千葉","東京","神奈川",
  "新潟","富山","石川","福井","山梨","長野","岐阜",
  "静岡","愛知","三重","滋賀","京都","大阪","兵庫",
  "奈良","和歌山","鳥取","島根","岡山","広島","山口",
  "徳島","香川","愛媛","高知","福岡","佐賀","長崎",
  "熊本","大分","宮崎","鹿児島","沖縄"
)
# fix variants of prefectures' names
.pref_variants <- 
  unique(
    c(
      .pref_full, 
      "東京都","京都府","大阪府", 
      .pref_core
      ))
.pref_variants <- 
  .pref_variants[order(nchar(.pref_variants), decreasing = TRUE)] 
# 
# ----- utilities -----
# Remove ASCII, space, alphabets, and numbers
clean_sheet_name <- function(x) {
  x %>%
    stringi::stri_trans_nfkc() %>%
    stringr::str_replace_all(
      "[A-Za-z0-9[:punct:]\\s]+", 
      ""
      ) %>% 
    stringr::str_squish()
}
# Extract 4-digits year from folders' names
extract_year <- function(path) {
  y <- str_extract(path, "(19|20)\\d{2}")
  as.integer(y)
}
# Parse prefecture / industry from sheets' names
parse_pref_industry_from_sheet <- function(sheet_raw) {
  s <- 
    clean_sheet_name(sheet_raw)
  if (is.na(s) || s == "") return(list(pref = NA_character_, industry = NA_character_))
  hit <- NA_character_
  for (v in .pref_variants) {
    if (str_detect(s, fixed(v))) { hit <- v; break }
  }
  if (is.na(hit)) {
    list(pref = NA_character_, industry = s)
  } else {
    pref <- gsub("^\\(|\\)$", "", hit)
    industry <- s %>%
      str_replace(fixed(hit), "") %>%
      str_replace("^[:：\\-ー・，,、]+", "")
    list(pref = pref, industry = industry)
  }
}
normalize_value_vec <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, ",", "")
  x <- dplyr::na_if(x, "-"); x <- na_if(x, "－"); x <- na_if(x, "—"); x <- na_if(x, "–")
  suppressWarnings(as.numeric(x))
}
# 
# ----- read_a_sheet -----
# A function reading a sheet
read_one_sheet <- function(file, sheet) {
  year <- extract_year(file)
  # Fix reading ranges of target cell
  rng  <- "D12:AI50"
  # Read data
  # If failed, the process is skipped.
  # (No skips were there)
  raw <- 
    tryCatch(
      readxl::read_excel(
        file, 
        sheet = sheet, 
        range = rng, 
        col_names = FALSE,
        col_types = "text"
        ),
      error = function(e) tibble()
      )
  if (!is.data.frame(raw) || nrow(raw) == 0) return(tibble())
  # Adjust the N. of columns
  if (ncol(raw) < length(COLS)) {
    raw <- dplyr::as_tibble(raw)
    raw <- dplyr::bind_cols(
      raw, tibble(
        matrix(
          NA, 
          nrow = nrow(raw), 
          ncol = length(COLS) - ncol(raw)
          )
        )
      )
  } else if (ncol(raw) > length(COLS)) {
    raw <- raw[, seq_len(length(COLS))]
  }
  names(raw) <- COLS
  # Adjust the N. of rows
  if (nrow(raw) < length(GENDER_AGE)) return(tibble())
  if (nrow(raw) > length(GENDER_AGE)) raw <- raw[seq_len(length(GENDER_AGE)), , drop = FALSE]
  # Transform into character
  raw <- raw %>%
    dplyr::mutate(
      across(
        all_of(COLS), 
        as.character
        )
      )
  # Change names and move to left
  raw <- 
    raw %>%
    dplyr::mutate(
      `性別_年齢` = GENDER_AGE
      ) %>%
    dplyr::relocate(
      `性別_年齢`, .before = 1
      )
  # Transform longer style
  long <- 
    raw %>%
    tidyr::pivot_longer(
      cols = -`性別_年齢`, 
      names_to = "項目_企業規模", 
      values_to = "値"
      ) %>%
    dplyr::mutate(
      値 = normalize_value_vec(値)
      )
  meta <- parse_pref_industry_from_sheet(sheet)
  # CConvert
  dplyr::tibble(
    年次     = year,
    都道府県 = meta$pref,
    産業     = meta$industry,
    シート名 = clean_sheet_name(sheet)
    ) %>%
    # merge in column
    dplyr::bind_cols(long)
}
# A function reading data and saving the data with .feather
# Location：out_sheets
save_one_sheet <- 
  function(d, j, out_dir = "out_sheets", feather_only = TRUE) {
    if (!nrow(d)) return(invisible())
    fs::dir_create(out_dir)
  y   <- d$年次[1]
  pr  <- d$都道府県[1] %||% "NA"
  ind <- d$産業[1]     %||% "NA"
  base <- sprintf("%d_%s_%s_%03d", y, pr, ind, j) %>% 
    stringi::stri_trans_nfkc()  %>% 
    stringr::str_replace_all("[\\s/\\\\:|?*<>\"']", "_")
  # swet saving location。
  feather_path <- 
    file.path(out_dir, paste0(base, ".feather"))
  # save
  arrow::write_feather(d, feather_path)
  # 
  # if (!feather_only) {
  #   csv_path <- file.path(out_dir, paste0(base, ".csv"))
  #   readr::write_csv(d, csv_path, na = "")
  # }
}
# 
# ----- combine. data -----
# Read all data
library(furrr)
# strategy
plan(multisession, workers = parallel::detectCores() - 1)

read_all <- function(data_dir = "data",
                     save_per_sheet = TRUE,
                     out_dir = "out_sheets",
                     feather_only = TRUE) {
  
  data_dir <- fs::path_expand(data_dir)
  if (!fs::dir_exists(data_dir)) stop("フォルダが見つかりません: ", data_dir)
  
  files_fs_glob <- fs::dir_ls(
    data_dir, recurse = TRUE, type = "file",
    glob = "**/*.{xls,xlsx,XLS,XLSX,xlsm,XLSM}"
  )
  files_fs_re <- fs::dir_ls(
    data_dir, recurse = TRUE, type = "file",
    regexp = "\\.(xls|xlsx|xlsm)$"
  )
  files_base <- list.files(
    data_dir, pattern="\\.(xls|xlsx|xlsm)$",
    recursive=TRUE, full.names=TRUE, ignore.case=TRUE
  )
  
  files <- unique(c(files_fs_glob, files_fs_re, files_base))
  message("Excelファイル数: ", length(files))
  if (length(files) == 0) return(tibble())
  
  furrr::future_map_dfr(
    files,
    function(f) {
      sh <- readxl::excel_sheets(f)
      # 内側は通常の map_dfr（逐次）
      map_dfr(seq_along(sh), function(j) {
        s <- sh[j]
        d <- read_one_sheet(f, s)
        if (save_per_sheet) save_one_sheet(d, j, out_dir = out_dir, feather_only = feather_only)
        d
      })
    },
    .options = furrr::furrr_options(scheduling = 1) # 大きめジョブを均等に配る
  )
}
# 
# ----- read.file -----
# 
df <- 
  read_all(
    "data", 
    save_per_sheet = TRUE, 
    out_dir = "out_sheets", 
    feather_only = TRUE
    )
# save with .feather
arrow::write_feather(
  df, 
  "wage_panel_tidy.feather"
  )
# .feather save location
OUT_DIR <- "out_sheets"
# Combine target data files (N=36,000)
# Obtain names of target files
files <- 
  fs::dir_ls(
    OUT_DIR, 
    glob = "*.feather", 
    type = "file"
    )
message("feather files: ", length(files))
# merge
df <- furrr::future_map_dfr(files, arrow::read_feather)
arrow::write_feather(df, "wage_census_prefecture_original.feather")
# 
# ----- proof.data -----
# つくったデータ中にある表記揺れを修正
# 正式都道府県辞書
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
  "北海道","青森","岩手","宮城","秋田","山形","福島","茨城","栃木","群馬","埼玉","千葉",
  "東京","神奈川","新潟","富山","石川","福井","山梨","長野","岐阜","静岡","愛知","三重",
  "滋賀","京都","大阪","兵庫","奈良","和歌山","鳥取","島根","岡山","広島","山口",
  "徳島","香川","愛媛","高知","福岡","佐賀","長崎","熊本","大分","宮崎","鹿児島","沖縄"
)
# 
pref_map <- tibble(
  都道府県_raw = c(pref_full, "東京都","京都府","大阪府", pref_core),
  都道府県_std = c(pref_full, "東京都","京都府","大阪府",
               c("北海道","青森県","岩手県","宮城県","秋田県","山形県","福島県",
                 "茨城県","栃木県","群馬県","埼玉県","千葉県","東京都","神奈川県",
                 "新潟県","富山県","石川県","福井県","山梨県","長野県","岐阜県",
                 "静岡県","愛知県","三重県","滋賀県","京都府","大阪府","兵庫県",
                 "奈良県","和歌山県","鳥取県","島根県","岡山県","広島県","山口県",
                 "徳島県","香川県","愛媛県","高知県","福岡県","佐賀県","長崎県",
                 "熊本県","大分県","宮崎県","鹿児島県","沖縄県"))
) %>% distinct()
# normalize names
normalize_size <- function(x){
  x <- as.character(x)
  x <- dplyr::recode(x,
                     "1000人以上" = "1,000人以上",
                     "100-999人"  = "100～999人",
                     "10-99人"    = "10～99人",
                     .default = x
  )
  x
}
#
df_norm <- 
  df %>%
  dplyr::mutate(
    性別_年齢 = stringi::stri_trans_nfkc(性別_年齢) %>% stringr::str_squish()
  ) %>%
  dplyr::mutate(
    性別 = dplyr::case_when(
      stringr::str_detect(性別_年齢, "^男女計(?:_|$)") ~ "男女計",
      stringr::str_detect(性別_年齢, "^男(?:_|$)")     ~ "男",
      stringr::str_detect(性別_年齢, "^女(?:_|$)")     ~ "女",
      TRUE ~ NA_character_
    ),
    年齢階級 = 性別_年齢 %>%
      stringr::str_remove("^男女計(?:_|$)") %>%
      stringr::str_remove("^男(?:_|$)") %>%
      stringr::str_remove("^女(?:_|$)") %>%
      dplyr::na_if(""),
    年齢階級 = dplyr::if_else(性別 == "男女計" & (is.na(年齢階級) | 年齢階級 == ""),
                          "総数", 年齢階級)
  ) %>%
  # 
  dplyr::left_join(
    pref_map, 
    by = c("都道府県" = "都道府県_raw"
           )
    ) %>%
  dplyr::mutate(都道府県 = coalesce(都道府県_std, 都道府県)) %>%
  dplyr::select(-都道府県_std) %>%
  # 
  dplyr::mutate(
    産業 = if_else(
      is.na(産業) | 産業 == "",
      {stringi::stri_trans_nfkc(シート名) %>%
          stringr::str_replace_all("[A-Za-z0-9[:punct:]\\s]+", "")},
      産業
    )
  )
# save
arrow::write_feather(df_norm, "wage_panel_tidy.feather")
# 
# ----- trim.file -----
# Remove unnecessary parts
# read data
wage_panel_tidy <- 
  arrow::read_feather("wage_panel_tidy.feather") %>% 
  data.table::setnames(
    c("year","prefecture","industry","sheet_name","gender_age","attribute","value","gender","age_class")
  )
# definition for transformation
prefecture <- readxl::read_excel("conversion.xlsx", sheet = "prefecture")
industry_name <- readxl::read_excel("conversion.xlsx", sheet = "industry_name")
age_class_table <- readxl::read_excel("conversion.xlsx", sheet = "age_class")
attribute_size <- readxl::read_excel("conversion.xlsx", sheet = "attribute_size")
# pickup
wage_panel <- 
  wage_panel_tidy %>% 
  filter(year > 2009) %>% 
  dplyr::left_join(prefecture, by = join_by(prefecture == name_jp)) %>% 
  dplyr::left_join(industry_name, by = join_by(industry == industry)) %>% 
  dplyr::left_join(age_class_table, by = join_by(age_class == age_class_jp)) %>% 
  dplyr::left_join(attribute_size, by = join_by(attribute == attribute)) %>% 
  dplyr::mutate(
    gender = dplyr::case_when(
      gender == "男女計" ~ "total",
      gender == "男" ~ "male",
      gender == "女" ~ "female",
      TRUE ~ "hoge"
    )
  ) %>% 
  dplyr::select(year, gender, name_en_prefecture, division, major_group, industry_name_en, age_class_en, attribute_en, company_size_en,value) %>% 
  data.table::setnames(
    c(
      "year", "gender", "prefecture", "division", "major_group", "industry", "age_class", "attribute", "company_size", "value"
      )
    ) %>% 
  tidyr::pivot_wider(
    names_from = attribute,
    values_from = value
    ) %>% 
  dplyr::filter(!age_class %in% c("under_19","70_and_over","total")) %>% 
  dplyr::filter(!gender %in% c("total")) %>% 
  dplyr::filter(!company_size %in% c("total")) %>% 
  dplyr::mutate(
    age_class = factor(age_class, levels = c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69")),
    company_size = factor(company_size, levels = c("10-99", "100-999", "1000_and_over"))
  ) %>% 
  dplyr::filter(major_group == "00") %>% 
  dplyr::filter(!is.na(age_class)) %>% 
  dplyr::mutate(across(where(is.character), factor)) 
# save
arrow::write_feather(
  wage_panel, 
  "wage_panel.feather"
  )
# ----- end ----- 

