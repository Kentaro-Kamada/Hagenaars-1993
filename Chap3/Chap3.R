library(tidyverse)
library(kamaken)
library(gt)
set.seed(123)

# データ読み込み
data <- read_csv('Chap3/tab3.1.csv')

# tab3.2 --------------------------------------------------------------------------------------

# lemのスクリプト作成
lat <- 'lat 1'
man <- 'man 5'
dim <- 'dim 2 2 2 2 2 2'
mod <- 'mod X A|X B|X C|X D|X E|X'
ite <- 'ite 10000'
dat <- str_c('dat [', str_c(data$n, collapse = ' '), ']')
see <- str_c('see ', rdunif(20, 999, 100))


# ファイルの削除
file.remove(list.files('lem/tab3.2', full.names = T, recursive = T))

# スクリプト書き出し
walk(see, ~{write_lines(c(lat, man, dim, mod, ite, dat, .), 
                        file = str_c('lem/tab3.2/inp/', str_remove(., ' '), '.inp'))})


# inputファイルとoutputファイルのパス指定
inp_path <- list.files('lem/tab3.2/inp', full.names = T)
out_path <- str_replace_all(inp_path, 'inp', 'out')

# lem実行
walk(str_c('lem95', inp_path, out_path, sep = ' '),
     ~{system(., wait = F)
       Sys.sleep(0.4)})


# 最大対数尤度の読み出し
max_loglik <-
  tibble(
    filepath = out_path,
    loglik = map_chr(filepath, read_file)
  ) %>% 
  mutate(
    seed = str_extract(loglik, '(?<=Seed random values   = ).+') %>% 
      parse_double(),
    loglik = str_extract(loglik, '(?<=Log-likelihood       = ).+') %>% 
      parse_double()
  ) %>% 
  arrange(desc(loglik)) %>% 
  mutate(rank = dense_rank(desc(loglik)))

# 適合度の読み出し

tibble(
  filepath = max_loglik$filepath[1],
  output = map_chr(filepath, read_file)
  ) %>% 
  transmute(
    Lsq = str_extract(output, 
                      '(?<=L-squared            = ).+(?=\\(.+\\))') %>% 
      parse_double(),
    Xsq = str_extract(output, 
                      '(?<=X-squared            = ).+(?=\\(.+\\))') %>% 
      parse_double(),
    df = str_extract(output, '(?<=Degrees of freedom   = ).+') %>% 
      parse_double()
  )
  
# 条件付確率の読み出し

bind_rows(
  read_delim(max_loglik$filepath[1], delim = ' ', col_names = F) %>% 
    select(X2, X3, X5, X6, X9) %>% 
    slice(161:185) %>% 
    rename(variable = X2, outcome = X3, 潜在クラス = X5, group = X6, 
           probability = X9),
  read_delim(max_loglik$filepath[1], delim = ' ', col_names = F) %>% 
    select(X2, X3, X5, X6, X9) %>% 
    slice(158:160) %>% 
    select(variable = X2, 潜在クラス = X3, group = X5, 
           probability = X9) 
) %>% 
  fill(variable) %>% 
  filter(!is.na(probability)) %>% 
  mutate(probability = str_remove(probability, '\\(.+\\)') %>% parse_double()) %>% 
  
  pivot_wider(names_from = 潜在クラス, values_from = probability) %>% 
  gt()
  

