library(tidyverse)
library(kamaken)
library(gt)
set.seed(123)

lem <- function(lat, man, dim, lab, mod, ite, dat, see, path) {
  # inpとoutのディレクトリ作成
  if(!dir.exists(path)) {
    dir.create(path)
    dir.create(str_c(path, '/inp'))
    dir.create(str_c(path, '/out'))
  }
  
  # ファイルの削除
  file.remove(list.files(path, full.names = T, recursive = T))
  
  # スクリプト書き出し
  walk(see, ~{write_lines(c(lat, man, dim, lab, mod, ite, dat, .), 
                          file = str_c(path, 'inp/', str_remove(., ' '), '.inp'))})
  
  
  # inputファイルとoutputファイルのパス指定
  inp_path <- list.files(str_c(path, 'inp'), full.names = T)
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
  
  return(max_loglik)
}

lem_max_loglik <- function(out_path) {
  
  # 最大対数尤度の読み出し
  tibble(
    filepath = list.files(out_path, full.names = T),
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
}

# データ読み込み
data <- read_csv('Chap5/tab5.1.csv')


# tab5.2 --------------------------------------------------------------------------------------

model1 <- 
  lem(lat = 'lat 2',
      man = 'man 8',
      dim = 'dim 2 2 2 2 2 2 2 2 2 3',
      lab = 'lab Y Z A B C D E S T G', 
      mod = 'mod SGTYZ A|Y B|Y E|Y C|Z D|Z',
      ite = 'ite 10000',
      dat = str_c('dat [', str_c(data$n, collapse = ' '), ']'),
      see = str_c('see ', rdunif(20, 999, 100)),
      path = 'lem/tab5.2/model1/'
  )

# SG->Tの交互作用を削除
model2 <- 
  lem(lat = 'lat 2',
      man = 'man 8',
      dim = 'dim 2 2 2 2 2 2 2 2 2 3',
      lab = 'lab Y Z A B C D E S T G', 
      mod = 'mod SG T|SG {SG ST GT} YZ|SGT {SGTYZ} A|Y B|Y E|Y C|Z D|Z',
      ite = 'ite 10000',
      dat = str_c('dat [', str_c(data$n, collapse = ' '), ']'),
      see = str_c('see ', rdunif(20, 999, 100)),
      path = 'lem/tab5.2/model2/'
  )

# SGT->YとSG->YとGT->Yを削除
# 1*2*1*1 + 1*2*1 + 2*1*1 = 6自由度節約
model3 <- 
  lem(lat = 'lat 2',
      man = 'man 8',
      dim = 'dim 2 2 2 2 2 2 2 2 2 3',
      lab = 'lab Y Z A B C D E S T G', 
      mod = 'mod SG T|SG {SG ST GT} Y|SGT {SGT STY GY} Z|SGTY {SGTYZ} A|Y B|Y E|Y C|Z D|Z',
      ite = 'ite 10000',
      dat = str_c('dat [', str_c(data$n, collapse = ' '), ']'),
      see = str_c('see ', rdunif(20, 999, 100)),
      path = 'lem/tab5.2/model3/'
  )

# ZへのパスをT,G,Yの主効果に限定
model4 <- 
  lem(lat = 'lat 2',
      man = 'man 8',
      dim = 'dim 2 2 2 2 2 2 2 2 2 3',
      lab = 'lab Y Z A B C D E S T G', 
      mod = 'mod SG T|SG {SG ST GT} Y|SGT {SGT STY GY} 
      Z|SGTY {SGTY TZ GZ YZ} A|Y B|Y E|Y C|Z D|Z',
      ite = 'ite 10000',
      dat = str_c('dat [', str_c(data$n, collapse = ' '), ']'),
      see = str_c('see ', rdunif(20, 999, 100)),
      path = 'lem/tab5.2/model4/'
  )

# G->EとG->Cのdirect pathを追加
model5 <- 
  lem(lat = 'lat 2',
      man = 'man 8',
      dim = 'dim 2 2 2 2 2 2 2 2 2 3',
      lab = 'lab Y Z A B C D E S T G', 
      mod = 'mod SG T|SG {SG ST GT} Y|SGT {SGT STY GY} Z|SGTY {SGTY TZ GZ YZ} 
      A|Y B|Y E|SGTY {SGTY YE GE} C|SGTYZ {SGTYZ ZC GC} D|Z',
      ite = 'ite 10000',
      dat = str_c('dat [', str_c(data$n, collapse = ' '), ']'),
      see = str_c('see ', rdunif(20, 999, 100)),
      path = 'lem/tab5.2/model5/'
  )

model1 <- lem_max_loglik('lem/tab5.2/model1/out')
model2 <- lem_max_loglik('lem/tab5.2/model2/out')
model3 <- lem_max_loglik('lem/tab5.2/model3/out')
model4 <- lem_max_loglik('lem/tab5.2/model4/out')
model5 <- lem_max_loglik('lem/tab5.2/model5/out')


# 適合度の読み出し
tibble(
  model = str_c('model', 1:5),
  filepath = c(model1$filepath[1], model2$filepath[1], model3$filepath[1], 
               model4$filepath[1], model5$filepath[1]),
  output = map_chr(filepath, read_file)
) %>% 
  transmute(
    model,
    Lsq = str_extract(output, 
                      '(?<=L-squared            = ).+(?=\\(.+\\))') %>% 
      parse_double(),
    Xsq = str_extract(output, 
                      '(?<=X-squared            = ).+(?=\\(.+\\))') %>% 
      parse_double(),
    df = str_extract(output, '(?<=Degrees of freedom   = ).+') %>% 
      parse_double()
  ) %>% 
  mutate(モデル対比 = str_c(lag(model), ' vs ', model),
              delta_Lsq = abs(lag(Lsq) - Lsq),
              delta_df = abs(lag(df) - df),
              p.value = pchisq(delta_Lsq, delta_df, lower.tail = F)
              ) %>% 
  gt() %>% 
  fmt_number(columns = c(Lsq, Xsq, delta_Lsq, p.value), decimals = 3)



# tab5.3 --------------------------------------------------------------------------------------

# モデル4とモデル5の結果をコピー
file.copy(from = model4$filepath[1], 'lem/tab5.3/model4.out')
file.copy(from = model5$filepath[1], 'lem/tab5.3/model5.out')

# モデル4
# ログリニアパラメータの読み出し
read_delim('lem/tab5.3/model4.out', delim = ' ', 
           col_names = F, skip = 507, n_max = 136) %>% 
  select(X3:X6) %>% 
  fill(X3) %>% 
  filter(str_detect(X4, '\\d')) %>% 
  separate(col = X6, into = as.character(1:6), sep = '\\s+') %>% 
  select(variable = X3, level_a = X4, level_b = X5, level_c = `1`,
         lambda = `2`) %>% 
  # 主効果を除く
  filter(str_detect(variable, '.{2,}')) %>% 
  # levelを結合
  mutate(level = str_c(level_a, level_b, level_c),
         lambda = parse_double(lambda),
         tau = exp(lambda)) %>% 
  select(variable, level, tau, lambda) %>% 
  # 成型
  group_by(variable) %>% 
  ftExtra::as_flextable(groups_to = 'merge') %>% 
  flextable::colformat_double(j = c('tau', 'lambda'), digits = 3)
  
# モデル5
read_delim('lem/tab5.3/model5.out', delim = ' ', 
           col_names = F, skip = 510, n_max = 150) %>% 
  select(X3:X6) %>% 
  fill(X3) %>% 
  filter(str_detect(X4, '\\d')) %>% 
  separate(col = X6, into = as.character(1:6), sep = '\\s+') %>% 
  select(variable = X3, level_a = X4, level_b = X5, level_c = `1`,
         lambda = `2`) %>% 
  # 主効果を除く
  filter(str_detect(variable, '.{2,}')) %>% 
  # levelを結合
  mutate(level = str_c(level_a, level_b, level_c),
         lambda = parse_double(lambda),
         tau = exp(lambda)) %>% 
  select(variable, level, tau, lambda) %>% 
  # 成型
  group_by(variable) %>% 
  ftExtra::as_flextable(groups_to = 'merge') %>% 
  flextable::colformat_double(j = c('tau', 'lambda'), digits = 3)


