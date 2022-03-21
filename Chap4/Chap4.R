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
data <- read_csv('Chap4/tab4.1.csv')


# tab4.2 --------------------------------------------------------------------------------------

model1 <- 
  lem(lat = 'lat 1',
      man = 'man 5',
      dim = 'dim 2 2 2 2 2 3',
      lab = 'lab Y A B E S G', 
      mod = 'mod SG Y|SG {SGY} A|Y B|Y E|Y',
      ite = 'ite 10000',
      dat = str_c('dat [', str_c(data$n, collapse = ' '), ']'),
      see = str_c('see ', rdunif(20, 999, 100)),
      path = 'lem/tab4.2/model1/'
  )

model2 <- 
  lem(lat = 'lat 1',
      man = 'man 5',
      dim = 'dim 2 2 2 2 2 3',
      lab = 'lab Y A B E S G', 
      mod = 'mod SG Y|SG {SG SY GY} A|Y B|Y E|Y',
      ite = 'ite 10000',
      dat = str_c('dat [', str_c(data$n, collapse = ' '), ']'),
      see = str_c('see ', rdunif(20, 999, 100)),
      path = 'lem/tab4.2/model2/'
      )

model3 <- 
  lem(lat = 'lat 1',
      man = 'man 5',
      dim = 'dim 2 2 2 2 2 3',
      lab = 'lab Y A B E S G', 
      mod = 'mod SG Y|SG {SG GY} A|Y B|Y E|Y',
      ite = 'ite 10000',
      dat = str_c('dat [', str_c(data$n, collapse = ' '), ']'),
      see = str_c('see ', rdunif(20, 999, 100)),
      path = 'lem/tab4.2/model3/'
  )

model4 <- 
  lem(lat = 'lat 1',
      man = 'man 5',
      dim = 'dim 2 2 2 2 2 3',
      lab = 'lab Y A B E S G', 
      mod = 'mod SG Y|SG {SG SY} A|Y B|Y E|Y',
      ite = 'ite 10000',
      dat = str_c('dat [', str_c(data$n, collapse = ' '), ']'),
      see = str_c('see ', rdunif(20, 999, 100)),
      path = 'lem/tab4.2/model4/'
  )

model5 <- 
  lem(lat = 'lat 1',
      man = 'man 5',
      dim = 'dim 2 2 2 2 2 3',
      lab = 'lab Y A B E S G', 
      mod = 'mod SG Y|SG {SG Y} A|Y B|Y E|Y',
      ite = 'ite 10000',
      dat = str_c('dat [', str_c(data$n, collapse = ' '), ']'),
      see = str_c('see ', rdunif(20, 999, 100)),
      path = 'lem/tab4.2/model5/'
  )

model6 <- 
  lem(lat = 'lat 1',
      man = 'man 5',
      dim = 'dim 2 2 2 2 2 3',
      lab = 'lab Y A B E S G', 
      mod = 'mod SG Y|SG {SG SY GY} A|YG {AY GA} B|YS {BY SB} E|Y',
      ite = 'ite 10000',
      dat = str_c('dat [', str_c(data$n, collapse = ' '), ']'),
      see = str_c('see ', rdunif(20, 999, 100)),
      path = 'lem/tab4.2/model6/'
  )


# 適合度の読み出し
tibble(
  model = str_c('model', 1:6),
  filepath = c(model1$filepath[1], model2$filepath[1], model3$filepath[1], 
               model4$filepath[1], model5$filepath[1], model6$filepath[1]),
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
  gt()

