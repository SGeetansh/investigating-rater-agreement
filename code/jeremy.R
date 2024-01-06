set.seed(42)
library(dplyr)


n_pos <- 100  # number of true positive items 
n_neg <- 100  # Number of true negative items

p_pos_correct <- 0.8  # Probability that a rater gives correct answer to a true positive
p_neg_correct <- 0.8 # probability that a rater gives correct answer to a true negative

items <- rep(1:(n_neg + n_pos)) 

n_raters_per_item <- 20

d <- data.frame(item = rep(items, n_raters_per_item),
     true_answer = rep(c(rep(1, n_pos),
                     rep(0, n_neg)), n_raters_per_item))


d <- d %>% dplyr::arrange(item)

d$rater <- 1:n_raters_per_item


d$rand <- runif(nrow(d))
d_pos <- d %>%
  dplyr::filter(true_answer == 1) %>%
  dplyr::mutate(answer = ifelse(rand < p_pos_correct, 1, 0))

d_neg <- d %>%
  dplyr::filter(true_answer == 0)%>%
  dplyr::mutate(answer = ifelse(rand < p_neg_correct, 0, 1))

d <- dplyr::bind_rows(d_pos, d_neg)


d %>% 
  dplyr::mutate(correct = (true_answer == answer)) %>%
  dplyr::group_by(true_answer) %>%
  dplyr::summarise(m = mean(correct))

table(d$answer)