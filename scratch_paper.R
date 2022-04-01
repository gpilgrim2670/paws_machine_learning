library(dplyr)
library(ggplot2)
library(readr)

train <- read_csv('train.csv')
test <- read_csv('test.csv')


train %>%
  group_by(pet_name) %>%
  summarise(avg = mean(distance),
            max = max(distance),
            min = min(distance),
            n = n(),
            day = n/480)


pet_ids_test <- test %>%
  select(pet_name, id)

pet_ids_train <- train %>%
  select(pet_name, id)

train %>%
  ggplot() +
  geom_point(aes(x = distance, y = lines_per_sec)) +
  facet_grid(.~pet_name) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  theme_bw()

train %>%
  bind_rows(test, .id = 'set') %>%
  replace(is.na(.), 0.5) %>%
  mutate(lines_per_sec = case_when(pet_name == 'nico' & set == '2' ~ (distance*0.415) + 0.41,
                                   TRUE ~ lines_per_sec)) %>%
  mutate(lines_per_sec = case_when(pet_name == 'poncie' & set == '2' ~ (distance*-0.037) + 0.41,
                                   TRUE ~ lines_per_sec)) %>%
  mutate(lines_per_sec = case_when(pet_name == 'teddy' & set == '2' ~ (distance*0.046) + 0.41,
                                   TRUE ~ lines_per_sec)) %>%
  mutate(lines_per_sec = case_when(pet_name == 'titus' & set == '2' ~ (distance*0.41) + 0.41,
                                   TRUE ~ lines_per_sec)) %>%
  ggplot() +
  geom_point(aes(x = distance, y = lines_per_sec, color = set)) +
  facet_grid(.~pet_name) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  theme_bw()


test_train <- train %>%
  bind_rows(test, .id = 'set') %>%
  group_by(pet_name) %>%
  mutate(row = as.numeric(stringr::str_extract(id, '\\d+'))) %>%
  arrange(distance, row)

# nico

nico <- test_train %>%
  filter(pet_name == 'nico') %>%
  ungroup() %>%
  select(distance, lines_per_sec) %>%
  zoo::na.approx() %>%
  as.data.frame()

nico_id <- test_train %>%
  filter(pet_name == 'nico') %>%
  ungroup() %>%
  select(id, set)

nico_full <- bind_cols(nico, nico_id) %>%
  mutate(random = runif(nrow(.), min = 0, max = 0.025)) %>%
  mutate(lines_per_sec = case_when(set == '2' & between(distance, 0.56150, 0.5615055) ~ 0.60 + random,
                                   TRUE ~ lines_per_sec))

nico_full %>%
  ggplot() +
  geom_point(aes(x = distance, y = lines_per_sec, color = set)) +
  theme_bw()

# poncie

poncie <- test_train %>%
  filter(pet_name == 'poncie') %>%
  ungroup() %>%
  select(distance, lines_per_sec) %>%
  zoo::na.approx() %>%
  as.data.frame()

poncie_id <- test_train %>%
  filter(pet_name == 'poncie') %>%
  ungroup() %>%
  select(id, set)

poncie_full <- bind_cols(poncie, poncie_id) %>%
  mutate(random = runif(nrow(.), min = 0, max = 0.025)) %>%
  mutate(lines_per_sec = case_when(set == '2' & between(distance, 0.5, 0.6) ~ 0.375 + random,
                                   TRUE ~ lines_per_sec))


poncie_full %>%
  ggplot() +
  geom_point(aes(x = distance, y = lines_per_sec, color = set)) +
  theme_bw()

# teddy

teddy <- test_train %>%
  filter(pet_name == 'teddy') %>%
  ungroup() %>%
  select(distance, lines_per_sec) %>%
  zoo::na.approx() %>%
  as.data.frame()

teddy_id <- test_train %>%
  filter(pet_name == 'teddy') %>%
  ungroup() %>%
  select(id, set)

teddy_full <- bind_cols(teddy, teddy_id) %>%
  mutate(random = runif(nrow(.), min = 0, max = 0.025)) %>%
  mutate(lines_per_sec = case_when(set == '2' & between(distance, 0.65, 0.8) ~ 0.425 + random,
                                   TRUE ~ lines_per_sec))


teddy_full %>%
  ggplot() +
  geom_point(aes(x = distance, y = lines_per_sec, color = set)) +
  theme_bw()

# titus

titus <- test_train %>%
  filter(pet_name == 'titus') %>%
  ungroup() %>%
  select(distance, lines_per_sec) %>%
  zoo::na.approx() %>%
  as.data.frame()

titus_id <- test_train %>%
  filter(pet_name == 'titus') %>%
  ungroup() %>%
  select(id, set)

titus_full <- bind_cols(titus, titus_id) %>%
  mutate(random = runif(nrow(.), min = 0, max = 0.025)) %>%
  mutate(lines_per_sec = case_when(set == '2' & between(distance, 0.26661, 0.27) ~ 0.5 + random,
                                   TRUE ~ lines_per_sec))


titus_full %>%
  ggplot() +
  geom_point(aes(x = distance, y = lines_per_sec, color = set)) +
  theme_bw()

# full set

test_train_full <- nico_full %>%
  bind_rows(poncie_full) %>%
  bind_rows(teddy_full) %>%
  bind_rows(titus_full)



test_train_full %>%
  arrange(id) %>%
  left_join(bind_rows(pet_ids_test, pet_ids_train)) %>%
  # mutate(random = runif(nrow(.), min = 0, max = 0.025)) %>%
  # mutate(lines_per_sec = case_when(set == '2' ~ 0.5 + random,
  #                                  TRUE ~ lines_per_sec))
  ggplot() +
  geom_point(aes(x = distance, y = lines_per_sec, color = set)) +
  facet_wrap(.~pet_name) +
  theme_bw()

test_complete <- test_train_full %>%
  filter(set == '2')

test_complete %>%
  arrange(id) %>%
  select(id, lines_per_sec) %>%
  right_join(submission_format %>% select(id)) %>%
  write.csv('gp.csv', row.names = FALSE)

# just plot and match slopes

slope <- train %>%
  bind_rows(test, .id = 'set') %>%
  replace(is.na(.), 0.5) %>%
  mutate(lines_per_sec = case_when(pet_name == 'nico' & set == '2' ~ (distance*0.415) + 0.41,
                                   TRUE ~ lines_per_sec)) %>%
  mutate(lines_per_sec = case_when(pet_name == 'poncie' & set == '2' ~ (distance*-0.037) + 0.41,
                                   TRUE ~ lines_per_sec)) %>%
  mutate(lines_per_sec = case_when(pet_name == 'teddy' & set == '2' ~ (distance*0.044) + 0.41,
                                   TRUE ~ lines_per_sec)) %>%
  mutate(lines_per_sec = case_when(pet_name == 'titus' & set == '2' ~ (distance*0.41) + 0.41,
                                   TRUE ~ lines_per_sec))

slope %>%
  ggplot() +
  geom_point(aes(x = distance, y = lines_per_sec, color = set)) +
  facet_grid(.~pet_name) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  theme_bw()

gp_3 <- slope %>%
  filter(set == '2') %>%
  arrange(id) %>%
  select(id, lines_per_sec) %>%
  right_join(submission_format %>% select(id))

write.csv(gp_3, 'gp_3.csv', row.names = FALSE)
