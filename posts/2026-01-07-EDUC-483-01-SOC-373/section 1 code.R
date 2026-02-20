# educ 483 section 1 code
# 1/7/2026, sadie richardson

library(tidyverse)

### 1: code lorenz curve

# make small dataset with 3 distributions
df1 <- data.frame(id = c(1:10),
                 a = c(5,5,5,5,5,15,15,15,15,15),
                 b = c(5,5,5,5,5,5,5,5,30,30),
                 c = c(5,5,5,5,10,10,10,10,10,30))

# make long for easier manipulation
df2 <- df1 %>% 
  pivot_longer(-id, names_to = "dist", values_to = "income")

# key step: sort by income
df3 <- df2 %>% 
  arrange(dist, income)

# cumulative proportion of population
df4 <- df3 %>% 
  group_by(dist) %>% 
  mutate(rank = row_number(),
         c_pop = rank/n())

# check: cumulative proportion should range from 0 to 1
summary(df4$c_pop)

# cumulative proportion of income
df5 <- df4 %>% 
  group_by(dist) %>% 
  mutate(r_inc = income/mean(income),
         c_inc = cumsum(r_inc)/n())

# check: cumulative proportion should range from 0 to 1
summary(df5$c_inc)

# plot

# note: for plotting, we want a 0 row so that the lorenz curves start at the origin. i add by hand here
df_plot <- df5 %>% 
  bind_rows(data.frame(dist = c("a", "b", "c"), c_pop = c(0,0,0), c_inc = c(0,0,0)))

ggplot(df_plot, aes(x = c_pop, y = c_inc, color = dist)) +
  geom_line() +
  geom_function(fun = function(x) x, color = "black") +
  labs(x = "Cumulative proportion of population",
       y = "Cumulative proportion of income",
       color = "Distribution") +
  theme_classic()

### 2: compute measures of inequality

# what proportion of income do the bottom 80% own?
# note: make sure data is sorted by c_pop before doing this type of calculation:
df <- df %>% 
  arrange(dist, c_pop)
df %>% filter(c_pop >= 0.8) %>% summarise(l80 = first(c_inc))

# what proportion of income do the top 10% own?
df %>% filter(c_pop >= 0.9) %>% summarise(l10 = 1-first(c_inc))

# what is the 75/25 income ratio?
df %>% 
  filter(c_pop >= 0.75) %>% 
  summarise(i75 = first(income)) %>% 
  left_join(df %>% 
              filter(c_pop >= 0.25) %>% 
              summarise(i25 = first(income))) %>% 
  mutate(incrat_7525= i75/i25)


# gini coefficient
# there are several equations you can use. this uses 2/N * sum(f_i - l_i)
df %>%
  mutate(diff = c_pop - c_inc) %>% 
  group_by(dist) %>% 
  summarise(sum_diff = sum(diff),
            n = max(id)) %>% 
  mutate(gini = 2/n * sum_diff)

# mean squared deviation
df %>% 
  mutate(diff = abs(1-r_inc)) %>% 
  group_by(dist) %>% 
  summarise(sum_diff = sum(diff),
            n = max(id)) %>% 
  mutate(msd = 1/n * sum_diff)

# squared coefficient of variation
df %>% 
  mutate(diff = (1-r_inc)^2) %>% 
  group_by(dist) %>% 
  summarise(sum_diff = sum(diff),
            n = max(id)) %>% 
  mutate(scv = 1/n * sum_diff)

# theil index
df %>% 
  mutate(diff = r_inc * log(r_inc)) %>% 
  group_by(dist) %>% 
  summarise(sum_diff = sum(diff),
            n = max(id)) %>% 
  mutate(theil = 1/n * sum_diff)

