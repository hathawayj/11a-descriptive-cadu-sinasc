# Script to update Graphic for slide aboue Term and Cesarean
# 

pacman::p_load(tidyverse, ggthemes)

dat <- tibble(Preterm = c("Late Premature", "Early Term", "Term", "Late Term") %>%
                rep(each = 4) %>% rep(2) %>% factor(levels = c("Late Premature", "Early Term", "Term", "Late Term")),
              Birth   = c("Cesarean", "Normal") %>% rep(each = 16),
              Year    = 2012:2015 %>% rep(8) ,
              Prop    = c(8.5, 8, 7.5, 7.1, 
                          39, 38, 37.5, 35.5, 
                          45.5, 46.9, 47.5, 49,
                          6.5, 7.1, 7.5, 8.4,
                          10, 9, 8.6, 8, 
                          28, 27.2, 27.1, 26.5,
                          52.5, 54, 54.6, 55.5,
                          8.5, 8.9, 9.7, 9.9))

# Original Plot
dat %>%
  mutate(Year = factor(Year)) %>%
  ggplot() +
  aes(x = Preterm, y = Prop, fill = Year) +
  geom_col(position = "dodge") +
  facet_wrap(~Birth) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "Proportion (%)")

# Change to time series with same facet
dat %>%
  ggplot() +
  aes(x = Year, y = Prop, color = Preterm) +
  geom_point() +
  geom_line() +
  facet_wrap(~Birth, nrow = 1, scales = "free_y")


# Insights: 
#    Of the Normal births most are happening at Term.
#    The percent allocation of term type between the two birth groups is similar for the 
#    Late Premature and Late Term.
#    
#    The Early Term category is the interesting group as there are many more, proportionaly,
#    in the Cesarean group. It could be medical, but it may be for convinience as there is
#    no change in the other two categories.


dat %>%
  mutate(Year = factor(Year)) %>%
  ggplot() +
  aes(x = Preterm, y = Prop) +
  geom_bar(fun.y = median, stat = "summary", aes(fill = "Median"), color = "black") +
  geom_jitter(height = 0, aes(color = Year), width = .25, size = 2) +
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) +
  scale_fill_manual(values = "lightgrey") +
  facet_wrap(~Birth, nrow = 1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(y = "Proportion (%)", fill = "")


# Change facet to Preterm level and put Birth type on x-axis
# Use years as observations to estimate prop for each birth type.
# Notice the inverse relationship between term and early term
# The two late groups don't show much of a change or difference between the proportion allocation
dat %>%
  mutate(Year = factor(Year)) %>%
  ggplot() +
  aes(x = Birth, y = Prop) +
  geom_boxplot() +
  geom_jitter(height = 0, aes(color = Year), width = .25) +
  facet_wrap(~Preterm, nrow = 1, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(y = "Proportion (%)")




# dat %>%
#   mutate(Year = factor(Year)) %>%
#   ggplot() +
#   aes(x = Preterm, y = Prop) +
#   geom_point(fun.y = median, stat = "summary", shape = 15, size = 3, color = "grey") +
#   geom_point(fun.y = median, stat = "summary", shape = 15, size = 2, aes(color = "Median")) +
#   geom_jitter(height = 0, aes(color = Year), width = .25) +
#   scale_color_manual(values = c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "black")) +
#   facet_wrap(~Birth, nrow = 1) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
#   labs(y = "Proportion (%)")
