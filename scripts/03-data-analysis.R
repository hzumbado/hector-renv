library(dplyr)
library(tidyr)
library(ggplot2)
library(huxtable)
library(rnaturalearth)

#### ADD CALL TO LIBRARY *******************************************************

# Load data----
data <- read.csv(file = "data/cleaned-data.csv")
study_year <- 2021L

# Summarise data----
# Total population
pop <- data |>
  filter(year == study_year) |>
  group_by(country) |>
  summarise(pop = sum(pop), .groups = "drop") |>
  select(country, pop)

# Age share
age <- data |>
  filter(year == study_year) |>
  group_by(country, age) |>
  summarise(pop = sum(pop), .groups = "drop") |>
  pivot_wider(names_from = age,
              values_from = pop) |>
  left_join(pop, by = "country") |>
  mutate(old = Y_GE65 / pop,
         young = Y_LT15 / pop) |>
  select(country, pop, old, young)

# Sex ratio
sex_ratio <- data |>
  filter(year == study_year) |>
  group_by(country, sex) |>
  summarise(pop = sum(pop), .groups = "drop") |>
  pivot_wider(names_from = sex,
              values_from = pop) |>
  mutate(sex_ratio = M / F) |>
  select(country, sex_ratio)

# Merge and save datasets in summary table
st <- age |>
  left_join(sex_ratio, by = "country") |>
  mutate(year = study_year,
         .before = country)

st |>
  write.csv(file = "output/tables/summary-table.csv",
            row.names = FALSE)

#### ADD HERE HUXTABLE-SCRIPT **************************************************

ht <- st |>
  select(-year) |>
  as_hux() |>
  set_contents(1, 1:5, c("Country",
                         "Population",
                         "Old-age share",
                         "Young-age share",
                         "Sex ratio")) |>
  set_number_format(row = 2:27, col = 2, value = fmt_pretty(digits = 1)) |>
  set_number_format(row = 2:27, col = 3:4, value = fmt_percent()) |>
  set_number_format(row = 2:27, col = 5, value = "%.3f") |>
  set_bold(row = 1, col = everywhere) |>
  set_bottom_border(row = c(1, 27), col = everywhere) |>
  set_font_size(10) |>
  set_lr_padding(3) |>
  set_tb_padding(1)

# Save table
ht |>
  quick_docx(file = "output/tables/summary-table.docx")

# Bar plot----
p1 <- st |>
  arrange(pop) |>
  ggplot(mapping = aes(x = factor(country, levels = country),
                       y = pop)) +
  geom_col(alpha = 0.6) +
  coord_flip() +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  labs(x = "", y = "", fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank())

# Save bar plot
ggsave(filename = "output/figures/pop-bar.png",
       plot = p1,
       device = "png",
       width = 18,
       height = 12,
       units = "cm",
       dpi = 300,
       bg = "white")

#### ADD HERE CHOROPLETH-SCRIPT ************************************************

map <- ne_countries(scale = "medium",
                    type = "countries",
                    continent = "Europe",
                    returnclass = "sf") |>
  left_join(st,
            by = c("iso_a2" = "country"))


p2 <- ggplot(data = map,
             mapping = aes(fill = pop)) +
  geom_sf(size = 0.1) +
  coord_sf(xlim = c(-26, 40),
           ylim = c(27, 83),
           expand = FALSE) +
  scale_fill_viridis_c(option = "E",
                       na.value = "lightgrey",
                       label = scales::unit_format(unit = "M",
                                                   scale = 1e-6)) +
  labs(fill = "Population \nsize") +
  theme_void()

# Save map plot
ggsave(filename = "output/figures/pop-map.png",
       plot = p2,
       device = "png",
       width = 12,
       height = 9,
       units = "cm",
       dpi = 300,
       bg = "white")


