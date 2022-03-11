library(dplyr)
library(lubridate)
library(forcats)
library(tidyr)
library(ggplot2)
library(waffle)
library(extrafont)
library(hrbrthemes)
library(prismatic)

library(extrafont)
loadfonts(device = "pdf", quiet = TRUE)

library(showtext)
font_add_google(name = "IBM Plex Mono", family = "IBM Plex Mono")
showtext_auto()

# Prep data ----

birth_year <- 1994
birth_month <- 11
current_year <- year(today())
current_month <- month(today())
current_age <- current_year - birth_year

life_data <- expand_grid(
  month = month.name,
  year = birth_year:current_year
) %>%
  mutate(month = fct_relevel(month, month.name)) %>%
  arrange(year, month) %>%
  group_by(year) %>%
  mutate(month_number = row_number()) %>%
  ungroup() %>%
  filter(!(year == birth_year & month_number < birth_month)) %>%
  filter(!(year == current_year & month_number > current_month)) # If you want to exclude after the current month - I didn't, because it looked weird!

# Add "eras" to be coloured
# "era" text can be used for annotation, and the fill colour will colour the waffle chart

eras <- tribble(
  ~year_month, ~era, ~fill_colour,
  "1994,11", "childhood", "#E8C4A2FF",
  "2006,9", "highschool", "#D8AF39FF",
  "2013,9", "undergrad", "#DE7862FF",
  "2016,9", "masters", "#E75B64FF",
  "2017,9", "research assistant", "#278B9AFF",
  "2018,8", "data analyst", "#5A6F80FF",
  "2019,10", "PhD", "#4C413FFF"
)

# Darken fill colour to be used for text annotations

eras[["text_colour"]] <- as.character(clr_darken(eras[["fill_colour"]], shift = 0.1))

life_data <- life_data %>%
  rowwise() %>%
  mutate(year_month = paste0(c(year, month_number), collapse = ",")) %>%
  ungroup() %>%
  left_join(eras, by = "year_month") %>%
  fill(era, fill_colour, text_colour) %>%
  mutate(fill_colour = fct_inorder(fill_colour))

# Add a plot_month and plot_year
# If the month number is >= birth month, then keep as is
# If the month number is less than birth month, then make the plot month number 12 + the month
# And make the plot year the year - 1

life_data <- life_data %>%
  mutate(plot_month = case_when(month_number >= birth_month ~ month_number,
                                month_number < birth_month ~ 12L + month_number),
         plot_year = case_when(month_number >= birth_month ~ year,
                               month_number < birth_month ~ year - 1L))

# Split life data into list based on era for using labels/colours later on

life_data_list <- split(life_data, life_data$era)

# Make waffle chart! ----

# Base plot

background_colour <- "#F7F7F7"

life_in_months_base <- life_data %>%
  count(fill_colour) %>% ## the count of each era is the number of months in that era
  add_row(fill_colour = background_colour,
    n = birth_month - current_month -1) %>%
  dplyr::arrange(-dplyr::row_number()) %>%
  ggplot(aes(fill = fill_colour, values = n)) +

  geom_waffle(color = background_colour, n_rows = 12, size = 1, flip=TRUE) +
  ## make each row a year/12 months
  coord_equal() +
  scale_x_continuous(limits = c(-1, 19)) +
  scale_fill_identity() +
  labs(y = NULL, x = NULL) +
  theme_ipsum(grid = "") +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = background_colour, color = background_colour),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

# Initial annotations ----

annotation_base_size <- 30 # Use ~30 for exporting at dpi 300, and ~3 for working interactively
annotation_lineheight <- 1
initial_annotations_font_family <- "IBM Plex Mono"
initial_annotations_colour <- "#666666"

initial_text <- function(x, y, label, size = annotation_base_size, colour = initial_annotations_colour, ...) {
  annotate("text", x = x, y = y, label = label, size = size, colour = colour, family = "IBM Plex Mono", fontface = "italic", ...)
}

initial_segment <- function(x, xend, y, yend, colour = initial_annotations_colour) {
  geom_segment(aes(x = x, xend = xend, y = y, yend = yend), colour = colour)
}

life_in_months_initial_annotations <- life_in_months_base +
  initial_text(x = 0.5, y = -1, label = paste0("Last updated ", Sys.Date()), size = annotation_base_size * 0.8, hjust = 0) +
  initial_text(x = 6.5, y = current_age + 1, label = "1 year") +
  initial_segment(x = 1, xend = 5,
    y = current_age + 1, yend = current_age + 1) +
  initial_segment(x = 1, xend = 1,
    y = current_age + 1 - 0.25, yend = current_age + 1 + 0.25) +
  initial_segment(x = 8, xend = 12,
    y = current_age + 1, yend = current_age + 1) +
  initial_segment(x = 12, xend = 12, y = current_age + 1 - 0.25,
    yend = current_age + 1 + 0.25) +
  initial_text(x = -0.25, y = current_age - 0.5, label = "age", size = annotation_base_size * 0.8, hjust = 0, angle = 90) +
  geom_segment(aes(x = -0.25, xend = -0.25, y = current_age - 1,
    yend = current_age - 3), arrow = arrow(length = unit(0.0175, "npc")), colour = initial_annotations_colour) +
  initial_text(x = 13.5, y = current_age + 2, label = "1 square = 1 month", size = annotation_base_size * 0.8, lineheight = annotation_lineheight, hjust = 0.4) +
  geom_curve(aes(x = 14, xend = 12, y = current_age + 1.5, yend = current_age), arrow = arrow(length = unit(0.0175, "npc")), curvature = -0.3, colour = initial_annotations_colour) +
  annotate("text", x = 0.5, y = current_age + 4, label = "my life\nin months:", hjust = 0, family = "IBM Plex Mono", fontface = "bold", lineheight = 0.25, size = annotation_base_size * 1.5)


# "Role" annotations ----

role_annotations_y <- -0.25
roles_size <- annotation_base_size * 1

role_text <- function(x, y = role_annotations_y, label, size = roles_size, ...) {
  annotate("text", x = x, y = y, label = label, size = size, colour = unique(unique(life_data_list[[label]][["text_colour"]])), family = "IBM Plex Mono", fontface = "italic", hjust = 0, ...)
}

life_in_months_role_annotations <- life_in_months_initial_annotations +
  role_text(x = 13, y = 24, label = "childhood") +
  role_text(x = 13, y = 14, label = "highschool") +
  role_text(x = 13, y = 8, label = "undergrad") +
  role_text(x = 13, y = 6, label = "masters") +
  role_text(x = 13, y = 5, label = "research assistant", lineheight = annotation_lineheight - 0.25) +
  role_text(x = 13, y = 4, label = "data analyst", lineheight = annotation_lineheight - 0.25) +
  role_text(x = 13, y = 2, label = "PhD", lineheight = annotation_lineheight - 0.25)

# Save final plot ----

ggsave("life_in_months.png", plot = life_in_months_role_annotations, device = "png", type = "cairo", width = 15, height = 22.4, dpi = 300)
