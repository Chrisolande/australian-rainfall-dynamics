# cleaning.R


df_clean <- df %>%
  mutate(
    date = as.Date(date),
    month = as.factor(month(date)),
    day = as.factor(wday(date, label = TRUE))
  ) %>%
  filter(!is.na(rainfall))

df_clean %>%
  head() %>%
  kable(caption = "Head of Cleaned Dataset") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

df_clean %>%
  tail() %>%
  kable(caption = "Tail of Cleaned Dataset") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
