# Viewing unclear rows to decide whether to keep or delete them

View(corpus_id |>
       slice(42010:42015))


# creating a new df that counts the number of rows per year

corpus_count <- corpus_filtered |>
  group_by(year) |>
  summarise(count = n()) |>
  arrange(year)
# viewing the counts per year

View(corpus_count)

# checking count of rows per party

corpus_party_count <- corpus_filtered |>
  group_by(party) |>
  summarise(count = n()) |>
  arrange(desc(count))
View(corpus_party_count)