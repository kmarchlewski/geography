
# Update documentation
devtools::document()

# Load everything:
devtools::load_all(export_all = FALSE)

# Run package tests:
devtools::test()

# Run a full R CMD check:
devtools::check()

library(geography)
#require(geography)

# detach("package:geography", unload = TRUE)

load_data()

read_tab(
  "city",
  c("Name", "Population"),
  sel_row("city", "Population < 2000", "Population > 1000")
)

read_tab("city", c("ID", "Name", "Population"), sel_row("city", "ID>4075"))
add_row("city", c("ID", "Name"), c(4080L, "Test City"))
read_tab("city", c("ID", "Name", "Population"), sel_row("city", "ID>4075"))
delete_row("city", 4080L)
read_tab("city", c("ID", "Name", "Population"), sel_row("city", "ID>4075"))
add_row("city", c("ID", "Name"), c(4080L, "Old City"))
read_tab("city", c("ID", "Name", "Population"), sel_row("city", "ID>4075"))
update_row("city", 4080L, "Name", "New City")
read_tab("city", c("ID", "Name", "Population"), sel_row("city", "ID>4075"))

upload_table(
  join_tab("city", "country", "ID", "Capital"),
  "country_capital"
)

read_tab("country_capital", c("Name_country", "Name_city"))

