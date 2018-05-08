library(readxl)

new_arg_q <- read_excel("data/new_pablo_Argentina.xlsx", sheet = "quarterly")
arg_q <- read_excel("data/pre_r_data/Argentina.xlsx", sheet = "quarterly")
new_arg_m <- read_excel("data/new_pablo_Argentina.xlsx", sheet = "monthly")
arg_m <- read_excel("data/pre_r_data/Argentina.xlsx", sheet = "monthly")

names_new_arg_q <- names(new_arg_q)
names_new_arg_m <- names(new_arg_m)
names_arg_q <- names(arg_q)
names_arg_m <- names(arg_m)

# TRUE
identical(names_arg_q, names_new_arg_q)

# FALSE
identical(names_arg_m, names_new_arg_m)

names_arg_m %in% names_new_arg_m

names_new_arg_m %in% names_arg_m

!names_new_arg_m %in% names_arg_m

names_new_arg_m[!names_new_arg_m %in% names_arg_m]


