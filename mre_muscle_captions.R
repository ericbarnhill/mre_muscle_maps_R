f.ref <- function(x) {
    stringr::str_extract(fig_nums(x), "[^:]*")
}
fig_nums <- captioner::captioner(prefix = "Fig.")
fig.1_cap <- fig_nums(name = "fig_1", 
                        caption = "Boxplots for stiffness (Pa) by muscle.")
fig.2_cap <- fig_nums(name = "fig_2", 
                        caption = "Boxplots for log stiffness (Pa) by muscle.")
fig.3_cap <- fig_nums(name = "fig_3", 
                        caption = "Boxplots for log stiffness (Pa) by muscle group.")