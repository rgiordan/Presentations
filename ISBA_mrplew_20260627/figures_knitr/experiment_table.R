
# Generate a latex table of key experiment information using the defined macros

prefix_names <- c("Alexander", "Lax", "Stories")
macro_names  <- c("NSur", "Ybar", "MrpMu", "RakingMu")

# Build each cell: $\<prefix><macro>{}$
make_cell <- function(prefix, macro) paste0("$\\", prefix, macro, "{}$")

row_names <- c(
  "NSur"= "$\\nsur$",  
  "NTar"= "$\\ntar$",
  "Ybar"="$\\overline{\\y}$",
  "MrpMu"="$\\muhat[\\mrp]$",
  "RakingMu"="$\\muhat[\\cal]$"
)

column_names <- c(
  "Alexander"=alexander$name, 
  "Stories"=stories$name, 
  "Lax"=laxphilips$name
)

# Build each row: cells joined by " & ", terminated with " \\"
rows <- sapply(macro_names, function(macro) {
  cells <- sapply(prefix_names, make_cell, macro = macro)
  row_name <- row_names[macro]
  paste(paste(c(row_name, cells), collapse = " & "), "\\\\")
})

# Column header row (just the prefix names, or whatever you like)
header <- paste(paste(c("", column_names[prefix_names]), collapse = " & "), "\\\\")

cat(
  "\\begin{tabular}{l", strrep("c", length(prefix_names)), "}\n",
  "\\hline\n",
  header, "\n",
  "\\hline\n",
  paste(rows, collapse = "\n"), "\n",
  "\\hline\n",
  "\\end{tabular}\n",
  sep = ""
)