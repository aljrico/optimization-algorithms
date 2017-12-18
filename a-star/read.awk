# Create node file
 awk -F "|" '($1 == "node"){print $0}' cataluna.csv |
cut -f1,2,3,10,11 -d "|" |
  > node-data-cat.csv

# Crate ways file
 awk -F "|" '($1 == "way"){print $0}' cataluna.csv |
cut -f1,9- -d "|" |
  > way-data-cat.csv


# Merge files

cat node-data-cat.csv way-data-cat.csv > final-cat.csv
