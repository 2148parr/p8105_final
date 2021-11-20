preliminary\_data\_cleaning
================

## Importing NHATS data and selecting only relevant variables

Using only round 5 data

``` r
nhats = read_sas("./final_data/nhats_r5.sas7bdat")  %>%
  select(spid, hc5depresan1, hc5depresan2, r5dgender, r5d2intvrage, rl5dracehisp, md5canewlker, 
         sn5dnumsn, fl5noonetalk, cg5ofmemprob, is5reasnprx1, hc5health, wb5truestme4) %>%
  janitor::clean_names() %>%
  drop_na() 
```
