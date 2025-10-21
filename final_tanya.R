pacman::p_load(
  tidyverse,
  rvest,
  ggplot2,
  plotly,
  arrow,
  duckdb,
  DBI
)



#------------------create parquet---------------------------
library(DBI)
library(duckdb)
library(arrow)


con <- dbConnect(duckdb::duckdb())

#------arrests data to parquet

dbExecute(con, "
    CREATE TABLE arrests AS
    SELECT * FROM read_csv_auto('arrest_data.csv');
")
dbExecute(con, "
    COPY arrests TO 'arrest_data.parquet' (FORMAT 'parquet');
")
arrests <- arrow::read_parquet("arrest_data.parquet")
arrests

#------licenses data to parquet
dbExecute(con, "
    CREATE TABLE licenses AS
    SELECT * FROM read_csv_auto(
        'licenses.csv',
        delim = ',',
        header = true,
        quote = '\"',
        ignore_errors = true,
        all_varchar = true
    );
")

dbExecute(con, "
    COPY licenses TO 'licenses.parquet' (FORMAT 'parquet');
")
licenses <- arrow::read_parquet("licenses.parquet")
licenses


dbDisconnect(con, shutdown = TRUE) # Disconnect from DuckDB and shut it down
