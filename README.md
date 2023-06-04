# Descriptive-statistics_prat1 in R 
-   <a href="#reading-the-csv-file" id="toc-reading-the-csv-file">Reading
    the CSV file</a>
    -   <a href="#estimation-of-mean" id="toc-estimation-of-mean">Estimation of
        mean</a>
    -   <a href="#estimation-of-median" id="toc-estimation-of-median">Estimation
        of Median</a>
    -   <a href="#estimation-of-mode" id="toc-estimation-of-mode">Estimation of
        Mode</a>
    -   <a href="#estimation-of-standard-deviation"
        id="toc-estimation-of-standard-deviation">Estimation of standard
        deviation</a>
    -   <a href="#estimation-of-variance"
        id="toc-estimation-of-variance">Estimation of variance</a>
-   <a href="#length-frequency-estimation-from-length-data"
    id="toc-length-frequency-estimation-from-length-data">Length Frequency
    estimation from length data</a>

Dear Friends,

Today we will perform some basic descriptive statistics from the given
ecological data.

# Reading the CSV file

``` r
#set working directory ( go to session,click on set working directory, choose directory in which your csv file is located)
# codes taken and modifed from https://statsandr.com/blog/descriptive-statistics-in-r/#introduction
data =read.csv(file = "water_chemistry.csv", header = TRUE)
head(data)
```

    ##   Site Water.temperature Transparency.cm. Specific.Conductivity TDS.mg.L.   pH
    ## 1  S-1              30.1               98                1514.0       989 7.78
    ## 2  S-2              30.4               96                1416.0       921 6.13
    ## 3  S-3              30.6              108                 142.3       103 8.33
    ## 4  S-4              30.6              102                1517.0       988 8.10
    ## 5  S-5              31.6               69                1531.0       996 8.23
    ## 6  S-6              31.4               92                1529.0       995 8.35
    ##    D.O Total.alkalinity Total.hardness Total.N2 Nitrate Available.Phosphate BOD
    ## 1 4.21              172            336   0.0636  0.0228              0.1034 3.8
    ## 2 5.09              172            340   0.0786  0.0330              0.1576 4.0
    ## 3 7.08              188            388   0.0556  0.0145              0.1084 4.2
    ## 4 5.26              168            352   0.0729  0.0237              0.0936 3.6
    ## 5 6.63              200            364   0.0490  0.0096              0.1256 4.0
    ## 6 7.44              168            344   0.0875  0.0167              0.0714 3.8
    ##   year
    ## 1 2021
    ## 2 2021
    ## 3 2021
    ## 4 2021
    ## 5 2021
    ## 6 2021

## Estimation of mean

``` r
#Mean of a coloumn/ head can be estimated by the following code, mean is 
mean(data$Water.temperature)
```

    ## [1] 31.585

## Estimation of Median

``` r
median(data$Water.temperature)
```

    ## [1] 31.55

## Estimation of Mode

``` r
# Base R does not have a standard inbuilt function to calculate mode, So, a function needs to be created for mode (the codes were taken and modified from tutorials point.com). 

mode=function(X){uniqX=unique(X)
  uniqX[which.max(tabulate(match(X, uniqX)))]}

#define the character/ coloum head which needs to be calculated for mode

X =data$Water.temperature

mode(X)
```

    ## [1] 30.6

``` r
mode(data$Water.temperature)
```

    ## [1] 30.6

## Estimation of standard deviation

``` r
sd(data$Water.temperature)
```

    ## [1] 0.8292514

## Estimation of variance

``` r
var(data$Water.temperature)
```

    ## [1] 0.6876579

Summary

``` r
#if you want to know the summary of the data
summary(data)
```

    ##      Site           Water.temperature Transparency.cm. Specific.Conductivity
    ##  Length:20          Min.   :30.10     Min.   : 56.00   Min.   : 142.3       
    ##  Class :character   1st Qu.:31.15     1st Qu.: 71.75   1st Qu.: 981.8       
    ##  Mode  :character   Median :31.55     Median : 75.00   Median :1048.5       
    ##                     Mean   :31.59     Mean   : 79.12   Mean   :1398.4       
    ##                     3rd Qu.:32.12     3rd Qu.: 85.25   3rd Qu.:1529.5       
    ##                     Max.   :33.50     Max.   :108.00   Max.   :2979.0       
    ##    TDS.mg.L.            pH             D.O        Total.alkalinity
    ##  Min.   : 103.0   Min.   :6.130   Min.   :4.210   Min.   :148.0   
    ##  1st Qu.: 638.2   1st Qu.:8.290   1st Qu.:6.550   1st Qu.:164.5   
    ##  Median : 682.0   Median :8.515   Median :7.015   Median :169.0   
    ##  Mean   : 911.8   Mean   :8.317   Mean   :7.026   Mean   :169.9   
    ##  3rd Qu.: 995.2   3rd Qu.:8.580   3rd Qu.:7.605   3rd Qu.:172.5   
    ##  Max.   :1939.0   Max.   :8.710   Max.   :9.700   Max.   :200.0   
    ##  Total.hardness     Total.N2          Nitrate        Available.Phosphate
    ##  Min.   :232.0   Min.   :0.04900   Min.   :0.00890   Min.   :0.02800    
    ##  1st Qu.:248.0   1st Qu.:0.05897   1st Qu.:0.01285   1st Qu.:0.03560    
    ##  Median :300.0   Median :0.07155   Median :0.01560   Median :0.04510    
    ##  Mean   :304.1   Mean   :0.07296   Mean   :0.01718   Mean   :0.06932    
    ##  3rd Qu.:355.0   3rd Qu.:0.08055   3rd Qu.:0.02250   3rd Qu.:0.10465    
    ##  Max.   :388.0   Max.   :0.12460   Max.   :0.03300   Max.   :0.15760    
    ##       BOD             year     
    ##  Min.   :2.000   Min.   :2021  
    ##  1st Qu.:2.400   1st Qu.:2021  
    ##  Median :3.300   Median :2022  
    ##  Mean   :3.075   Mean   :2022  
    ##  3rd Qu.:3.800   3rd Qu.:2022  
    ##  Max.   :4.200   Max.   :2022

``` r
# if you want to know the summary of the data without the 1st coloumn, i.e sit numbers
summary(data[, -1])
```

    ##  Water.temperature Transparency.cm. Specific.Conductivity   TDS.mg.L.     
    ##  Min.   :30.10     Min.   : 56.00   Min.   : 142.3        Min.   : 103.0  
    ##  1st Qu.:31.15     1st Qu.: 71.75   1st Qu.: 981.8        1st Qu.: 638.2  
    ##  Median :31.55     Median : 75.00   Median :1048.5        Median : 682.0  
    ##  Mean   :31.59     Mean   : 79.12   Mean   :1398.4        Mean   : 911.8  
    ##  3rd Qu.:32.12     3rd Qu.: 85.25   3rd Qu.:1529.5        3rd Qu.: 995.2  
    ##  Max.   :33.50     Max.   :108.00   Max.   :2979.0        Max.   :1939.0  
    ##        pH             D.O        Total.alkalinity Total.hardness 
    ##  Min.   :6.130   Min.   :4.210   Min.   :148.0    Min.   :232.0  
    ##  1st Qu.:8.290   1st Qu.:6.550   1st Qu.:164.5    1st Qu.:248.0  
    ##  Median :8.515   Median :7.015   Median :169.0    Median :300.0  
    ##  Mean   :8.317   Mean   :7.026   Mean   :169.9    Mean   :304.1  
    ##  3rd Qu.:8.580   3rd Qu.:7.605   3rd Qu.:172.5    3rd Qu.:355.0  
    ##  Max.   :8.710   Max.   :9.700   Max.   :200.0    Max.   :388.0  
    ##     Total.N2          Nitrate        Available.Phosphate      BOD       
    ##  Min.   :0.04900   Min.   :0.00890   Min.   :0.02800     Min.   :2.000  
    ##  1st Qu.:0.05897   1st Qu.:0.01285   1st Qu.:0.03560     1st Qu.:2.400  
    ##  Median :0.07155   Median :0.01560   Median :0.04510     Median :3.300  
    ##  Mean   :0.07296   Mean   :0.01718   Mean   :0.06932     Mean   :3.075  
    ##  3rd Qu.:0.08055   3rd Qu.:0.02250   3rd Qu.:0.10465     3rd Qu.:3.800  
    ##  Max.   :0.12460   Max.   :0.03300   Max.   :0.15760     Max.   :4.200  
    ##       year     
    ##  Min.   :2021  
    ##  1st Qu.:2021  
    ##  Median :2022  
    ##  Mean   :2022  
    ##  3rd Qu.:2022  
    ##  Max.   :2022

``` r
# in summary you will get ( min, Max, Mean, median, 1st quartile, 3rd quartile, which is all the elements of boxplot including mean, from min and Max we will get the  range of the data)

# if you donot want teh 1st coloumn and last coloumn (14th coloumn) in the summary, then 
summary(data[, - c(1, 14)])
```

    ##  Water.temperature Transparency.cm. Specific.Conductivity   TDS.mg.L.     
    ##  Min.   :30.10     Min.   : 56.00   Min.   : 142.3        Min.   : 103.0  
    ##  1st Qu.:31.15     1st Qu.: 71.75   1st Qu.: 981.8        1st Qu.: 638.2  
    ##  Median :31.55     Median : 75.00   Median :1048.5        Median : 682.0  
    ##  Mean   :31.59     Mean   : 79.12   Mean   :1398.4        Mean   : 911.8  
    ##  3rd Qu.:32.12     3rd Qu.: 85.25   3rd Qu.:1529.5        3rd Qu.: 995.2  
    ##  Max.   :33.50     Max.   :108.00   Max.   :2979.0        Max.   :1939.0  
    ##        pH             D.O        Total.alkalinity Total.hardness 
    ##  Min.   :6.130   Min.   :4.210   Min.   :148.0    Min.   :232.0  
    ##  1st Qu.:8.290   1st Qu.:6.550   1st Qu.:164.5    1st Qu.:248.0  
    ##  Median :8.515   Median :7.015   Median :169.0    Median :300.0  
    ##  Mean   :8.317   Mean   :7.026   Mean   :169.9    Mean   :304.1  
    ##  3rd Qu.:8.580   3rd Qu.:7.605   3rd Qu.:172.5    3rd Qu.:355.0  
    ##  Max.   :8.710   Max.   :9.700   Max.   :200.0    Max.   :388.0  
    ##     Total.N2          Nitrate        Available.Phosphate      BOD       
    ##  Min.   :0.04900   Min.   :0.00890   Min.   :0.02800     Min.   :2.000  
    ##  1st Qu.:0.05897   1st Qu.:0.01285   1st Qu.:0.03560     1st Qu.:2.400  
    ##  Median :0.07155   Median :0.01560   Median :0.04510     Median :3.300  
    ##  Mean   :0.07296   Mean   :0.01718   Mean   :0.06932     Mean   :3.075  
    ##  3rd Qu.:0.08055   3rd Qu.:0.02250   3rd Qu.:0.10465     3rd Qu.:3.800  
    ##  Max.   :0.12460   Max.   :0.03300   Max.   :0.15760     Max.   :4.200

``` r
#if you want the summary by year and without 1st coloumn
by(data[, -1], data$year, summary)
```

    ## data$year: 2021
    ##  Water.temperature Transparency.cm. Specific.Conductivity   TDS.mg.L.     
    ##  Min.   :30.10     Min.   : 69.00   Min.   : 142.3        Min.   : 103.0  
    ##  1st Qu.:30.60     1st Qu.: 78.62   1st Qu.:1514.8        1st Qu.: 988.2  
    ##  Median :31.10     Median : 87.50   Median :1530.0        Median : 995.5  
    ##  Mean   :31.02     Mean   : 88.15   Mean   :1803.6        Mean   :1175.1  
    ##  3rd Qu.:31.48     3rd Qu.: 97.50   3rd Qu.:2500.0        3rd Qu.:1630.5  
    ##  Max.   :31.80     Max.   :108.00   Max.   :2979.0        Max.   :1939.0  
    ##        pH             D.O        Total.alkalinity Total.hardness 
    ##  Min.   :6.130   Min.   :4.210   Min.   :168.0    Min.   :332.0  
    ##  1st Qu.:8.133   1st Qu.:5.603   1st Qu.:172.0    1st Qu.:341.0  
    ##  Median :8.280   Median :7.090   Median :172.0    Median :358.0  
    ##  Mean   :8.050   Mean   :6.953   Mean   :177.6    Mean   :358.8  
    ##  3rd Qu.:8.325   3rd Qu.:7.897   3rd Qu.:183.0    3rd Qu.:377.0  
    ##  Max.   :8.710   Max.   :9.700   Max.   :200.0    Max.   :388.0  
    ##     Total.N2          Nitrate        Available.Phosphate      BOD      
    ##  Min.   :0.04900   Min.   :0.00960   Min.   :0.04430     Min.   :3.40  
    ##  1st Qu.:0.05672   1st Qu.:0.01505   1st Qu.:0.08063     1st Qu.:3.60  
    ##  Median :0.06205   Median :0.02065   Median :0.10590     Median :3.80  
    ##  Mean   :0.06612   Mean   :0.02003   Mean   :0.10318     Mean   :3.77  
    ##  3rd Qu.:0.07717   3rd Qu.:0.02347   3rd Qu.:0.12190     3rd Qu.:3.95  
    ##  Max.   :0.08750   Max.   :0.03300   Max.   :0.15760     Max.   :4.20  
    ##       year     
    ##  Min.   :2021  
    ##  1st Qu.:2021  
    ##  Median :2021  
    ##  Mean   :2021  
    ##  3rd Qu.:2021  
    ##  Max.   :2021  
    ## ------------------------------------------------------------ 
    ## data$year: 2022
    ##  Water.temperature Transparency.cm. Specific.Conductivity   TDS.mg.L.    
    ##  Min.   :31.30     Min.   :56.00    Min.   : 936.0        Min.   :609.0  
    ##  1st Qu.:31.80     1st Qu.:68.75    1st Qu.: 973.5        1st Qu.:633.0  
    ##  Median :32.15     Median :72.00    Median : 994.0        Median :648.0  
    ##  Mean   :32.15     Mean   :70.10    Mean   : 993.1        Mean   :648.4  
    ##  3rd Qu.:32.45     3rd Qu.:72.00    3rd Qu.:1012.5        3rd Qu.:674.5  
    ##  Max.   :33.50     Max.   :75.00    Max.   :1049.0        Max.   :682.0  
    ##        pH             D.O        Total.alkalinity Total.hardness 
    ##  Min.   :8.500   Min.   :6.420   Min.   :148.0    Min.   :232.0  
    ##  1st Qu.:8.530   1st Qu.:6.553   1st Qu.:157.0    1st Qu.:246.5  
    ##  Median :8.575   Median :6.885   Median :163.0    Median :248.0  
    ##  Mean   :8.584   Mean   :7.098   Mean   :162.2    Mean   :249.4  
    ##  3rd Qu.:8.640   3rd Qu.:7.430   3rd Qu.:168.0    3rd Qu.:255.0  
    ##  Max.   :8.680   Max.   :8.390   Max.   :174.0    Max.   :268.0  
    ##     Total.N2          Nitrate        Available.Phosphate      BOD      
    ##  Min.   :0.05390   Min.   :0.00890   Min.   :0.02800     Min.   :2.00  
    ##  1st Qu.:0.06525   1st Qu.:0.01022   1st Qu.:0.03370     1st Qu.:2.20  
    ##  Median :0.07645   Median :0.01340   Median :0.03550     Median :2.40  
    ##  Mean   :0.07979   Mean   :0.01433   Mean   :0.03545     Mean   :2.38  
    ##  3rd Qu.:0.08715   3rd Qu.:0.01713   3rd Qu.:0.03750     3rd Qu.:2.40  
    ##  Max.   :0.12460   Max.   :0.02640   Max.   :0.04590     Max.   :3.20  
    ##       year     
    ##  Min.   :2022  
    ##  1st Qu.:2022  
    ##  Median :2022  
    ##  Mean   :2022  
    ##  3rd Qu.:2022  
    ##  Max.   :2022

``` r
#Now, if you want to remove"'year' and 'site'" from the summary and also want to classify by year
by(data[,  - c(1, 14)], data$year, summary)
```

    ## data$year: 2021
    ##  Water.temperature Transparency.cm. Specific.Conductivity   TDS.mg.L.     
    ##  Min.   :30.10     Min.   : 69.00   Min.   : 142.3        Min.   : 103.0  
    ##  1st Qu.:30.60     1st Qu.: 78.62   1st Qu.:1514.8        1st Qu.: 988.2  
    ##  Median :31.10     Median : 87.50   Median :1530.0        Median : 995.5  
    ##  Mean   :31.02     Mean   : 88.15   Mean   :1803.6        Mean   :1175.1  
    ##  3rd Qu.:31.48     3rd Qu.: 97.50   3rd Qu.:2500.0        3rd Qu.:1630.5  
    ##  Max.   :31.80     Max.   :108.00   Max.   :2979.0        Max.   :1939.0  
    ##        pH             D.O        Total.alkalinity Total.hardness 
    ##  Min.   :6.130   Min.   :4.210   Min.   :168.0    Min.   :332.0  
    ##  1st Qu.:8.133   1st Qu.:5.603   1st Qu.:172.0    1st Qu.:341.0  
    ##  Median :8.280   Median :7.090   Median :172.0    Median :358.0  
    ##  Mean   :8.050   Mean   :6.953   Mean   :177.6    Mean   :358.8  
    ##  3rd Qu.:8.325   3rd Qu.:7.897   3rd Qu.:183.0    3rd Qu.:377.0  
    ##  Max.   :8.710   Max.   :9.700   Max.   :200.0    Max.   :388.0  
    ##     Total.N2          Nitrate        Available.Phosphate      BOD      
    ##  Min.   :0.04900   Min.   :0.00960   Min.   :0.04430     Min.   :3.40  
    ##  1st Qu.:0.05672   1st Qu.:0.01505   1st Qu.:0.08063     1st Qu.:3.60  
    ##  Median :0.06205   Median :0.02065   Median :0.10590     Median :3.80  
    ##  Mean   :0.06612   Mean   :0.02003   Mean   :0.10318     Mean   :3.77  
    ##  3rd Qu.:0.07717   3rd Qu.:0.02347   3rd Qu.:0.12190     3rd Qu.:3.95  
    ##  Max.   :0.08750   Max.   :0.03300   Max.   :0.15760     Max.   :4.20  
    ## ------------------------------------------------------------ 
    ## data$year: 2022
    ##  Water.temperature Transparency.cm. Specific.Conductivity   TDS.mg.L.    
    ##  Min.   :31.30     Min.   :56.00    Min.   : 936.0        Min.   :609.0  
    ##  1st Qu.:31.80     1st Qu.:68.75    1st Qu.: 973.5        1st Qu.:633.0  
    ##  Median :32.15     Median :72.00    Median : 994.0        Median :648.0  
    ##  Mean   :32.15     Mean   :70.10    Mean   : 993.1        Mean   :648.4  
    ##  3rd Qu.:32.45     3rd Qu.:72.00    3rd Qu.:1012.5        3rd Qu.:674.5  
    ##  Max.   :33.50     Max.   :75.00    Max.   :1049.0        Max.   :682.0  
    ##        pH             D.O        Total.alkalinity Total.hardness 
    ##  Min.   :8.500   Min.   :6.420   Min.   :148.0    Min.   :232.0  
    ##  1st Qu.:8.530   1st Qu.:6.553   1st Qu.:157.0    1st Qu.:246.5  
    ##  Median :8.575   Median :6.885   Median :163.0    Median :248.0  
    ##  Mean   :8.584   Mean   :7.098   Mean   :162.2    Mean   :249.4  
    ##  3rd Qu.:8.640   3rd Qu.:7.430   3rd Qu.:168.0    3rd Qu.:255.0  
    ##  Max.   :8.680   Max.   :8.390   Max.   :174.0    Max.   :268.0  
    ##     Total.N2          Nitrate        Available.Phosphate      BOD      
    ##  Min.   :0.05390   Min.   :0.00890   Min.   :0.02800     Min.   :2.00  
    ##  1st Qu.:0.06525   1st Qu.:0.01022   1st Qu.:0.03370     1st Qu.:2.20  
    ##  Median :0.07645   Median :0.01340   Median :0.03550     Median :2.40  
    ##  Mean   :0.07979   Mean   :0.01433   Mean   :0.03545     Mean   :2.38  
    ##  3rd Qu.:0.08715   3rd Qu.:0.01713   3rd Qu.:0.03750     3rd Qu.:2.40  
    ##  Max.   :0.12460   Max.   :0.02640   Max.   :0.04590     Max.   :3.20

# Length Frequency estimation from length data

``` r
length=c(12,15,20, 21,25,31,34,35,37,40, 41,46,51, 72)

breaks= seq(1,61, by=10)

breaks= c(1,11, 21, 31, 41, 51,61, 71, 81) 

length.cut=cut(length, breaks, right=FALSE)

length.frequency = table(length.cut)

length.frequency
```

    ## length.cut
    ##  [1,11) [11,21) [21,31) [31,41) [41,51) [51,61) [61,71) [71,81) 
    ##       0       3       2       5       2       1       0       1

``` r
sessionInfo()
```

    ## R version 4.2.2 (2022-10-31 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 22621)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_India.utf8  LC_CTYPE=English_India.utf8   
    ## [3] LC_MONETARY=English_India.utf8 LC_NUMERIC=C                  
    ## [5] LC_TIME=English_India.utf8    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] compiler_4.2.2  fastmap_1.1.1   cli_3.6.0       tools_4.2.2    
    ##  [5] htmltools_0.5.5 rstudioapi_0.14 yaml_2.3.7      rmarkdown_2.21 
    ##  [9] knitr_1.42      xfun_0.37       digest_0.6.31   rlang_1.1.0    
    ## [13] evaluate_0.20



