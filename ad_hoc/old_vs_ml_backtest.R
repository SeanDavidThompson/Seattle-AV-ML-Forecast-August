> fcst_apr <- read_csv(here("data","wrangled","forecast_all_apr2026-09-29.csv"))
New names:                                                                    
• `` -> `...1`
Rows: 120 Columns: 4
── Column specification ──────────────────────────────────────────────────────
Delimiter: ","
chr (1): forecast_group
dbl (3): ...1, year, gyy

ℹ Use `spec()` to retrieve the full column specification for this data.
ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
> fcst_aug <- read_csv(here("data","wrangled","forecast_all_aug2026-09-29.csv"))
New names:                                                                    
• `` -> `...1`
Rows: 127 Columns: 4
── Column specification ──────────────────────────────────────────────────────
Delimiter: ","
chr (1): forecast_group
dbl (3): ...1, year, gyy

ℹ Use `spec()` to retrieve the full column specification for this data.
ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
> fcst_oct <- read_csv(here("data","wrangled","forecast_all_oct2026-09-29.csv"))
New names:                                                                    
• `` -> `...1`
Rows: 120 Columns: 4
── Column specification ──────────────────────────────────────────────────────
Delimiter: ","
chr (1): forecast_group
dbl (3): ...1, year, gyy

ℹ Use `spec()` to retrieve the full column specification for this data.
ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
> 
> fcst_apr |> 
+   print(n = Inf)
# A tibble: 120 × 4
     ...1  year       gyy forecast_group
    <dbl> <dbl>     <dbl> <chr>         
  1     1  2018 NA        Hotels        
  2     2  2019  0.0719   Hotels        
  3     3  2020 -0.0665   Hotels        
  4     4  2021 -0.169    Hotels        
  5     5  2022 -0.156    Hotels        
  6     6  2023 -0.0611   Hotels        
  7     7  2024  0.101    Hotels        
  8     8  2025  0.221    Hotels        
  9     9  2026  0.00916  Hotels        
 10    10  2027  0.0449   Hotels        
 11    11  2028  0.0637   Hotels        
 12    12  2029  0.0784   Hotels        
 13    13  2030  0.0559   Hotels        
 14    14  2012 NA        Industrial    
 15    15  2013  0.0582   Industrial    
 16    16  2014  0.0832   Industrial    
 17    17  2015  0.108    Industrial    
 18    18  2016  0.130    Industrial    
 19    19  2017  0.0672   Industrial    
 20    20  2018  0.0713   Industrial    
 21    21  2019  0.105    Industrial    
 22    22  2020  0.0851   Industrial    
 23    23  2021  0.167    Industrial    
 24    24  2022  0.186    Industrial    
 25    25  2023  0.0146   Industrial    
 26    26  2024 -0.0122   Industrial    
 27    27  2025  0.0101   Industrial    
 28    28  2026  0.0174   Industrial    
 29    29  2027  0.0682   Industrial    
 30    30  2028  0.0775   Industrial    
 31    31  2029  0.0590   Industrial    
 32    32  2030  0.0524   Industrial    
 33    33  2031  0.0441   Industrial    
 34    34  2032  0.0383   Industrial    
 35    35  2033  0.0362   Industrial    
 36    36  2012 NA        Multifamily   
 37    37  2013  0.0582   Multifamily   
 38    38  2014  0.0601   Multifamily   
 39    39  2015  0.104    Multifamily   
 40    40  2016  0.0772   Multifamily   
 41    41  2017  0.0607   Multifamily   
 42    42  2018  0.0534   Multifamily   
 43    43  2019  0.0526   Multifamily   
 44    44  2020  0.0821   Multifamily   
 45    45  2021  0.0708   Multifamily   
 46    46  2022  0.108    Multifamily   
 47    47  2023 -0.0751   Multifamily   
 48    48  2024 -0.0857   Multifamily   
 49    49  2025 -0.000376 Multifamily   
 50    50  2026  0.0585   Multifamily   
 51    51  2027  0.0655   Multifamily   
 52    52  2028  0.0591   Multifamily   
 53    53  2029  0.0585   Multifamily   
 54    54  2030  0.0553   Multifamily   
 55    55  2031  0.0392   Multifamily   
 56    56  2032  0.0306   Multifamily   
 57    57  2033  0.0302   Multifamily   
 58    58  2012 NA        Major Office  
 59    59  2013  0.0675   Major Office  
 60    60  2014  0.115    Major Office  
 61    61  2015  0.106    Major Office  
 62    62  2016  0.0899   Major Office  
 63    63  2017  0.0391   Major Office  
 64    64  2018  0.0261   Major Office  
 65    65  2019  0.0288   Major Office  
 66    66  2020  0.0492   Major Office  
 67    67  2021  0.0164   Major Office  
 68    68  2022  0.00307  Major Office  
 69    69  2023 -0.113    Major Office  
 70    70  2024 -0.132    Major Office  
 71    71  2025 -0.0775   Major Office  
 72    72  2026 -0.0165   Major Office  
 73    73  2027  0.0451   Major Office  
 74    74  2028  0.0704   Major Office  
 75    75  2029  0.0726   Major Office  
 76    76  2030  0.0710   Major Office  
 77    77  2031  0.0657   Major Office  
 78    78  2032  0.0331   Major Office  
 79    79  2033  0.0260   Major Office  
 80    80  2034  0.0247   Major Office  
 81    81  2012 NA        Retail        
 82    82  2013  0.0834   Retail        
 83    83  2014  0.0579   Retail        
 84    84  2015  0.150    Retail        
 85    85  2016  0.0725   Retail        
 86    86  2017  0.0279   Retail        
 87    87  2018  0.0358   Retail        
 88    88  2019  0.0534   Retail        
 89    89  2020  0.0692   Retail        
 90    90  2021  0.0661   Retail        
 91    91  2022  0.0545   Retail        
 92    92  2023  0.0235   Retail        
 93    93  2024  0.0241   Retail        
 94    94  2025  0.00487  Retail        
 95    95  2026  0.00211  Retail        
 96    96  2027  0.0210   Retail        
 97    97  2028  0.0213   Retail        
 98    98  2029  0.0217   Retail        
 99    99  2030  0.0201   Retail        
100   100  2031  0.0200   Retail        
101   101  2032  0.0176   Retail        
102   102  2033  0.0170   Retail        
103   103  2018 NA        Residential   
104   104  2019  0.0104   Residential   
105   105  2020  0.0794   Residential   
106   106  2021  0.144    Residential   
107   107  2022  0.208    Residential   
108   108  2023 -0.0702   Residential   
109   109  2024  0.0869   Residential   
110   110  2025  0.0361   Residential   
111   111  2026  0.0263   Residential   
112   112  2027  0.0400   Residential   
113   113  2028  0.0444   Residential   
114   114  2029  0.0476   Residential   
115   115  2030  0.0463   Residential   
116   116  2031  0.0448   Residential   
117   117  2032  0.0406   Residential   
118   118  2033  0.0401   Residential   
119   119  2034  0.0384   Residential   
120   120  2035  0.0390   Residential   
> 
> fcst_aug |> 
+   print(n = Inf)
# A tibble: 127 × 4
     ...1  year       gyy forecast_group
    <dbl> <dbl>     <dbl> <chr>         
  1     1  2018 NA        Hotels        
  2     2  2019  0.0712   Hotels        
  3     3  2020 -0.0675   Hotels        
  4     4  2021 -0.170    Hotels        
  5     5  2022 -0.155    Hotels        
  6     6  2023 -0.0619   Hotels        
  7     7  2024  0.101    Hotels        
  8     8  2025  0.202    Hotels        
  9     9  2026  0.0662   Hotels        
 10    10  2027  0.00214  Hotels        
 11    11  2028  0.0341   Hotels        
 12    12  2029  0.0177   Hotels        
 13    13  2030  0.0130   Hotels        
 14    14  2031  0.0157   Hotels        
 15    15  2032  0.0167   Hotels        
 16    16  2033  0.0174   Hotels        
 17    17  2034  0.0142   Hotels        
 18    18  2012 NA        Industrial    
 19    19  2013  0.0536   Industrial    
 20    20  2014  0.0795   Industrial    
 21    21  2015  0.104    Industrial    
 22    22  2016  0.125    Industrial    
 23    23  2017  0.0626   Industrial    
 24    24  2018  0.0746   Industrial    
 25    25  2019  0.109    Industrial    
 26    26  2020  0.0843   Industrial    
 27    27  2021  0.176    Industrial    
 28    28  2022  0.186    Industrial    
 29    29  2023  0.00911  Industrial    
 30    30  2024 -0.00537  Industrial    
 31    31  2025 -0.000199 Industrial    
 32    32  2026 -0.0628   Industrial    
 33    33  2027  0.0114   Industrial    
 34    34  2028  0.0644   Industrial    
 35    35  2029  0.0754   Industrial    
 36    36  2030  0.0632   Industrial    
 37    37  2031  0.0480   Industrial    
 38    38  2032  0.0435   Industrial    
 39    39  2033  0.0411   Industrial    
 40    40  2034  0.0392   Industrial    
 41    41  2012 NA        Multifamily   
 42    42  2013  0.0587   Multifamily   
 43    43  2014  0.0542   Multifamily   
 44    44  2015  0.100    Multifamily   
 45    45  2016  0.0759   Multifamily   
 46    46  2017  0.0600   Multifamily   
 47    47  2018  0.0553   Multifamily   
 48    48  2019  0.0578   Multifamily   
 49    49  2020  0.0881   Multifamily   
 50    50  2021  0.0665   Multifamily   
 51    51  2022  0.109    Multifamily   
 52    52  2023 -0.0768   Multifamily   
 53    53  2024 -0.0889   Multifamily   
 54    54  2025 -0.00934  Multifamily   
 55    55  2026  0.0436   Multifamily   
 56    56  2027  0.0473   Multifamily   
 57    57  2028  0.0591   Multifamily   
 58    58  2029  0.0604   Multifamily   
 59    59  2030  0.0490   Multifamily   
 60    60  2031  0.0308   Multifamily   
 61    61  2032  0.0286   Multifamily   
 62    62  2033  0.0281   Multifamily   
 63    63  2034  0.0273   Multifamily   
 64    64  2012 NA        Major Office  
 65    65  2013  0.0684   Major Office  
 66    66  2014  0.113    Major Office  
 67    67  2015  0.103    Major Office  
 68    68  2016  0.0878   Major Office  
 69    69  2017  0.0431   Major Office  
 70    70  2018  0.0233   Major Office  
 71    71  2019  0.0321   Major Office  
 72    72  2020  0.0556   Major Office  
 73    73  2021  0.0194   Major Office  
 74    74  2022  0.00277  Major Office  
 75    75  2023 -0.114    Major Office  
 76    76  2024 -0.135    Major Office  
 77    77  2025 -0.0795   Major Office  
 78    78  2026 -0.0276   Major Office  
 79    79  2027 -0.0155   Major Office  
 80    80  2028  0.00684  Major Office  
 81    81  2029  0.0319   Major Office  
 82    82  2030  0.0486   Major Office  
 83    83  2031  0.0257   Major Office  
 84    84  2032  0.0166   Major Office  
 85    85  2033  0.0157   Major Office  
 86    86  2034  0.0140   Major Office  
 87    87  2012 NA        Retail        
 88    88  2013  0.0770   Retail        
 89    89  2014  0.0604   Retail        
 90    90  2015  0.153    Retail        
 91    91  2016  0.0773   Retail        
 92    92  2017  0.0272   Retail        
 93    93  2018  0.0337   Retail        
 94    94  2019  0.0517   Retail        
 95    95  2020  0.0702   Retail        
 96    96  2021  0.0659   Retail        
 97    97  2022  0.0546   Retail        
 98    98  2023  0.0307   Retail        
 99    99  2024  0.0244   Retail        
100   100  2025  0.0250   Retail        
101   101  2026  0.0274   Retail        
102   102  2027  0.0346   Retail        
103   103  2028  0.0544   Retail        
104   104  2029  0.0517   Retail        
105   105  2030  0.0472   Retail        
106   106  2031  0.0352   Retail        
107   107  2032  0.0228   Retail        
108   108  2033  0.0215   Retail        
109   109  2034  0.0197   Retail        
110   110  2018 NA        Residential   
111   111  2019  0.00237  Residential   
112   112  2020  0.0712   Residential   
113   113  2021  0.135    Residential   
114   114  2022  0.197    Residential   
115   115  2023 -0.0735   Residential   
116   116  2024  0.0778   Residential   
117   117  2025  0.0208   Residential   
118   118  2026  0.0363   Residential   
119   119  2027  0.0408   Residential   
120   120  2028  0.0442   Residential   
121   121  2029  0.0463   Residential   
122   122  2030  0.0458   Residential   
123   123  2031  0.0448   Residential   
124   124  2032  0.0413   Residential   
125   125  2033  0.0415   Residential   
126   126  2034  0.0404   Residential   
127   127  2035  0.0413   Residential   
> 
> fcst_oct |> 
+   print(n = Inf)
# A tibble: 120 × 4
     ...1  year      gyy forecast_group
    <dbl> <dbl>    <dbl> <chr>         
  1     1  2018 NA       Hotels        
  2     2  2019  0.0715  Hotels        
  3     3  2020 -0.0670  Hotels        
  4     4  2021 -0.170   Hotels        
  5     5  2022 -0.154   Hotels        
  6     6  2023 -0.0617  Hotels        
  7     7  2024  0.101   Hotels        
  8     8  2025  0.194   Hotels        
  9     9  2026  0.0238  Hotels        
 10    10  2027  0.0150  Hotels        
 11    11  2028  0.0891  Hotels        
 12    12  2029  0.125   Hotels        
 13    13  2030  0.0494  Hotels        
 14    14  2012 NA       Industrial    
 15    15  2013  0.0591  Industrial    
 16    16  2014  0.0834  Industrial    
 17    17  2015  0.110   Industrial    
 18    18  2016  0.132   Industrial    
 19    19  2017  0.0667  Industrial    
 20    20  2018  0.0712  Industrial    
 21    21  2019  0.108   Industrial    
 22    22  2020  0.0853  Industrial    
 23    23  2021  0.170   Industrial    
 24    24  2022  0.193   Industrial    
 25    25  2023  0.0167  Industrial    
 26    26  2024 -0.00930 Industrial    
 27    27  2025  0.0237  Industrial    
 28    28  2026 -0.0372  Industrial    
 29    29  2027  0.00919 Industrial    
 30    30  2028  0.0681  Industrial    
 31    31  2029  0.0780  Industrial    
 32    32  2030  0.0651  Industrial    
 33    33  2031  0.0490  Industrial    
 34    34  2032  0.0434  Industrial    
 35    35  2033  0.0411  Industrial    
 36    36  2012 NA       Multifamily   
 37    37  2013  0.0603  Multifamily   
 38    38  2014  0.0535  Multifamily   
 39    39  2015  0.0989  Multifamily   
 40    40  2016  0.0754  Multifamily   
 41    41  2017  0.0578  Multifamily   
 42    42  2018  0.0544  Multifamily   
 43    43  2019  0.0565  Multifamily   
 44    44  2020  0.0864  Multifamily   
 45    45  2021  0.0679  Multifamily   
 46    46  2022  0.110   Multifamily   
 47    47  2023 -0.0874  Multifamily   
 48    48  2024 -0.0876  Multifamily   
 49    49  2025 -0.00930 Multifamily   
 50    50  2026  0.00251 Multifamily   
 51    51  2027  0.0219  Multifamily   
 52    52  2028  0.0619  Multifamily   
 53    53  2029  0.0552  Multifamily   
 54    54  2030  0.0436  Multifamily   
 55    55  2031  0.0320  Multifamily   
 56    56  2032  0.0268  Multifamily   
 57    57  2033  0.0265  Multifamily   
 58    58  2012 NA       Major Office  
 59    59  2013  0.0668  Major Office  
 60    60  2014  0.103   Major Office  
 61    61  2015  0.0922  Major Office  
 62    62  2016  0.0856  Major Office  
 63    63  2017  0.0326  Major Office  
 64    64  2018  0.0163  Major Office  
 65    65  2019  0.0274  Major Office  
 66    66  2020  0.0344  Major Office  
 67    67  2021  0.0698  Major Office  
 68    68  2022  0.0471  Major Office  
 69    69  2023 -0.0774  Major Office  
 70    70  2024 -0.0858  Major Office  
 71    71  2025 -0.0452  Major Office  
 72    72  2026 -0.0151  Major Office  
 73    73  2027 -0.0154  Major Office  
 74    74  2028  0.0151  Major Office  
 75    75  2029  0.0439  Major Office  
 76    76  2030  0.0523  Major Office  
 77    77  2031  0.0314  Major Office  
 78    78  2032  0.0205  Major Office  
 79    79  2033  0.0179  Major Office  
 80    80  2034  0.0159  Major Office  
 81    81  2012 NA       Retail        
 82    82  2013  0.0757  Retail        
 83    83  2014  0.0599  Retail        
 84    84  2015  0.153   Retail        
 85    85  2016  0.0777  Retail        
 86    86  2017  0.0273  Retail        
 87    87  2018  0.0341  Retail        
 88    88  2019  0.0520  Retail        
 89    89  2020  0.0667  Retail        
 90    90  2021  0.0666  Retail        
 91    91  2022  0.0547  Retail        
 92    92  2023  0.0294  Retail        
 93    93  2024  0.0222  Retail        
 94    94  2025  0.0213  Retail        
 95    95  2026  0.0303  Retail        
 96    96  2027  0.0177  Retail        
 97    97  2028  0.0617  Retail        
 98    98  2029  0.0578  Retail        
 99    99  2030  0.0431  Retail        
100   100  2031  0.0281  Retail        
101   101  2032  0.0213  Retail        
102   102  2033  0.0196  Retail        
103   103  2018 NA       Residential   
104   104  2019  0.00209 Residential   
105   105  2020  0.0715  Residential   
106   106  2021  0.134   Residential   
107   107  2022  0.195   Residential   
108   108  2023 -0.0726  Residential   
109   109  2024  0.0762  Residential   
110   110  2025  0.0288  Residential   
111   111  2026 -0.0118  Residential   
112   112  2027  0.0104  Residential   
113   113  2028  0.0320  Residential   
114   114  2029  0.0462  Residential   
115   115  2030  0.0524  Residential   
116   116  2031  0.0559  Residential   
117   117  2032  0.0546  Residential   
118   118  2033  0.0550  Residential   
119   119  2034  0.0527  Residential   
120   120  2035  0.0517  Residential  
