%let pgm=utl-altair-slc-using-jaccard-analysis-to-identify-customer-purchasing-patterns;

%stop_submission;

Altair slc using jaccard analysis to identify customer purchasing patterns

Too long to post on a list, see github
https://github.com/rogerjdeangelis/utl-altair-slc-using-jaccard-analysis-to-identify-customer-purchasing-patterns

What you can do with Jaccard analysis

Most unique customer: Mary (different shopping pattern)
Most typical customer: John (represents average customer)
Most different customer pair: Jane & Jack (similarity: 0 ) (no purchases in commom)

Most similar customer pair:Sam & Alex (similarity:0.667)
Create a campaign for this tye of custome

Altair Community
https://community.altair.com/discussion/47640/jaccard-distance-with-rapid-miner?tab=all

please provide a simple self-contained example using jaccard distance with input and output r dataframes using r

/*
 _                   _
(_)_ __  _ __  _   _| |_
| | `_ \| `_ \| | | | __|
| | | | | |_) | |_| | |_
|_|_| |_| .__/ \__,_|\__|
        |_|
*/

libname workx sas7bdat "d:/wpswrkx";

proc datasets lib=workx kill;
run;

/*--- PURCHASES ---*/

options validvarname=v7;
data workx.have;
   input
      Customer_ID $
      Electronics
      Clothing books
      Home_Goods
      Sports
      Beauty
      Toys
      Grocery ;
cards4;
C001 1 1 0 1 0 1 0 1
C002 1 0 1 0 0 1 0 1
C003 0 1 1 0 1 0 0 1
C004 0 1 0 1 0 1 1 0
C005 1 0 1 0 0 0 1 1
C006 1 1 0 1 1 0 0 0
C007 0 1 0 1 1 1 0 1
C008 1 0 1 0 1 0 0 1
C009 0 1 1 0 0 1 1 0
C010 0 0 1 1 0 0 1 1
;;;;
run;

/**************************************************************************************************************************/
/*  WORKX.HAVE total obs=10                                                                                               */
/*        Customer_                                        Home_                                                          */
/* Obs       ID        Electronics    Clothing    books    Goods    Sports    Beauty    Toys    Grocery                   */
/*                                                                                                                        */
/*   1      C001            1             1         0        1         0         1        0        1                      */
/*   2      C002            1             0         1        0         0         1        0        1                      */
/*   3      C003            0             1         1        0         1         0        0        1                      */
/*   4      C004            0             1         0        1         0         1        1        0                      */
/*   5      C005            1             0         1        0         0         0        1        1                      */
/*   6      C006            1             1         0        1         1         0        0        0                      */
/*   7      C007            0             1         0        1         1         1        0        1                      */
/*   8      C008            1             0         1        0         1         0        0        1                      */
/*   9      C009            0             1         1        0         0         1        1        0                      */
/*  10      C010            0             0         1        1         0         0        1        1                      */
/**************************************************************************************************************************/


/*
 _ __  _ __ ___   ___ ___  ___ ___
| `_ \| `__/ _ \ / __/ _ \/ __/ __|
| |_) | | | (_) | (_|  __/\__ \__ \
| .__/|_|  \___/ \___\___||___/___/
|_|
*/

proc print data=workx.have heading=vertical;
run;quit;

options set=RHOME "C:\Progra~1\R\R-4.5.2\bin\r";
proc r;
export data=workx.have r=input_df;
submit;
# ============================================================
# Customer Purchase Behavior Analysis - FULLY CORRECTED VERSION
# Using Jaccard Distance to find similar shopping patterns
# ============================================================

library(vegan)
library(reshape2)

# ============================================================
# INPUT DATAFRAME: Customer purchase history
# ============================================================

set.seed(123)

print("============================================================")
print("INPUT DATAFRAME: Customer Purchase History")
print("============================================================")
print(input_df)
cat("\n")

# ============================================================
# CALCULATE JACCARD DISTANCE MATRIX
# ============================================================

purchase_matrix <- input_df[, -1]
jaccard_dist <- vegdist(purchase_matrix, method = "jaccard")
jaccard_matrix <- as.matrix(jaccard_dist)

# ============================================================
# OUTPUT DATAFRAME 1: Distance Matrix
# ============================================================

output_df1 <- as.data.frame(jaccard_matrix)
names(output_df1) <- input_df$Customer_ID
row.names(output_df1) <- input_df$Customer_ID

print("============================================================")
print("OUTPUT DATAFRAME 1: Jaccard Distance Matrix")
print("(Lower values = more similar purchase patterns)")
print("============================================================")
print(round(output_df1, 3))
cat("\n")

# ============================================================
# OUTPUT DATAFRAME 2: Similarity Score (1 - Distance)
# ============================================================

similarity_matrix <- 1 - jaccard_matrix
output_df2 <- as.data.frame(similarity_matrix)
names(output_df2) <- input_df$Customer_ID
row.names(output_df2) <- input_df$Customer_ID

print("============================================================")
print("OUTPUT DATAFRAME 2: Jaccard Similarity Matrix")
print("(Higher values = more similar purchase patterns)")
print("============================================================")
print(round(output_df2, 3))
cat("\n")

# ============================================================
# OUTPUT DATAFRAME 3: Top Similar Customers (FULLY CORRECTED)
# ============================================================

# Create empty vectors for results
customer_ids <- character()
top_similar <- character()
similarity_scores <- numeric()

# Loop through each customer
for(i in 1:nrow(output_df2)) {
  customer_id <- input_df$Customer_ID[i]

  # Get similarities as a numeric vector (CRITICAL FIX)
  sim_vector <- as.numeric(output_df2[i, ])
  names(sim_vector) <- names(output_df2)

  # Remove self (similarity = 1)
  sim_vector_no_self <- sim_vector[names(sim_vector) != customer_id]

  # Sort descending
  sim_sorted <- sort(sim_vector_no_self, decreasing = TRUE)

  # Get top 3
  top3_names <- names(sim_sorted[1:min(3, length(sim_sorted))])
  top3_score <- round(sim_sorted[1], 3)

  customer_ids <- c(customer_ids, customer_id)
  top_similar <- c(top_similar, paste(top3_names, collapse = ", "))
  similarity_scores <- c(similarity_scores, top3_score)
}

output_df3 <- data.frame(
  Customer_ID = customer_ids,
  Top_3_Similar_Customers = top_similar,
  Max_Similarity_Score = similarity_scores
)

print("============================================================")
print("OUTPUT DATAFRAME 3: Recommendations Based on Similarity")
print("============================================================")
print(output_df3)
cat("\n")

# ============================================================
# BUSINESS INSIGHTS
# ============================================================

print("============================================================")
print("BUSINESS INSIGHTS")
print("============================================================")

# Most unique customer (highest avg distance to others)
avg_distances <- rowMeans(jaccard_matrix)
most_unique <- input_df$Customer_ID[which.max(avg_distances)]
most_typical <- input_df$Customer_ID[which.min(avg_distances)]

print(paste("- Most unique customer:", most_unique,
            "(different shopping pattern)"))
print(paste("- Most typical customer:", most_typical,
            "(represents average customer)"))

# Customer pairs using similarity matrix
melted <- melt(as.matrix(output_df2))
names(melted) <- c("Customer1", "Customer2", "Similarity")
melted <- melted[melted$Customer1 != melted$Customer2, ]
melted$Similarity <- round(melted$Similarity, 3)

# Most similar pair
top_pair <- melted[order(-melted$Similarity)[1], ]
print(paste("- Most similar customer pair:",
            as.character(top_pair$Customer1), "&", as.character(top_pair$Customer2),
            "(similarity:", top_pair$Similarity, ")"))

# Most different pair
bottom_pair <- melted[order(melted$Similarity)[1], ]
print(paste("- Most different customer pair:",
            as.character(bottom_pair$Customer1), "&", as.character(bottom_pair$Customer2),
            "(similarity:", bottom_pair$Similarity, ")"))

# ============================================================
# Calculate average similarity for each customer
# ============================================================

avg_similarity <- rowMeans(output_df2)
avg_similarity_df <- data.frame(
  Customer_ID = input_df$Customer_ID,
  Avg_Similarity_To_All = round(avg_similarity, 3)
)

# CORRECTED VERSION - No duplicate column
avg_similarity_df <- data.frame(
  Avg_Similarity_To_All = round(avg_similarity, 3)
)
rownames(avg_similarity_df) <- input_df$Customer_ID

print("============================================================")
print("AVERAGE SIMILARITY SCORES")
print("============================================================")
print(avg_similarity_df[order(-avg_similarity_df$Avg_Similarity_To_All), , drop = FALSE])

endsubmit;
import r=input_df      data=workx.input          ;
import r=output_df1    data=workx.jaccard_matrix ;
import r=output_df2    data=workx.similarity_matrix;
import r=output_df3    data=workx.top_customers;
run;quit;


/**************************************************************************************************************************/
/* PYTHON                                                           |       SLC                                           */
/* [1] "=========================================================== |  WORKX.DISTANCE_MATRIX(JACCARD) total obs=10        */
/* [1] "INPUT DATAFRAME: Customer Purchase History"                 |  DIAGONAL IS DISTANCE 0 MEANS IDENTICAL PURCHAS     */
/* [1] "=========================================================== |                                                     */
/*                                                                  |        C001  C002  C003  C004  C005  C006  C007     */
/*        C      E                                                  |  C001     0  0.50  0.71  0.50  0.71  0.50  0.33     */
/*        u      l              H                                   |  C002  0.50     0  0.67  0.86  0.40  0.86  0.71     */
/*        s      e              o                                   |  C003  0.71  0.67     0  0.86  0.67  0.67  0.50     */
/*        t      c    C         m                                   |  C004  0.50  0.86  0.86     0  0.86  0.67  0.50     */
/*        o      t    l         e                   G               |  C005  0.71  0.40  0.67  0.86     0  0.86  0.88     */
/*        m      r    o         _    S    B         r               |  C006  0.50  0.86  0.67  0.67  0.86     0  0.50     */
/*        e      o    t    b    G    p    e         o               |  C007  0.33  0.71  0.50  0.50  0.88  0.50     0     */
/*        r      n    h    o    o    o    a    T    c               |  C008  0.71  0.40  0.40  1.00  0.40  0.67  0.71     */
/*  O     _      i    i    o    o    r    u    o    e               |  C009  0.71  0.67  0.67  0.40  0.67  0.86  0.71     */
/*  b     I      c    n    k    d    t    t    y    r               |  C010  0.71  0.67  0.67  0.67  0.40  0.86  0.71     */
/*  s     D      s    g    s    s    s    y    s    y               |                                                     */
/*                                                                  |                                                     */
/*  1    C001    1    1    0    1    0    1    0    1               |  WORKX.SIMILARITY_MATRIX total obs=10               */
/*  2    C002    1    0    1    0    0    1    0    1               |                                                     */
/*  3    C003    0    1    1    0    1    0    0    1               |         C001  C002  C003  C004  C005  C006  C00     */
/*  4    C004    0    1    0    1    0    1    1    0               |                                                     */
/*  5    C005    1    0    1    0    0    0    1    1               |  C001      1  0.50  0.29  0.50  0.29  0.50  0.6     */
/*  6    C006    1    1    0    1    1    0    0    0               |  C002   0.50     1  0.33  0.14  0.60  0.14  0.2     */
/*  7    C007    0    1    0    1    1    1    0    1               |  C003   0.29  0.33     1  0.14  0.33  0.33  0.5     */
/*  8    C008    1    0    1    0    1    0    0    1               |  C004   0.50  0.14  0.14     1  0.14  0.33  0.5     */
/*  9    C009    0    1    1    0    0    1    1    0               |  C005   0.29  0.60  0.33  0.14     1  0.14  0.1     */
/* 10    C010    0    0    1    1    0    0    1    1               |  C006   0.50  0.14  0.33  0.33  0.14     1  0.5     */
/*                                                                  |  C007   0.67  0.29  0.50  0.50  0.12  0.50          */
/*                                                                  |  C008   0.29  0.60  0.60  0.00  0.60  0.33  0.2     */
/* [1] "=========================================================== |  C009   0.29  0.33  0.33  0.60  0.33  0.14  0.2     */
/* [1] "OUTPUT DATAFRAME 1: Jaccard Distance Matrix"                |  C010   0.29  0.33  0.33  0.33  0.60  0.14  0.2     */
/* [1] "(Lower values = more similar purchase patterns)"            |                                                     */
/* [1] "=========================================================== |                                                     */
/*       C001  C002  C003  C004  C005  C006  C007  C008  C009  C010 |  WORKX.TOP_CUSTOMERS total obs=10                   */
/* C001 0.000 0.500 0.714 0.500 0.714 0.500 0.333 0.714 0.714 0.714 |                                                     */
/* C002 0.500 0.000 0.667 0.857 0.400 0.857 0.714 0.400 0.667 0.667 |  Customer_     Top_3_Similar_           Max_Sim     */
/* C003 0.714 0.667 0.000 0.857 0.667 0.667 0.500 0.400 0.667 0.667 |     ID           Customers                  Sco     */
/* C004 0.500 0.857 0.857 0.000 0.857 0.667 0.500 1.000 0.400 0.667 |                                                     */
/* C005 0.714 0.400 0.667 0.857 0.000 0.857 0.875 0.400 0.667 0.400 |               First Second  Third        Simila     */
/* C006 0.500 0.857 0.667 0.667 0.857 0.000 0.500 0.667 0.857 0.857 |                                                     */
/* C007 0.333 0.714 0.500 0.500 0.875 0.500 0.000 0.714 0.714 0.714 |    C001       C007, C002,   C004         0.667      */
/* C008 0.714 0.400 0.400 1.000 0.400 0.667 0.714 0.000 0.857 0.667 |    C002       C005, C008,   C001         0.600      */
/* C009 0.714 0.667 0.667 0.400 0.667 0.857 0.714 0.857 0.000 0.667 |    C003       C008, C007,   C002         0.600      */
/* C010 0.714 0.667 0.667 0.667 0.400 0.857 0.714 0.667 0.667 0.000 |    C004       C009, C001,   C007         0.600      */
/*                                                                  |    C005       C002, C008,   C010         0.600      */
/* [1] "=========================================================== |    C006       C001, C007,   C003         0.500      */
/* [1] "OUTPUT DATAFRAME 2: Jaccard Similarity Matrix"              |    C007       C001, C003,   C004         0.667      */
/* [1] "(Higher values = more similar purchase patterns)"           |    C008       C002, C003,   C005         0.600      */
/* [1] "=========================================================== |    C009       C004, C002,   C003         0.600      */
/*       C001  C002  C003  C004  C005  C006  C007  C008  C009  C010 |    C010       C005, C002,   C003         0.600      */
/* C001 1.000 0.500 0.286 0.500 0.286 0.500 0.667 0.286 0.286 0.286 |                                                     */
/* C002 0.500 1.000 0.333 0.143 0.600 0.143 0.286 0.600 0.333 0.333 |                                                     */
/* C003 0.286 0.333 1.000 0.143 0.333 0.333 0.500 0.600 0.333 0.333 |                                                     */
/* C004 0.500 0.143 0.143 1.000 0.143 0.333 0.500 0.000 0.600 0.333 |                                                     */
/* C005 0.286 0.600 0.333 0.143 1.000 0.143 0.125 0.600 0.333 0.600 |                                                     */
/* C006 0.500 0.143 0.333 0.333 0.143 1.000 0.500 0.333 0.143 0.143 |                                                     */
/* C007 0.667 0.286 0.500 0.500 0.125 0.500 1.000 0.286 0.286 0.286 |                                                     */
/* C008 0.286 0.600 0.600 0.000 0.600 0.333 0.286 1.000 0.143 0.333 |                                                     */
/* C009 0.286 0.333 0.333 0.600 0.333 0.143 0.286 0.143 1.000 0.333 |                                                     */
/* C010 0.286 0.333 0.333 0.333 0.600 0.143 0.286 0.333 0.333 1.000 |                                                     */
/*                                                                  |                                                     */
/* [1] "=========================================================== |                                                     */
/* [1] "OUTPUT DATAFRAME 3: Recommendations Based on Similarity"    |                                                     */
/* [1] "=========================================================== |                                                     */
/*    Customer_ID Top_3_Similar_Customers Max_Similarity_Score      |                                                     */
/* 1         C001        C007, C002, C004                0.667      |                                                     */
/* 2         C002        C005, C008, C001                0.600      |                                                     */
/* 3         C003        C008, C007, C002                0.600      |                                                     */
/* 4         C004        C009, C001, C007                0.600      |                                                     */
/* 5         C005        C002, C008, C010                0.600      |                                                     */
/* 6         C006        C001, C007, C003                0.500      |                                                     */
/* 7         C007        C001, C003, C004                0.667      |                                                     */
/* 8         C008        C002, C003, C005                0.600      |                                                     */
/* 9         C009        C004, C002, C003                0.600      |                                                     */
/* 10        C010        C005, C002, C003                0.600      |                                                     */
/*                                                                  |                                                     */
/* [1]  =========================================================== |                                                     */
/* [1] BUSINESS INSIGHTS"                                           |                                                     */
/* [1] ============================================================ |                                                     */
/* [1] - Most unique customer: C006 (different shopping pattern)    |                                                     */
/* [1] - Most typical customer: C001 (represents average customer)  |                                                     */
/* [1] - Most similar customer pair:C007 & C001(similarity:0.667)   |                                                     */
/* [1] - Most different customer pair:C008 & C004 (similarity: 0 )  |                                                     */
/* [1] ============================================================ |                                                     */
/* [1] AVERAGE SIMILARITY SCORES"                                   |                                                     */
/* [1] ============================================================ |                                                     */
/*                                                                  |                                                     */
/*      Avg_Similarity_To_All                                       |                                                     */
/* C001                 0.460                                       |                                                     */
/* C007                 0.443                                       |                                                     */
/* C002                 0.427                                       |                                                     */
/* C003                 0.420                                       |                                                     */
/* C008                 0.418                                       |                                                     */
/* C005                 0.416                                       |                                                     */
/* C010                 0.398                                       |                                                     */
/* C009                 0.379                                       |                                                     */
/* C004                 0.370                                       |                                                     */
/* C006                 0.357                                       |                                                     */
/**************************************************************************************************************************/

/*
| | ___   __ _
| |/ _ \ / _` |
| | (_) | (_| |
|_|\___/ \__, |
         |___/
*/
1                                          Altair SLC         14:53 Tuesday, April  7, 2026

NOTE: Copyright 2002-2025 World Programming, an Altair Company
NOTE: Altair SLC 2026 (05.26.01.00.000758)
      Licensed to Roger DeAngelis
NOTE: This session is executing on the X64_WIN11PRO platform and is running in 64 bit mode

NOTE: AUTOEXEC processing beginning; file is C:\wpsoto\autoexec.sas
NOTE: AUTOEXEC source line
1       +  ï»¿ods _all_ close;
           ^
ERROR: Expected a statement keyword : found "?"
NOTE: Library workx assigned as follows:
      Engine:        SAS7BDAT
      Physical Name: d:\wpswrkx

NOTE: Library slchelp assigned as follows:
      Engine:        WPD
      Physical Name: C:\Progra~1\Altair\SLC\2026\sashelp

NOTE: Library worksas assigned as follows:
      Engine:        SAS7BDAT
      Physical Name: d:\worksas

NOTE: Library workwpd assigned as follows:
      Engine:        WPD
      Physical Name: d:\workwpd


LOG:  14:53:02
NOTE: 1 record was written to file PRINT

NOTE: The data step took :
      real time : 0.015
      cpu time  : 0.000


NOTE: AUTOEXEC processing completed

1          libname workx sas7bdat "d:/wpswrkx";
NOTE: Library workx assigned as follows:
      Engine:        SAS7BDAT
      Physical Name: d:\wpswrkx


Altair SLC

The DATASETS Procedure

         Directory

Libref           WORKX
Engine           SAS7BDAT
Physical Name    d:\wpswrkx

                                    Members

                                 Member
  Number    Member Name          Type         File Size      Date Last Modified

-------------------------------------------------------------------------------

       1    HAVE                 DATA              9216      07APR2026:14:38:44
       2    INPUT                DATA              9216      07APR2026:14:38:46
       3    JACCARD_MATRIX       DATA              9216      07APR2026:14:38:46
       4    SIMILARITY_MATRIX    DATA              9216      07APR2026:14:38:46
       5    TOP_CUSTOMERS        DATA              5120      07APR2026:14:38:46
2
3         proc datasets lib=workx kill;
4         run;
NOTE: Deleting WORKX.have (type=DATA)
NOTE: Deleting WORKX.input (type=DATA)
NOTE: Deleting WORKX.jaccard_matrix (type=DATA)
NOTE: Deleting WORKX.similarity_matrix (type=DATA)
NOTE: Deleting WORKX.top_customers (type=DATA)
5
6         options validvarname=v7;
NOTE: Procedure datasets step took :
      real time : 0.031
      cpu time  : 0.000


7         data workx.have;
8            input
9               Customer_ID $
10              Electronics
11              Clothing books
12              Home_Goods
13              Sports
14              Beauty
15              Toys
16              Grocery ;
17        cards4;

NOTE: Data set "WORKX.have" has 10 observation(s) and 9 variable(s)
NOTE: The data step took :
      real time : 0.000
      cpu time  : 0.000


18        C001 1 1 0 1 0 1 0 1
19        C002 1 0 1 0 0 1 0 1
20        C003 0 1 1 0 1 0 0 1
21        C004 0 1 0 1 0 1 1 0
22        C005 1 0 1 0 0 0 1 1
23        C006 1 1 0 1 1 0 0 0
24        C007 0 1 0 1 1 1 0 1
25        C008 1 0 1 0 1 0 0 1
26        C009 0 1 1 0 0 1 1 0
27        C010 0 0 1 1 0 0 1 1
28        ;;;;
29        run;
30
31        options set=RHOME "C:\Progra~1\R\R-4.5.2\bin\r";
32        proc r;
NOTE: Using R version 4.5.2 (2025-10-31 ucrt) from C:\Program Files\R\R-4.5.2
33        export data=workx.have r=input_df;
NOTE: Creating R data frame 'input_df' from data set 'WORKX.have'

34        submit;
35        # ============================================================
36        # Customer Purchase Behavior Analysis - FULLY CORRECTED VERSION
37        # Using Jaccard Distance to find similar shopping patterns
38        # ============================================================
39
40        library(vegan)
41        library(reshape2)
42
43        # ============================================================
44        # INPUT DATAFRAME: Customer purchase history
45        # ============================================================
46
47        set.seed(123)
48
49        print("============================================================")
50        print("INPUT DATAFRAME: Customer Purchase History")
51        print("============================================================")
52        print(input_df)
53        cat("\n")
54
55        # ============================================================
56        # CALCULATE JACCARD DISTANCE MATRIX
57        # ============================================================
58
59        purchase_matrix <- input_df[, -1]
60        jaccard_dist <- vegdist(purchase_matrix, method = "jaccard")
61        jaccard_matrix <- as.matrix(jaccard_dist)
62
63        # ============================================================
64        # OUTPUT DATAFRAME 1: Distance Matrix
65        # ============================================================
66
67        output_df1 <- as.data.frame(jaccard_matrix)
68        names(output_df1) <- input_df$Customer_ID
69        row.names(output_df1) <- input_df$Customer_ID
70
71        print("============================================================")
72        print("OUTPUT DATAFRAME 1: Jaccard Distance Matrix")
73        print("(Lower values = more similar purchase patterns)")
74        print("============================================================")
75        print(round(output_df1, 3))
76        cat("\n")
77
78        # ============================================================
79        # OUTPUT DATAFRAME 2: Similarity Score (1 - Distance)
80        # ============================================================
81
82        similarity_matrix <- 1 - jaccard_matrix
83        output_df2 <- as.data.frame(similarity_matrix)
84        names(output_df2) <- input_df$Customer_ID
85        row.names(output_df2) <- input_df$Customer_ID
86
87        print("============================================================")
88        print("OUTPUT DATAFRAME 2: Jaccard Similarity Matrix")
89        print("(Higher values = more similar purchase patterns)")
90        print("============================================================")
91        print(round(output_df2, 3))
92        cat("\n")
93
94        # ============================================================
95        # OUTPUT DATAFRAME 3: Top Similar Customers (FULLY CORRECTED)
96        # ============================================================
97
98        # Create empty vectors for results
99        customer_ids <- character()
100       top_similar <- character()
101       similarity_scores <- numeric()
102
103       # Loop through each customer
104       for(i in 1:nrow(output_df2)) {
105         customer_id <- input_df$Customer_ID[i]
106
107         # Get similarities as a numeric vector (CRITICAL FIX)
108         sim_vector <- as.numeric(output_df2[i, ])
109         names(sim_vector) <- names(output_df2)
110
111         # Remove self (similarity = 1)
112         sim_vector_no_self <- sim_vector[names(sim_vector) != customer_id]
113
114         # Sort descending
115         sim_sorted <- sort(sim_vector_no_self, decreasing = TRUE)
116
117         # Get top 3
118         top3_names <- names(sim_sorted[1:min(3, length(sim_sorted))])
119         top3_score <- round(sim_sorted[1], 3)
120
121         customer_ids <- c(customer_ids, customer_id)
122         top_similar <- c(top_similar, paste(top3_names, collapse = ", "))
123         similarity_scores <- c(similarity_scores, top3_score)
124       }
125
126       output_df3 <- data.frame(
127         Customer_ID = customer_ids,
128         Top_3_Similar_Customers = top_similar,
129         Max_Similarity_Score = similarity_scores
130       )
131
132       print("============================================================")
133       print("OUTPUT DATAFRAME 3: Recommendations Based on Similarity")
134       print("============================================================")
135       print(output_df3)
136       cat("\n")
137
138       # ============================================================
139       # BUSINESS INSIGHTS
140       # ============================================================
141
142       print("============================================================")
143       print("BUSINESS INSIGHTS")
144       print("============================================================")
145
146       # Most unique customer (highest avg distance to others)
147       avg_distances <- rowMeans(jaccard_matrix)
148       most_unique <- input_df$Customer_ID[which.max(avg_distances)]
149       most_typical <- input_df$Customer_ID[which.min(avg_distances)]
150
151       print(paste("- Most unique customer:", most_unique,
152                   "(different shopping pattern)"))
153       print(paste("- Most typical customer:", most_typical,
154                   "(represents average customer)"))
155
156       # Customer pairs using similarity matrix
157       melted <- melt(as.matrix(output_df2))
158       names(melted) <- c("Customer1", "Customer2", "Similarity")
159       melted <- melted[melted$Customer1 != melted$Customer2, ]
160       melted$Similarity <- round(melted$Similarity, 3)
161
162       # Most similar pair
163       top_pair <- melted[order(-melted$Similarity)[1], ]
164       print(paste("- Most similar customer pair:",
165                   as.character(top_pair$Customer1), "&", as.character(top_pair$Customer2),
166                   "(similarity:", top_pair$Similarity, ")"))
167
168       # Most different pair
169       bottom_pair <- melted[order(melted$Similarity)[1], ]
170       print(paste("- Most different customer pair:",
171                   as.character(bottom_pair$Customer1), "&", as.character(bottom_pair$Customer2),
172                   "(similarity:", bottom_pair$Similarity, ")"))
173
174       # ============================================================
175       # Calculate average similarity for each customer
176       # ============================================================
177
178       avg_similarity <- rowMeans(output_df2)
179       avg_similarity_df <- data.frame(
180         Customer_ID = input_df$Customer_ID,
181         Avg_Similarity_To_All = round(avg_similarity, 3)
182       )
183
184       # CORRECTED VERSION - No duplicate column
185       avg_similarity_df <- data.frame(
186         Avg_Similarity_To_All = round(avg_similarity, 3)
187       )
188       rownames(avg_similarity_df) <- input_df$Customer_ID
189
190       print("============================================================")
191       print("AVERAGE SIMILARITY SCORES")
192       print("============================================================")
193       print(avg_similarity_df[order(-avg_similarity_df$Avg_Similarity_To_All), , drop = FALSE])
194
195       endsubmit;

NOTE: Submitting statements to R:

> # ============================================================
> # Customer Purchase Behavior Analysis - FULLY CORRECTED VERSION
> # Using Jaccard Distance to find similar shopping patterns
> # ============================================================
>
> library(vegan)
Loading required package: permute
Warning messages:
1: package 'vegan' was built under R version 4.5.3
2: package 'permute' was built under R version 4.5.3
> library(reshape2)
Warning message:
package 'reshape2' was built under R version 4.5.3
>
> # ============================================================
> # INPUT DATAFRAME: Customer purchase history
> # ============================================================
>
> set.seed(123)
>
> print("============================================================")
> print("INPUT DATAFRAME: Customer Purchase History")
> print("============================================================")
> print(input_df)
> cat("\n")
>
> # ============================================================
> # CALCULATE JACCARD DISTANCE MATRIX
> # ============================================================
>
> purchase_matrix <- input_df[, -1]
> jaccard_dist <- vegdist(purchase_matrix, method = "jaccard")
> jaccard_matrix <- as.matrix(jaccard_dist)
>
> # ============================================================
> # OUTPUT DATAFRAME 1: Distance Matrix
> # ============================================================
>
> output_df1 <- as.data.frame(jaccard_matrix)
> names(output_df1) <- input_df$Customer_ID
> row.names(output_df1) <- input_df$Customer_ID
>
> print("============================================================")
> print("OUTPUT DATAFRAME 1: Jaccard Distance Matrix")
> print("(Lower values = more similar purchase patterns)")
> print("============================================================")
> print(round(output_df1, 3))
> cat("\n")
>
> # ============================================================
> # OUTPUT DATAFRAME 2: Similarity Score (1 - Distance)
> # ============================================================
>
> similarity_matrix <- 1 - jaccard_matrix
> output_df2 <- as.data.frame(similarity_matrix)
> names(output_df2) <- input_df$Customer_ID
> row.names(output_df2) <- input_df$Customer_ID
>
> print("============================================================")
> print("OUTPUT DATAFRAME 2: Jaccard Similarity Matrix")
> print("(Higher values = more similar purchase patterns)")
> print("============================================================")
> print(round(output_df2, 3))
> cat("\n")
>
> # ============================================================
> # OUTPUT DATAFRAME 3: Top Similar Customers (FULLY CORRECTED)
> # ============================================================
>
> # Create empty vectors for results
> customer_ids <- character()
> top_similar <- character()
> similarity_scores <- numeric()
>
> # Loop through each customer
> for(i in 1:nrow(output_df2)) {
+   customer_id <- input_df$Customer_ID[i]
+
+   # Get similarities as a numeric vector (CRITICAL FIX)
+   sim_vector <- as.numeric(output_df2[i, ])
+   names(sim_vector) <- names(output_df2)
+
+   # Remove self (similarity = 1)
+   sim_vector_no_self <- sim_vector[names(sim_vector) != customer_id]
+
+   # Sort descending
+   sim_sorted <- sort(sim_vector_no_self, decreasing = TRUE)
+
+   # Get top 3
+   top3_names <- names(sim_sorted[1:min(3, length(sim_sorted))])
+   top3_score <- round(sim_sorted[1], 3)
+
+   customer_ids <- c(customer_ids, customer_id)
+   top_similar <- c(top_similar, paste(top3_names, collapse = ", "))
+   similarity_scores <- c(similarity_scores, top3_score)
+ }
>
> output_df3 <- data.frame(
+   Customer_ID = customer_ids,
+   Top_3_Similar_Customers = top_similar,
+   Max_Similarity_Score = similarity_scores
+ )
>
> print("============================================================")
> print("OUTPUT DATAFRAME 3: Recommendations Based on Similarity")
> print("============================================================")
> print(output_df3)
> cat("\n")
>
> # ============================================================
> # BUSINESS INSIGHTS
> # ============================================================
>
> print("============================================================")
> print("BUSINESS INSIGHTS")
> print("============================================================")
>
> # Most unique customer (highest avg distance to others)
> avg_distances <- rowMeans(jaccard_matrix)
> most_unique <- input_df$Customer_ID[which.max(avg_distances)]
> most_typical <- input_df$Customer_ID[which.min(avg_distances)]
>
> print(paste("- Most unique customer:", most_unique,
+             "(different shopping pattern)"))
> print(paste("- Most typical customer:", most_typical,
+             "(represents average customer)"))
>
> # Customer pairs using similarity matrix
> melted <- melt(as.matrix(output_df2))
> names(melted) <- c("Customer1", "Customer2", "Similarity")
> melted <- melted[melted$Customer1 != melted$Customer2, ]
> melted$Similarity <- round(melted$Similarity, 3)
>
> # Most similar pair
> top_pair <- melted[order(-melted$Similarity)[1], ]
> print(paste("- Most similar customer pair:",
+             as.character(top_pair$Customer1), "&", as.character(top_pair$Customer2),
+             "(similarity:", top_pair$Similarity, ")"))
>
> # Most different pair
> bottom_pair <- melted[order(melted$Similarity)[1], ]
> print(paste("- Most different customer pair:",
+             as.character(bottom_pair$Customer1), "&", as.character(bottom_pair$Customer2),
+             "(similarity:", bottom_pair$Similarity, ")"))
>
> # ============================================================
> # Calculate average similarity for each customer
> # ============================================================
>
> avg_similarity <- rowMeans(output_df2)
> avg_similarity_df <- data.frame(
+   Customer_ID = input_df$Customer_ID,
+   Avg_Similarity_To_All = round(avg_similarity, 3)
+ )
>
> # CORRECTED VERSION - No duplicate column
> avg_similarity_df <- data.frame(
+   Avg_Similarity_To_All = round(avg_similarity, 3)
+ )
> rownames(avg_similarity_df) <- input_df$Customer_ID
>
> print("============================================================")
> print("AVERAGE SIMILARITY SCORES")
> print("============================================================")
> print(avg_similarity_df[order(-avg_similarity_df$Avg_Similarity_To_All), , drop = FALSE])
>

NOTE: Processing of R statements complete

196       import r=input_df      data=workx.input          ;
NOTE: Creating data set 'WORKX.input' from R data frame 'input_df'
NOTE: Data set "WORKX.input" has 10 observation(s) and 9 variable(s)

197       import r=output_df1    data=workx.jaccard_matrix ;
NOTE: Creating data set 'WORKX.jaccard_matrix' from R data frame 'output_df1'
NOTE: Data set "WORKX.jaccard_matrix" has 10 observation(s) and 10 variable(s)

198       import r=output_df2    data=workx.similarity_matrix;
NOTE: Creating data set 'WORKX.similarity_matrix' from R data frame 'output_df2'
NOTE: Data set "WORKX.similarity_matrix" has 10 observation(s) and 10 variable(s)

199       import r=output_df3    data=workx.top_customers;
NOTE: Creating data set 'WORKX.top_customers' from R data frame 'output_df3'
NOTE: Data set "WORKX.top_customers" has 10 observation(s) and 3 variable(s)

200       run;quit;
NOTE: Procedure r step took :
      real time : 2.117
      cpu time  : 0.046


201
202
ERROR: Error printed on page 1

NOTE: Submitted statements took :
      real time : 2.259
      cpu time  : 0.125

/*              _
  ___ _ __   __| |
 / _ \ `_ \ / _` |
|  __/ | | | (_| |
 \___|_| |_|\__,_|

*/
