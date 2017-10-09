
context("table")
test_that("RemoveRowsAndOrColumns works",
          {
              x <- matrix(NA, 3, 3, dimnames = list(LETTERS[1:3],LETTERS[1:3]))
              x <- RemoveRowsAndOrColumns(x, "A", c("C","A"))
              expect_equal(prod(dim(x)), 2)
          })

dat <- structure(c(39.9370629370629, 45.9459459459459, 43.1311475409836,
54.2222222222222, 43.7954545454545, 42, 40.2748538011696, 44.6754966887417,
42.3385093167702), .Dim = c(9L, 1L), .Dimnames = list(c("Coca Cola ",
"Diet Coke", "Coke Zero", "Pepsi Light ", "Pepsi Max", "Pepsi ",
"NET Sugarred", "NET Sugarless", "NET"), "Age in years"), statistic = "Average", name = "Q3. Age in years by Preferred cola", questions = c("Q3. Age in years",
"Preferred cola"))

test_that("RemoveRowsAndOrColumns handles NULL",
          {
              d2 <- RemoveRowsAndOrColumns(dat, row.names.to.remove = NULL)
              expect_equal(nrow(d2), 9)
          })

test_that("RemoveRowsAndOrColumns handles spaces in middle",
          {
              d3 <- RemoveRowsAndOrColumns(dat, row.names.to.remove = "NET Sugarless,NET Sugarred,   Net")
              expect_equal(nrow(d3), 6)
          })

test_that("RemoveRowsAndOrColumns handles trailing spaces",
          {
              d4 <- RemoveRowsAndOrColumns(dat, row.names.to.remove = "Pepsi")
              expect_equal(nrow(d4), 8)
          })

test_that("RemoveRowAndOrColumns ignores lower/upper case",
          {
              d5 <- RemoveRowsAndOrColumns(dat, row.names.to.remove = "diet coke, coke zero")
              expect_equal(nrow(d5), 7)
          })

test_that("RemoveRowsAndOrColumns perserves attriubtes",
{
    out <- RemoveRowsAndOrColumns(dat, row.names.to.remove = "diet coke, coke zero")
    expect_equal(attr(out, "name"), attr(dat, "name"))
    expect_equal(attr(out, "questions"), attr(dat, "questions"))
    expect_equal(attr(out, "statistic"), attr(dat, "statistic"))
})

        dat <- structure(list(Q6_A = structure(c(3L, 5L, 5L, 6L, 4L, 1L, 3L,
    6L, 5L, 6L, 6L, 5L, 5L, 4L, 3L, 6L, 6L, 5L, 5L, 4L), .Label = c("Don t Know",
    "Hate", "Dislike", "Neither like nor dislike", "Like", "Love"
    ), class = "factor", label = structure("Q6. Coca Cola", .Names = "Q6_A")),
        Q6_B = structure(c(5L, 2L, 6L, 3L, 6L, 1L, 4L, 3L, 5L, 6L,
        2L, 3L, 3L, 3L, 6L, 5L, 5L, 3L, 3L, 4L), .Label = c("Don t Know",
        "Hate", "Dislike", "Neither like nor dislike", "Like", "Love"
        ), class = "factor", label = structure("Q6. Diet Coke", .Names = "Q6_B")),
        Q6_C = structure(c(3L, 5L, 3L, 3L, 4L, 1L, 5L, 5L, 1L, 6L,
        2L, 3L, 3L, 5L, 3L, 5L, 5L, 3L, 5L, 6L), .Label = c("Don t Know",
        "Hate", "Dislike", "Neither like nor dislike", "Like", "Love"
        ), class = "factor", label = structure("Q6. Coke Zero", .Names = "Q6_C")),
        Q6_D = structure(c(4L, 5L, 4L, 3L, 4L, 1L, 3L, 4L, 5L, 5L,
        6L, 5L, 4L, 4L, 5L, 5L, 3L, 5L, 4L, 4L), .Label = c("Don t Know",
        "Hate", "Dislike", "Neither like nor dislike", "Like", "Love"
        ), class = "factor", label = structure("Q6. Pepsi", .Names = "Q6_D")),
        Q6_E = structure(c(2L, 4L, 2L, 3L, 6L, 6L, 3L, 3L, 5L, 5L,
        2L, 3L, 3L, 4L, 6L, 1L, 2L, 3L, 4L, 4L), .Label = c("Don t Know",
        "Hate", "Dislike", "Neither like nor dislike", "Like", "Love"
        ), class = "factor", label = structure("Q6. Pepsi Light", .Names = "Q6_E")),
        Q6_F = structure(c(6L, 6L, 2L, 3L, 3L, 6L, 3L, 5L, 4L, 4L,
        2L, 3L, 3L, 5L, 3L, 1L, 2L, 3L, 4L, 4L), .Label = c("Don t Know",
        "Hate", "Dislike", "Neither like nor dislike", "Like", "Love"
        ), class = "factor", label = structure("Q6. Pepsi Max", .Names = "Q6_F"))), .Names = c("Q6_A",
    "Q6_B", "Q6_C", "Q6_D", "Q6_E", "Q6_F"), row.names = c(NA, 20L
                                                           ), class = "data.frame")
    attr(dat, "statistic") <- "means"

test_that("RemoveRowsAndOrColumns keeps data.frame col. attrs",
{
    out <- RemoveRowsAndOrColumns(dat, column.names.to.remove = "Q6_A", row.names.to.remove = "3")
    expect_equal(flipFormat::Labels(out), flipFormat::Labels(dat)[-1])
    expect_equal(dim(out), dim(dat) - c(1, 1))
    expect_equal(attr(out, "statistic"), attr(dat, "statistic"))
})

test_that("RemoveRowsAndOrColumns keeps data.frame col. attrs, only one col remains",
{
    out <- RemoveRowsAndOrColumns(dat, column.names.to.remove = c("Q6_B", "Q6_F", "Q6_C", "Q6_D", "Q6_A"))
    expect_equal(flipFormat::Labels(out), flipFormat::Labels(dat)[5])
    expect_equal(dim(out), c(nrow(dat), 1L))
    expect_equal(attr(out, "statistic"), attr(dat, "statistic"))
})

test_that("RemoveRowsAndOrColumns data.frame no col. attrs",
{
    dat <- data.frame(x = 1:5, y = 1:5, z = as.factor(c(1,2,1,2,0)))
    attr(dat, "statistic") <- "SUM"
    out <- RemoveRowsAndOrColumns(dat, column.names.to.remove = "x", row.names.to.remove = "2")
    expect_equal(dim(out), dim(dat) - c(1, 1))
    expect_equal(colnames(out), colnames(dat)[-1])
    expect_equal(attr(out, "statistic"), attr(dat, "statistic"))

    out <- RemoveRowsAndOrColumns(dat, column.names.to.remove = c("x", "z"))
    expect_equal(dim(out), dim(dat) - c(0, 2))
    expect_is(out, "data.frame")

    dat <- data.frame(x = 1:2, z = 1:2)
    out <- RemoveRowsAndOrColumns(dat)
    expect_equal(dim(dat), dim(out))
})


test_that("RemoveRowsAndOrColumns DS-1565 CC comment",
{
    dat <- structure(list(foo = structure(c(1,
    2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
    20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
    36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51,
    52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67,
    68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83,
    84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99,
    100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112,
    113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125,
    126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138,
    139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151,
    152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164,
    165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177,
    178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190,
    191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 1001, 1002,
    1003, 1004, 1005, 1006, 1007, 1008, 1009, 1010, 1011, 1012, 1013,
    1014, 1015, 1016, 1017, 1018, 1019, 1020, 1021, 1022, 1023, 1024,
    1025, 1026, 1027, 1028, 1029, 1030, 1031, 1032, 1033, 1034, 1035,
    1036, 1037, 1038, 1039, 1040, 1041, 1042, 1043, 1044, 1045, 1046,
    1047, 1048, 1049, 1050, 1051, 1052, 1053, 1054, 1055, 1056, 1057,
    1058, 1059, 1060, 1061, 1062, 1063, 1064, 1065, 1066, 1067, 1068,
    1069, 1070, 1071, 1072, 1073, 1074, 1075, 1076, 1077, 1078, 1079,
    1080, 1081, 1082, 1083, 1084, 1085, 1086, 1087, 1088, 1089, 1090,
    1091, 1092, 1093, 1094, 1095, 1096, 1097, 1098, 1099, 1100, 1101,
    1102, 1103, 1104, 1105, 1106, 1107, 1108, 1109, 1110, 1111, 1112,
    1113, 1114, 1115, 1116, 1117, 1118, 1119, 1120, 1121, 1122, 1123,
    1124, 1125, 1126, 1127, 1128, 1129, 1130, 1131, 1132, 1133, 1134,
    1135, 1136, 1137, 1138, 1139, 1140, 1141, 1142, 1143, 1144, 1145,
    1146, 1147, 1148, 1149, 1150, 1151, 1152, 1153, 1154, 1155, 1156,
    1157, 1158, 1159, 1160, 1161, 1162, 1163, 1164, 1165, 1166, 1167,
    1168, 1169, 1170, 1171, 1172, 1173, 1174, 1175, 1176, 1177, 1178,
    1179, 1180, 1181, 1182, 1183, 1184, 1185, 1186, 1187, 1188, 1189,
    1190, 1191, 1192, 1193, 1194, 1195, 1196, 1197, 1198, 1199, 1200,
    2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011,
    2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022,
    2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030, 2031, 2032, 2033,
    2034, 2035, 2036, 2037, 2038, 2039, 2040, 2041, 2042, 2043, 2044,
    2045, 2046, 2047, 2048, 2049, 2050, 2051, 2052, 2053, 2054, 2055,
    2056, 2057, 2058, 2059, 2060, 2061, 2062, 2063, 2064, 2065, 2066,
    2067, 2068, 2069, 2070, 2071, 2072, 2073, 2074, 2075, 2076, 2077,
    2078, 2079, 2080, 2081, 2082, 2083, 2084, 2085, 2086, 2087, 2088,
    2089, 2090, 2091, 2092, 2093, 2094, 2095, 2096, 2097, 2098, 2099,
    2100, 2101, 2102, 2103, 2104, 2105, 2106, 2107, 2108, 2109, 2110,
    2111, 2112, 2113, 2114, 2115, 2116, 2117, 2118, 2119, 2120, 2121,
    2122, 2123, 2124, 2125, 2126, 2127, 2128, 2129, 2130, 2131, 2132,
    2133, 2134, 2135, 2136, 2137, 2138, 2139, 2140, 2141, 2142, 2143,
    2144, 2145, 2146, 2147, 2148, 2149, 2150, 2151, 2152, 2153, 2154,
    2155, 2156, 2157, 2158, 2159, 2160, 2161, 2162, 2163, 2164, 2165,
    2166, 2167, 2168, 2169, 2170, 2171, 2172, 2173, 2174, 2175, 2176,
    2177, 2178, 2179, 2180, 2181, 2182, 2183, 2184, 2185, 2186, 2187,
    2188, 2189, 2190, 2191, 2192, 2193, 2194, 2195, 2196, 2197, 2198,
    2199, 2200, 3001, 3002, 3003, 3004, 3005, 3006, 3007, 3008, 3009,
    3010, 3011, 3012, 3013, 3014, 3015, 3016, 3017, 3018, 3019, 3020,
    3021, 3022, 3023, 3024, 3025, 3026, 3027, 3028, 3029, 3030, 3031,
    3032, 3033, 3034, 3035, 3036, 3037, 3038, 3039, 3040, 3041, 3042,
    3043, 3044, 3045, 3046, 3047, 3048, 3049, 3050, 3051, 3052, 3053,
    3054, 3055, 3056, 3057, 3058, 3059, 3060, 3061, 3062, 3063, 3064,
    3065, 3066, 3067, 3068, 3069, 3070, 3071, 3072, 3073, 3074, 3075,
    3076, 3077, 3078, 3079, 3080, 3081, 3082, 3083, 3084, 3085, 3086,
    3087, 3088, 3089, 3090, 3091, 3092, 3093, 3094, 3095, 3096, 3097,
    3098, 3099, 3100, 3101, 3102, 3103, 3104, 3105, 3106, 3107, 3108,
    3109, 3110, 3111, 3112, 3113, 3114, 3115, 3116, 3117, 3118, 3119,
    3120, 3121, 3122, 3123, 3124, 3125, 3126, 3127, 3128, 3129, 3130,
    3131, 3132, 3133, 3134, 3135, 3136, 3137, 3138, 3139, 3140, 3141,
    3142, 3143, 3144, 3145, 3146, 3147, 3148, 3149, 3150, 3151, 3152,
    3153, 3154, 3155, 3156, 3157, 3158, 3159, 3160, 3161, 3162, 3163,
    3164, 3165, 3166, 3167, 3168, 3169, 3170, 3171, 3172, 3173, 3174,
    3175, 3176, 3177, 3178, 3179, 3180, 3181, 3182, 3183, 3184, 3185,
    3186, 3187, 3188, 3189, 3190, 3191, 3192, 3193, 3194, 3195, 3196,
    3197, 3198, 3199, 3200), name = "UniqueID", label = "Unique Identifier", question = "Unique Identifier", questiontype = "Number"),
        bar = structure(c(3L,
        5L, 4L, 5L, 7L, 1L, 2L, 5L, 3L, 7L, 6L, 2L, 7L, 6L, 6L, 7L,
        4L, 7L, 7L, 6L, 7L, 4L, 6L, 4L, 6L, 2L, 5L, 3L, 2L, 8L, 3L,
        4L, 3L, 6L, 2L, 8L, 2L, 5L, 6L, 6L, 7L, 2L, 5L, 7L, 7L, 4L,
        6L, 5L, 7L, 4L, 2L, 3L, 5L, 8L, 5L, 7L, 8L, 4L, 6L, 4L, 2L,
        7L, 2L, 7L, 4L, 8L, 2L, 3L, 6L, 6L, 6L, 2L, 7L, 2L, 5L, 4L,
        1L, 7L, 3L, 4L, 5L, 5L, 5L, 8L, 5L, 5L, 6L, 6L, 4L, 5L, 4L,
        5L, 6L, 3L, 8L, 5L, 8L, 3L, 2L, 2L, 6L, 7L, 7L, 8L, 5L, 5L,
        5L, 7L, 2L, 7L, 5L, 2L, 6L, 6L, 8L, 5L, 6L, 2L, 7L, 6L, 3L,
        6L, 7L, 5L, 6L, 5L, 7L, 3L, 4L, 6L, 3L, 6L, 4L, 6L, 5L, 5L,
        2L, 7L, 8L, 6L, 2L, 5L, 4L, 3L, 5L, 2L, 2L, 6L, 2L, 2L, 2L,
        3L, 5L, 4L, 3L, 7L, 6L, 2L, 3L, 7L, 8L, 2L, 3L, 3L, 4L, 7L,
        5L, 3L, 5L, 5L, 2L, 3L, 3L, 7L, 5L, 2L, 6L, 4L, 5L, 7L, 3L,
        3L, 6L, 6L, 8L, 8L, 6L, 8L, 6L, 2L, 5L, 1L, 6L, 7L, 7L, 3L,
        6L, 6L, 5L, 6L, 12L, 11L, 13L, 11L, 10L, 13L, 14L, 8L, 12L,
        11L, 14L, 9L, 13L, 13L, 8L, 8L, 12L, 14L, 10L, 8L, 11L, 8L,
        10L, 13L, 11L, 10L, 10L, 10L, 9L, 14L, 11L, 8L, 14L, 10L,
        13L, 8L, 14L, 9L, 10L, 13L, 8L, 13L, 13L, 9L, 11L, 10L, 11L,
        12L, 9L, 12L, 11L, 14L, 12L, 12L, 14L, 8L, 10L, 9L, 13L,
        11L, 13L, 14L, 9L, 10L, 14L, 9L, 8L, 14L, 12L, 9L, 9L, 9L,
        11L, 11L, 14L, 10L, 8L, 9L, 12L, 13L, 9L, 8L, 14L, 8L, 13L,
        11L, 10L, 9L, 10L, 10L, 10L, 12L, 14L, 11L, 9L, 8L, 10L,
        9L, 13L, 10L, 13L, 9L, 13L, 8L, 9L, 12L, 11L, 14L, 12L, 10L,
        13L, 9L, 14L, 9L, 14L, 13L, 10L, 11L, 14L, 14L, 10L, 13L,
        14L, 12L, 10L, 9L, 10L, 9L, 13L, 8L, 10L, 11L, 10L, 10L,
        13L, 11L, 13L, 9L, 9L, 14L, 10L, 9L, 12L, 8L, 12L, 13L, 11L,
        9L, 12L, 8L, 14L, 9L, 11L, 9L, 12L, 10L, 8L, 13L, 11L, 9L,
        8L, 9L, 8L, 9L, 10L, 14L, 12L, 10L, 13L, 13L, 12L, 11L, 14L,
        11L, 8L, 14L, 11L, 14L, 10L, 14L, 13L, 11L, 9L, 11L, 11L,
        12L, 12L, 9L, 14L, 11L, 13L, 12L, 14L, 14L, 13L, 11L, 13L,
        11L, 14L, 8L, 15L, 20L, 18L, 20L, 16L, 19L, 20L, 18L, 20L,
        20L, 16L, 16L, 18L, 15L, 20L, 20L, 16L, 17L, 16L, 17L, 20L,
        20L, 15L, 20L, 18L, 15L, 20L, 15L, 21L, 18L, 16L, 19L, 20L,
        16L, 19L, 19L, 17L, 16L, 15L, 16L, 20L, 15L, 15L, 20L, 20L,
        16L, 21L, 17L, 18L, 20L, 19L, 21L, 18L, 16L, 15L, 19L, 20L,
        15L, 18L, 20L, 17L, 16L, 20L, 17L, 16L, 15L, 19L, 19L, 14L,
        18L, 16L, 19L, 16L, 20L, 20L, 16L, 21L, 18L, 16L, 15L, 15L,
        15L, 15L, 20L, 16L, 15L, 15L, 19L, 16L, 19L, 19L, 20L, 18L,
        16L, 20L, 18L, 17L, 19L, 15L, 16L, 21L, 16L, 20L, 16L, 18L,
        15L, 20L, 16L, 20L, 16L, 20L, 17L, 17L, 20L, 21L, 18L, 17L,
        18L, 16L, 21L, 16L, 20L, 16L, 18L, 16L, 18L, 16L, 18L, 20L,
        17L, 16L, 17L, 17L, 15L, 17L, 18L, 16L, 20L, 19L, 15L, 18L,
        19L, 18L, 15L, 15L, 20L, 17L, 16L, 16L, 19L, 21L, 18L, 19L,
        15L, 16L, 15L, 21L, 17L, 20L, 19L, 19L, 19L, 15L, 18L, 16L,
        17L, 20L, 19L, 21L, 17L, 15L, 19L, 19L, 21L, 14L, 16L, 20L,
        15L, 15L, 16L, 15L, 17L, 21L, 18L, 17L, 16L, 19L, 16L, 18L,
        15L, 18L, 19L, 17L, 18L, 16L, 20L, 17L, 16L, 19L, 21L, 27L,
        23L, 22L, 25L, 26L, 22L, 23L, 25L, 23L, 26L, 26L, 25L, 24L,
        24L, 26L, 22L, 27L, 26L, 24L, 23L, 24L, 25L, 24L, 26L, 25L,
        24L, 21L, 27L, 24L, 21L, 27L, 23L, 25L, 23L, 26L, 27L, 22L,
        22L, 26L, 22L, 22L, 23L, 24L, 26L, 25L, 25L, 26L, 23L, 22L,
        24L, 22L, 26L, 26L, 27L, 27L, 24L, 21L, 25L, 27L, 23L, 22L,
        25L, 21L, 27L, 24L, 23L, 21L, 24L, 24L, 23L, 26L, 23L, 23L,
        23L, 26L, 23L, 25L, 23L, 25L, 24L, 26L, 25L, 25L, 25L, 24L,
        22L, 27L, 22L, 21L, 26L, 27L, 27L, 27L, 27L, 27L, 22L, 23L,
        24L, 23L, 27L, 25L, 24L, 27L, 21L, 22L, 27L, 26L, 24L, 22L,
        21L, 23L, 24L, 27L, 21L, 27L, 26L, 27L, 26L, 26L, 21L, 25L,
        26L, 22L, 24L, 26L, 24L, 24L, 23L, 24L, 27L, 21L, 23L, 23L,
        25L, 22L, 21L, 25L, 23L, 26L, 23L, 27L, 23L, 23L, 22L, 22L,
        23L, 27L, 24L, 25L, 23L, 26L, 23L, 22L, 22L, 21L, 27L, 24L,
        24L, 23L, 24L, 22L, 24L, 24L, 25L, 23L, 27L, 27L, 25L, 25L,
        27L, 22L, 27L, 23L, 27L, 26L, 21L, 25L, 23L, 24L, 27L, 23L,
        21L, 25L, 24L, 24L, 27L, 24L, 22L, 22L, 24L, 21L, 23L, 25L,
        23L, 24L, 26L, 23L, 24L, 23L, 27L), .Label = c("19-Dec-11-01-Jan-12",
        "02-Jan-12-15-Jan-12", "16-Jan-12-29-Jan-12", "30-Jan-12-12-Feb-12",
        "13-Feb-12-26-Feb-12", "27-Feb-12-11-Mar-12", "12-Mar-12-25-Mar-12",
        "26-Mar-12-08-Apr-12", "09-Apr-12-22-Apr-12", "23-Apr-12-06-May-12",
        "07-May-12-20-May-12", "21-May-12-03-Jun-12", "04-Jun-12-17-Jun-12",
        "18-Jun-12-01-Jul-12", "02-Jul-12-15-Jul-12", "16-Jul-12-29-Jul-12",
        "30-Jul-12-12-Aug-12", "13-Aug-12-26-Aug-12", "27-Aug-12-09-Sep-12",
        "10-Sep-12-23-Sep-12", "24-Sep-12-07-Oct-12", "08-Oct-12-21-Oct-12",
        "22-Oct-12-04-Nov-12", "05-Nov-12-18-Nov-12", "19-Nov-12-02-Dec-12",
        "03-Dec-12-16-Dec-12", "17-Dec-12-30-Dec-12"), class = c("ordered",
        "factor"), name = "date", label = "Interview Date", question = "Interview Date", questiontype = "Date")), .Names = c("foo",
    "bar"
    ), row.names = c(NA, 800L), class = "data.frame")
    expect_silent(RemoveRowsAndOrColumns(dat))
    expect_equal(dim(RemoveRowsAndOrColumns(dat)), dim(dat))
})
