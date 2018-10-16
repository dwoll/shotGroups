#####-----------------------------------------------------------------------
## CEP estimate based on correlated bivariate normal distribution - mvnEll.R, hoyt.R
CEPCorrNormal <-
function(CEPlevel=0.5, ctr=c(0, 0), sigma=diag(length(ctr)), accuracy=FALSE) {
    sigma <- as.matrix(sigma)
    p     <- ncol(sigma)
    CorrNormal <- if(accuracy) {  # POA != POI
        ## quantile from offset circle probability - most general case
        qmvnEll(CEPlevel, mu=numeric(p), sigma=sigma, e=diag(p), x0=ctr)
    } else {                      # POA == POI
        if(p == 2L) {             # 2D -> exact Hoyt distribution
            HP <- getHoytParam(sigma)
            qHoyt(CEPlevel, qpar=HP$q, omega=HP$omega)
        } else {                  # 1D/3D case
            qmvnEll(CEPlevel, mu=numeric(p), sigma=sigma, e=diag(p), x0=numeric(p))
        }
    }

    setNames(CorrNormal, CEPlevel)
}

####-----------------------------------------------------------------------
## Grubbs estimates (Grubbs, 1964, p54, p55-56) - grubbs.R
## Grubbs-Patnaik CEP estimate based on Patnaik two-moment central chi^2 approximation
CEPGrubbsPatnaik <-
function(CEPlevel=0.5, ctr=c(0, 0), sigma=diag(length(ctr)), accuracy=FALSE) {
    GPP <- getGrubbsParam(sigma, ctr=ctr, accuracy=accuracy)
    GrubbsPatnaik <- qChisqGrubbs(CEPlevel, m=GPP$m, v=GPP$v, n=GPP$n, type="Patnaik")
    setNames(GrubbsPatnaik, CEPlevel)
}

## Grubbs-Pearson CEP estimate based on Pearson three-moment central chi^2 approximation
CEPGrubbsPearson <-
function(CEPlevel=0.5, ctr=c(0, 0), sigma=diag(length(ctr)), accuracy=FALSE) {
    GPP <- getGrubbsParam(sigma, ctr=ctr, accuracy=accuracy)
    GrubbsPearson <- qChisqGrubbs(CEPlevel, m=GPP$m, v=GPP$v, nPrime=GPP$nPrime, type="Pearson")
    setNames(GrubbsPearson, CEPlevel)
}

## Grubbs-Liu CEP estimate based on four-moment non-central chi^2
## approximation (Liu, Tang & Zhang, 2009)
CEPGrubbsLiu <-
function(CEPlevel=0.5, ctr=c(0, 0), sigma=diag(length(ctr)), accuracy=FALSE) {
    GPP <- getGrubbsParam(sigma, ctr=ctr, accuracy=accuracy)
    GrubbsLiu <- qChisqGrubbs(CEPlevel, m=GPP$m, v=GPP$v, muX=GPP$muX,
                              varX=GPP$varX, l=GPP$l, delta=GPP$delta, type="Liu")
    setNames(GrubbsLiu, CEPlevel)
}

#####-----------------------------------------------------------------------
## Rayleigh CEP estimate from Singh, 1992 - rayleigh.R, rice.R, maxwell.R
CEPRayleigh <-
function(CEPlevel=0.5, ctr=c(0, 0), sigma=diag(length(ctr)), accuracy=FALSE, doRob=FALSE, xy) {
    sigma <- as.matrix(sigma)
    p     <- ncol(sigma)
    Rayleigh <- if(p == 1L) {     # 1D
        if(accuracy) {            # POA != POI -> non-central F distribution
            RiceParam <- getRiceParam(xy, doRob=doRob)
            N   <- length(xy)
            NU  <- RiceParam$nu
            SD  <- setNames(RiceParam$sigma["sigma"], NULL)
            NCP <- 1*(NU/SD)^2    # N*f^2, N=1 because interested in individual shot, not mean
            SD * sqrt(qf(CEPlevel, df1=1, df2=N-1, ncp=NCP))
        } else {                  # POA = POI -> half normal / central F distribution
            N  <- length(xy)
            SD <- getRayParam(xy, doRob=doRob)$sigma["sigma"]
            SD * sqrt(qchisq(CEPlevel, df=p))
        }
    } else if(p == 2L) {          # 2D
        if(accuracy) {            # POA != POI -> Rice distribution
            RiceParam <- getRiceParam(xy, doRob=doRob)
            qRice(CEPlevel, nu=RiceParam$nu, sigma=RiceParam$sigma["sigma"])
        } else {                  # POA = POI -> Rayleigh
            RayParam <- getRayParam(xy, doRob=doRob)
            qRayleigh(CEPlevel, scale=RayParam$sigma["sigma"])
        }
    } else if(p == 3L) {          # 3D
        MaxParam <- getRayParam(xy, doRob=doRob)
        if(accuracy) {            # POA != POI -> offset sphere probability
            ## circular covariance matrix with estimated M-B param sigma
            sigMat <- diag(rep(MaxParam$sigma["sigma"]^2, p))
            qmvnEll(CEPlevel, sigma=sigMat, mu=numeric(p), x0=ctr, e=diag(p))
        } else {                  # POA = POI -> Maxwell-Boltzmann distribution
            qMaxwell(CEPlevel, sigma=MaxParam$sigma["sigma"])
        }
    } else {
        warning("Rayleigh CEP is only available for 1D/2D/3D-data")
        rep(NA_real_, length(CEPlevel))
    }

    setNames(Rayleigh, CEPlevel)
}

#####-----------------------------------------------------------------------
## Zhang estimate from Zhang & An (2012) p570 eq. 7
CEPZhang <-
function(CEPlevel=0.5, ctr=c(0, 0), sigma=diag(length(ctr)), accuracy=FALSE,
         isotropic=FALSE, doRob=FALSE, xy) {
    sigma <- as.matrix(sigma)
    p     <- ncol(sigma)

    ## make sure eigenvalues >= 0 when very small
    ev     <- eigen(sigma)$values    # eigenvalues
    lambda <- ev*sign(ev)

    if(!accuracy) {
        ctr <- c(0, 0)
    }
    
    Zhang50 <- if((p == 2L) && isotropic) {    # assume equal variances
        sigma <- getRayParam(xy=xy, level=CEPlevel, doRob=doRob)[["sigma"]]["sigma"]
        r0    <- sqrt(sum((ctr/sigma)^2))      # p 570
        print(r0)
        if(r0 <= sqrt(5)) {
            A0 <-  1.17712063
            A1 <-  0.01635468
            A2 <-  0.21387229
            A3 <-  0.13972335
            A4 <- -0.08426949
            A5 <-  0.01284512
            sigma*(A0 + A1*r0 + A2*r0^2 + A3*r0^3 + A4*r0^4 + A5*r0^5)
        } else {
            if(r0 <= 3) {
                d <-   4
                B0 <-  2.2458024
                B1 <-  0.2200099
                B2 <- -0.01003873
                B3 <-  0.00078179
                B4 <- -0.00005397
                B5 <-  0.00000204
            } else if(r0 <= 4) {
                d  <-  9
                B0 <-  3.165246
                B1 <-  0.15761986
                B2 <- -0.00387106
                B3 <-  0.00017928
                B4 <- -0.00000822
                B5 <-  0.00000022
            } else if(r0 <= 5) {
                d  <- 16
                B0 <-  4.1243779
                B1 <-  0.12114747
                B2 <- -0.00177091
                B3 <-  0.00005016
                B4 <- -0.0000015
                B5 <-  0.00000003
            } else if(r0 <= sqrt(45)) {
                d  <- 25
                B0 <-  5.099676
                B1 <-  0.09800860
                B2 <- -0.00093530
                B3 <-  0.00001679
                B4 <- -0.00000029
                B5 <-  0 
            } else {
                warning("Isotropic case only defined up to r0 <= sqrt(45)")
                d  <- NA_real_
                B0 <- NA_real_
                B1 <- NA_real_
                B2 <- NA_real_
                B3 <- NA_real_
                B4 <- NA_real_
                B5 <- NA_real_
            }
            sigma * (B0 + B1*(r0^2 - d)   +
                          B2*(r0^2 - d)^2 +
                          B3*(r0^2 - d)^3 +
                          B4*(r0^2 - d)^4 +
                          B5*(r0^2 - d)^5)
        }
    } else if((p == 2L)) {                   # do not assume equal variances
        r1 <- sqrt(sum(ctr^2/lambda))        # p568
    
        ## coefficients for eq. 7 (personal communication)
        Bijk <- c("i  j  k        B
                  0  0  0       -1.50800
                  0  0  1       37.06914
                  0  0  2     -261.85730
                  0  0  3      992.70823
                  0  0  4    -2127.35633
                  0  0  5     2432.20862
                  0  0  6     -866.72077
                  0  0  7    -1043.20376
                  0  0  8     1007.57426
                  0  0  9      201.09441
                  0  0 10     -547.24802
                  0  0 11      178.38609
                  0  1  0       30.73407
                  0  1  1     -559.72454
                  0  1  2     4340.04504
                  0  1  3   -18573.68822
                  0  1  4    48237.39491
                  0  1  5   -78643.98158
                  0  1  6    80365.58824
                  0  1  7   -51299.58759
                  0  1  8    24063.13327
                  0  1  9   -14082.77353
                  0  1 10     8340.22172
                  0  1 11    -2217.07894
                  0  2  0     -140.53635
                  0  2  1     2615.24869
                  0  2  2   -21136.32531
                  0  2  3    95616.46894
                  0  2  4  -266698.90970
                  0  2  5   478107.64088
                  0  2  6  -557690.04739
                  0  2  7   425021.90498
                  0  2  8  -221989.27415
                  0  2  9    95629.50576
                  0  2 10   -38007.28570
                  0  2 11     8670.58029
                  0  3  0      307.16842
                  0  3  1    -5592.48090
                  0  3  2    45454.19525
                  0  3  3  -210124.94755
                  0  3  4   605819.65479
                  0  3  5 -1134958.18022
                  0  3  6  1399681.23997
                  0  3  7 -1135207.32583
                  0  3  8   611593.91350
                  0  3  9  -236561.30418
                  0  3 10    74259.96471
                  0  3 11   -14670.14344
                  0  4  0     -338.98709
                  0  4  1     5886.07371
                  0  4  2   -46868.89957
                  0  4  3   216011.08369
                  0  4  4  -628036.11139
                  0  4  5  1196820.36257
                  0  4  6 -1511973.91364
                  0  4  7  1259646.73040
                  0  4  8  -686657.86707
                  0  4  9   251291.10861
                  0  4 10   -67419.94690
                  0  4 11    11638.94476
                  0  5  0      150.64942
                  0  5  1    -2481.96624
                  0  5  2    19094.1026
                  0  5  3   -86299.71627
                  0  5  4   248634.95180
                  0  5  5  -473172.89850
                  0  5  6   600573.05422
                  0  5  7  -504295.29927
                  0  5  8   275477.36039
                  0  5  9   -97818.1152801
                  0  5 10    23834.43367
                  0  5 11    -3696.11403
                  1  0  0        7.30917
                  1  0  1     -121.95931
                  1  0  2      840.42710
                  1  0  3    -3062.11230
                  1  0  4     6186.79357
                  1  0  5    -6054.59489
                  1  0  6     -208.97884
                  1  0  7     6535.71511
                  1  0  8    -5133.24462
                  1  0  9     -260.95477
                  1  0 10     1917.57177
                  1  0 11     -645.78090
                  1  1  0     -110.31830
                  1  1  1     2017.70282
                  1  1  2   -15683.19635
                  1  1  3    67488.60741
                  1  1  4  -176701.28435
                  1  1  5   290580.88672
                  1  1  6  -298955.31158
                  1  1  7   191453.37431
                  1  1  8   -90293.34635
                  1  1  9    54007.00700
                  1  1 10   -32486.81832
                  1  1 11     8681.02815
                  1  2  0      498.12492
                  1  2  1    -9427.49801
                  1  2  2    77155.89222
                  1  2  3  -354370.79904
                  1  2  4 10006477.27304
                  1  2  5 -1840218.83052
                  1  2  6  2190104.05741
                  1  2  7 -1701102.53064
                  1  2  8   899284.68715
                  1  2  9  -383894.11296
                  1  2 10   149222.77548
                  1  2 11   -33723.07562
                  1  3  0    -1035.26688
                  1  3  1    19430.28736
                  1  3  2  -161868.38075
                  1  3  3   767936.17634
                  1  3  4 -2276698.65746
                  1  3  5  4389361.94030
                  1  3  6 -5567199.26107
                  1  3  7  4629872.46270
                  1  3  8 -2529969.48968
                  1  3  9   961497.85815
                  1  3 10  -285581.48386
                  1  3 11    54243.94576
                  1  4  0     1101.18523
                  1  4  1   -19807.40970
                  1  4  2   162886.78484
                  1  4  3  -776205.30786
                  1  4  4  2336828.66380
                  1  4  5 -4612153.15252
                  1  4  6  6025709.64684
                  1  4  7 -5168691.92531
                  1  4  8  2862608.29402
                  1  4  9 -1024135.74071
                  1  4 10   251524.75518
                  1  4 11   -39658.01666
                  1  5  0     -487.75337
                  1  5  1     8290.53777
                  1  5  2   -65870.58342
                  1  5  3   308197.20491
                  1  5  4  -921057.63790
                  1  5  5  1819395.02951
                  1  5  6 -2393829.33582
                  1  5  7  2073889.98097
                  1  5  8 -1152239.51773
                  1  5  9   398819.42046
                  1  5 10   -86623.36549
                  1  5 11    11513.64407
                  2  0  0       -8.39680
                  2  0  1      137.59442
                  2  0  2     -926.41844
                  2  0  3     3265.94858
                  2  0  4    -6100.14231
                  2  0  5     4278.05349
                  2  0  6     4933.42238
                  2  0  7   -12641.00423
                  2  0  8     8961.99007
                  2  0  9     -256.37747
                  2  0 10    -2517.52899
                  2  0 11      872.69812
                  2  1  0      151.85852
                  2  1  1    -2764.43629
                  2  1  2    21418.68153
                  2  1  3   -92079.73132
                  2  1  4   240640.93680
                  2  1  5  -392722.30429
                  2  1  6   395837.40643
                  2  1  7  -243259.82654
                  2  1  8   112039.90079
                  2  1  9   -74462.27711
                  2  1 10    48378.10091
                  2  1 11   -13174.53964
                  2  2  0     -696.05269
                  2  2  1    13235.79923
                  2  2  2  -108719.94280
                  2  2  3   502418.93939
                  2  2  4 -1437223.47953
                  2  2  5  2643175.11851
                  2  2  6 -3154239.38546
                  2  2  7  2448763.15222
                  2  2  8 -1297826.56619
                  2  2  9   566806.86704
                  2  2 10  -228178.84490
                  2  2 11    52471.09248
                  2  3  0     1400.85858
                  2  3  1   -26862.13543
                  2  3  2   227395.73358
                  2  3  3 -1096233.13091
                  2  3  4  3302421.47593
                  2  3  5 -6460706.72431
                  2  3  6  8294685.64827
                  2  3  7 -6962294.81091
                  2  3  8  3829716.07859
                  2  3  9 -1463576.88726
                  2  3 10   437814.27491
                  2  3 11   -83738.96835
                  2  4  0    -1432.43625
                  2  4  1    26735.89822
                  2  4  2  -226271.58166
                  2  4  3  1106145.22484
                  2  4  4 -3409574.42966
                  2  4  5  6873381.61298
                  2  4  6 -9144047.77589
                  2  4  7  7956459.96580
                  2  4  8 -4444585.05705
                  2  4  9  1586114.68543
                  2  4 10  -381893.75700
                  2  4 11    58951.23793
                  2  5  0      622.91812
                  2  5  1   -11035.33184
                  2  5  2    90838.15831
                  2  5  3  -438500.09965
                  2  5  4  1348016.12322
                  2  5  5 -2731098.77184
                  2  5  6  3673259.51515
                  2  5  7  -3238879.5041
                  2  5  8  1817714.84110
                  2  5  9  -624450.71184
                  2  5 10   129495.99917
                  2  5 11   -15978.20880
                  3  0  0        4.80967
                  3  0  1      -77.99167
                  3  0  2      516.96171
                  3  0  3    -1760.05286
                  3  0  4     2978.06661
                  3  0  5     -938.44505
                  3  0  6    -5613.89495
                  3  0  7    10344.99174
                  3  0  8    -6992.49989
                  3  0  9     -507.98776
                  3  0 10     1593.73407
                  3  0 11     -563.14246
                  3  1  0     -106.25020
                  3  1  1     1916.56093
                  3  1  2   -14700.28712
                  3  1  3   762447.47246
                  3  1  4  -160293.41157
                  3  1  5   253462.21660
                  3  1  6  -240535.58640
                  3  1  7   131669.95187
                  3  1  8   -55767.81377
                  3  1  9    47198.42290
                  3  1 10   -35116.21231
                  3  1 11     9820.81246
                  3  2  0      511.67374
                  3  2  1    -9673.11429
                  3  2  2    78791.45853
                  3  2  3  -360766.93776
                  3  2  4  1020321.00332
                  3  2  5 -1846866.00449
                  3  2  6  2154711.78200
                  3  2  7 -1626738.07815
                  3  2  8   852404.97838
                  3  2  9  -395825.66688
                  3  2 10   175015.53634
                  3  2 11   -41872.09886
                  3  3  0    -1027.83063
                  3  3  1    19916.0826 
                  3  3  2  -168649.08473
                  3  3  3   810228.93041
                  3  3  4 -2426699.32685
                  3  3  5  4707457.17127
                  3  3  6 -5975393.05967
                  3  3  7  4953340.57762
                  3  3  8 -2714680.08066
                  3  3  9  1072562.25276
                  3  3 10  -347415.58697
                  3  3 11    70336.92513
                  3  4  0     1011.50846
                  3  4  1   -19520.35600
                  3  4  2   167740.84261
                  3  4  3  -825219.08597
                  3  4  4  2547613.20509
                  3  4  5 -5126865.40135
                  3  4  6  6791764.92505
                  3  4  7 -5879408.35900
                  3  4  8  8282864.52405
                  3  4  9 -1196814.35705
                  3  4 10   308131.74625
                  3  4 11   -51281.82176
                  3  5  0     -419.17227
                  3  5  1     7799.23359
                  3  5  2   -66151.92153
                  3  5  3   325082.85879
                  3  5  4 -1009967.60753
                  3  5  5  2058208.63394
                  3  5  6 -2775472.43675
                  3  5  7  2449243.21250
                  3  5  8 -1377607.57999
                  3  5  9   479600.83760
                  3  5 10  -104225.66822
                  3  5 11    13904.43044
                  4  0  0       -1.41012
                  4  0  1       22.70521
                  4  0  2     -148.46582
                  4  0  3      489.74223
                  4  0  4     -747.89192
                  4  0  5      -89.38056
                  4  0  6     2297.32756
                  4  0  7    -3725.25391
                  4  0  8     2452.57493
                  4  0  9     -251.29178
                  4  0 10     -464.39654
                  4  0 11      165.46501
                  4  1  0       36.91482
                  4  1  1     -657.65046
                  4  1  2     4965.58653
                  4  1  3   -20654.80936
                  4  1  4    51363.19899
                  4  1  5   -76954.43695
                  4  1  6    65622.22571
                  4  1  7   -27839.71942
                  4  1  8     9047.66734
                  4  1  9   -13506.39268
                  4  1 10    12032.45580
                  4  1 11    -3452.84254
                  4  2  0     -189.23560
                  4  2  1     3534.90907
                  4  2  2   -28340.16312
                  4  2  3   127301.53392
                  4  2  4  -351465.57188
                  4  2  5   616163.17964
                  4  2  6  -688181.61871
                  4  2  7   492046.07078
                  4  2  8  -251414.70516
                  4  2  9   129307.34993
                  4  2 10   -65040.55204
                  4  2 11    16270.97692
                  4  3  0      389.49639
                  4  3  1    -7517.54072
                  4  3  2    62883.56972
                  4  3  3  -297233.52178
                  4  3  4   872933.18776
                  4  3  5 -1653991.63581
                  4  3  6  2041991.02768
                  4  3  7 -1644559.84876
                  4  3  8   891362.10361
                  4  3  9  -372834.21958
                  4  3 10   136205.14245
                  4  3 11   -29615.35099
                  4  4  0     -375.58749
                  4  4  1     7349.92108
                  4  4  2   -63036.60176
                  4  4  3   307356.35957
                  4  4  4  -936607.41424
                  4  4  5  1854763.70135
                  4  4  6  -2412369.0135
                  4  4  7  2051411.99056
                  4  4  8 -1138662.20737
                  4  4  9   431751.74719
                  4  4 10  -124649.03235
                  4  4 11    23056.76527
                  4  5  0      146.79700
                  4  5  1    -2827.54869
                  4  5  2    24329.22818
                  4  5  3  -120032.35962
                  4  5  4   372079.75852
                  4  5  5  -753454.52592
                  4  5  6  1006974.01091
                  4  5  7  -880553.11272
                  4  5  8   494280.92111
                  4  5  9  -176872.67948
                  4  5 10    42410.61903
                  4  5 11    -6478.29063
                  5  0  0        0.16415
                  5  0  1       -2.62846
                  5  0  2       17.01287
                  5  0  3      -54.90894
                  5  0  4       78.17490
                  5  0  5       32.16035
                  5  0  6     -297.35896
                  5  0  7      454.60877
                  5  0  8     -293.38420
                  5  0  9      -37.40186
                  5  0 10       44.46108
                  5  0 11      -15.65335
                  5  1  0       -4.95117
                  5  1  1       86.91975
                  5  1  2     -643.82509
                  5  1  3     2608.72592
                  5  1  4    -6236.22491
                  5  1  5     8724.68903
                  5  1  6    -6379.03883
                  5  1  7     1475.79282
                  5  1  8       45.93268
                  5  1  9     1381.76407
                  5  1 10    -1495.997
                  5  1 11      435.75596
                  5  2  0       27.39345
                  5  2  1     -503.58840
                  5  2  2     3956.27375
                  5  2  3   -17332.34425
                  5  2  4    46350.38848
                  5  2  5   -77842.78318
                  5  2  6    81803.47877
                  5  2  7   -53864.40054
                  5  2  8    26400.02294
                  5  2  9   -15729.73024
                  5  2 10     9100.16729
                  5  2 11    -2363.21625
                  5  3  0      -58.88322
                  5  3  1     1121.35889
                  5  3  2    -9198.10897
                  5  3  3    42452.68426
                  5  3  4  -121199.98901
                  5  3  5   221998.94268
                  5  3  6  -263183.30573
                  5  3  7   202902.59108
                  5  3  8  -108017.59066
                  5  3  9    48909.67004
                  5  3 10   -20479.54143
                  5  3 11     4749.49844
                  5  4  0       56.60322
                  5  4  1    -1105.18410
                  5  4  2      961.78170
                  5  4  3   -44835.28439
                  5  4  4   133645.11524
                  5  4  5  -257933.62666
                  5  4  6   325955.13839
                  5  4  7  -269490.82354
                  5  4  8   148047.67616
                  5  4  9   -59284.13086
                  5  4 10    19575.20456
                  5  4 11    -3990.41775
                  5  5  0      -20.77692
                  5  5  1      407.57911
                  5  5  2    -3526.02970
                  5  5  3    17340.16294
                  5  5  4   -53246.66952
                  5  5  5   106338.89246
                  5  5  6  -139773.52823
                  5  5  7   120269.52289
                  5  5  8   -67191.30684
                  5  5  9    24988.18495
                  5  5 10    -6761.78806
                  5  5 11     1175.12760")
        
        Bmat <- data.matrix(read.table(text=Bijk, header=TRUE))
            
        b <- Bmat[ , "B"]
        i <- Bmat[ , "i"]
        j <- Bmat[ , "j"]
        k <- Bmat[ , "k"]
        sum(b * r1^(i-j) * ctr[1]^j * sqrt(lambda[2])^k * sqrt(lambda[1])^(-j-k+1), na.rm=TRUE)
    } else {
        warning("Zhang CEP is only available for 2D-data")
        rep(NA_real_, length(CEPlevel))
    }
    
    if(any(CEPlevel != 0.5)) {
        warning("Zhang CEP is only available for CEPlevel 0.5")
    }
        
    Zhang <- ifelse(CEPlevel == 0.5, Zhang50, NA_real_)
    setNames(Zhang, CEPlevel)
}

#####-----------------------------------------------------------------------
## Krempasky CEP estimate from Krempasky, 2003
CEPKrempasky <-
function(CEPlevel=0.5, sigma=diag(2), accuracy=FALSE) {
    sigma <- as.matrix(sigma)
    p     <- ncol(sigma)
    ## only available for 2D 50% POA=POI case
    Krempasky50 <- if((p == 2L) && !accuracy) {
        ## estimated correlation, covariance and standard deviations
        rho    <- cov2cor(sigma)[1, 2]
        covXY  <- sigma[1, 2]
        sigmaX <- sqrt(sigma[1, 1])
        sigmaY <- sqrt(sigma[2, 2])

        ## rotation angle gamma and rotation matrix
        gamma <- atan((-2*covXY + sqrt(4*covXY^2 + (sigmaY^2 - sigmaX^2)^2)) /
                          (sigmaY^2 - sigmaX^2))

        A <- cbind(c(cos(gamma), sin(gamma)), c(-sin(gamma), cos(gamma)))

        ## covariance matrix, correlation and standard deviations of rotated data
        sigmaRot   <- t(A) %*% sigma %*% A     # covariance matrix
        rhoPrime   <- cov2cor(sigmaRot)[1, 2]  # correlation
        sigmaPrime <- sqrt(sigmaRot[1, 1])
        # isTRUE(all.equal(sigmaRot[1, 1], sigmaRot[2, 2])) # supposed to be equal

        CEP00 <- sigmaPrime*1.1774100225154746910115693264596996377473856893858
        C2    <- 0.3267132048600136726456919696354558579811249664099
        C4    <- 0.0568534980324428522403361717417556935145611584613
        ## CEP00 <- sigmaPrime*sqrt(2*log(2)) # sigmaPrime * qRayleigh(0.5, 1)
        ## C2    <- 0.5*(1 - log(2)/2)
        ## C4    <- C2*(log(2) - log(2)^2/4 - 0.5) + 3/8 - (9/16)*log(2) + (3/16)*log(2)^2 - (1/64)*log(2)^3 - C2^2*log(2)/2
        CEP00*(1 - 0.5*C2*rhoPrime^2 - 0.5*(C4 + 0.25*C2^2)*rhoPrime^4)
    } else {
        if(p != 2L) {
            warning("Krempasky CEP is only available for 2D-data")
        }

        if(accuracy) {
            warning("Krempasky CEP is only available for accuracy=FALSE")
        }

        rep(NA_real_, length(CEPlevel))
    }

    if(any(CEPlevel != 0.5)) {
        warning("Krempasky CEP is only available for CEPlevel 0.5")
    }

    Krempasky <- ifelse(CEPlevel == 0.5, Krempasky50, NA_real_)
    setNames(Krempasky, CEPlevel)
}

#####-----------------------------------------------------------------------
## Ignani estimate from Ignani (2010)
CEPIgnani <-
function(CEPlevel=0.5, sigma=diag(2), accuracy=FALSE) {
    sigma <- as.matrix(sigma)
    p     <- ncol(sigma)

    ## make sure eigenvalues >= 0 when very small
    ev      <- eigen(sigma)$values    # eigenvalues
    lambda  <- ev*sign(ev)
    alpha   <- sqrt(lambda[2] / lambda[1])
    beta    <- if(p == 3L) { sqrt(lambda[3] / lambda[2]) } else { 0 }
    betaVec <- c(1, beta, beta^2, beta^3)

    ## coefficients for polynomials in table 1
    tbl1 <- c("coef   R0.5    R0.9   R0.95   R0.99
               c11  0.6754  1.6494  1.9626  2.5686
               c12 -0.1547  0.0332 -0.0906 -0.1150
               c13  0.2616  1.3376  1.3214 -0.3475
               c14  1.0489 -0.8445 -0.3994  1.3570
               c21 -0.0208 -0.0588  0.0100  0.1479
               c22  1.1739 -0.5605  0.2722  0.9950
               c23  1.9540 -4.7170 -5.4821  1.3223
               c24 -5.5678  5.7135  3.9732 -4.8917
               c31  1.1009  0.3996  0.0700 -0.4285
               c32 -2.6375  1.5739  0.0462 -1.9795
               c33 -1.4838  5.3623  7.1658 -1.1104
               c34  6.5837 -7.9347 -5.7194  6.9617
               c41 -0.5821  0.1636  0.4092  0.7371
               c42  1.5856 -1.0747 -0.1953  1.2495
               c43 -0.0678 -1.7785 -3.0134 -0.2061
               c44 -2.3324  3.2388  2.4661 -2.8968")
    coefDF <- read.table(text=tbl1, header=TRUE)

    getR <- function(level) {
        column <- paste0("R", level)
        c1 <- coefDF[ 1:4,  column]
        c2 <- coefDF[ 5:8,  column]
        c3 <- coefDF[ 9:12, column]
        c4 <- coefDF[13:16, column]
        sqrt(lambda[1]) * (crossprod(c1, betaVec) +
                           crossprod(c2, betaVec)*alpha +
                           crossprod(c3, betaVec)*alpha^2 +
                           crossprod(c4, betaVec)*alpha^3)
    }

    Ignani <- if((p %in% c(2L, 3L)) && !accuracy) {
        vapply(CEPlevel, function(x) {
            if(x %in% c(0.5, 0.9, 0.95, 0.99)) { getR(x) } else { NA_real_ }
        }, numeric(1))
    } else {
        if(!(p %in% c(2L, 3L))) {
            warning("Ignani CEP is only available for 2D/3D-data")
        }

        if(accuracy) {
            warning("Ignani CEP is only available for accuracy=FALSE")
        }

        rep(NA_real_, length(CEPlevel))
    }

    if(!any(CEPlevel %in% c(0.5, 0.9, 0.95, 0.99))) {
        warning("Ignani CEP is only available for CEPlevel 0.5, 0.9, 0.95, 0.99")
    }

    setNames(Ignani, CEPlevel)
}

#####-----------------------------------------------------------------------
## RMSE-based estimate from van Diggelen (2007)
## CEP = sqrt(qchisq(0.5, 2)) * RMSEx
## RMSExy = sqrt(2) * RMSEx = sqrt(2) * (1/sqrt(qchisq(0.5, 2)))*CEP
CEPRMSE <-
function(CEPlevel=0.5, sigma=diag(2), accuracy=FALSE, xy) {
    sigma <- as.matrix(sigma)
    p     <- ncol(sigma)

    ## root mean squared error
    RMSE_AT <- sqrt(sum(colMeans(xy^2))) # non-centered data
    RMSE_AF <- sqrt(sum(diag(sigma)))    # centered data

    ## conversion factor from RMSExy or RMSExyz to CEP
    ## 2D: (1/sqrt(2)) * qRayleigh(CEPlevel, scale=1)
    ##     for 50%, this is just sqrt(log(2))
    ## 3D: (1/sqrt(3)) * qMaxwell(CEPlevel, sigma=1)
    RMSE2CEP <- (1/sqrt(p)) * sqrt(qchisq(CEPlevel, df=p))
    RMSE <- if(accuracy) {
        ## POA != POI -> essentially Rice (2D) / offset sphere (3D)
        RMSE2CEP * RMSE_AT
    } else {
        ## POA = POI -> essentially Rayleigh (2D) / Maxwell-Boltzmann(3D)
        RMSE2CEP * RMSE_AF
    }

    setNames(RMSE, CEPlevel)
}

#####-----------------------------------------------------------------------
## Ethridge CEP estimate from Ethridge (1983) after Puhek (1992)
CEPEthridge <-
function(CEPlevel=0.5, accuracy=FALSE, xy) {
    lnDTC <- if(accuracy) {              # log distance to group center (radius)
        rSqSum <- sqrt(rowSums(xy^2))    # log radii to origin = point of aim
        log(rSqSum)
    } else {
        log(getDistToCtr(xy))            # log radii to group center
    }

    mLnDTC   <- mean(lnDTC)              # mean log radius
    medLnDTC <- median(lnDTC)            # median log radius
    varLnDTC <- var(lnDTC)               # variance log radius

    ## weighted mean after Hogg (1967)
    ## sample kurtosis log radius
    kLnDTC <- mean((lnDTC - mLnDTC)^4) / mean((lnDTC - mLnDTC)^2)^2
    dHogg  <- pmax(1 + (0.03 * (kLnDTC-3)^3 * (lnDTC-medLnDTC)^2 / varLnDTC), 0.01)
    wHogg  <- (1/dHogg) / sum(1/dHogg)   # weighting factors
    uHogg  <- sum(wHogg * lnDTC)         # log median radius estimate
    Ethridge50 <- exp(uHogg)

    if(any(CEPlevel != 0.5)) {
        warning("Ethridge CEP estimate is only available for CEPlevel 0.5")
    }

    Ethridge <- ifelse(CEPlevel == 0.5, Ethridge50, NA_real_)
    setNames(Ethridge, CEPlevel)
}

#####-----------------------------------------------------------------------
## modified RAND-234 CEP estimate for 50% from Williams, 1997
## using the semi-major and semi-minor axes of the error ellipse (PCA)
CEPRAND <-
function(CEPlevel=0.5, ctr=c(0, 0), sigma=diag(length(ctr)), accuracy=FALSE) {
    sigma <- as.matrix(sigma)
    p     <- ncol(sigma)

    ## make sure eigenvalues >= 0 when very small
    ev     <- eigen(sigma)$values    # eigenvalues
    lambda <- ev*sign(ev)

    RAND50 <- if(p == 2L) {          # only available for 2D case
        RAND50MPI <- 0.5620*sqrt(lambda[1]) + 0.6152*sqrt(lambda[2])
        if(accuracy) {        # take systematic location bias into account
            bias <- sqrt(sum(ctr^2)) / RAND50MPI
            if(bias > 2.2) {
                warning(c("RAND location bias estimate is ",
                          round(bias, 2), " (> 2.2),\n",
                          "more than what RAND CEP should be considered for"))
            }

            ## cubic regression to take bias into account
            RAND50MPI * (1.0039 - 0.0528*bias + 0.4786*bias^2 - 0.0793*bias^3)
        } else {                         # ignore location bias
            RAND50MPI
        }                                # if(accuracy)
    } else {                       # 1D/3D case
        warning("RAND CEP estimate is only available for 2D-data")
        rep(NA_real_, length(CEPlevel))
    }

    if(any(CEPlevel != 0.5)) {
        warning("RAND CEP estimate is only available for CEPlevel 0.5")
    }

    RAND <- ifelse(CEPlevel == 0.5, RAND50, NA_real_)
    setNames(RAND, CEPlevel)
}

#####-----------------------------------------------------------------------
## Valstar CEP estimate for 50% from Williams, 1997
## using the semi-major and semi-minor axes of the error ellipse (PCA)
CEPValstar <-
function(CEPlevel=0.5, ctr=c(0, 0), sigma=diag(length(ctr)), accuracy=FALSE) {
    sigma  <- as.matrix(sigma)
    p      <- ncol(sigma)
    aspRat <- sqrt(kappa(sigma, exact=TRUE))

    ## make sure eigenvalues >= 0 when very small
    ev     <- eigen(sigma)$values    # eigenvalues
    lambda <- ev*sign(ev)

    Valstar50 <- if(p == 2L) {       # only available for 2D case
        ValstarMPI <- if((1/aspRat) <= 0.369) {
            0.675*sqrt(lambda[1]) + sqrt(lambda[2])/(1.2*sqrt(lambda[1]))
        } else {
            0.5620*sqrt(lambda[1]) + 0.6152*sqrt(lambda[2])  # almost RAND
        }

        if(accuracy) {               # POA != POI
            Valstar <- sqrt(ValstarMPI^2 + sum(ctr^2))
        } else {                     # POA = POI
            ValstarMPI
        }                            # if(accuracy)
    } else {                         # 1D/3D case
        warning("Valstar CEP estimate is only available for 2D-data")
        rep(NA_real_, length(CEPlevel))
    }

    if(any(CEPlevel != 0.5)) {
        warning("Valstar CEP estimate is only available for CEPlevel 0.5")
    }

    Valstar <- ifelse(CEPlevel == 0.5, Valstar50, NA_real_)
    setNames(Valstar, CEPlevel)
}

# ## Siouris, GM. 1993. Appendix A
# CEPSiouris <- function(sigma) {
#     sigma  <- as.matrix(sigma)
#     p      <- ncol(sigma)
#     aspRat <- sqrt(kappa(sigma, exact=TRUE))
#
#     ## make sure eigenvalues >= 0 when very small
#     ev     <- eigen(sigma)$values    # eigenvalues
#     lambda <- ev*sign(ev)
#
#     if(p == 2L) {                # 2D
#         theta <- 0.5 * atan(2*sqrt(lambda[1]*lambda[2]) / (lambda[1] - lambda[2]))
#         0.589*(sqrt(lambda[1]*cos(theta)^2 + lambda[2]*sin(theta)^2) +
#                sqrt(lambda[1]*sin(theta)^2 + lambda[2]*cos(theta)^2))
#     } else if(p == 3L) {         # 3D
#         varTotal <- sum(lambda)
#         V <- 2*sum(lambda^2) / varTotal^2
#         sqrt(varTotal*(1-(V/9)^3))
#     }
# }
#
# ## Shultz, ME. 1963. Circular error probability of a quantity affected by a bias
# CEPShultz <- function(ctr, sigma) {
#     sigma  <- as.matrix(sigma)
#     p      <- ncol(sigma)
#     aspRat <- sqrt(kappa(sigma, exact=TRUE))
#
#     ## make sure eigenvalues >= 0 when very small
#     ev     <- eigen(sigma)$values    # eigenvalues
#     lambda <- ev*sign(ev)
#
#     muH <- sqrt(sum(ctr^2))
#     sdC <- mean(sqrt(lambda))
#     CE90 <- 2.1272*sdC + 0.1674*muH + 0.3623*(muH^2/sdC) - 0.055*(muH^3/sdC)
# }
#
# ## Ager, TP. 2004. An Analysis of Metric Accuracy Definitions
# ## and Methods of Computation NIMA InnoVision
# CEPAger <- function(ctr, sigma) {
#     sigma  <- as.matrix(sigma)
#     p      <- ncol(sigma)
#     aspRat <- sqrt(kappa(sigma, exact=TRUE))
#
#     ## make sure eigenvalues >= 0 when very small
#     ev     <- eigen(sigma)$values    # eigenvalues
#     lambda <- ev*sign(ev)
#
#     muH <- sqrt(sum(ctr^2))
#     sdC <- mean(sqrt(lambda))
#
#     CE90 <- if(muH/sdC <= 0.1) {
#         2.1460*sdC
#     } else if(muH/sdC <= 3) {
#         2.1272*sdC + 0.1674*muH + 0.3623*(muH^2/sdC) - 0.055*(muH^3/sdC)
#     } else {
#         0.9860*muH + 1.4548*sdC
#     }
# }
