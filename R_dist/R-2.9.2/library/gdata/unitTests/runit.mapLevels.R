### runit.mapLevels.R
###------------------------------------------------------------------------
### What: Unit tests for mapLevels et al.
### $Id: runit.mapLevels.R 993 2006-10-30 17:10:08Z ggorjan $
### Time-stamp: <2006-10-29 16:41:41 ggorjan>
###------------------------------------------------------------------------

### {{{ --- Test setup ---

if(FALSE) {
  library("RUnit")
  library("gdata")
}

### }}}
### {{{ mapLevels, is.*, as.*, [.*

test.mapLevels <- function()
{
  ## Integer and numeric
  checkException(mapLevels(1:3)) # wrong class(x)
  checkException(mapLevels(1.5)) # wrong class(x)

  ## Factor
  f <- factor(c("B", "C", "A"))
  fMapInt <- list(A=as.integer(1), B=as.integer(2), C=as.integer(3))
  fMapInt1 <- list(B=as.integer(1), C=as.integer(2))
  fMapCha <- list(A="A", B="B", C="C")
  fMapInt <- as.levelsMap(fMapInt)
  fMapInt1 <- as.levelsMap(fMapInt1)
  fMapCha <- as.levelsMap(fMapCha)
  fMapCha1 <- fMapCha[c(1, 3)] # this will test also [.levelsMap
  checkIdentical(mapLevels(f), fMapInt)
  checkTrue(is.levelsMap(mapLevels(f))) # test for is.levelsMap
  checkTrue(is.levelsMap(fMapInt))      # test for as.levelsMap
  checkTrue(!gdata:::.isCharacterMap(fMapInt))
  checkIdentical(mapLevels(f, sort=FALSE), fMapInt) # sort is not used for factors
  checkIdentical(mapLevels(f[1:2], drop=TRUE), fMapInt1)
  checkIdentical(mapLevels(f, codes=FALSE), fMapCha)
  checkIdentical(mapLevels(f[c(2, 3)], drop=TRUE, codes=FALSE), fMapCha1)

  ## Character
  cha <- c("Z", "M", "A")
  chaMapInt <- list(A=as.integer(1), M=as.integer(2), Z=as.integer(3))
  chaMapIntO <- list(Z=as.integer(1), M=as.integer(2), A=as.integer(3))
  chaMapInt1 <- list(M=as.integer(1), Z=as.integer(2))
  chaMapCha <- list(A="A", M="M", Z="Z")
  chaMapInt <- as.levelsMap(chaMapInt)
  chaMapIntO <- as.levelsMap(chaMapIntO)
  chaMapInt1 <- as.levelsMap(chaMapInt1)
  chaMapCha <- as.levelsMap(chaMapCha)
  checkIdentical(mapLevels(cha), chaMapInt)
  checkIdentical(mapLevels(cha, sort=FALSE), chaMapIntO) # sort works for characters
  checkIdentical(mapLevels(cha[1:2], drop=TRUE), chaMapInt1)
  checkIdentical(mapLevels(cha, codes=FALSE), chaMapCha)

  ## List
  l <- list(f=f, cha=cha)
  l1 <- list(cha=cha, f=f)
  l2 <- list(cha=cha, f=f, i=1:10)
  lMapInt <- list(f=fMapInt, cha=chaMapInt)
  lMapCha <- list(f=fMapCha, cha=chaMapCha)
  lMapInt <- as.listLevelsMap(lMapInt)
  lMapCha <- as.listLevelsMap(lMapCha)
  lMapChaC <- as.list(sort(unique(c(cha, as.character(f)))))
  lMapChaCO <- as.list(unique(c(cha, as.character(f))))
  names(lMapChaC) <- unlist(lMapChaC)
  names(lMapChaCO) <- unlist(lMapChaCO)
  lMapChaC <- as.levelsMap(lMapChaC)
  lMapChaCO <- as.levelsMap(lMapChaCO)
  checkIdentical(mapLevels(l), lMapInt)
  checkTrue(is.listLevelsMap(mapLevels(l))) # test for is.listLevelsMap
  checkTrue(is.listLevelsMap(lMapInt))      # test for as.listLevelsMap
  checkIdentical(mapLevels(l, codes=FALSE), lMapCha)
  checkException(mapLevels(l, combine=TRUE)) # can not combine integer maps
  checkIdentical(mapLevels(l, codes=FALSE, combine=TRUE), lMapChaC)
  checkIdentical(mapLevels(l1, codes=FALSE, combine=TRUE), lMapChaC)
  checkIdentical(mapLevels(l1, codes=FALSE, combine=TRUE, sort=FALSE), lMapChaCO)
  checkException(mapLevels(l2)) # only char and factor

  ## Data.frame
  df <- data.frame(f1=factor(c("G", "Abc", "Abc", "D", "F")),
                   f2=factor(c("Abc", "Abc", "B", "D", "K")),
                         cha=c("jkl", "A", "D", "K", "L"),
                   int=1:5)
  dfMapInt <- list(f1=mapLevels(df$f1), f2=mapLevels(df$f2), cha=mapLevels(df$cha))
  dfMapInt <- as.listLevelsMap(dfMapInt)
  dfMapInt1 <- dfMapInt[c(1, 3)] # this will test also [.listLevelsMap
  checkException(mapLevels(df)) # wrong class of int
  checkIdentical(mapLevels(df[, 1:3]), dfMapInt)
  checkIdentical(mapLevels(df[, c(1, 3)]), dfMapInt1)
}

### }}}
### {{{ .check*

test.checkLevelsMap <- function(x)
{
  ## --- levelsMap ---

  ## not a list
  checkException(gdata:::.checkLevelsMap(x="A", method="raw"))
  ## list without names
  checkException(gdata:::.checkLevelsMap(x=list("A"), method="raw"))
  fMapInt <- list(A=as.integer(1), B=as.integer(2), C=as.integer(3))
  ## x should be levelsMap
  checkException(gdata:::.checkLevelsMap(x=fMapInt, method="class"))

  ## --- listLevelsMap ---

  map <- list(as.levelsMap(fMapInt), as.levelsMap(fMapInt))
  map1 <- list(fMapInt, fMapInt)
  class(map1) <- "listLevelsMap"
  ## x should be a listLevelsMap
  checkException(gdata:::.checkListLevelsMap(x=map, method="class"))
  ## x should be also a list of levelsMaps
  checkException(gdata:::.checkListLevelsMap(x=map1, method="class"))
  ## the rest is done with levelsMap tests
}

### }}}
### {{{ c.*

test.cLevelsMap <- function()
{
  f1 <- factor(letters[c(2, 1)])
  f2 <- factor(letters[c(3, 1, 2)])
  mapCha1 <- mapLevels(f1, codes=FALSE) # get maps
  mapCha2 <- mapLevels(f2, codes=FALSE)
  mapCha1S <- mapLevels(as.character(f1), codes=FALSE, sort=FALSE)
  mapCha2S <- mapLevels(as.character(f2), codes=FALSE, sort=FALSE)
  mapChaTest <- list(a="a", b="b")
  mapChaTest1 <- list(a="a", b="b", c="c")
  mapChaTest2 <- list(c="c", a="a", b="b")
  class(mapChaTest) <- class(mapChaTest1) <- class(mapChaTest2) <- "levelsMap"
  mapChaTest3 <- list(mapChaTest, mapChaTest1, mapChaTest, mapChaTest1)
  class(mapChaTest3)  <- "listLevelsMap"
  checkIdentical(c(mapCha1), mapChaTest)
  checkIdentical(c(mapCha2, mapCha1), mapChaTest1)
  checkIdentical(c(mapCha2S, mapCha1S, sort=FALSE), mapChaTest2)

  l <- list(f1, f2)
  mapCha <- mapLevels(l, codes=FALSE)
  checkIdentical(c(mapCha, mapCha), mapChaTest3)
  checkIdentical(c(mapCha, recursive=TRUE), mapChaTest1)

  checkException(c(mapLevels(f1))) # can not combine integer “levelsMaps”

  ## Example with maps of different length of components
  map1 <- list(A=c("a", "e", "i", "o", "u"), B="b", C="c", C="m",
               D=c("d", "e"), F="f")
  map2 <- list(A=c("a", "z", "w", "y", "x"), F="f", G=c("g", "h", "j"),
               i="i", k=c("k", "l"), B="B")
  map0Test <- list(A=c("a", "e", "i", "o", "u"), B="b", C="c", C="m",
                   D=c("d", "e"), F="f",
                   A=c("z", "w", "y", "x"), G=c("g", "h", "j"),
                   i="i", k=c("k", "l"), B="B")
  map0Test <- as.levelsMap(map0Test)
  mapTest <- sort(map0Test)
  map1 <- as.levelsMap(map1)
  map2 <- as.levelsMap(map2)
  map <- c(map1, map2)
  map0 <- c(map1, map2, sort=FALSE)
  checkIdentical(map, mapTest)
  checkIdentical(map0, map0Test)
}

### }}}
### {{{ unique

test.uniqueLevelsMap <- function()
{
  map <- list(A=c(1, 2, 1, 3), B=4, C=1, C=5, D=c(6, 8), E=7, B=4,
              D=c(6, 8))
  map1 <- map
  map1[[1]] <- map[[1]][c(1, 2, 4)]
  map1[[7]] <- NULL # remove B=4
  map1[[7]] <- NULL # remove D=c(6, 8)
  ## unique (used in as.levelsMap), will remove duplicates (A=1)
  checkIdentical(as.levelsMap(map1), as.levelsMap(map))
}

### }}}
### {{{ mapLevels<-

"test.mapLevels<-" <- function()
{
  ## Some errors
  checkException("mapLevels<-"(1.1, value=2))  # wrong class(x)
  checkException("mapLevels<-"(complex(1.1), value=2)) # wrong class(x)

  f <- factor(c("A", "B", "C"))
  fMapInt <- mapLevels(f)
  ## can not apply integer "levelsMap" to "character"
  checkException("mapLevels<-"(as.character(f), value=fMapInt))

  fMapCha <- mapLevels(f, codes=FALSE)
  ## can not apply character levelsMap to "integer"
  checkException("mapLevels<-"(as.integer(f), value=chaMapCha))

  fMapFuzz <- fMapInt
  fMapFuzz[[1]] <- "A"
  ## all components of 'value' must be of the same class
  checkException("mapLevels<-"(as.character(f), value=fMapFuzz))
  checkException("mapLevels<-"(as.integer(f), value=fMapFuzz))

  ## x integer, value integer levelsMap
  f <- factor(letters[c(10, 15, 1, 2)])
  fMapInt <- mapLevels(f)
  fInt <- as.integer(f)
  mapLevels(fInt) <- fMapInt
  checkIdentical(fInt, f)

  ## x factor, value integer levelsMap
  fInt <- factor(as.integer(f))
  mapLevels(fInt) <- fMapInt
  checkIdentical(fInt, f)

  ## above is essentially the same as levels<-.factor
  fInt1 <- factor(as.integer(f))
  levels(fInt1) <- fMapInt
  checkIdentical(fInt1, f)

  ## x character, value character levelsMap
  cha <- c("B", "A", "C")
  chaMapCha <- as.levelsMap(list(A1="A", B2="B", C3="C"))
  mapLevels(cha) <- chaMapCha
  chaTest <- factor(c("B2", "A1", "C3"))
  checkIdentical(cha, chaTest)
  ## and a bit more for components of length > 1
  cha <- c("G", "I", "B", "A", "C", "D", "Z")
  chaMapCha <- as.levelsMap(list(A1=c("A", "G", "I"), B2="B", C3=c("C", "D")))
  mapLevels(cha) <- chaMapCha
  chaTest <- factor(c("A1", "A1", "B2", "A1", "C3", "C3", NA))
  checkIdentical(cha, chaTest)

  ## x factor, value character levelsMap
  f <- factor(c("G", "I", "B", "A", "C", "D", "Z"))
  fMapCha <- as.levelsMap(list(A1=c("A", "G", "I"), B2="B", C3=c("C", "D")))
  mapLevels(f) <- fMapCha
  fTest <- factor(c("A1", "A1", "B2", "A1", "C3", "C3", NA))
  checkIdentical(f, fTest)

  ## Two factors and character map
  f1 <- factor(letters[1:10])
  f2 <- factor(letters[5:14])
  checkIdentical(as.integer(f1), as.integer(f2)) # the same integer codes
  mapCha1 <- mapLevels(f1, codes=FALSE) # get maps
  mapCha2 <- mapLevels(f2, codes=FALSE)
  mapCha <- c(mapCha1, mapCha2)         # combine maps
  ## apply map
  mapLevels(f1) <- mapCha # the same as levels(f1) <- mapCha
  mapLevels(f2) <- mapCha # the same as levels(f2) <- mapCha
  checkIdentical(as.integer(f1), 1:10) # \ internal codes are now
  checkIdentical(as.integer(f2), 5:14) # / "consistent" among factors

  ## The same with list
  l <- list(f1=f1, f2=f2)
  mapCha <- mapLevels(l, codes=FALSE, combine=TRUE)
  mapLevels(l) <- mapCha
  checkIdentical(as.integer(l$f1), 1:10) # \ internal codes are now
  checkIdentical(as.integer(l$f2), 5:14) # / "consistent" among factors

  ## and data.frame
  df <- data.frame(f1=f1, f2=f2)
  mapCha <- mapLevels(df, codes=FALSE, combine=TRUE)
  mapLevels(df) <- mapCha
  checkIdentical(as.integer(df$f1), 1:10) # \ internal codes are now
  checkIdentical(as.integer(df$f2), 5:14) # / "consistent" among factors

}

### }}}
### {{{ Dear Emacs
## Local variables:
## folded-file: t
## End:
### }}}

###------------------------------------------------------------------------
### runit.mapLevels.R ends here
