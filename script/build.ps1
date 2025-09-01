cabal build kit --enable-tests
cabal test kit

$binFile = cabal list-bin kit
cp $binFile .\dist\kit.exe

