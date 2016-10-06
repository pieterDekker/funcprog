import System
a=1:2:drop 2(concat.zipWith replicate a.cycle$[1,2])
main=do args<-getArgs;putStr$concatMap((' ':).show)$take(read$args!!0::Int)a
