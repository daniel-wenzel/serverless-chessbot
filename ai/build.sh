ghc --make -o +RTS -threaded -N -RTS ./bin/bot SmartBot.hs Main.hs 
strip  --strip-unneeded --remove-section=.comment ./bin/bot
du -a ./bin/bot
