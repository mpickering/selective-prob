{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-simpl -O0 #-}
module Test where


import SelectiveProb
import Selective

ex = $$(runCodeF $ runToIO ex2)
--foo = $$(runCodeF $ runToIO (sample (cpure [|| ex ||]) 100))
--foo = $$(runCodeF $ runToIO $ sample ex2 100)

--qux = $$(distMean  $ sample t2c 1)
