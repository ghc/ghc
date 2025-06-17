{-# LANGUAGE GHC2021 #-}

module T24396c where


f = id

{-# WARNING data f "warning on data level" #-}

data F

{-# WARNING type F "warning on type level" #-}


g = id

{-# DEPRECATED data g "deprecation on data level" #-}

data G

{-# DEPRECATED type G "deprecation on type level" #-}
