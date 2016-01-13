{-# LANGUAGE FlexibleInstances#-}
module Group where
    
class Group a where
    mulInv :: a -> a       -- multiplicative inverse
    mulId  :: a            -- multiplicative identity
              
    mul :: a -> a -> Maybe a     -- multiplication

