
-- Test file 9: newtypes

newtype T1 = A Int
newtype T2 = B Int
newtype T3 = C Char
-- assert hash T1 == hash T2   isomorphic newtype = same hash
-- assert hash T1 /= hash T3   not isomorphic newtype = different hash
-- assert hash A == hash B     isomorphic newtype datacon = same hash
-- assert hash A /= hash C     not isomorphic newtype datacon = different hash

main = return ()
