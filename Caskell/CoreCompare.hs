
module Caskell.CoreCompare
(
    (==),
    compare,
    order
) where

import qualified GHC
import qualified Name
import qualified CoreSyn
import qualified Literal
import qualified Var
import qualified IdInfo
import qualified PatSyn
import qualified TcType
import qualified TyCon
import qualified DataCon
import qualified ConLike
import qualified Class
import qualified HscTypes
import qualified NameEnv
import qualified TyCoRep
import qualified Unique
import BasicTypes

import Caskell.Hash

-- gets the first non-EQ ordering, or EQ if its all EQ or null
order :: [Ordering] -> Ordering
order l = o where
    nl = dropWhile (==EQ) l
    o = if null nl then EQ else head nl

-- stable order of dataCons important for hashing
instance Ord DataCon.DataCon where
    -- TODO: compare the arguments
    compare a b = cmp where
      name_a = DataCon.dataConName a
      name_b = DataCon.dataConName b
      args_a = DataCon.dataConOrigArgTys a
      args_b = DataCon.dataConOrigArgTys b
      tag_a = DataCon.dataConTag a
      tag_b = DataCon.dataConTag b

      eq = name_a == name_b -- identical if EQ
      cmp1 = compare args_a args_b
      cmp2 = compare tag_a tag_b

      cmp = if eq then EQ else order [cmp1, cmp2]


instance Eq TyCoRep.Type where
    (==) (TyCoRep.TyVarTy avar) (TyCoRep.TyVarTy bvar) = avar == bvar
    (==) (TyCoRep.AppTy t1 t2) (TyCoRep.AppTy t3 t4) = (t1 == t3) && (t2 == t4)
    (==) (TyCoRep.TyConApp atcon aargs) (TyCoRep.TyConApp btcon bargs) = (atcon == btcon) && (aargs == bargs)
    (==) (TyCoRep.ForAllTy abndr aty) (TyCoRep.ForAllTy bbndr bty) = (abndr == bbndr) && (aty == bty)
    (==) a@(TyCoRep.FunTy{}) b@(TyCoRep.FunTy{}) =
        (TyCoRep.ft_af a == TyCoRep.ft_af b)
     && (TyCoRep.ft_arg a == TyCoRep.ft_arg b)
     && (TyCoRep.ft_res a == TyCoRep.ft_res b)

    (==) (TyCoRep.LitTy alt) (TyCoRep.LitTy blt) = alt == blt
    -- TODO: coercions
    (==) _ _ = False

core_type_datacon_order :: TyCoRep.Type -> Int
core_type_datacon_order (TyCoRep.TyVarTy _)    = 0
core_type_datacon_order (TyCoRep.AppTy _ _)    = 1
core_type_datacon_order (TyCoRep.TyConApp _ _) = 2
core_type_datacon_order (TyCoRep.ForAllTy _ _) = 3
core_type_datacon_order (TyCoRep.FunTy{})      = 4
core_type_datacon_order (TyCoRep.LitTy _)      = 5
core_type_datacon_order (TyCoRep.CastTy _ _)   = 6
core_type_datacon_order (TyCoRep.CoercionTy _) = 7

instance Ord TyCon.TyCon where
    compare a b = cmp where
        eq = TyCon.tyConUnique a == TyCon.tyConUnique b
        -- TODO: compare more...?
        cmp = if eq then EQ else LT

instance Ord TyCoRep.Type where
    compare (TyCoRep.TyVarTy avar) (TyCoRep.TyVarTy bvar) = compare avar bvar
    compare (TyCoRep.AppTy t1 t2) (TyCoRep.AppTy t3 t4) = order [compare t1 t3, compare t2 t4]

    compare (TyCoRep.TyConApp atcon aargs) (TyCoRep.TyConApp btcon bargs) 
        = order [compare atcon btcon, compare aargs bargs]
    
    compare (TyCoRep.ForAllTy abndr aty) (TyCoRep.ForAllTy bbndr bty)
        = order [compare abndr bbndr, compare aty bty]
        
    compare a@(TyCoRep.FunTy{}) b@(TyCoRep.FunTy{})
        = order [ compare (TyCoRep.ft_af a) (TyCoRep.ft_af b)
                , compare (TyCoRep.ft_arg a) (TyCoRep.ft_arg b)
                , compare (TyCoRep.ft_res a) (TyCoRep.ft_res b)
                ]

    compare (TyCoRep.LitTy alt) (TyCoRep.LitTy blt)
        = compare alt blt
    
    -- TODO: coercions
    
    compare a b = compare (core_type_datacon_order a) (core_type_datacon_order b)

instance (Eq a, Eq b) => Eq (Var.VarBndr a b) where
    Var.Bndr ax ay == Var.Bndr bx by = (ax == bx) && (ay == by)

instance (Ord a, Ord b) => Ord (Var.VarBndr a b) where
    compare (Var.Bndr ax ay) (Var.Bndr bx by) = cmp where
        xcmp = compare ax bx
        ycmp = compare ay by
        cmp = if xcmp /= EQ then xcmp else ycmp
