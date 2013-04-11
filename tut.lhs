> {-# LANGUAGE GADTs, DeriveDataTypeable, ConstraintKinds,
>              PolyKinds, KindSignatures, DataKinds, TypeOperators,
>              TypeFamilies, UndecidableInstances,
>              MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
>              TypeHoles, StandaloneDeriving #-}

> import Control.Monad.RWS.Lazy
> import Control.Applicative
> import Control.Monad.Logic
> import GHC.TypeLits hiding (Nat)


LogicT Tutorial
----------------

A powerset implementation

> powerset :: [a] -> [[a]]
> powerset = observeAll . filterM (const (pure  True <|> pure False))

Let's build a pattern language that matches Haskell values
(Our goal is to arrive at something not unlike Edward Kmett's
Data.Analytics.Datalog.)

> type family Union (l :: [Symbol]) (r :: [Symbol]) :: [Symbol]
> type instance where
>   '[] `Union` r = r
>   '["A"] `Union` '["B"] = '["A", "B"]
> -- TODO: how to implement this?

> data Item :: [Symbol] -> * -> * where
>   Val :: Eq t => t -> Item '[] t
>   Var :: Sing (n :: Symbol) -> Item '[n] t
>   Pair :: (Eq (t, u), Show t, Show u) => Item v t -> Item w u -> Item (v `Union` w) (t, u)

> deriving instance Show t => Show (Item vars t)

> i1 :: Item '["A"] (Int, Int)
> i1 = Pair (Val 5) (Var (sing :: Sing "A"))
> i2 :: Item '["A", "B"] (Int, Int)
> i2 = Pair (Var (sing :: Sing "A")) (Var (sing :: Sing "B"))
> i3 = Val 42

We want to match two items and look for a solution(s)

> match :: MonadState s m => Item v t -> Item w t -> LogicT m (Item '[] t)
> match (Val t) (Val u) = do guard $ t == u; return $ Val t
> match (Pair t t') (Pair u u') = Pair <$> match t u <*> match t' u'
> -- match (Var s) (Val t) = elim s >>= return (Val t)

> -- elim :: Sing Symbol -> a

> match' :: Item v t -> Item w t -> LogicT (RWS () () ()) (Item '[] t)
> match' = match

> m0 = match' i3 i3
> m1 = match' (Val 1) i3
> m2 = match' (Pair (Val 1) i3) (Pair (Val 1) i3)
> m2' = observeAllT m2
> m3 = match' (Pair i3 i3) (Pair (Val 1) i3)

