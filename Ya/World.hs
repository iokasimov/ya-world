module Ya.World where

import Ya

import "ghc-prim" GHC.Prim (State#, RealWorld)
import "ghc-prim" GHC.Types (IO (IO))

type World = IO

instance Mapping Straight Straight Arrow Arrow World World
 where mapping = rewrap / \m x -> bindIO x (returnIO `fio` m)

instance Mapping Straight Straight Arrow Arrow (World `T'TT'I` L () World) World
 where mapping = rewrap / \m (T'TT'I ioio) -> bindIO (bindIO ioio unwrap) (returnIO `fio` m)

instance Mapping Straight Straight Arrow Arrow (Day Straight Arrow LM LM World World i ii) World
 where mapping = rewrap / \from (U_V_UU_UUU_UUUU_T'TT'I_II_III (These (These x y) (U_I_II f))) -> bindIO x (\xx -> from `compose` f `compose` These xx `fo` y)

instance Mapping Straight Straight Arrow Arrow (Straight Arrow ()) World
 where mapping = rewrap / \from (U_I_II f) -> returnIO `ha` from `li` f ()

returnIO :: a -> World a
returnIO x = IO (\ s -> (# s, x #))

bindIO :: World a -> (a -> World b) -> World b
bindIO (IO m) k = IO (\s -> case m s of (# new_s, x #) -> unIO (k x) new_s)

unIO :: World a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO (IO x) = x

pattern Raw :: World e -> L () World e
pattern Raw e = Labeled e
