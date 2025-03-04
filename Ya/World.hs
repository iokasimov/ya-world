module Ya.World where

import Ya

import "ghc-prim" GHC.Prim (State#, RealWorld)
import "ghc-prim" GHC.Types (IO (IO))

type World = IO

instance Mapping U_I_II U_I_II Arrow Arrow World World
 where mapping = rewrap / \m x -> bindIO x (returnIO `fio` m)

instance Mapping U_I_II U_I_II Arrow Arrow (World `T'TT'I` L () World) World
 where mapping = rewrap / \m (T'TT'I ioio) -> bindIO (bindIO ioio unwrap) (returnIO `fio` m)

instance Mapping U_I_II U_I_II Arrow Arrow (Day U_I_II Arrow P P World World i ii) World
 where mapping = rewrap / \from (U_V_UU_UUU_UUUU_T'TT'I_II_III (These (These x y) (U_I_II f))) -> bindIO x (\xx -> from `compose` f `compose` These xx `fo` y)

instance Mapping U_I_II U_I_II Arrow Arrow (U_I_II Arrow ()) World
 where mapping = rewrap / \from (U_I_II f) -> returnIO `ha` from `li` f ()

returnIO :: a -> World a
returnIO x = IO (\ s -> (# s, x #))

bindIO :: World a -> (a -> World b) -> World b
bindIO (IO m) k = IO (\s -> case m s of (# new_s, x #) -> unIO (k x) new_s)

unIO :: World a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO (IO x) = x

pattern Raw, Out, World :: World e -> L () World e
pattern Raw e = Labeled e
pattern Out e = Labeled e
pattern World e = Labeled e
