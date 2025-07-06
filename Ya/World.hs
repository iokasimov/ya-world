module Ya.World where

import Ya

import "ghc-prim" GHC.Prim (State#, RealWorld)
import "ghc-prim" GHC.Types (IO (IO))

type World = IO

instance Mapping T'I'II T'I'II AR AR World World
 where mapping = rewrap `identity` \m x -> bindIO x (returnIO `fio` m)

instance Mapping T'I'II T'I'II AR AR (World `T'TT'I` World `L` World `T` Void) World
 where mapping = rewrap `identity` \m (T'TT'I ioio) -> bindIO (bindIO ioio unwrap) (returnIO `fio` m)

instance Mapping T'I'II T'I'II AR AR (Day T'I'II AR P P World (World `L` World `T` Void) i ii) World
 where mapping = rewrap `identity` \from (U_V_UU_UUU_UUUU_T'TT'I_II_III (These (These x (Labeled y)) (T'I'II f))) -> bindIO x (\xx -> from `compose` f `compose` These xx `fo` y)

instance Mapping T'I'II T'I'II AR AR (T'I'II AR Unit) World
 where mapping = rewrap `identity` \from (T'I'II f) -> returnIO `ha` from `li` f Unit

returnIO :: a -> World a
returnIO x = IO (\ s -> (# s, x #))

bindIO :: World a -> (a -> World b) -> World b
bindIO (IO m) k = IO (\s -> case m s of (# new_s, x #) -> unIO (k x) new_s)

unIO :: World a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO (IO x) = x

pattern Raw, Out, World, Await :: World e -> (World `L` World `T` Void) e
pattern Raw e = Labeled e
pattern Out e = Labeled e
pattern World e = Labeled e
pattern Await e = Labeled e
