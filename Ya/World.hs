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
 where mapping = rewrap `identity` \from (U_V_UU_UUU_UUUU_T'TT'I_II_III (These (These x (Label y)) (T'I'II f))) -> bindIO x (\xx -> from `compose` f `compose` These xx `fo` y)

instance Mapping T'I'II T'I'II AR AR (T'I'II AR Unit) World
 where mapping = rewrap `identity` \from (T'I'II f) -> returnIO `ha` from `li` f Unit

instance Mapping T'I'II T'I'II (AR) (AR) (World `T'TT'I` S'I'II e `L` S'I'II e `T` Recursive) World where
 mapping = rewrap `identity` \source -> \(T'TT'I x) ->
  x `yok_` Label @_ @_ @Void
    `ha__` constant @(AR) (map @T'I'II @T'I'II source (T'TT'I x))
      `la` intro @World @(AR) `ha` source
    `ha__` unwrap @(AR)
    `ha__` unwrap @(AR)

instance Mapping T'I'II T'I'II (AR) (AR) (World `T'TT'I` I `L` I `T` Recursive) World where
 mapping = rewrap `identity` \source -> \(T'TT'I x) ->
  x `yok` Label @_ @_ @Void `ha` constant @(AR) (map @T'I'II @T'I'II @_ @_ @_ @World source (T'TT'I x)) `ha` unwrap @(AR) `ha` unwrap @(AR)

returnIO :: a -> World a
returnIO x = IO (\ s -> (# s, x #))

bindIO :: World a -> (a -> World b) -> World b
bindIO (IO m) k = IO (\s -> case m s of (# new_s, x #) -> unIO (k x) new_s)

unIO :: World a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO (IO x) = x

pattern Raw, Out, World, Await :: World e `AR__` World `L` World `T` Void `T` e
pattern Raw e = Label e
pattern Out e = Label e
pattern World e = Label e
pattern Await e = Label e
