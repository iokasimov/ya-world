module Ya.World where

import Ya

import "ghc-prim" GHC.Prim (State#, RealWorld)
import "ghc-prim" GHC.Types (IO (IO))

type World = IO

instance Mapping T'I'II T'I'II AR AR World World where
 mapping = rewrap `identity` \m x ->
  bindIO x (returnIO `fio` m)

instance Mapping T'I'II T'I'II AR AR (World `T'TT'I` World `L` World `T` Void) World where
 mapping = rewrap `identity` \m (T'TT'I ioio) ->
  bindIO (bindIO ioio supertype) (returnIO `fio` m)

instance Mapping T'I'II T'I'II AR AR (Day T'I'II AR P P World (World `L` World `T` Void) i ii) World where
 mapping = rewrap `identity` \from (U_V_UU_UUU_UUUU_T'TT'I_II_III (These (These x (Label y)) (T'I'II f))) -> 
  bindIO x (\xx -> from `compose` f `compose` These xx `fo` y)

instance Mapping T'I'II T'I'II AR AR (T'I'II AR Unit) World where
 mapping = rewrap `identity` \from (T'I'II f) ->
  returnIO `ha` from `har'st` f

instance Mapping T'I'II T'I'II (AR) (AR) (World `T'TT'I` S'I'II e `L` S'I'II e `T` Recursive) World where
 mapping = rewrap `identity` \source -> \(T'TT'I x) ->
  x `yok_` Label @_ @_ @Void
    `ha__` constant @(AR) (map @T'I'II @T'I'II source (T'TT'I x))
      `has` (\x' -> x' `ryu` Enter @World) `ha` source
    `ha__` supertype @(AR)
    `ha__` supertype @(AR)

instance Mapping T'I'II T'I'II (AR) (AR) (World `T'TT'I` I `L` I `T` Recursive) World where
 mapping = rewrap `identity` \source -> \(T'TT'I x) ->
  x `yok` Label @_ @_ @Void `ha` constant @(AR) (map @T'I'II @T'I'II @_ @_ @_ @World source (T'TT'I x)) `ha` supertype @(AR) `ha` supertype @(AR)

returnIO :: a -> World a
returnIO x = IO (\xx -> (# xx, x #))

bindIO :: World a -> (a -> World b) -> World b
bindIO (IO x) f = IO (\xx -> case x xx of (# xxx, xxxx #) -> unIO (f xxxx) xxx)

unIO :: World a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO (IO x) = x

pattern Await :: World e `AR__` World `L` World `T` Void `T` e
pattern Await e = Label e
