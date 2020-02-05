{-| 
Module      : Promise
Description : Monad promises as they can be defined out of monad references
Copyright   : (c) David Janin, 2019
License     : BSD (by default)
Maintainer  : David Janin <janin@labri.fr>
Stability   : experimental
Portability : Portable

This module is essentially for illustration purpose. Promises are
explicitly defined as they could be defined in, say, OCaml. We also
/re-define/ some existing type and function from Haskell library.


-}

module Promise where

import MonadReference

-- * Promises
    
-- | Monadic references and promises can be related as follows. There
-- we describe only the /type/ of promises. Strictly speaking, promises
-- are only monad actions created by (or equivalent with) forks of
-- monad action. In other words, an action @m@ is a promise when
--
-- > m = forkToRef m'
--
-- for some @m' :: m a@.
--
-- This type constructor is nevertheless a functor as describe by its
-- functor instance that is valid for that general thanks to semantic
-- laws given above. On the contrary, the monad instance is NOT valid
-- in general. While monad left unit and monad associativity laws are
-- satisfied in the general case as shown by simple counter examples
-- such as with
--
-- > f = \_ -> getChar >>= forkToRef . return 
--
-- for which we have @ f >>= return a != f a@ as defined in this
-- instance.  Indeed, the right action immediatly return a reference
-- to the next input character while the right action waits for that
-- character to be typed before returning a reference to that
-- character.
--
-- For the right unit monad law, one must restrict to functions of the
-- form @forkToRef . f@ with @f : a -> m b@, that is, one must restrict
-- to functions returning promises as defined above.
newtype Promise m a = Promise { thePromise :: m (MRef m a)}
        
instance MonadRef m => Functor (Promise m) where
    fmap f (Promise m) = Promise (fmapRef f m)
                                    
instance MonadRef m => Applicative (Promise m) where
    pure = return
    (Promise mf) <*> (Promise m)
        = Promise (mf >>= \r -> forkToRef (readRef r >>= \f -> m >>= readRef >>= \a -> return (f a)))
                                                                
instance MonadRef m => Monad (Promise m) where
    return = Promise . returnRef
    (>>=) (Promise m) f
        = Promise (bindRef m (thePromise . f))

-- | Turns a monad action into a promise. As with monad transformers, we have:
--
-- > liftRef . return = return
-- > liftRef (bind m >>= f) = bind (liftRef m) (liftRef . f)
--
-- However, 'Promise' is NOT  a monad transformer for it only applies to monad with references.
liftRef :: MonadRef m => m a -> Promise m a
liftRef = Promise . forkToRef

-- | Turns a promise into a monad action. As with monad transformers with a run, we have:
--
-- > runRef . liftRef = id
--
runRef :: MonadRef m => Promise m a -> m a
runRef p = (thePromise p) >>= readRef 
               
    
-- * Async recoded (as an illustration)

-- | > Async =  MRef IO a
type Async a = MRef IO a

-- | > async = forkToRef    
async :: IO a -> IO (Async a)
async m = forkToRef m

-- | > withAsync m = forkToref m >>= f
withAsync :: IO a -> (Async a -> IO b) -> IO b          
withAsync m f = async m >>= f

-- | > wait = readRef                
wait :: Async a -> IO a
wait = readRef
